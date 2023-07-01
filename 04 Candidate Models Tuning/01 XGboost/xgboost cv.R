
rm(list = ls())







source("Master Functions.R")
source("Master Packages.R")

# read in the data 
source("./03 Pipeline Scripts/01 Read and Prepare All Data.R")
      # output of this file is `train_f`

# Example of a call of XGboost model 
              # 
              # example_xgboost <- 
              #   xgboost(
              #     data = smoted_train_f %>% select(-Class) %>% as.matrix(), 
              #     label = smoted_train_f %>% select(Class) %>% as.matrix(), 
              #     max.depth = 25, 
              #     eta = .05, 
              #     nthread = 2, 
              #     nrounds = 100, 
              #     objective = "binary:logistic"
              #     )

CV_K <- 10
N_imputes <- 50

# impute once to SMOTE 
# Then create fold ids and store them 

# then start 10-fold cross validation: 
  # in each iteration 
  # split into fold-train and fold-test 
    # within each of 10-folds do 50 imputations and predictions: 
    # for each of 1 to 50 imputations 
    # impute, smote, train XGboost, predcict. 
    # store predictions into a very large prepared data set

train_id_class_cols <- train_f %>% select(Id, Class, Alpha)
train_impute_cols <- train_f %>% select(-Id, -Class, -Alpha) # this data set will be used over and over again  
                                                            # with the new imputed values 

# impute these data 50 times. Now we can use one imputation to create a file of SMOTE size 
imp1_train <- mice(train_impute_cols, m = N_imputes, 
             method = rep("pmm", length(train_impute_cols)))
      # these values will be used to populate missing values in the `train_impute_cols` at each iteration 

which(sapply(imp1_train$imp, nrow) > 0) %>% names() -> cols_to_impute_train # these are all variables that we imputed

train_impute_cols_f <- 
  pull_and_fill_ith_impute(
    data_to_populate = train_impute_cols, 
    columns_to_populate = cols_to_impute_train, 
    all_imputations = imp1_train, 
    imputed_values_set_to_pull = 1)

complete_train_f <- 
  cbind(train_impute_cols_f, train_id_class_cols)

full_train_f <- create_smote_train(input_train_data = complete_train_f, smote_type = "alpha balance")

set.seed(12584)
fold_ids <- assign_k_fold_ids(data = full_train_f, number_K = 10) %>% select(fold_id) %>% unlist() %>% 
  set_names("")

# NOW We can create the process of K-fold cross validation 


##################################################
# OPTION 1: ITERATE OVER IMPUTATIONS AND CV-FOLDS
##################################################

# results data

results = expand.grid(
  Impute = c(1:N_imputes), 
  Fold = c(1:CV_K)
) %>% 
  mutate(auc = rep(NA, nrow(.)), 
         log_loss = rep(NA, nrow(.))
         )

time_start = Sys.time()

for( IMPUTES in 1:N_imputes){
  
  # use stored imputations to fill in missing values with ith set of missing values 
  train_impute_iter_f <- 
    pull_and_fill_ith_impute(
      data_to_populate = train_impute_cols, 
      columns_to_populate = cols_to_impute_train, 
      all_imputations = imp1_train, 
      imputed_values_set_to_pull = IMPUTES)
  
  complete_iter_train_f <- 
    cbind(train_impute_iter_f, train_id_class_cols)
  
  full_iter_train_f <- 
    create_smote_train(
      input_train_data = complete_iter_train_f, 
      smote_type = "alpha balance"
      ) %>% select(-Alpha_numeric)
  
  full_iter_train_f$fold_id = fold_ids
  
  for(FOLD in 1:CV_K){
    
    print(paste0("Started iteration for", IMPUTES, "th Imputation set and ", FOLD, "th CV fold"))
    
    full_iter_train_f_cv_train <- full_iter_train_f %>% filter(fold_id != FOLD) %>% select(-fold_id)
    full_iter_train_f_cv_test  <- full_iter_train_f %>% filter(fold_id == FOLD) %>% select(-fold_id)
    
    # train the model
    cv_iter_model = 
      xgboost(
        verbose = 0, 
        
        data = full_iter_train_f_cv_train %>% select(-Class) %>% as.matrix(),
        label = full_iter_train_f_cv_train %>% select(Class) %>% as.matrix(),
        max.depth = 25,
        eta = .05,
        nthread = 2,
        nrounds = 100,
        objective = "binary:logistic"
        )
    
    full_iter_train_f_cv_test$predictions <- 
      predict(cv_iter_model, full_iter_train_f_cv_test %>% select(-Class) %>% as.matrix())
    
    results[results$Impute == IMPUTES & 
              results$Fold == FOLD,]$auc <- # Sroting AUC for this fold for this imputation set  
      
      auc(
        roc(predictor = full_iter_train_f_cv_test$predictions, 
            response = full_iter_train_f_cv_test$Class)
        )
    
    results[results$Impute == IMPUTES & 
              results$Fold == FOLD,]$log_loss <- 
      log_loss_estimation(data = full_iter_train_f_cv_test)
    
  }
}

time_end = Sys.time()

print(paste0(round(time_start - time_end, 4), " seconds between start and end of CV"))

summary(results$log_loss)

hist(results$log_loss)

ggplot(data = results, 
       aes(x = Impute, y = log_loss, 
           group = as.factor(Fold), 
           color = as.factor(Fold))) + geom_line(size = 1, alpha = .5)


ggplot(data = results, 
       aes(x = Fold, y = log_loss, 
           group = Impute, 
           color = Impute)) + geom_line(size = 1, alpha = .1) + 
  scale_color_gradientn(colours = rainbow(5))

####################################################################
# OPTION 1 SUMMARY: IT APPEARS THAT 50 IMPUTATIONS CONVERGE! 
####################################################################


####################################################################
# OPTION 2: ADD THREE MORE LOOPING VARIABLES FOR XGboost TRAINING, Remove looping over imputation sets for 
#                 hyperparameter tuning 
####################################################################

# create one imputed and smoted data set that we will use to create approximate hyperparameter tuning 

train_impute_cols_f <- 
  pull_and_fill_ith_impute(
    data_to_populate = train_impute_cols, 
    columns_to_populate = cols_to_impute_train, 
    all_imputations = imp1_train, 
    imputed_values_set_to_pull = 1)

complete_train_f <- 
  cbind(train_impute_cols_f, train_id_class_cols)

full_train_hyperparameter_train <- 
  create_smote_train(input_train_data = complete_train_f, smote_type = "alpha balance") %>% 
  select(-Alpha_numeric)

full_train_hyperparameter_train$fold_id = fold_ids

eta_learning_rate_options = c(0.01, 0.05, 0.1)
max_depth_options = c(10, 20, 30)
n_rounds_options = c(100, 300, 500)

results2 = expand.grid(
  Fold = c(1:CV_K), 
  Eta = c(1:length(eta_learning_rate_options)),
  Depth = c(1:length(max_depth_options)),
  Rounds = c(1:length(n_rounds_options))
) %>% 
  mutate(auc = rep(NA, nrow(.)), 
         log_loss = rep(NA, nrow(.)), 
         train_time_sec = rep(NA, nrow(.))
         )

# start collecting the data 

iteration_current = 1
time_start = Sys.time()

for(E in 1:length(eta_learning_rate_options)){
  for(D in 1:length(max_depth_options)){
    for(R in 1:length(n_rounds_options)){
        for(FOLD in 1:CV_K){
          
          print(paste0("Total iteration: ", iteration_current, 
                       ". Using Learning Rate ", eta_learning_rate_options[E], 
                       ". Using depth of trees ", max_depth_options[D], 
                       ". Using rounds parameter ", n_rounds_options[R],
                       ". ", FOLD, "th CV fold"))
          
          full_iter_train_f_cv_train <- full_train_hyperparameter_train %>% filter(fold_id != FOLD) %>% select(-fold_id)
          full_iter_train_f_cv_test  <- full_train_hyperparameter_train %>% filter(fold_id == FOLD) %>% select(-fold_id)
          
          # train the model
          
          train_start = Sys.time()
          cv_iter_model = 
            xgboost(
              verbose = 0, 
              
              data = full_iter_train_f_cv_train %>% select(-Class) %>% as.matrix(),
              label = full_iter_train_f_cv_train %>% select(Class) %>% as.matrix(),
              max.depth = max_depth_options[D],
              eta = eta_learning_rate_options[E],
              nthread = 2,
              nrounds = n_rounds_options[R],
              objective = "binary:logistic"
              )
          train_end= Sys.time()
          
          full_iter_train_f_cv_test$predictions <- 
            predict(cv_iter_model, full_iter_train_f_cv_test %>% select(-Class) %>% as.matrix())
          
          results2[ 
            results2$Depth == D & 
            results2$Eta == E & 
            results2$Rounds == R & 
            results2$Fold == FOLD,]$auc <- # Sroting AUC for this fold for this imputation set  
            
            auc(
              roc(predictor = full_iter_train_f_cv_test$predictions, 
                  response = full_iter_train_f_cv_test$Class)
              )
          
          
          results2[ 
            results2$Depth == D & 
            results2$Eta == E & 
            results2$Rounds == R & 
            results2$Fold == FOLD,]$log_loss <- 
            
            log_loss_estimation(data = full_iter_train_f_cv_test)
          
          results2[ 
            results2$Depth == D & 
            results2$Eta == E & 
            results2$Rounds == R & 
            results2$Fold == FOLD,]$train_time_sec <- train_end - train_start
          
          
          iteration_current = iteration_current + 1 
        }
      }
    }
}

time_end = Sys.time()

print(paste0(round(time_end - time_start, 4), " minutes between start and end of CV"))

results2 %>% 
  group_by(
    Eta, Depth, Rounds
  ) %>% 
  summarise(mean_loss = mean(log_loss), 
            sd = sd(log_loss)) %>% 
  arrange(mean_loss) %>% 
  View()

# best parameters: 

eta_learning_rate_options[3]
max_depth_options[1]

