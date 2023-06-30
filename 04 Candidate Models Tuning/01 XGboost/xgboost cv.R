
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

n_rounds_options = c(100, 200, 300, 400, 500)
n_rounds_options = c(25, 50, 75, 100, 125)
n_rounds_options = c(.01, .025, .05, .075, .1)

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
           color = Impute)) + geom_line(size = 1, alpha = .5) + 
  scale_color_gradientn(colours = rainbow(5))
