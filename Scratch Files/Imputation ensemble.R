
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/Master Packages.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/Master Functions.R")

train <- read_csv("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/true train.csv") %>% 
  select(-`...1`)

train <- define_new_features(DATA = train)
train$EJ <- ifelse(train$EJ == "A", 1, 0)
# impute train data 

# md.pattern(train)

train_id_class_cols <- train %>% select(Id, Class)
train_impute_cols <- train %>% select(-Id, -Class)

imp1 <- mice(train_impute_cols, m = 50, 
             method = rep("pmm", length(train_impute_cols)))


which(sapply(imp1$imp, nrow) > 0) %>% names() -> cols_to_impute# these are all variables that we imputed

train_impute_cols_f <- train_impute_cols

for(i in 1:length(cols_to_impute)){
 
  col_iter <- cols_to_impute[i]
  
  train_impute_cols_f[[col_iter]][is.na(train_impute_cols_f[[col_iter]]) ] <- imp1$imp[[col_iter]][,2]
}

train_f <- 
  cbind(train_impute_cols_f, train_id_class_cols)

xg_boost_lm_1_model <- 
  xgboost(
    data = train_f %>% select(-Id, -Class) %>% as.matrix(), 
    label = train_f %>% select(Class) %>% as.matrix(), 
    max.depth = 25, 
    eta = .05, 
    nthread = 2, 
    nrounds = 100, 
    objective = "binary:logistic"
    )

###########
# Test 


test <- read_csv("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/true validation.csv") %>% 
  select(-`...1`)

test <- define_new_features(DATA = test)
test$EJ <- ifelse(test$EJ == "A", 1, 0)
# impute train data 

# md.pattern(train)

test_id_class_cols <- test %>% select(Id, Class)
test_impute_cols <- test %>% select(-Id, -Class)

imp1_test <- mice(test_impute_cols, m = 50, 
             method = rep("pmm", length(test_impute_cols)))


which(sapply(imp1_test$imp, nrow) > 0) %>% names() -> cols_to_impute_test # these are all variables that we imputed

test_impute_cols_f <- test_impute_cols

for(i in 1:length(cols_to_impute_test)){
 
  col_iter <- cols_to_impute_test[i]
  
  test_impute_cols_f[[col_iter]][is.na(test_impute_cols_f[[col_iter]]) ] <- imp1_test$imp[[col_iter]][,2]
}

test_f <- 
  cbind(test_impute_cols_f, test_id_class_cols)

predict(xg_boost_lm_1_model, test_f %>% select(-Id, -Class) %>% as.matrix()) -> predicted_prob

test_f$predictions <- predicted_prob

ggplot(data = test_f, 
       aes(x = predictions, color = as.factor(Class), group = as.factor(Class))) + geom_density()

summary(test_f %>% filter(Class == 1) %>% select(predictions))

log_loss_estimation(data = test_f)


###########
# Upsample data and retrain the model 

table(train_f$Class)

SMOTE(train_f %>% select(-Id), train_f$Class, K = 5, dup_size = 5) -> smoted_train

smoted_train_f <- smoted_train$data  %>% select(-class)

table(smoted_train_f$Class)

smoted_xg_boost_lm_1_model <- 
  xgboost(
    data = smoted_train_f %>% select(-Class) %>% as.matrix(), 
    label = smoted_train_f %>% select(Class) %>% as.matrix(), 
    max.depth = 25, 
    eta = .05, 
    nthread = 2, 
    nrounds = 100, 
    objective = "binary:logistic"
    )

xgb.save(smoted_xg_boost_lm_1_model, './Models/xgb.model')

test_load <-xgb.load('./Models/xgb.model')

test_f <- 
  cbind(test_impute_cols_f, test_id_class_cols)

predict(test_load, test_f %>% select(-Id, -Class) %>% as.matrix()) -> predicted_prob

test_f$predictions <- predicted_prob

ggplot(data = test_f, 
       aes(x = predictions, color = as.factor(Class), group = as.factor(Class))) + geom_density()

summary(test_f %>% filter(Class == 1) %>% select(predictions))

log_loss_estimation(data = test_f)

