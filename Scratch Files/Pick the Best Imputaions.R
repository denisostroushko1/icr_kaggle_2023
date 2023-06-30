

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

imp1 <- mice(train_impute_cols, m = 25, 
             method = rep("pmm", length(train_impute_cols)))

plot(imp1)

imp1 <- mice(train_impute_cols, m = 50, 
             method = rep("pmm", length(train_impute_cols)))

plot(imp1)

imp1 <- mice(train_impute_cols, m = 100, 
             method = rep("pmm", length(train_impute_cols)))

plot(imp1)

## 50 Imputations seems to be the sweetspot

###########
# Imputation using NORM does not make sense becasue the mean of some predicted values appears to be below 0. 
# There should be no biomarkers and health data that take on negative values in the context of this competiton

imp1 <- mice(train_impute_cols, m = 25, 
             method = rep("norm", length(train_impute_cols)))

plot(imp1)


imp2 <- mice(train_impute_cols, m = 50, 
             method = rep("norm", length(train_impute_cols)))

plot(imp2)


