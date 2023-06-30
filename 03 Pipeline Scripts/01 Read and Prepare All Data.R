

train <- read_csv("./01 Data/true train.csv") %>% 
  select(-`...1`)

train <- define_new_features(DATA = train)
train$EJ <- ifelse(train$EJ == "A", 1, 0)
# impute train data 

greeks <- read_csv("./01 Data/greeks.csv")

# add Alpha from greeks to make sure we can do smote-ing later 

train_f <- 
  train %>% 
  left_join(
    greeks %>% select(Id, Alpha), 
    by = "Id"
  )

# add K-fold (10-fold) cross validation id for the data for hyperparameter tuning. 

