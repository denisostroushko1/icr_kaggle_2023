
set.seed(17682636)

source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/Master Packages.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/Master Functions.R")

data <- read_csv("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/train.csv")

# Split data into training and validation sets using stratified sampling
trainIndex <- createDataPartition(data$Class, p = 0.7, list = FALSE, times = 1)

training_data <- data[trainIndex, ]
validation_data <- data[-trainIndex, ]

nrow(training_data)
nrow(validation_data)

table(training_data$Class)
table(validation_data$Class)

table(training_data$Class)/nrow(training_data)
table(validation_data$Class)/nrow(validation_data)


write.csv(training_data, "/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/true train.csv")
write.csv(validation_data, "/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/true validation.csv")

