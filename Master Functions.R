
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/define_new_features.R")

source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/log_loss_estimation.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/log_loss_from_kagle.R")

source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/pretty_roc_curve.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/roc_based_classification_cutoff.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/replace_extreme_probability.R")

source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/create_smote_train.R")
source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/assign_k_fold_ids.R")

source("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/02 Functions/pull_and_fill_ith_impute.R")


list_of_predictors <- 
  
  c('AB','AF','AH','AM','AR','AX','AY','AZ','BC','BD',
    'BN','BP','BQ','BR','BZ','CB','CC','CD','CF','CH','CL',
    'CR','CS','CU','CW','DA','DE','DF','DH','DI','DL','DN','DU',
    'DV','DY','EB','EE','EG','EH','EJ','EL','EP','EU','FC','FD','FE',
    'FI','FL','FR','FS','GB','GE','GF','GH','GI','GL')
  
