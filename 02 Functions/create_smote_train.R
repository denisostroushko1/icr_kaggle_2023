
create_smote_train <- 
  function(
    input_train_data, 
    smote_type){
    
    if(!smote_type %in% c("target class balance", 
                          "class balance alpha preserve", 
                          "alpha balance")){
      print("Unknown SMOTE request")
    }
    
    if(smote_type == "target class balance"){
      
      SMOTE(train_f %>% select(-Id), train_f$Class, K = 5, dup_size = 5) -> smoted_train
      smoted_train$data %>% select(-class) -> final_data
      
    }
    
    if(smote_type == "class balance alpha preserve" & 
       !"Alpha" %in% colnames(input_train_data)){
      
      print("Column `Alpha` not provided")
      
    }
    
    if(smote_type == "class balance alpha preserve"){
      
       # turn Alpha into a numeric variable 
      input_train_data$Alpha_numeric <- 
        factor(input_train_data$Alpha, 
               levels = sort(unique(input_train_data$Alpha)), 
               labels = seq_along(sort(unique(input_train_data$Alpha)))
               ) %>% 
        as.numeric(.)
      
      length(which(input_train_data$Alpha_numeric != 1)) -> total_non_1
      length(which(input_train_data$Alpha_numeric == 1)) -> total_1
      
      # Smote second alpha 
      
        # I want to make sure that each alpha level is as repserented as 0 class, so we gonna have imbalance going the other way 
      
      dup_size_calculated = total_1 / total_non_1
  
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 2 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_calculated) -> smote_alpha_2
    
      smote_alpha_2$data %>% nrow()
      smote_alpha_2$data %>% select(Class) %>% table()
      smote_alpha_2$data %>% select(class) %>% table()
      
      smote_alpha_2$syn_data %>% select(-class) -> alpha_2_syn_data
      
      # Smote third alpha 
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 3 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_calculated) -> smote_alpha_3
    
      smote_alpha_3$data %>% nrow()
      smote_alpha_3$data %>% select(Class) %>% table()
      smote_alpha_3$data %>% select(class) %>% table()
      
      smote_alpha_3$syn_data %>% select(-class) -> alpha_3_syn_data
      
      # Smote fourth alpha 
      dup_size_4 =  dup_size_calculated
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 4 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_calculated) -> smote_alpha_4
    
      smote_alpha_4$data %>% nrow()
      smote_alpha_4$data %>% select(Class) %>% table()
      smote_alpha_4$data %>% select(class) %>% table()
      
      smote_alpha_4$syn_data %>% select(-class) -> alpha_4_syn_data
      
      #final sythetic and original data 
      final_data <- 
        rbind(train_f %>% select(-Id) %>% mutate(Alpha_numeric = 1), 
              alpha_2_syn_data %>% mutate(Alpha_numeric = 2),
              alpha_3_syn_data %>% mutate(Alpha_numeric = 3),
              alpha_4_syn_data %>% mutate(Alpha_numeric = 4))
      
    }
    
    
    if(smote_type == "alpha balance" & 
       !"Alpha" %in% colnames(input_train_data)){
      
      print("Column `Alpha` not provided")
      
    }
    
    if(smote_type == "alpha balance"){
      
      # turn Alpha into a numeric variable 
      input_train_data$Alpha_numeric <- 
        factor(input_train_data$Alpha, 
               levels = sort(unique(input_train_data$Alpha)), 
               labels = seq_along(sort(unique(input_train_data$Alpha)))
               ) %>% 
        as.numeric(.)
      
      # Smote second alpha 
      
        # I want to make sure that each alpha level is as repserented as 0 class, so we gonna have imbalance going the other way 
      
      dup_size_2 = round(table(input_train_data$Alpha_numeric)[1]/table(input_train_data$Alpha_numeric)[2])
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 2 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_2) -> smote_alpha_2
    
      smote_alpha_2$data %>% nrow()
      smote_alpha_2$data %>% select(Class) %>% table()
      smote_alpha_2$data %>% select(class) %>% table()
      
      smote_alpha_2$syn_data %>% select(-class) -> alpha_2_syn_data
      
      # Smote third alpha 
      dup_size_3 = round(table(input_train_data$Alpha_numeric)[1]/table(input_train_data$Alpha_numeric)[3])
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 3 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_3) -> smote_alpha_3
    
      smote_alpha_3$data %>% nrow()
      smote_alpha_3$data %>% select(Class) %>% table()
      smote_alpha_3$data %>% select(class) %>% table()
      
      smote_alpha_3$syn_data %>% select(-class) -> alpha_3_syn_data
      
      # Smote fourth alpha 
      dup_size_4 = round(table(input_train_data$Alpha_numeric)[1]/table(input_train_data$Alpha_numeric)[4])
      SMOTE(X = input_train_data %>% select(-Id, -Alpha, -Alpha_numeric), 
            target = 
              case_when(
                input_train_data$Alpha_numeric == 4 ~ 1, 
                T ~ 0
              ), 
            K = 5, 
            dup_size = dup_size_4) -> smote_alpha_4
    
      smote_alpha_4$data %>% nrow()
      smote_alpha_4$data %>% select(Class) %>% table()
      smote_alpha_4$data %>% select(class) %>% table()
      
      smote_alpha_4$syn_data %>% select(-class) -> alpha_4_syn_data
      
      #final sythetic and original data 
      final_data <- 
        rbind(input_train_data %>% select(-Id, -Alpha) %>% mutate(Alpha_numeric = 1), 
              alpha_2_syn_data %>% mutate(Alpha_numeric = 2),
              alpha_3_syn_data %>% mutate(Alpha_numeric = 3),
              alpha_4_syn_data %>% mutate(Alpha_numeric = 4))
    }
    
    return(final_data)
    
  }