log_loss_estimation <- 
  function(data, column = "predictions"){
    
    class_0_N <- data %>% filter(Class == 0) %>% nrow()
    class_1_N <- data %>% filter(Class == 1) %>% nrow()
    
    pos_class = data$Class
    neg_class = 1 - data$Class
    
    pos_prob = data[[column]]
    pos_prob = sapply(pos_prob, replace_extreme_probability)
    
    neg_prob = 1 - data[[column]]
    
    total_pos_class = sum(pos_class * log(pos_prob))
    pos_class_w = total_pos_class * 1/ class_1_N
  
    total_neg_class = sum(neg_class * log(neg_prob))
    neg_class_w = total_neg_class * 1/ class_0_N
  
    log_loss = -(neg_class_w + pos_class_w)/2
    
    return(log_loss)
    # When comparing models using balanced log loss, lower values indicate better performance. 
    
  }
