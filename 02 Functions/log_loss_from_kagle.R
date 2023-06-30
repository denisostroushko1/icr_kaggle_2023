log_loss_from_kagle <- 
  function(data, column = "predictions"){
  
    N_0 = sum(1 - data$Class)
    N_1 = sum(data$Class)
    # calculate the weights for each class to balance classes
    w_0 = 1 / N_0
    w_1 = 1 / N_1
    # calculate the predicted probabilities for each class
    p_1 = data$predictions
    p_0 = 1 - p_1
    
    # calculate the summed log loss for each class
    log_loss_0 = sum((1 -  data$Class) * log(p_0))
    log_loss_1 = -sum(data$Class * log(p_1))
    # calculate the weighted summed logarithmic loss
    # (factgor of 2 included to give same result as LL with balanced input)
    balanced_log_loss = 2*(w_0 * log_loss_0 + w_1 * log_loss_1) / (w_0 + w_1)
    # return the average log loss
    ressy = balanced_log_loss/(N_0+N_1)
    
    return(ressy)
    
    }