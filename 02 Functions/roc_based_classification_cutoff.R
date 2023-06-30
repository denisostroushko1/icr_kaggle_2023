
roc_based_classification_cutoff <- 
  function(
    PREDS, 
    CLASS){
     ins_roc <- 
      roc(
        response = CLASS, 
        predictor = PREDS
      )
    
    roc_df <- data.frame(
      fpr = ins_roc$specificities,
      tpr = ins_roc$sensitivities,
      cutoff = ins_roc$thresholds
    )
    
    roc_df$plot_fpr = 1 - roc_df$fpr
    roc_df <- roc_df %>% arrange(tpr, plot_fpr)

    roc_df$distnace = 
      with(roc_df, 
           sqrt(
             (1-tpr)^2 + 
               (0 - plot_fpr)^2
           ))
    
    return(
      roc_df %>% filter(distnace == min(distnace)) %>% select(cutoff) %>% unlist()
    )
  }