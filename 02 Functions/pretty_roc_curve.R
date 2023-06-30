
pretty_roc_curve <- 
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

    my_colors <- colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(5)
    # Create plot
    
    ggplot(data = roc_df, aes(x = plot_fpr, y = tpr, color = cutoff)) +
      geom_path(size = 1) +
      scale_color_gradientn(colors = my_colors) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_x_continuous(labels = seq(from = 1, to = 0, by = -.25)) + 
      labs(title = paste0("ROC Curve. AUC: ", round(ins_roc$auc, 2)), 
           x = "False Positive Rate", y = "True Positive Rate", color = "Cutoff") + 
      theme_minimal()
  }

