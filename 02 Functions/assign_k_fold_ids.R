
assign_k_fold_ids <- 
  function(data, 
           number_K){
    
    data$fold_id <- sample(1:number_K, size = nrow(data), replace = TRUE)
    
    return(data)
  }