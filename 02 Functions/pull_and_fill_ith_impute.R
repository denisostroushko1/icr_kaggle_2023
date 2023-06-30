
pull_and_fill_ith_impute <- 
  function(
    data_to_populate, 
    columns_to_populate, 
    all_imputations, 
    imputed_values_set_to_pull){
    
    df = data_to_populate
    
    for(i in 1:length(columns_to_populate)){
       
        col_iter <- columns_to_populate[i]
        
        df[[col_iter]][is.na(df[[col_iter]]) ] <- 
          all_imputations$imp[[col_iter]][,imputed_values_set_to_pull]
      }

    return(df)
  }


