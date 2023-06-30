
replace_extreme_probability <- 
  function(x){
    max(min(x, 1 - 1e-15), 1e-15)
  }