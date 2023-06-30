
data.frame(
  
  variables = 
    colnames(train), 
  
  n_na_vals = 
    train %>% 
      summarise(
        t(across(everything(), ~ length(which(is.na(.)))))
        
      ) %>% unlist(), 
  
  n_unique_values = 
    train %>% 
      summarise(
        t(across(everything(), ~ length(unique(.))))
        
      ) %>% unlist(), 
  
  variable_type = 
    train %>% 
      summarise(
        t(across(everything(), ~class(.)))
      )%>% unlist()
    
) %>% magrittr::set_rownames(NULL) %>% 
  filter(!variables %in% c("Id", "EJ", "Class")) %>% 
  
  mutate(
    percent_na = n_na_vals / nrow(train)
  ) -> 
  
  metadata 





data.frame(
  
  variables = 
    colnames(valid), 
  
  n_na_vals = 
    valid %>% 
      summarise(
        t(across(everything(), ~ length(which(is.na(.)))))
        
      ) %>% unlist(), 
  
  n_unique_values = 
    valid %>% 
      summarise(
        t(across(everything(), ~ length(unique(.))))
        
      ) %>% unlist(), 
  
  variable_type = 
    valid %>% 
      summarise(
        t(across(everything(), ~class(.)))
      )%>% unlist()
    
) %>% magrittr::set_rownames(NULL) %>% 
  filter(!variables %in% c("Id", "EJ", "Class")) %>% 
  
  mutate(
    percent_na = n_na_vals / nrow(valid)
  ) -> 
  
  metadata_valid


greeks <- read_csv("/Users/denisostroushko/Desktop/R/GitRepos/icr_kaggle_2023/01 Data/greeks.csv")

greeks_w_label <- 
  greeks %>% 
  left_join(
    train %>% select(Id, Class), 
    by = "Id"
  )

View(greeks_w_label)

greeks_w_label %>% 
  filter(Class %in% c(0,1)) %>% 
  group_by(Alpha) %>% 
  summarise(n = n(), 
            p = n()/nrow(.)
            ) %>% 
  arrange(Alpha) 

