
pivot_comp_df <- function(comp_df){
  comp_df %>% 
    arrange(AIC) %>% 
    pivot_longer(cols = c(AIC, BIC)) %>% 
    group_by(name) %>% 
    mutate(value = value - value[Model == "Full model"]) %>% 
    ungroup()
  
}