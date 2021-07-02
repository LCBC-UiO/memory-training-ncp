compare_models <- function(models){
  # Call anova() function repeatedly
  
  comp_df <- eval(
    parse(
      text = paste0("anova(", paste(paste0("models[[", seq_along(models), "]]"),
                                    collapse = ", "), ", test = FALSE)")
    )
  )
  
  comp_df$call <- NULL
  comp_df$Model <- names(models)[comp_df$Model]
  as_tibble(comp_df) %>% 
    arrange(Model)
}