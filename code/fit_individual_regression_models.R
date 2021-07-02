fit_individual_regression_models <- function(
  corrdat, formula1, formula2, mod1_name, mod2_name,
  beta1 = "time", beta2 = "time"
  ){
  corrdat %>%
    # Split the dataframe by CrossProject_ID, creating a sub-dataframe per participant
    nest_by(CrossProject_ID) %>%
    pmap_dfr(
      # This function is applied once for each CrossProject_ID
      function(CrossProject_ID, data){
        mod1 <- coef(lm(formula1, data = data))
        mod2 <- coef(lm(formula2, data = data))
        # Put the results in a dataframe which is returned
        tibble(
          CrossProject_ID = CrossProject_ID,
          age_group = unique(data$age_group),
          !!mod1_name := mod1[[!!beta1]],
          !!mod2_name := mod2[[!!beta2]]
        )
      }
    )
}
