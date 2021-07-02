compute_correlation_estimates <- function(slopedat, slope1, slope2){
  slopedat %>%
    nest_by(age_group) %>%
    pmap_dfr(function(age_group, data){
      tst_cross_slope <- cor.test(pull(data, !!slope1), pull(data, !!slope2))
      
      tibble(
        `Age group` = age_group,
        `Slope correlation` = sprintf("%.2f (%.2f, %.2f)", tst_cross_slope$estimate,
                                      tst_cross_slope$conf.int[[1]], tst_cross_slope$conf.int[[2]])
      )
    }) %>%
    pivot_longer(cols = -`Age group`, names_to = "What", values_to = "Correlation") %>%
    knitr::kable()
}

