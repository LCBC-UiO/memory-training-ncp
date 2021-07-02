compute_hippo_slope_memory_followup_correlation <- function(df){
  df %>%
    nest_by(age_group) %>%
    pmap_dfr(function(age_group, data){
      tst_cross_slope <- with(
        data, cor.test(hwt, hippo_slope, method = "pearson"))
      
      tibble(
        `Age group` = age_group,
        `Pearson correlation` = sprintf("%.2f (p=%.3f, CI: [%.2f, %.2f])",
                                        tst_cross_slope$estimate,
                                        tst_cross_slope$p.value,
                                        tst_cross_slope$conf.int[[1]],
                                        tst_cross_slope$conf.int[[2]])
      )
    }) %>%
    pivot_longer(cols = -`Age group`, names_to = "What", values_to = "Correlation") %>%
    knitr::kable()
}