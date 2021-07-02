tabulate_memory_parameters <- function(mod, memdat, response = "HundredWords_Total"){
  estimates <- summary(mod)$tTable %>% 
    bind_cols(var = rownames(.), .) %>% 
    dplyr::select(-Value, -`Std.Error`) %>% 
    bind_cols(
      as.data.frame(intervals(mod, level = .95, which = "fixed")$fixed)
    ) %>% 
    rename(est = `est.`) %>% 
    mutate(
      across(c(lower, est, upper), 
             ~ . * sd(memdat[[!!response]]) + 
               str_detect(var, "b0\\.age") * mean(memdat[[!!response]])),
      age_group = factor(coalesce(
        str_extract(var, "Older"), str_extract(var, "Young"), "Common effect"),
        levels = c(levels(memdat$age_group), "Common effect")
      ),
      var = str_remove(var, "\\.age\\_group"),
      var = str_remove(var, "Young"),
      var = str_remove(var, "Older"),
      var = factor(
        recode(var, `b0` = "Intercept",
               `b0.SexMale` = "Male - female offset",
               `ba:age_dev` = "Age slope",
               `br.retests_dummy1om` = ">=1 retest",
               `bt:trainings_dummy1` = "1st training",
               `bt:trainings_dummy2` = "2nd training",
               lambda = "Exp. decay"),
        levels = c("Intercept", "Male - female offset", "Age slope", ">=1 retest",
                   "1st training", "2nd training", "Exp. decay")
      )
    ) %>% 
    dplyr::select(age_group, var, lower, est, upper, 
                  DF, `t-value`, `p-value`)
  
  estimates %>%
    dplyr::select(-`t-value`, -DF) %>%
    mutate(
      across(c(lower, est, upper), ~ if_else(abs(.) < 10, sprintf("%.2f", .), sprintf("%.1f", .))),
      across(c(`p-value`), ~ if_else(. < 1e-3, sprintf("%.1e", .), sprintf("%.3f", .)))
    ) %>%
    arrange(age_group, var) %>%
    mutate(
      est = glue("{est} ({lower}, {upper})")
    ) %>%
    dplyr::select(-lower, -upper) %>%
    dplyr::select(age_group, var, est, `p-value`) %>%
    rename(
      `Age group` = age_group,
      Variable = var,
      `Estimate (95% CI)` = est
    ) %>%
    knitr::kable()
}