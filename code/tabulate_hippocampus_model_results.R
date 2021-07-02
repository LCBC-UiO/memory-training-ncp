tabulate_hippocampus_model_results <- function(mod){
  nicepars <- c(
    "Xage_groupYoung" = "Young intercept",
    "Xage_groupOlder" = "Older intercept",
    "Xicv_z" = "ICV (1 standard deviation)",
    "XSexMale" = "Male-female diff",
    "Xage_groupYoung:time" = "Young slope",
    "Xage_groupOlder:time" = "Older slope",
    "Xage_groupYoung:trainednow" = "Young trained now offset",
    "Xage_groupOlder:trainednow" = "Older trained now offset",
    "Xage_groupYoung:trainedprev" = "Young trained prev offset",
    "Xage_groupOlder:trainedprev" = "Older trained prev offset"
  )
  
  intervals(mod, which = "fixed")$fixed %>%
    as.data.frame() %>%
    cbind(var = rownames(.), .) %>%
    mutate(
      `p-value` = summary(mod)$tTable[, "p-value"]
    ) %>%
    mutate(
      across(c(lower, `est.`, upper), ~ if_else(. > 100, sprintf("%.0f", .), sprintf("%.1f", .))),
      across(`p-value`, ~ if_else(. < 1e-3, sprintf("%.1e", .), sprintf("%.3f", .))),
      Description = nicepars[var],
      `Estimate (95% CI)` = glue("{est.} ({lower}, {upper})")
    ) %>%
    rename(Parameter = var) %>%
    dplyr::select(Parameter, Description, `Estimate (95% CI)`, `p-value`) %>%
    knitr::kable(row.names = FALSE)
}