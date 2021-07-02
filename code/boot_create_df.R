boot_create_df <- function(bootgrid, memdat){
  bootgrid %>%
    group_by(age_group, time) %>%
    summarise(
      across(starts_with("fit"), list(
        lower = ~ quantile(., probs = .025, names = FALSE),
        upper = ~ quantile(., probs = .975, names = FALSE))),
      .groups = "drop"
    ) %>%
    mutate(
      fit0_est = predfun(mod, grid0),
      fit_est = predfun(mod, grid)
    ) %>%
    mutate(
      across(starts_with("fit"),
             ~ as.numeric(.) * sd(memdat$HundredWords_Total) + mean(memdat$HundredWords_Total))
    ) %>%
    pivot_longer(
      cols = starts_with("fit"),
      names_to = c("type", ".value"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(
      traintype = if_else(str_detect(type, "fit0"), "No training", "Training at time 0")
    )
}