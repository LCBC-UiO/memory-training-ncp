create_figure_2 <- function(bootgrid, memdat, mod, grids){
  bootdf <- bootgrid %>%
    group_by(age_group, time) %>%
    summarise(
      across(starts_with("fit"), list(
        lower = ~ quantile(., probs = .025, names = FALSE),
        upper = ~ quantile(., probs = .975, names = FALSE))),
      .groups = "drop"
    ) %>%
    mutate(
      fit0_est = boot_predfun(mod, grids$grid0),
      fit_est = boot_predfun(mod, grids$grid)
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
  
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  bootdf %>%
    filter(time <= 5) %>%
    ggplot(aes(x = time, y = est, ymin = lower, ymax = upper, fill = traintype)) +
    geom_line() +
    geom_ribbon(data = filter(bootdf, time <= 5), alpha = .3) +
    facet_wrap(vars(age_group), scales = "free_y") +
    scale_fill_manual(values = cbPalette[-1]) +
    xlab("Years since training") +
    ylab("Level") +
    labs(fill = NULL, linetype = NULL) +
    theme(
      legend.position = c(.35, .8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
}