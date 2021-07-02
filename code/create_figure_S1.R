# Create Supplementary Figure 1
create_figure_S1 <- function(bootgrid, memdat, mod, grids){
  bootdiffdf <- bootgrid %>% 
    group_by(age_group, time) %>% 
    summarise(
      se = sd(fit),
      se0 = sd(fit0),
      .groups = "drop"
    ) %>% 
    mutate(
      diff_est = boot_predfun(mod, grids$grid) - boot_predfun(mod, grids$grid0),
      diff_se = sqrt(se^2 + se0^2),
      diff_lower = diff_est + qnorm(.025) * diff_se,
      diff_upper = diff_est + qnorm(.975) * diff_se,
      across(starts_with("diff"), ~ . * sd(memdat$HundredWords_Total))
    )
  
  lab_df <- bootdiffdf %>% 
    group_by(age_group) %>% 
    filter(diff_lower < 0) %>% 
    filter(diff_lower == max(diff_lower)) %>% 
    ungroup() %>% 
    mutate(
      lb = glue("{time} years")
    )
  
  ggplot(bootdiffdf, aes(x = time, y = diff_est, ymin = diff_lower, ymax = diff_upper)) +
    geom_line() +
    geom_hline(yintercept = 0, alpha = .4) +
    geom_ribbon(alpha = .4) +
    geom_label(data = lab_df, aes(y = diff_lower, label = lb)) +
    facet_wrap(vars(age_group)) +
    xlab("Years since training") +
    ylab("Difference between training and non-training")
}