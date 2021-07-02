
create_figure_4 <- function(slopedat){
  
  mod_old <- lm(hwt_slope ~ wp1_slope, data = filter(slopedat, age_group == "Older"))
  mod_young <- lm(hwt_slope ~ wp1_slope, data = filter(slopedat, age_group == "Young"))
  
  curve_fits <- tibble(
    age_group = factor(c("Young", "Older"), levels = c("Young", "Older")),
    intercept = c(coef(mod_young)[[1]], coef(mod_old)[[1]]),
    slope = c(coef(mod_young)[[2]], coef(mod_old)[[2]]),
    rsquared = unname(TeX(paste0("$R^{2}=$", round(c(summary(mod_young)$r.squared, summary(mod_old)$r.squared), 3)))),
    xpos = c(-40, -40),
    ypos = c(60, 60)
  )
  
  slopedat %>%
    mutate(age_group = factor(age_group, levels = c("Young", "Older"))) %>%
    ggplot(aes(x = wp1_slope, y = hwt_slope)) +
    geom_point() +
    geom_abline(data = curve_fits, aes(slope = slope, intercept = intercept)) +
    geom_text(data = curve_fits, aes(x = xpos, y = ypos, label = rsquared), parse = TRUE, size = 3) +
    xlab("Word pair 1 slope") +
    ylab("Hundred words total slope") +
    facet_wrap(vars(age_group)) +
    theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
}