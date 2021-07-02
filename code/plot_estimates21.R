plot_estimates21 <- function(estimates){
  ggplot(estimates, aes(x = var, y = est, group = age_group, color = age_group)) +
    geom_point(position = position_dodge(width = .1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2,
                  position = position_dodge(width = .1)) +
    theme(
      legend.position = c(.3, .8),
      panel.border = element_blank(),
      axis.line = element_line(),
      panel.grid = element_blank(),
      strip.background = element_blank()
      ) +
    labs(color = NULL) +
    xlab("") +
    ylab("Value") +
    ggthemes::scale_color_colorblind()
}