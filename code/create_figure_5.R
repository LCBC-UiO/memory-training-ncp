create_figure_5 <- function(df, group){
  group_df <- df %>% 
    filter(age_group == !!group)
  
  p1 <- ggplot(group_df, aes(x = time, y = fit1))  +
    geom_segment(data = filter(group_df, !is.na(fit1_last)), 
                 aes(x = time_last, xend = time, y = fit1_last,
                     yend = fit1, color = trained_seg)) +
    geom_line(aes(y = fit0), linetype = "dashed") +
    geom_point(aes(color = trained_seg)) +
    labs(color = "Training condition") +
    xlab("Years since baseline") +
    ylab(TeX("Hippocampal volume")) +
    ggthemes::scale_color_colorblind() +
    theme(
      legend.position = c(.25, .25),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    ) +
    labs(color = NULL)
  
  p2 <- ggplot(group_df, aes(x = time, y = diff, ymin = diff_lower, ymax = diff_upper)) +
    geom_segment(data = filter(group_df, !is.na(fit1_last)),
                 aes(x = time_last, xend = time, y = diff_last,
                     yend = diff, color = trained_seg)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(alpha = .4) +
    geom_point(aes(color = trained_seg)) +
    ggthemes::scale_color_colorblind() +
    theme(legend.position = "none") +
    xlab("Years since baseline") +
    ylab(TeX("Hippocampal volume difference")) +
    theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    ) +
    labs(color = NULL)
  
  p1 + p2
}