create_figure_3 <- function(memdat_wp_long){
  ggplot(memdat_wp_long, aes(x = time, y = WordPair_1,
                             group = CrossProject_ID)) +
    geom_line(alpha = .2) +
    geom_point(aes(color = factor(trainings))) +
    facet_grid(cols = vars(age_group)) +
    labs(color = "Trainings") +
    xlab("Years since baseline") +
    ylab("Word pair 1 score") +
    ggthemes::scale_color_colorblind() +
    theme(
      legend.position = c(.9, .905),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key.height = unit(.5, "cm")
    ) + 
    scale_x_sqrt()
}