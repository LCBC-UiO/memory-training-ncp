# This function creates Figure 1 in the main manuscript.
create_figure_1 <- function(memdat_long){
  
  ggplot(memdat_long, aes(x = time, y = HundredWords_Total,
                          group = CrossProject_ID)) +
    geom_line(alpha = .2, size = .3) +
    geom_point(aes(color = factor(trainings)), size = .8) +
    facet_grid(cols = vars(age_group)) +
    labs(color = "Trainings") +
    scale_color_colorblind() + 
    xlab("Years since baseline") +
    ylab("Hundred words total score") +
    theme(
      legend.position = c(.8, .8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    ) + 
    scale_x_sqrt()
}
