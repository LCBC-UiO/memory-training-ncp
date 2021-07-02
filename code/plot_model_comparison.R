plot_model_comparison <- function(comp_df){

  comp_df_long <- pivot_comp_df(comp_df)
 
  
  ggplot(comp_df_long, aes(x = Model, y = value, group = name, color = name)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, color = "gray") + 
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = c(.8, .5)
    ) +
    labs(color = NULL) +
    xlab("") +
    ylab("AIC/BIC difference") +
    coord_flip() + 
    ggthemes::scale_color_colorblind() 
}