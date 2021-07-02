# This function creates Supplementary Figure 4
create_figure_S4 <- function(memdat_long){
  memdat_long %>%
    filter(time_since_training < 10) %>%
    ggplot(aes(x = time_since_training)) +
    facet_wrap(vars(age_group)) +
    geom_histogram(binwidth = 1/12) +
    xlab("Years since last training")
}