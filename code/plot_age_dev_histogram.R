plot_age_dev_histogram <- function(memdat_long){
  ggplot(memdat_long, aes(x = age_dev)) + 
    geom_histogram(binwidth = .5) +
    facet_grid(cols = vars(age_group)) +
    xlab("Difference from mean age in age group (years)")
}