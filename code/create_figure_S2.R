create_figure_S2 <- function(hippodat_long){
  hippodat_long %>%
    arrange(CrossProject_ID, time) %>%
    group_by(CrossProject_ID) %>%
    filter(trained %in% c("now", "prev"), lead(trained) %in% c("now", "prev")) %>%
    mutate(dt = (lead(time, default = 0) - time) * 52) %>%
    filter(dt > 0) %>%
    ggplot(aes(x = dt)) + geom_histogram(bins = 10) +
    xlab("Weeks since previous training") 
}