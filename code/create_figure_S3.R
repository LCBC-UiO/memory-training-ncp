create_figure_S3 <- function(memdat_long){
  memdat_long %>%
    group_by(CrossProject_ID) %>%
    filter(any(NCP_Condition == "post-train"), any(NCP_Condition == "follow-up"),
           NCP_Condition %in% c("follow-up", "post-train")) %>%
    filter(time == nth(time, n() - 1) | time == nth(time, n())) %>%
    summarise(interval = max(time) - min(time), .groups = "drop") %>%
    ggplot(aes(x = interval)) +
    geom_histogram(binwidth = 1/6) + 
    xlab("Time between last training and follow-up")
}