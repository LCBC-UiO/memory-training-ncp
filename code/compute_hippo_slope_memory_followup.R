compute_hippo_slope_memory_followup <- function(corrdat_hippo3){
  corrdat_hippo3 %>%
    # Create one dataframe per participant
    nest_by(CrossProject_ID) %>%
    pmap_dfr(function(CrossProject_ID, data){
      # Extract the 100-words score at follow-up
      hwt <- data %>%
        filter(NCP_Condition == "follow-up") %>%
        pull(HundredWords_Total)
      
      # Compute hippocampal volume slope during training
      hippo_slope <- coef(lm(
        hippocampus ~ time,
        data = filter(data, NCP_Condition != "follow-up")))[["time"]]
      
      # Return a dataframe with the values
      tibble(
        CrossProject_ID = CrossProject_ID, age_group = unique(data$age_group),
        hwt = hwt, hippo_slope = hippo_slope
      )
    })
}