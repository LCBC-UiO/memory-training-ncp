compute_hippo_slope_overall_memory_fits <- function(corrdat_hippo2){
  corrdat_hippo2 %>%
    nest_by(CrossProject_ID, age_group) %>%
    pmap_dfr(function(CrossProject_ID, age_group, data){
      # Intercept of memory throughout period
      hwt_level <- coef(lm(HundredWords_Total ~ time,
                           data = filter(data, !is.na(HundredWords_Total))))["(Intercept)"]
      # Hippocampal slope
      hippo_slope <- coef(lm(hippocampus ~ time,
                             data = filter(data, NCP_Condition != "follow-up")))["time"]
      # Return values
      tibble(
        CrossProject_ID = CrossProject_ID, age_group = age_group,
        hwt_level = hwt_level, hippo_slope = hippo_slope
      )
    })
}