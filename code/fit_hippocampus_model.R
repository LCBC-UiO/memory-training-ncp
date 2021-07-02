fit_hippocampus_model <- function(hippodat_long){
  # Use mgcv::gamm rather than nlme::lme to get better prediction functions
  mod <- gamm(
    # Fixed effect formula
    hippocampus ~ 0 + age_group + age_group : time +
      age_group : trained + icv_z + Sex,
    # Dataset containing only participants with two or more observations
    data = hippodat_long,
    # Random intercept and slope per participant
    random = list(CrossProject_ID =~ time),
    method = "ML"
  )
}