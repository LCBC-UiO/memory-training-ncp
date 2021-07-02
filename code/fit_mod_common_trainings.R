fit_mod_common_trainings <- function(memdat_long){
  nlme(
    model = hwt_z ~ b0 + ba + 
      bt * exp(-lambda * time_since_training) + br,
    data = memdat_long,
    fixed = list(
      b0 ~ 0 + SexMale + age_group, 
      ba ~ 0 + age_group:age_dev,
      bt ~ 0 + trainings_dummy1 + trainings_dummy2,
      lambda ~ 0 + age_group,
      br ~ 0 + age_group:retests_dummy1 + 
        age_group:retests_dummy2om
    ),
    random = b0 ~ 1,
    groups = ~ CrossProject_ID,
    start = c(
      b0 = c(0, -.9, -.1), ba = c(0, 0), bt = rep(1, 2), 
      lambda = c(0, 0), br = rep(.2, 4)
    )
  )
}