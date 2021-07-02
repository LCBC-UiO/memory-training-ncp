fit_mod_common_single_retests <- function(memdat_long, response = "hwt_z"){
  nlme(
    model = as.formula(paste(response, " ~ b0 + ba + bt * exp(-lambda * time_since_training) + br")),
    data = memdat_long,
    fixed = list(
      b0 ~ 0 + SexMale + age_group, 
      ba ~ 0 + age_group:age_dev,
      bt ~ 0 + age_group:trainings_dummy1 + 
        age_group:trainings_dummy2,
      lambda ~ 0 + age_group,
      br ~ 0 + retests_dummy1om
    ),
    random = b0 ~ 1,
    groups = ~ CrossProject_ID,
    start = c(
      b0 = c(0, -.9, -.1), ba = c(0, 0), bt = rep(1, 4), 
      lambda = c(0, 0), br = .2
    )
  )
}