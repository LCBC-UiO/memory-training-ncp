fit_mod_full <- function(memdat_long){
  nlme(
    # hwt_z is Z-transformed HundredWords_Total
    model = hwt_z ~ b0 + ba + 
      bt * exp(-lambda * time_since_training) + br,
    # take dataset containing only participants with more than one measurement
    data = memdat_long,
    fixed = list(
      # define an intercept per age group and a Male-vs-Female difference
      b0 ~ 0 + SexMale + age_group, 
      # aging effect, estimate separately per age group
      ba ~ 0 + age_group:age_dev,
      # immediate training effect of the first and then second training
      bt ~ 0 + age_group:trainings_dummy1 + 
        age_group:trainings_dummy2,
      # exponential decay of training effect, per age group
      lambda ~ 0 + age_group,
      # retest effect per age group and for 1 and for >=2
      br ~ 0 + age_group:retests_dummy1 + 
        age_group:retests_dummy2om
    ),
    # random intercept
    random = b0 ~ 1,
    # define the grouping variable corresponding to participant id
    groups = ~ CrossProject_ID,
    start = c(
      b0 = c(0, -.9, -.1), ba = c(0, 0), bt = rep(1, 4), 
      lambda = c(0, 0), br = rep(.2, 4)
    )
  )
}