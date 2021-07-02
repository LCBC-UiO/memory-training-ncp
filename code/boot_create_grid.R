boot_create_grid <- function(){
  grid <- crossing(
    age_group = factor(
      c("Young", "Older"), levels = levels(memdat$age_group)),
    time = seq(from = 0, to = 10, by = .1),
    trainings_dummy1 = 1, trainings_dummy2 = 0,
    retests_dummy1 = 0, trainings = 1, retests = 0,
    retests_dummy1om = 0, retests_dummy2om = 0,
    SexMale = 0
  ) %>%
    mutate(
      time_since_training = time,
      age_dev = time - mean(time)
    )
  
  grid0 <- grid %>%
    mutate(
      trainings_dummy1 = 0, time_since_training = 0,
      trainings = 0
    )
  
  list(grid0 = grid0, grid = grid)
  
}