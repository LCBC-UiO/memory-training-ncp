create_hippocampus_grid <- function(hippodat_long){
  # Training times
  tt1 <- .2
  tt2 <- .6
  
  grid <- crossing(
    time = c(0, .2, .4, .6, .8),
    age_group = factor(levels = levels(hippodat_long$age_group)),
    icv_z = 0, Sex = "Female",
  ) %>%
    mutate(
      trained = case_when(
        time < tt1 ~ "none",
        round(time, 3) == tt1 ~ "now",
        time < tt2 ~ "prev",
        round(time, 3) == tt2 ~ "now",
        time < 1 ~ "prev",
        TRUE ~ "none"
      )
    )
  
  grid0 <- grid %>%
    mutate(trained = "none")
  
  list(grid0 = grid0, grid = grid)
}