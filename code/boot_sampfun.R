boot_sampfun <- function(fitted, data, response) {
  dd <- data %>%
    mutate(
      pred = as.numeric(predict(fitted, levels = 1)),
      res = as.numeric(residuals(fitted))
    )
  ## sample groups with replacement
  iv <- as.character(unique(data$CrossProject_ID))
  bsamp1 <- sample(iv, size = length(iv), replace = TRUE)
  ## within groups, sample *residuals* with replacement
  map_dfr(bsamp1, function(x) {
    dd %>%
      filter(CrossProject_ID == x)
  }, .id = "boot_id") %>%
    group_by(boot_id) %>% # comment this line to get global residual bootstrap
    mutate(
      !!response := pred + sample(res, size = n(), replace = TRUE)
    ) %>%
    ungroup()
}