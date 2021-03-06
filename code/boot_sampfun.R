boot_sampfun <- function(fitted, data, response, boot_type = "case-ir") {
  dd <- data %>%
    mutate(
      pred = as.numeric(predict(fitted, levels = 1)),
      res = as.numeric(residuals(fitted))
    )
  ## sample groups with replacement
  iv <- as.character(unique(data$CrossProject_ID))
  bsamp1 <- sample(iv, size = length(iv), replace = TRUE)
  ## within groups, sample *residuals* with replacement
  bootdat <- map_dfr(bsamp1, function(x) {
    dd %>% 
      filter(CrossProject_ID == x)
  }, .id = "boot_id")
  
  ## if TRUE, within groups, sample *residuals* with replacement
  if(boot_type == "case-ir"){
    bootdat <- bootdat %>% 
      group_by(boot_id)
  }
  
  bootdat %>% 
    mutate(
      !!response := pred + if_else(
        !!boot_type == "case-none", res, sample(res, size = n(), replace = TRUE))
    ) %>% 
    ungroup()
}