create_figure_5_data <- function(mod, grids){
  # Zero out terms not related to differences
  inds <- c("age_groupYoung", "age_groupOlder", "icv_z", "SexMale",
            "age_groupYoung:time", "age_groupOlder:time")
  
  # Linear predict matrix
  Xp <- predict(mod$gam, newdata = grids$grid, type = "lpmatrix")
  Xp0 <- predict(mod$gam, newdata = grids$grid0, type = "lpmatrix")
  bhat <- coef(mod$gam)
  S <- vcov(mod$gam)
  S[inds, inds] <- 0
  
  Xpdiff <- Xp - Xp0
  diff <- Xpdiff %*% bhat
  diffse <- sqrt(diag(Xpdiff %*% S %*% t(Xpdiff)))
  
  df <- grids$grid %>%
    mutate(
      fit1 = as.numeric(predict(mod$gam, newdata = grids$grid)),
      fit0 = as.numeric(predict(mod$gam, newdata = grids$grid0)),
      diff = as.numeric(diff),
      diffse = diffse,
      diff_lower = diff + qnorm(.025) * diffse,
      diff_upper = diff + qnorm(.975) * diffse,
      trained = factor(
        recode(trained, `none` = "Baseline",
               `now` = "Post training", `prev` = "Post rest"),
        levels = c("Baseline", "Post training", "Post rest"))
    ) %>%
    group_by(age_group) %>%
    mutate(
      across(c(time, fit1, fit0, diff), list(last =~ lag(.)))
    )
  
  
  df$trained_seg <- df$trained
  levels(df$trained_seg) <- c("Baseline", "Training period", "Rest period")
  
  df
}