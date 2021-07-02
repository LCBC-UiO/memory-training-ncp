illustrate_training_effect_parameters <- function(trainings){
  
  df <- if(trainings == 1L){
    crossing(
      time = seq(from = -.2, to = 4, by = .01),
      beta = c(-1, 5),
      lambda = c(-.02, 0, .2, 2)
    ) %>% 
      mutate(
        f = (time >= 0) * beta * exp(-lambda * time),
        beta = case_when(
          beta < 0 ~ glue("Negative~effect: beta[t21]=={beta}"),
          beta > 0 ~ glue("Positive~effect: beta[t21] == {beta}"),
          TRUE ~ NA_character_
        ))
  } else {
    crossing(
      time = seq(from = -.2, to = 4, by = .01),
      beta1 = 5,
      beta2 = 6,
      lambda = c(.2, 1, 2)
    ) %>% 
      mutate(
        trainings = (time >= 0) + (time >= 1),
        time_since_training = if_else(time <= 1, time, time - 1),
        f = ((trainings == 1) * beta1 + (trainings == 2) * beta2) * exp(-lambda * time_since_training),
        beta = glue("beta[t21] == {beta1} ~ and ~ beta[t22] == {beta2}")
      )
  }

  
  annot_df <- df %>% 
    filter(time == max(time)) %>% 
    mutate(
      l = sprintf("%.2f", lambda),
      label = glue("lambda[2] == {l}")
    )
  
  ggplot(df, aes(x = time, y = f, group = lambda)) +
    geom_line(alpha = .5) +
    geom_blank(aes(x = time * 1.1, y = f * 1.1)) +
    geom_text(data = annot_df, aes(x = time, y = f, label = label),
              parse = TRUE, nudge_x = .3) +
    facet_wrap(vars(beta), scales = "free_y", labeller = label_parsed) +
    xlab("Time since training") +
    ylab("Level")
  
}