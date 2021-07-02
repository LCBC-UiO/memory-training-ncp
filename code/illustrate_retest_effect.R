illustrate_retest_effect <- function(knots){
  df <- if(knots == 2L){
    tribble(
      ~ id, ~ beta_r11, ~ beta_r12,
      "1", 3, 4,
      "2", 3, 6,
      "3", 5, 7
    ) %>% 
      mutate(retests = list(0:4)) %>% 
      unnest(cols = retests) %>% 
      mutate(
        f = (retests == 1) * beta_r11 + (retests >= 2) * beta_r12
      )
  } else if(knots == 1L){
    tribble(
      ~ id, ~ beta_r1,
      "1", 3,
      "2", 7
    ) %>% 
      mutate(retests = list(0:4)) %>% 
      unnest(cols = retests) %>% 
      mutate(
        f = (retests >= 1) * beta_r1
      )
  }
  
  annot_df <- if(knots == 2L){
    df %>% 
      filter(retests == max(retests)) %>% 
      mutate(
        label1 = glue("beta[r11] == {beta_r11}"),
        label2 = glue("beta[r12] == {beta_r12}")
      )
    } else {
      df %>% 
        filter(retests == max(retests)) %>% 
        mutate(label1 = glue("beta[r1] == {beta_r1}"))
    }
  
  p <- ggplot(df, aes(x = retests, y = f, group = id)) +
    geom_point() +
    geom_line(alpha = .6) +
    geom_blank(aes(x = retests * 1.1, y = f * 1.1)) +
    geom_text(data = annot_df, aes(x = retests, y = f, label = label1),
              nudge_x = .3, nudge_y = .15, parse = TRUE) +
    xlab("Tests taken") +
    ylab("Retest effect")
  
  if(knots == 2L){
    p <- p + 
      geom_text(data = annot_df, aes(x = retests, y = f, label = label2),
                nudge_x = .3, nudge_y = -.15, parse = TRUE)
  }
  
  p
}