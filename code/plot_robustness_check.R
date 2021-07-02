plot_robustness_check <- function(models, grids, memdat, comp_df){
  models_to_compare <- models[
    setdiff(names(models),
            c("No exponential", "Linear training effect", 
              "Common training effect btw age groups"))]
  
  preds <- imap_dfr(models_to_compare, function(m, nm){
    grids$grid %>%
      mutate(
        diff = (boot_predfun(m, .) - boot_predfun(m, grids$grid0)) *
          sd(memdat$HundredWords_Total),
        Model = nm
      )
  }) %>%
    inner_join(
      pivot_comp_df(comp_df) %>%
        filter(name == "AIC") %>%
        mutate(value = value - value[Model == "Common single retest effect btw age groups"]) %>%
        dplyr::select(Model, value), by = "Model")
  
  
  plot1 <- ggplot(preds, aes(x = time, y = diff, group = Model, color = value)) +
    geom_line(data = filter(preds, Model == "Common single retest effect btw age groups"),
              color = "yellow", size = 3) +
    geom_line() +
    geom_hline(yintercept = 0, alpha = .4) +
    facet_grid(cols = vars(age_group)) +
    scale_color_continuous(trans = "reverse") +
    labs(color = "AIC vs chosen model") +
    xlab("Years since training") +
    ylab("Difference between training and non-training")
  
  plot2 <- ggplot(filter(preds, time > 4), aes(x = time, y = diff, group = Model, color = value)) +
    geom_line(data = filter(preds, Model == "Common single retest effect btw age groups", time > 4),
              color = "yellow", size = 3) +
    geom_line() +
    facet_grid(cols = vars(age_group)) +
    scale_color_continuous(trans = "reverse") +
    theme(legend.position = "none") +
    xlab("Years since training") +
    ylab("Difference between training and non-training")
  
  
  list(plot1 = plot1, plot2 = plot2)
}