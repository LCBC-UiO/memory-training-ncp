boot_predfun <- function(mod, grid){
  betas <- fixef(mod)
  grid %>%
    mutate(
      fit = as.numeric(
        SexMale * coalesce(betas["b0.SexMale"], 0) +
          betas[glue("b0.age_group{age_group}")] +
          age_dev * coalesce(betas[glue("ba.age_group{age_group}:age_dev")], 0) +
          time * coalesce(betas[glue("ba.age_group{age_group}:time")], 0) +
          (
            trainings_dummy1 * coalesce(
              betas[glue("bt.age_group{age_group}:trainings_dummy1")],
              betas[glue("bt.trainings_dummy1")], 0) +
              trainings_dummy2 * coalesce(
                betas[glue("bt.age_group{age_group}:trainings_dummy2")],
                betas[glue("bt.trainings_dummy2")], 0) +
              coalesce(betas[glue("bt.age_group{age_group}:trainings")], 0) * trainings
          ) *
          coalesce(exp(- coalesce(
            betas[glue("lambda.age_group{age_group}")],
            betas[glue("lambda")]) * time_since_training), 1) +
          (
            retests_dummy1om * coalesce(
              betas[glue("br.retests_dummy1om")],
              betas[glue("br.retests_dummy1om")], 0) +
              retests_dummy1 * coalesce(
                betas[glue("br.age_group{age_group}:retests_dummy1")],
                betas[glue("br.retests_dummy1")], 0) +
              retests_dummy2om * coalesce(
                betas[glue("br.age_group{age_group}:retests_dummy2om")],
                betas[glue("br.retests_dummy2om")], 0) +
              retests * coalesce(betas[glue("br.age_group{age_group}:retests")],
                                 betas[glue("br.retests")], 0)
          )
      )
    ) %>%
    pull(fit)
}