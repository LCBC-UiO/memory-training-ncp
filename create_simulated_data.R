library(tidyverse)
# Keeping this number fixed gives the exact same dataset each time.
set.seed(3344)
# Number of participants in each age group
n_start_rest <- 100
n_start_train <- 100
n_control <- 50

hippodat <- tibble(
  BL_Age = c(runif(n_start_rest + n_start_train + n_control, min = 21, max = 35), 
             runif(n_start_rest + n_start_train + n_control, min = 69, max = 83)),
) %>% 
  mutate(
    age_group = factor(if_else(BL_Age <= 35, "Young", "Older")),
    CrossProject_ID = row_number(),
    NCP_Group = rep(c(rep("Start rest", n_start_rest), rep("Start train", n_start_train), 
                      rep("Super passive control", n_control)), 2),
    Sex = sample(c("Female", "Male"), nrow(.), replace = TRUE),
    SexMale = as.numeric(Sex == "Male"),
    icv = rnorm(nrow(.), mean = if_else(Sex == "Female", 1467910, 1653305),
                sd = if_else(Sex == "Female", 118914, 126000)),
    hwt_baseline = rnorm(nrow(.), mean = if_else(age_group == "Young", 18, 10),
                         sd = if_else(age_group == "Young", 7, 4)),
    wp1_baseline = rnorm(nrow(.), mean = if_else(age_group == "Young", 19, 5),
                         sd = if_else(age_group == "Young", 9, 5)),
    hippocampus_baseline = if_else(age_group == "Young", 8476, 7404) + rnorm(nrow(.), sd = 1000)
    ) %>% 
  nest_by(CrossProject_ID, .keep = TRUE) %>% 
  pmap_dfr(function(CrossProject_ID, data){
    data %>% 
      mutate(
        time = case_when(
          NCP_Group == "Super passive control" ~ list(c(0, runif(1, 1, 5))),
          NCP_Group %in% c("Start rest", "Start train") ~ 
            list(c(0, runif(1, 0.18, .22), runif(1, 0.4, .45), runif(1, 0.6, .68), 
                   runif(1, 0.8, .88), runif(1, 2, 5))),
          TRUE ~ list(NA)
          ),
        NCP_Condition = case_when(
          NCP_Group == "Super passive control" ~ list(rep("passive", 2)),
          NCP_Group == "Start rest" ~ list(c("baseline", "post-rest", "post-train",
                                             "post-rest", "post-train", "follow-up")),
          NCP_Group == "Start train" ~ list(c("baseline", "post-train", "post-rest",
                                             "post-train", "post-rest", "follow-up")),
          TRUE ~ list(NA)
        )
      )
  }) %>% 
  unnest(cols = c(time, NCP_Condition)) %>% 
  group_by(CrossProject_ID) %>% 
  mutate(
    Age = BL_Age + time,
    total_tps = n(),
    training_times = list(time[NCP_Condition == "post-train"]),
    time_since_training = map2_dbl(time, training_times,
                                   ~ min(c(.x - .y[.x >= .y], 10000))),
    retests = row_number() - 1,
    trainings = cumsum(NCP_Condition == "post-train"),
    ever_trained = any(NCP_Condition == "post-train")
    ) %>% 
  ungroup() %>% 
  mutate(
    HundredWords_Total = pmax(0, floor(hwt_baseline + (trainings > 1) * if_else(age_group == "Young", 20, 8) + rnorm(nrow(.), sd = 10))),
    WordPair_1 = pmax(0, floor(wp1_baseline + (trainings > 1) * if_else(age_group == "Young", 4, 2) + rnorm(nrow(.), sd = 10))),
    hwt_z = (HundredWords_Total - mean(HundredWords_Total)) / sd(HundredWords_Total),
    wp1_z = (WordPair_1 - mean(WordPair_1)) / sd(WordPair_1),
    icv_z = (icv - mean(icv)) / sd(icv),
    hippocampus = hippocampus_baseline + if_else(age_group == "Young", - 33 * time, - 107 * time) + 321 * icv_z + rnorm(nrow(.), sd = 100),
    hippocampus_z = (hippocampus - mean(hippocampus)) / sd(hippocampus),
    trainings_dummy1 = as.numeric(trainings == 1),
    trainings_dummy2 = as.numeric(trainings == 2),
    retests_dummy1 = as.numeric(retests == 1),
    retests_dummy1om = as.numeric(retests >= 1),
    retests_dummy2om = as.numeric(retests >= 2),
    trained_by_now = as.numeric(trainings > 0),
    trained = factor(case_when(
      NCP_Condition == "post-train" ~ "now",
      NCP_Condition == "post-rest" & trainings > 0 ~ "prev",
      TRUE ~ "none"
    ), levels = c("none", "now", "prev")),
    CrossProject_ID = fct_relevel(as.character(CrossProject_ID))
  ) %>% 
  select(-training_times, -hwt_baseline, -wp1_baseline) %>% 
  group_by(age_group) %>% 
  mutate(age_dev = Age - mean(Age)) %>% 
  ungroup()

hippodat_long <- hippodat
memdat <- memdat_long <- memdat_wp <- memdat_wp_long <- hippodat %>% filter(NCP_Group != "Super passive control")

corrdat <- memdat %>% 
  filter(NCP_Group %in% c("Start rest", "Start train"), 
       NCP_Condition != "follow-up") %>%
  group_by(CrossProject_ID) %>%
  filter(any(NCP_Condition == "baseline"), n() > 1) %>%
  filter(sum(!is.na(WordPair_1)) >= 2) %>%
  ungroup()

corrdat_hippo <- hippodat %>%
  filter(NCP_Group %in% c("Start rest", "Start train"), NCP_Condition != "follow-up") %>%
  group_by(CrossProject_ID) %>%
  filter(any(NCP_Condition == "baseline"), n() > 1) %>%
  ungroup()

corrdat_hippo2 <- hippodat %>%
  filter(NCP_Group %in% c("Start rest", "Start train")) %>%
  group_by(CrossProject_ID) %>%
  filter(any(NCP_Condition == "baseline"),
         sum(NCP_Condition %in% c("post-train", "post-rest") & !is.na(hippocampus)) >= 2,
         any(NCP_Condition == "follow-up" & !is.na(HundredWords_Total))) %>%
  ungroup()

corrdat_hippo3 <- hippodat %>%
  filter(NCP_Group %in% c("Start rest", "Start train")) %>%
  group_by(CrossProject_ID) %>%
  filter(any(NCP_Condition == "baseline"),
         sum(NCP_Condition %in% c("post-train", "post-rest") & !is.na(hippocampus)) >= 2,
         any(NCP_Condition == "follow-up" & !is.na(HundredWords_Total))) %>%
  ungroup()
