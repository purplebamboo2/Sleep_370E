
library(readxl)
library(dplyr)

dat <- read_excel("merged_diaries_actigraphy.xlsx",
                  sheet = "best_cleaned_analysis_ready") %>%
  filter(recommended_keep == TRUE)

person_id <- 1

indiv <- dat %>%
  filter(`Global Participant ID` == person_id)

to_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("yes", "y", "true", "1") ~ 1,
    x_chr %in% c("no", "n", "false", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(as.character(x)))
  )
}

safe_cor <- function(x, y) {
  keep <- complete.cases(x, y)
  if (sum(keep) < 2) {
    return(NA_real_)
  } else {
    return(cor(x[keep], y[keep]))
  }
}

indiv2 <- indiv %>%
  transmute(
    day = `Study Day`,
    date = authoritative_merged_date,
    
    sleep_quality = `Sleep Quality`,
    sufficient_sleep = to_binary(`Sufficient Sleep`),
    trouble_sleeping = to_binary(`Trouble Sleeping`),
    anxiety = `Anxiety Rating`,
    
    sleep_duration = `Sleep Duration`,
    sleep_onset = `Sleep Onset Clean Decimal Hour`,
    sleep_offset = `Sleep Offset Clean Decimal Hour`,
    
    acti_duration = `C_ACTI_Duration`,
    acti_efficiency = `C_ACTI_Efficiency`,
    acti_waso = `C_ACTI_Waso`,
    acti_sol = `C_ACTI_SOL`,
    
    caffeine = to_binary(`Caffeine Consumption`),
    devices = Devices_Total_Minutes,
    socialmedia = SocialMedia_Total_Minutes
  )

summary_table <- indiv2 %>%
  summarize(
    n_days = n(),
    mean_sleep_quality = mean(sleep_quality, na.rm = TRUE),
    sd_sleep_quality = sd(sleep_quality, na.rm = TRUE),
    prop_sufficient_sleep = mean(sufficient_sleep, na.rm = TRUE),
    prop_trouble_sleeping = mean(trouble_sleeping, na.rm = TRUE),
    mean_anxiety = mean(anxiety, na.rm = TRUE),
    mean_acti_efficiency = mean(acti_efficiency, na.rm = TRUE),
    mean_acti_duration = mean(acti_duration, na.rm = TRUE),
    mean_acti_waso = mean(acti_waso, na.rm = TRUE),
    mean_acti_sol = mean(acti_sol, na.rm = TRUE),
    mean_sleep_duration = mean(sleep_duration, na.rm = TRUE),
    mean_devices = mean(devices, na.rm = TRUE),
    mean_socialmedia = mean(socialmedia, na.rm = TRUE),
    prop_caffeine = mean(caffeine, na.rm = TRUE)
  )

print(summary_table)

cor_results <- indiv2 %>%
  summarize(
    n_quality_efficiency_pairs = sum(complete.cases(sleep_quality, acti_efficiency)),
    n_quality_duration_pairs   = sum(complete.cases(sleep_quality, acti_duration)),
    n_quality_waso_pairs       = sum(complete.cases(sleep_quality, acti_waso)),
    n_quality_sol_pairs        = sum(complete.cases(sleep_quality, acti_sol)),
    
    corr_quality_efficiency = safe_cor(sleep_quality, acti_efficiency),
    corr_quality_duration   = safe_cor(sleep_quality, acti_duration),
    corr_quality_waso       = safe_cor(sleep_quality, acti_waso),
    corr_quality_sol        = safe_cor(sleep_quality, acti_sol)
  )

print(cor_results)

comparison_table <- indiv2 %>%
  select(
    day,
    date,
    sleep_quality,
    acti_efficiency,
    acti_duration,
    acti_waso,
    acti_sol,
    sufficient_sleep,
    trouble_sleeping
  )

print(comparison_table)