packages <- c("readxl", "dplyr", "tidyr", "stringr", "writexl")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# ----------------------------
# 2. Read file
# ----------------------------
file_path <- "merged_diaries_actigraphy.xlsx"
sheet_name <- "best_cleaned_analysis_ready"

dat <- read_excel(file_path, sheet = sheet_name)

# ----------------------------
# 3. Keep only rows recommended for analysis
# ----------------------------
dat <- dat %>%
  filter(recommended_keep == TRUE)

# ----------------------------
# 4. Build working dataset using your actual columns
# ----------------------------
sleep_dat <- dat %>%
  transmute(
    participant_id = `Global Participant ID`,
    study_day = `Study Day`,
    date = authoritative_merged_date,
    
    # outcome
    sleep_quality = `Sleep Quality`,
    
    # sleep timing / duration
    sleep_duration = `Sleep Duration`,
    sleep_onset = `Sleep Onset Clean Decimal Hour`,
    sleep_offset = `Sleep Offset Clean Decimal Hour`,
    acti_duration = `C_ACTI_Duration`,
    acti_efficiency = `C_ACTI_Efficiency`,
    acti_sleep_time = `C_ACTI_SleepTime`,
    acti_waso = `C_ACTI_Waso`,
    acti_sol = `C_ACTI_SOL`,
    acti_wake_bouts = `C_ACTI_NumberOfWakeBouts`,
    
    # potential explanatory behaviors / context
    weekend = Weekend_Indicator,
    school_day = `School Day`,
    summer_break = Summer_Break_Indicator,
    sleep_environment = `Sleep Environment`,
    tech_present = `Tech Present`,
    tech_sleep_env = `Technology Sleep Enviornment`,
    
    caffeine_use = `Caffeine Consumption`,
    caffeine_freq_total = `Caffeine Consumption Frequency Total`,
    alcohol_use = `Alcohol Consumption`,
    alcohol_amt = `Alcohol Consumption Amt`,
    marijuana_use = `Marijuana Consumption`,
    marijuana_amt = `Marijuana Consumption Amt`,
    medication_use = `Medication Use`,
    melatonin_use = `Melatonin Use`,
    melatonin_amt = `Melatonin Use Amt`,
    
    physical_activity_mins = `Physical Activity Mins`,
    tv_mins = TV_Total_Minutes,
    videos_mins = Videos_Total_Minutes,
    videogames_mins = VideoGames_Total_Minutes,
    devices_mins = Devices_Total_Minutes,
    socialmedia_mins = SocialMedia_Total_Minutes,
    videochat_mins = VideoChat_Total_Minutes,
    
    first_meal_time = `First Meal Clean Decimal Time`,
    last_meal_time = `Last Meal Clean Decimal Time`,
    
    anxiety = `Anxiety Rating`,
    alertness = `Alertness Rating`,
    wake_difficulty = `Wake Difficulty Rating`,
    sufficient_sleep = `Sufficient Sleep`,
    trouble_sleeping = `Trouble Sleeping`,
    sleep_problems = `Sleep Problems`,
    reporting_waso = `Reporting WASO`,
    bodily_pain = `Bodily Pain`,
    
    slp_noise_outside = `Slp Env Noise Outside`,
    slp_noise_inside = `Slp Env Noise Inside`,
    slp_snoring = `Slp Env Snoring`,
    slp_bed_problem = `Slp Env Bed`,
    slp_temp_problem = `Slp Env Temp`,
    slp_tech_problem = `Slp Env Tech`,
    slp_light_problem = `Slp Env Light`
  )

# ----------------------------
# 5. Convert likely yes/no style variables to numeric indicators
#    This makes summaries easier even if values are text like Yes/No
# ----------------------------
to_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  
  case_when(
    x_chr %in% c("yes", "y", "true", "1") ~ 1,
    x_chr %in% c("no", "n", "false", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(as.character(x)))
  )
}

sleep_dat <- sleep_dat %>%
  mutate(
    weekend = to_binary(weekend),
    school_day = to_binary(school_day),
    summer_break = to_binary(summer_break),
    tech_present = to_binary(tech_present),
    tech_sleep_env = to_binary(tech_sleep_env),
    caffeine_use = to_binary(caffeine_use),
    alcohol_use = to_binary(alcohol_use),
    marijuana_use = to_binary(marijuana_use),
    medication_use = to_binary(medication_use),
    melatonin_use = to_binary(melatonin_use),
    sufficient_sleep = to_binary(sufficient_sleep),
    trouble_sleeping = to_binary(trouble_sleeping),
    sleep_problems = to_binary(sleep_problems),
    reporting_waso = to_binary(reporting_waso),
    slp_noise_outside = to_binary(slp_noise_outside),
    slp_noise_inside = to_binary(slp_noise_inside),
    slp_snoring = to_binary(slp_snoring),
    slp_bed_problem = to_binary(slp_bed_problem),
    slp_temp_problem = to_binary(slp_temp_problem),
    slp_tech_problem = to_binary(slp_tech_problem),
    slp_light_problem = to_binary(slp_light_problem)
  )

# ----------------------------
# 6. Participant-level summaries
#    This is what we use to identify top-quartile sleepers
# ----------------------------
participant_summary <- sleep_dat %>%
  group_by(participant_id) %>%
  summarize(
    n_days = n(),
    
    # main outcome
    mean_sleep_quality = mean(sleep_quality, na.rm = TRUE),
    sd_sleep_quality = sd(sleep_quality, na.rm = TRUE),
    
    # sleep measures
    mean_sleep_duration = mean(sleep_duration, na.rm = TRUE),
    sd_sleep_duration = sd(sleep_duration, na.rm = TRUE),
    mean_sleep_onset = mean(sleep_onset, na.rm = TRUE),
    sd_sleep_onset = sd(sleep_onset, na.rm = TRUE),
    mean_sleep_offset = mean(sleep_offset, na.rm = TRUE),
    sd_sleep_offset = sd(sleep_offset, na.rm = TRUE),
    
    mean_acti_duration = mean(acti_duration, na.rm = TRUE),
    mean_acti_efficiency = mean(acti_efficiency, na.rm = TRUE),
    mean_acti_sleep_time = mean(acti_sleep_time, na.rm = TRUE),
    mean_acti_waso = mean(acti_waso, na.rm = TRUE),
    mean_acti_sol = mean(acti_sol, na.rm = TRUE),
    mean_acti_wake_bouts = mean(acti_wake_bouts, na.rm = TRUE),
    
    # behaviors / contextual factors
    prop_weekend = mean(weekend, na.rm = TRUE),
    prop_school_day = mean(school_day, na.rm = TRUE),
    prop_summer_break = mean(summer_break, na.rm = TRUE),
    
    prop_tech_present = mean(tech_present, na.rm = TRUE),
    prop_tech_sleep_env = mean(tech_sleep_env, na.rm = TRUE),
    
    prop_caffeine_use = mean(caffeine_use, na.rm = TRUE),
    mean_caffeine_freq_total = mean(caffeine_freq_total, na.rm = TRUE),
    
    prop_alcohol_use = mean(alcohol_use, na.rm = TRUE),
    mean_alcohol_amt = mean(alcohol_amt, na.rm = TRUE),
    
    prop_marijuana_use = mean(marijuana_use, na.rm = TRUE),
    mean_marijuana_amt = mean(marijuana_amt, na.rm = TRUE),
    
    prop_medication_use = mean(medication_use, na.rm = TRUE),
    prop_melatonin_use = mean(melatonin_use, na.rm = TRUE),
    mean_melatonin_amt = mean(melatonin_amt, na.rm = TRUE),
    
    mean_physical_activity_mins = mean(physical_activity_mins, na.rm = TRUE),
    
    mean_tv_mins = mean(tv_mins, na.rm = TRUE),
    mean_videos_mins = mean(videos_mins, na.rm = TRUE),
    mean_videogames_mins = mean(videogames_mins, na.rm = TRUE),
    mean_devices_mins = mean(devices_mins, na.rm = TRUE),
    mean_socialmedia_mins = mean(socialmedia_mins, na.rm = TRUE),
    mean_videochat_mins = mean(videochat_mins, na.rm = TRUE),
    
    mean_first_meal_time = mean(first_meal_time, na.rm = TRUE),
    mean_last_meal_time = mean(last_meal_time, na.rm = TRUE),
    
    mean_anxiety = mean(anxiety, na.rm = TRUE),
    mean_alertness = mean(alertness, na.rm = TRUE),
    mean_wake_difficulty = mean(wake_difficulty, na.rm = TRUE),
    prop_sufficient_sleep = mean(sufficient_sleep, na.rm = TRUE),
    prop_trouble_sleeping = mean(trouble_sleeping, na.rm = TRUE),
    prop_sleep_problems = mean(sleep_problems, na.rm = TRUE),
    prop_reporting_waso = mean(reporting_waso, na.rm = TRUE),
    mean_bodily_pain = mean(bodily_pain, na.rm = TRUE),
    
    prop_noise_outside = mean(slp_noise_outside, na.rm = TRUE),
    prop_noise_inside = mean(slp_noise_inside, na.rm = TRUE),
    prop_snoring = mean(slp_snoring, na.rm = TRUE),
    prop_bed_problem = mean(slp_bed_problem, na.rm = TRUE),
    prop_temp_problem = mean(slp_temp_problem, na.rm = TRUE),
    prop_tech_problem = mean(slp_tech_problem, na.rm = TRUE),
    prop_light_problem = mean(slp_light_problem, na.rm = TRUE),
    
    .groups = "drop"
  )

# ----------------------------
# 7. Identify top quartile sleepers
#    Based on participant mean sleep quality
# ----------------------------
top_quartile_cutoff <- quantile(participant_summary$mean_sleep_quality, 0.75, na.rm = TRUE)

participant_summary <- participant_summary %>%
  mutate(
    top_quartile_sleeper = ifelse(mean_sleep_quality >= top_quartile_cutoff, 1, 0)
  )

top_sleepers <- participant_summary %>%
  filter(top_quartile_sleeper == 1)

non_top_sleepers <- participant_summary %>%
  filter(top_quartile_sleeper == 0)

cat("Top quartile cutoff for participant mean sleep quality:", top_quartile_cutoff, "\n")
cat("Number of top-quartile sleepers:", nrow(top_sleepers), "\n")

# ----------------------------
# 8. Descriptive statistics for top quartile sleepers only
# ----------------------------
top_sleepers_descriptives <- top_sleepers %>%
  summarize(
    n_participants = n(),
    
    mean_sleep_quality = mean(mean_sleep_quality, na.rm = TRUE),
    sd_sleep_quality = sd(mean_sleep_quality, na.rm = TRUE),
    
    mean_sleep_duration = mean(mean_sleep_duration, na.rm = TRUE),
    sd_sleep_duration = sd(mean_sleep_duration, na.rm = TRUE),
    
    mean_sleep_onset = mean(mean_sleep_onset, na.rm = TRUE),
    mean_sleep_offset = mean(mean_sleep_offset, na.rm = TRUE),
    
    mean_acti_duration = mean(mean_acti_duration, na.rm = TRUE),
    mean_acti_efficiency = mean(mean_acti_efficiency, na.rm = TRUE),
    mean_acti_sleep_time = mean(mean_acti_sleep_time, na.rm = TRUE),
    mean_acti_waso = mean(mean_acti_waso, na.rm = TRUE),
    mean_acti_sol = mean(mean_acti_sol, na.rm = TRUE),
    
    prop_weekend = mean(prop_weekend, na.rm = TRUE),
    prop_school_day = mean(prop_school_day, na.rm = TRUE),
    prop_summer_break = mean(prop_summer_break, na.rm = TRUE),
    
    prop_tech_present = mean(prop_tech_present, na.rm = TRUE),
    prop_tech_sleep_env = mean(prop_tech_sleep_env, na.rm = TRUE),
    
    prop_caffeine_use = mean(prop_caffeine_use, na.rm = TRUE),
    mean_caffeine_freq_total = mean(mean_caffeine_freq_total, na.rm = TRUE),
    
    prop_alcohol_use = mean(prop_alcohol_use, na.rm = TRUE),
    mean_alcohol_amt = mean(mean_alcohol_amt, na.rm = TRUE),
    
    prop_marijuana_use = mean(prop_marijuana_use, na.rm = TRUE),
    mean_marijuana_amt = mean(mean_marijuana_amt, na.rm = TRUE),
    
    prop_medication_use = mean(prop_medication_use, na.rm = TRUE),
    prop_melatonin_use = mean(prop_melatonin_use, na.rm = TRUE),
    mean_melatonin_amt = mean(mean_melatonin_amt, na.rm = TRUE),
    
    mean_physical_activity_mins = mean(mean_physical_activity_mins, na.rm = TRUE),
    
    mean_tv_mins = mean(mean_tv_mins, na.rm = TRUE),
    mean_videos_mins = mean(mean_videos_mins, na.rm = TRUE),
    mean_videogames_mins = mean(mean_videogames_mins, na.rm = TRUE),
    mean_devices_mins = mean(mean_devices_mins, na.rm = TRUE),
    mean_socialmedia_mins = mean(mean_socialmedia_mins, na.rm = TRUE),
    mean_videochat_mins = mean(mean_videochat_mins, na.rm = TRUE),
    
    mean_first_meal_time = mean(mean_first_meal_time, na.rm = TRUE),
    mean_last_meal_time = mean(mean_last_meal_time, na.rm = TRUE),
    
    mean_anxiety = mean(mean_anxiety, na.rm = TRUE),
    mean_alertness = mean(mean_alertness, na.rm = TRUE),
    mean_wake_difficulty = mean(mean_wake_difficulty, na.rm = TRUE),
    prop_sufficient_sleep = mean(prop_sufficient_sleep, na.rm = TRUE),
    prop_trouble_sleeping = mean(prop_trouble_sleeping, na.rm = TRUE),
    prop_sleep_problems = mean(prop_sleep_problems, na.rm = TRUE),
    mean_bodily_pain = mean(mean_bodily_pain, na.rm = TRUE),
    
    prop_noise_outside = mean(prop_noise_outside, na.rm = TRUE),
    prop_noise_inside = mean(prop_noise_inside, na.rm = TRUE),
    prop_snoring = mean(prop_snoring, na.rm = TRUE),
    prop_bed_problem = mean(prop_bed_problem, na.rm = TRUE),
    prop_temp_problem = mean(prop_temp_problem, na.rm = TRUE),
    prop_tech_problem = mean(prop_tech_problem, na.rm = TRUE),
    prop_light_problem = mean(prop_light_problem, na.rm = TRUE)
  )

print(top_sleepers_descriptives)

# ----------------------------
# 9. Compare top quartile sleepers vs everyone else
#    This is useful for presentation because it tells you
#    what the top sleepers do differently
# ----------------------------
compare_groups <- function(df, group_var = "top_quartile_sleeper") {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      n = n(),
      
      mean_sleep_quality = mean(mean_sleep_quality, na.rm = TRUE),
      mean_sleep_duration = mean(mean_sleep_duration, na.rm = TRUE),
      mean_acti_efficiency = mean(mean_acti_efficiency, na.rm = TRUE),
      
      prop_caffeine_use = mean(prop_caffeine_use, na.rm = TRUE),
      mean_caffeine_freq_total = mean(mean_caffeine_freq_total, na.rm = TRUE),
      
      prop_alcohol_use = mean(prop_alcohol_use, na.rm = TRUE),
      prop_marijuana_use = mean(prop_marijuana_use, na.rm = TRUE),
      prop_melatonin_use = mean(prop_melatonin_use, na.rm = TRUE),
      
      mean_physical_activity_mins = mean(mean_physical_activity_mins, na.rm = TRUE),
      
      mean_devices_mins = mean(mean_devices_mins, na.rm = TRUE),
      mean_socialmedia_mins = mean(mean_socialmedia_mins, na.rm = TRUE),
      mean_tv_mins = mean(mean_tv_mins, na.rm = TRUE),
      mean_videos_mins = mean(mean_videos_mins, na.rm = TRUE),
      
      mean_anxiety = mean(mean_anxiety, na.rm = TRUE),
      prop_sufficient_sleep = mean(prop_sufficient_sleep, na.rm = TRUE),
      prop_trouble_sleeping = mean(prop_trouble_sleeping, na.rm = TRUE),
      
      prop_noise_outside = mean(prop_noise_outside, na.rm = TRUE),
      prop_noise_inside = mean(prop_noise_inside, na.rm = TRUE),
      prop_light_problem = mean(prop_light_problem, na.rm = TRUE),
      prop_temp_problem = mean(prop_temp_problem, na.rm = TRUE),
      prop_tech_problem = mean(prop_tech_problem, na.rm = TRUE),
      
      sd_sleep_onset = mean(sd_sleep_onset, na.rm = TRUE),
      sd_sleep_offset = mean(sd_sleep_offset, na.rm = TRUE),
      sd_sleep_duration = mean(sd_sleep_duration, na.rm = TRUE),
      
      .groups = "drop"
    )
}

group_comparison <- compare_groups(participant_summary)
print(group_comparison)

# ----------------------------
# 10. Rank the biggest differences
#     Helpful for presentation slides
# ----------------------------
vars_to_compare <- participant_summary %>%
  select(
    top_quartile_sleeper,
    mean_sleep_duration, mean_acti_efficiency,
    prop_caffeine_use, mean_caffeine_freq_total,
    prop_alcohol_use, prop_marijuana_use, prop_melatonin_use,
    mean_physical_activity_mins,
    mean_devices_mins, mean_socialmedia_mins,
    mean_tv_mins, mean_videos_mins,
    mean_anxiety, prop_sufficient_sleep, prop_trouble_sleeping,
    prop_noise_outside, prop_noise_inside, prop_light_problem,
    prop_temp_problem, prop_tech_problem,
    sd_sleep_onset, sd_sleep_offset, sd_sleep_duration
  )

difference_table <- lapply(names(vars_to_compare)[-1], function(v) {
  top_mean <- mean(vars_to_compare[[v]][participant_summary$top_quartile_sleeper == 1], na.rm = TRUE)
  other_mean <- mean(vars_to_compare[[v]][participant_summary$top_quartile_sleeper == 0], na.rm = TRUE)
  
  data.frame(
    variable = v,
    top_quartile_mean = top_mean,
    other_participants_mean = other_mean,
    difference = top_mean - other_mean
  )
}) %>%
  bind_rows() %>%
  arrange(desc(abs(difference)))

print(difference_table)

# ----------------------------
# 11. Save outputs
# ----------------------------
write_xlsx(
  list(
    participant_summary = participant_summary,
    top_sleepers_only = top_sleepers,
    top_sleepers_descriptives = top_sleepers_descriptives,
    group_comparison = group_comparison,
    ranked_differences = difference_table
  ),
  "top_quartile_sleepers_descriptive_stats.xlsx"
)

write.csv(participant_summary, "participant_summary_top_quartile_sleepers.csv", row.names = FALSE)
write.csv(top_sleepers, "top_quartile_sleepers_only.csv", row.names = FALSE)
write.csv(group_comparison, "top_vs_other_sleepers_comparison.csv", row.names = FALSE)
write.csv(difference_table, "top_sleepers_ranked_differences.csv", row.names = FALSE)
