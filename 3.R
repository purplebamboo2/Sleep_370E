packages <- c("readxl", "dplyr", "writexl")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(writexl)

# ----------------------------
# 2. Read data
# ----------------------------
dat <- read_excel("merged_diaries_actigraphy.xlsx",
                  sheet = "best_cleaned_analysis_ready") %>%
  filter(recommended_keep == TRUE)

# ----------------------------
# 3. Helper to convert yes/no variables
# ----------------------------
to_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("yes", "y", "true", "1") ~ 1,
    x_chr %in% c("no", "n", "false", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(as.character(x)))
  )
}

# ----------------------------
# 4. Build participant-level summary
# ----------------------------
participant_summary <- dat %>%
  transmute(
    participant_id = `Global Participant ID`,
    sleep_quality = `Sleep Quality`,
    sleep_duration = `Sleep Duration`,
    sleep_onset = `Sleep Onset Clean Decimal Hour`,
    acti_efficiency = `C_ACTI_Efficiency`,
    caffeine_use = to_binary(`Caffeine Consumption`),
    devices_mins = Devices_Total_Minutes,
    socialmedia_mins = SocialMedia_Total_Minutes,
    physical_activity_mins = `Physical Activity Mins`,
    anxiety = `Anxiety Rating`,
    trouble_sleeping = to_binary(`Trouble Sleeping`),
    light_problem = to_binary(`Slp Env Light`)
  ) %>%
  group_by(participant_id) %>%
  summarize(
    mean_sleep_quality = mean(sleep_quality, na.rm = TRUE),
    mean_sleep_duration = mean(sleep_duration, na.rm = TRUE),
    mean_acti_efficiency = mean(acti_efficiency, na.rm = TRUE),
    prop_caffeine_use = mean(caffeine_use, na.rm = TRUE),
    mean_devices_mins = mean(devices_mins, na.rm = TRUE),
    mean_socialmedia_mins = mean(socialmedia_mins, na.rm = TRUE),
    mean_physical_activity_mins = mean(physical_activity_mins, na.rm = TRUE),
    mean_anxiety = mean(anxiety, na.rm = TRUE),
    prop_trouble_sleeping = mean(trouble_sleeping, na.rm = TRUE),
    prop_light_problem = mean(light_problem, na.rm = TRUE),
    sd_sleep_onset = sd(sleep_onset, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 5. Identify top-quartile sleepers
# ----------------------------
cutoff <- quantile(participant_summary$mean_sleep_quality, 0.75, na.rm = TRUE)

participant_summary <- participant_summary %>%
  mutate(top_quartile_sleeper = ifelse(mean_sleep_quality >= cutoff, 1, 0))

# ----------------------------
# 6. Create compact slide-ready comparison table
# ----------------------------
slide_table <- data.frame(
  Variable = c(
    "Sleep quality",
    "Sleep duration",
    "Actigraphy sleep efficiency",
    "Caffeine use proportion",
    "Device minutes",
    "Social media minutes",
    "Physical activity minutes",
    "Anxiety rating",
    "Trouble sleeping proportion",
    "Sleep onset variability",
    "Light problem proportion"
  ),
  Top_Quartile_Sleepers = c(
    mean(participant_summary$mean_sleep_quality[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_sleep_duration[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_acti_efficiency[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$prop_caffeine_use[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_devices_mins[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_socialmedia_mins[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_physical_activity_mins[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$mean_anxiety[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$prop_trouble_sleeping[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$sd_sleep_onset[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE),
    mean(participant_summary$prop_light_problem[participant_summary$top_quartile_sleeper == 1], na.rm = TRUE)
  ),
  Other_Participants = c(
    mean(participant_summary$mean_sleep_quality[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_sleep_duration[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_acti_efficiency[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$prop_caffeine_use[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_devices_mins[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_socialmedia_mins[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_physical_activity_mins[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$mean_anxiety[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$prop_trouble_sleeping[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$sd_sleep_onset[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE),
    mean(participant_summary$prop_light_problem[participant_summary$top_quartile_sleeper == 0], na.rm = TRUE)
  )
)

slide_table <- slide_table %>%
  mutate(
    Difference = Top_Quartile_Sleepers - Other_Participants
  )

print(slide_table)

# ----------------------------
# 7. Optional: round for easy presentation use
# ----------------------------
slide_table_rounded <- slide_table %>%
  mutate(
    Top_Quartile_Sleepers = round(Top_Quartile_Sleepers, 2),
    Other_Participants = round(Other_Participants, 2),
    Difference = round(Difference, 2)
  )

print(slide_table_rounded)

# ----------------------------
# 8. Save outputs
# ----------------------------
write.csv(slide_table_rounded,
          "top_sleepers_presentation_table.csv",
          row.names = FALSE)

write_xlsx(
  list(
    presentation_table = slide_table_rounded,
    participant_summary = participant_summary
  ),
  "top_sleepers_presentation_table.xlsx"
)
