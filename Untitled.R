# ============================================================
# Sleep Quality Analysis
# Workbook: merged_diaries_actigraphy.xlsx
# Sheet: best_cleaned_analysis_ready
# ============================================================

# ----------------------------
# 1. Load packages
# ----------------------------
packages <- c("readxl", "dplyr", "stringr", "tidyr", "ggplot2", "cluster")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(cluster)

# ----------------------------
# 2. Read in data
# ----------------------------
file_path <- "merged_diaries_actigraphy.xlsx"
sheet_name <- "best_cleaned_analysis_ready"

sleep_dat <- read_excel(file_path, sheet = sheet_name)

# ----------------------------
# 3. Keep only cleaned rows recommended for analysis
# ----------------------------
sleep_dat <- sleep_dat %>%
  filter(recommended_keep == TRUE)

# ----------------------------
# 4. Select and rename relevant variables
# ----------------------------
sleep_dat2 <- sleep_dat %>%
  transmute(
    participant_id = `Global Participant ID`,
    study_day = `Study Day`,
    date = authoritative_merged_date,
    
    # perceived sleep score
    sleep_quality = `Sleep Quality`,
    
    # diary-based duration
    sleep_duration_diary = `Sleep Duration`,
    
    # cleaned diary timing variables
    sleep_onset_diary = `Sleep Onset Clean Decimal Hour`,
    sleep_offset_diary = `Sleep Offset Clean Decimal Hour`,
    
    # actigraphy variables
    sleep_duration_acti = `C_ACTI_Duration`,
    sleep_efficiency_acti = `C_ACTI_Efficiency`,
    sleep_time_acti = `C_ACTI_SleepTime`
  )

# ----------------------------
# 5. a) Identify good sleeper top quartile distribution
# ----------------------------
# Top quartile cutoff based on perceived sleep quality
sleep_quality_q3 <- quantile(sleep_dat2$sleep_quality, 0.75, na.rm = TRUE)

sleep_dat2 <- sleep_dat2 %>%
  mutate(
    good_sleeper_top_quartile = ifelse(sleep_quality >= sleep_quality_q3, 1, 0)
  )

# overall distribution
good_sleeper_distribution <- sleep_dat2 %>%
  summarize(
    total_rows = n(),
    top_quartile_cutoff = sleep_quality_q3,
    n_good_sleeper_rows = sum(good_sleeper_top_quartile, na.rm = TRUE),
    prop_good_sleeper_rows = mean(good_sleeper_top_quartile, na.rm = TRUE)
  )

print(good_sleeper_distribution)

# participant-level proportion of days in top quartile
participant_good_sleep <- sleep_dat2 %>%
  group_by(participant_id) %>%
  summarize(
    n_days = n(),
    mean_sleep_quality = mean(sleep_quality, na.rm = TRUE),
    median_sleep_quality = median(sleep_quality, na.rm = TRUE),
    prop_days_top_quartile = mean(good_sleeper_top_quartile, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participant_good_sleeper = ifelse(
      mean_sleep_quality >= quantile(mean_sleep_quality, 0.75, na.rm = TRUE),
      1, 0
    )
  )

print(participant_good_sleep)

# ----------------------------
# 6. b) Run individual measures (SQL, duration, variability)
# ----------------------------
# Here SQL = sleep quality level (perceived sleep score)
# Variability = within-person SD across days

individual_measures <- sleep_dat2 %>%
  group_by(participant_id) %>%
  summarize(
    n_days = n(),
    
    # Sleep quality / SQL
    mean_sql = mean(sleep_quality, na.rm = TRUE),
    median_sql = median(sleep_quality, na.rm = TRUE),
    sd_sql = sd(sleep_quality, na.rm = TRUE),
    
    # Diary duration
    mean_duration_diary = mean(sleep_duration_diary, na.rm = TRUE),
    median_duration_diary = median(sleep_duration_diary, na.rm = TRUE),
    sd_duration_diary = sd(sleep_duration_diary, na.rm = TRUE),
    
    # Actigraphy duration
    mean_duration_acti = mean(sleep_duration_acti, na.rm = TRUE),
    median_duration_acti = median(sleep_duration_acti, na.rm = TRUE),
    sd_duration_acti = sd(sleep_duration_acti, na.rm = TRUE),
    
    # Actigraphy sleep time
    mean_sleep_time_acti = mean(sleep_time_acti, na.rm = TRUE),
    sd_sleep_time_acti = sd(sleep_time_acti, na.rm = TRUE),
    
    # Actigraphy sleep efficiency
    mean_sleep_efficiency = mean(sleep_efficiency_acti, na.rm = TRUE),
    sd_sleep_efficiency = sd(sleep_efficiency_acti, na.rm = TRUE),
    
    # Diary timing variability
    mean_onset_diary = mean(sleep_onset_diary, na.rm = TRUE),
    sd_onset_diary = sd(sleep_onset_diary, na.rm = TRUE),
    
    mean_offset_diary = mean(sleep_offset_diary, na.rm = TRUE),
    sd_offset_diary = sd(sleep_offset_diary, na.rm = TRUE),
    
    .groups = "drop"
  )

print(individual_measures)

# ----------------------------
# Optional overall variability index
# ----------------------------
# Standardize several variability measures and average them
individual_measures <- individual_measures %>%
  mutate(
    z_sd_sql = as.numeric(scale(sd_sql)),
    z_sd_duration_diary = as.numeric(scale(sd_duration_diary)),
    z_sd_duration_acti = as.numeric(scale(sd_duration_acti)),
    z_sd_onset_diary = as.numeric(scale(sd_onset_diary)),
    z_sd_offset_diary = as.numeric(scale(sd_offset_diary))
  ) %>%
  rowwise() %>%
  mutate(
    variability_index = mean(
      c(z_sd_sql, z_sd_duration_diary, z_sd_duration_acti,
        z_sd_onset_diary, z_sd_offset_diary),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

print(individual_measures)

# ----------------------------
# 7. c) Individual sleep pattern "profiles"
# ----------------------------
# Create profiles using participant means + variability
profile_input <- individual_measures %>%
  select(
    participant_id,
    mean_sql,
    mean_duration_diary,
    mean_duration_acti,
    mean_sleep_efficiency,
    variability_index
  ) %>%
  drop_na()

# Standardize for clustering
profile_scaled <- scale(profile_input %>%
                          select(-participant_id))

# k = 3 profiles
set.seed(123)
km_fit <- kmeans(profile_scaled, centers = 3, nstart = 50)

profile_input$profile_cluster <- factor(km_fit$cluster)

# Look at cluster centers
cluster_centers <- as.data.frame(km_fit$centers)
cluster_centers$profile_cluster <- factor(1:nrow(cluster_centers))
print(cluster_centers)

# Add descriptive labels manually based on center pattern
cluster_labels <- cluster_centers %>%
  mutate(
    profile_label = case_when(
      mean_sql > 0 & mean_sleep_efficiency > 0 & variability_index < 0 ~ "High quality / efficient / stable",
      mean_sql < 0 & mean_duration_diary < 0 ~ "Lower quality / shorter sleep",
      variability_index > 0 ~ "Irregular / variable sleepers",
      TRUE ~ "Intermediate sleepers"
    )
  ) %>%
  select(profile_cluster, profile_label)

participant_profiles <- profile_input %>%
  left_join(cluster_labels, by = "profile_cluster")

print(participant_profiles)

# Merge profiles back onto individual measures
individual_measures <- individual_measures %>%
  left_join(
    participant_profiles %>%
      select(participant_id, profile_cluster, profile_label),
    by = "participant_id"
  )

# ----------------------------
# 8. Save outputs
# ----------------------------
write.csv(sleep_dat2, "sleep_quality_daily_scored.csv", row.names = FALSE)
write.csv(good_sleeper_distribution, "sleep_quality_top_quartile_distribution.csv", row.names = FALSE)
write.csv(participant_good_sleep, "participant_good_sleeper_summary.csv", row.names = FALSE)
write.csv(individual_measures, "individual_sleep_measures_and_profiles.csv", row.names = FALSE)
write.csv(participant_profiles, "participant_sleep_profiles.csv", row.names = FALSE)

# ----------------------------
# 9. Plots
# ----------------------------

# Sleep quality distribution
ggplot(sleep_dat2, aes(x = sleep_quality)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = sleep_quality_q3, linetype = "dashed") +
  labs(
    title = "Distribution of Perceived Sleep Quality",
    x = "Sleep Quality",
    y = "Count"
  )

# Mean sleep quality vs diary duration by profile
ggplot(individual_measures, aes(x = mean_duration_diary, y = mean_sql, color = profile_label)) +
  geom_point(size = 3) +
  labs(
    title = "Individual Sleep Pattern Profiles",
    x = "Mean Diary Sleep Duration",
    y = "Mean Sleep Quality"
  )

# Variability index by participant
ggplot(individual_measures, aes(x = reorder(participant_id, variability_index), y = variability_index)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Participant Sleep Variability Index",
    x = "Participant",
    y = "Variability Index"
  )


# ----------------------------
# Create HIGH vs LOW groups
# ----------------------------

profile_df <- individual_measures

# Option 1: Median split (clean for visualization)
cutoff <- median(profile_df$mean_sql, na.rm = TRUE)

profile_df <- profile_df %>%
  mutate(
    quality_group = ifelse(mean_sql >= cutoff,
                           "High quality sleepers",
                           "Lower quality sleepers")
  )

# ----------------------------
# Plot (2 groups only)
# ----------------------------
library(ggplot2)

ggplot(profile_df, aes(x = mean_duration_diary, y = mean_sql, color = quality_group)) +
  geom_point(size = 4) +
  labs(
    title = "Individual Sleep Patterns (High vs Low Quality)",
    x = "Mean Diary Sleep Duration",
    y = "Mean Sleep Quality",
    color = "Sleep Group"
  ) +
  theme_minimal()

