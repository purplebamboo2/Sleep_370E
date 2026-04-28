library(readxl)
library(dplyr)

dat <- read_excel("merged_diaries_actigraphy.xlsx",
                  sheet = "best_cleaned_analysis_ready") %>%
  filter(recommended_keep == TRUE)

# See all available participant IDs
unique(dat$`Global Participant ID`)

# Pick one valid participant ID from the list above
person_id <- "PUSH_102"

to_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("yes", "y", "true", "1") ~ 1,
    x_chr %in% c("no", "n", "false", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(as.character(x)))
  )
}

indiv2 <- dat %>%
  filter(`Global Participant ID` == person_id) %>%
  transmute(
    day = `Study Day`,
    date = Date,
    sleep_quality = `Sleep Quality`,
    acti_efficiency = `C_ACTI_Efficiency`,
    acti_duration = `C_ACTI_Duration`,
    acti_waso = `C_ACTI_Waso`,
    acti_sol = `C_ACTI_SOL`,
    sufficient_sleep = to_binary(`Sufficient Sleep`),
    trouble_sleeping = to_binary(`Trouble Sleeping`)
  )

print(indiv2)

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