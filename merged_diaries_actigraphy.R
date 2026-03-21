## Packages
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

## File paths
diary_path <- "./combined_data.xlsx"
actigraphy_path <- "./PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx"
output_path <- "./merged_diaries_actigraphy.xlsx"

## Input checks
if (!file.exists(diary_path)) {
  stop("combined_data.xlsx was not found. Run Discrepancy.R first.")
}

if (!file.exists(actigraphy_path)) {
  stop("PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx was not found.")
}

## ID helpers
normalize_push_id <- function(x) {
  id_digits <- str_extract(as.character(x), "[0-9]+")

  ifelse(
    is.na(id_digits),
    NA_character_,
    paste0("PUSH_", str_pad(id_digits, width = 3, side = "left", pad = "0"))
  )
}

normalize_study_day <- function(x) {
  as.integer(str_extract(as.character(x), "[0-9]+"))
}

## Calendar windows
# CPS attendance windows used for school-year and summer coding.
# Summer runs from the day after school ends to the day before classes resume.
school_year_windows <- data.frame(
  window_label = c(
    "2020-2021 School Year",
    "2021-2022 School Year",
    "2022-2023 School Year",
    "2023-2024 School Year"
  ),
  start_date = as.Date(c(
    "2020-09-08",
    "2021-08-30",
    "2022-08-22",
    "2023-08-21"
  )),
  end_date = as.Date(c(
    "2021-06-22",
    "2022-06-14",
    "2023-06-07",
    "2024-06-06"
  )),
  stringsAsFactors = FALSE
)

summer_break_windows <- data.frame(
  window_label = c(
    "Summer 2021",
    "Summer 2022",
    "Summer 2023"
  ),
  start_date = as.Date(c(
    "2021-06-23",
    "2022-06-15",
    "2023-06-08"
  )),
  end_date = as.Date(c(
    "2021-08-29",
    "2022-08-21",
    "2023-08-20"
  )),
  stringsAsFactors = FALSE
)

## Calendar helpers
date_in_windows <- function(date_vector, windows_df) {
  vapply(
    date_vector,
    function(single_date) {
      if (is.na(single_date)) {
        return(FALSE)
      }

      any(
        single_date >= windows_df$start_date &
          single_date <= windows_df$end_date
      )
    },
    logical(1)
  )
}

## Summer indicator
classify_summer_break <- function(date_vector) {
  in_school_year <- date_in_windows(date_vector, school_year_windows)
  in_summer_break <- date_in_windows(date_vector, summer_break_windows)

  case_when(
    in_summer_break ~ 1,
    in_school_year ~ 0,
    TRUE ~ NA_real_
  )
}

## Weekend indicator
classify_weekend <- function(date_vector) {
  day_of_week <- as.POSIXlt(date_vector)$wday

  case_when(
    is.na(date_vector) ~ NA_real_,
    # Friday and Saturday nights count as weekend nights.
    day_of_week %in% c(5, 6) ~ 1,
    TRUE ~ 0
  )
}

## Key check
check_unique_key <- function(data, key_cols, data_name) {
  duplicate_keys <- data %>%
    filter(if_all(all_of(key_cols), ~ !is.na(.x))) %>%
    count(across(all_of(key_cols)), name = "n") %>%
    filter(n > 1)

  if (nrow(duplicate_keys) > 0) {
    stop(
      paste0(
        data_name,
        " has duplicate rows for the merge key. ",
        "Review merge_participant_id + merge_study_day before merging."
      )
    )
  }
}

## Read diary data
diary_data <- read_excel(diary_path) %>%
  mutate(
    merge_participant_id = normalize_push_id(`Participant ID`),
    merge_study_day = normalize_study_day(`Study Day`),
    diary_date = as.Date(Date)
  )

## Read actigraphy data
actigraphy_data <- read_excel(actigraphy_path) %>%
  mutate(
    merge_participant_id = normalize_push_id(ParticipantID),
    merge_study_day = normalize_study_day(StudyDay),
    actigraphy_date = as.Date(C_ACTI_Date)
  )

# Join by participant and study day to avoid many-to-many matches.
check_unique_key(
  diary_data,
  c("merge_participant_id", "merge_study_day"),
  "Diary data"
)
check_unique_key(
  actigraphy_data,
  c("merge_participant_id", "merge_study_day"),
  "Actigraphy data"
)

## Spreadsheet tracking columns
# Add front-end tracking columns. Use actigraphy dates first for the
# seasonal and weekend indicators, then fall back to diary dates.
add_global_tracking_columns <- function(data) {
  data %>%
    mutate(
      indicator_reference_date = coalesce(actigraphy_date, diary_date),
      `Global Participant ID` = merge_participant_id,
      Data_Source = case_when(
        merge_status == "actigraphy_only" ~ "Actigraphy",
        merge_status == "diary_only" ~ "Diary",
        merge_status == "matched" ~ "Both",
        TRUE ~ "Review"
      ),
      Summer_Break_Indicator = classify_summer_break(indicator_reference_date),
      Weekend_Indicator = classify_weekend(indicator_reference_date),
      .before = 1
    ) %>%
    relocate(Data_Source, .after = `Global Participant ID`) %>%
    relocate(Summer_Break_Indicator, .after = Data_Source) %>%
    relocate(Weekend_Indicator, .after = Summer_Break_Indicator) %>%
    select(-indicator_reference_date)
}

## Raw merged file
merged_data <- full_join(
  diary_data,
  actigraphy_data,
  by = c("merge_participant_id", "merge_study_day")
) %>%
  mutate(
    has_diary_row = !is.na(`Participant ID`) | !is.na(Date),
    has_actigraphy_row = !is.na(ParticipantID) | !is.na(C_ACTI_Date),
    merge_status = case_when(
      has_diary_row & has_actigraphy_row ~ "matched",
      has_diary_row ~ "diary_only",
      has_actigraphy_row ~ "actigraphy_only",
      TRUE ~ NA_character_
    ),
    date_diff_days = as.integer(diary_date - actigraphy_date)
  ) %>%
  arrange(merge_participant_id, merge_study_day) %>%
  add_global_tracking_columns()

## Date gap summary
date_alignment_summary <- merged_data %>%
  filter(merge_status == "matched") %>%
  count(date_diff_days, name = "n") %>%
  arrange(desc(n), date_diff_days)

## Unmatched diary rows
diary_only_rows <- merged_data %>%
  filter(merge_status == "diary_only")

## Unmatched actigraphy rows
actigraphy_only_rows <- merged_data %>%
  filter(merge_status == "actigraphy_only")

## Initial workbook write
write_xlsx(
  list(
    merged_data = merged_data,
    date_alignment_summary = date_alignment_summary,
    diary_only_rows = diary_only_rows,
    actigraphy_only_rows = actigraphy_only_rows
  ),
  output_path
)

## Raw merge counts
cat("Merged file written to:", output_path, "\n")
cat("Total rows in merged_data:", nrow(merged_data), "\n")
cat("Matched rows:", sum(merged_data$merge_status == "matched", na.rm = TRUE), "\n")
cat("Diary-only rows:", nrow(diary_only_rows), "\n")
cat("Actigraphy-only rows:", nrow(actigraphy_only_rows), "\n")

# ============================================================
# Best Cleaned Merged Data ----
# ============================================================

## Year repair helper
# Replace the diary year with the actigraphy year when month and day match.
replace_year_with_reference <- function(date_to_fix, reference_date) {
  repaired_text <- ifelse(
    is.na(date_to_fix) | is.na(reference_date),
    NA_character_,
    paste0(format(reference_date, "%Y"), format(date_to_fix, "-%m-%d"))
  )

  as.Date(repaired_text)
}

## Clean matched rows
# Build the cleaned matched file here and keep the raw merge above intact.
best_cleaned_merged_data <- merged_data %>%
  # Clean only the matched rows here.
  filter(merge_status == "matched") %>%
  mutate(
    # Candidate year-only repair.
    year_repaired_diary_date = replace_year_with_reference(
      diary_date,
      actigraphy_date
    ),

    # Gap after the year-only repair.
    year_repair_diff_days = as.integer(
      year_repaired_diary_date - actigraphy_date
    ),

    # Flag large gaps that collapse after swapping the year.
    obvious_year_typo = !is.na(date_diff_days) &
      abs(date_diff_days) >= 300L &
      !is.na(year_repaired_diary_date) &
      year_repair_diff_days %in% c(-1L, 0L, 1L),

    # Use actigraphy as the main date when it exists.
    authoritative_date_source = case_when(
      !is.na(actigraphy_date) ~ "actigraphy",
      !is.na(year_repaired_diary_date) ~ "year_repaired_diary",
      !is.na(diary_date) ~ "raw_diary",
      TRUE ~ NA_character_
    ),

    # Main row date for analysis.
    authoritative_merged_date = coalesce(
      actigraphy_date,
      year_repaired_diary_date,
      diary_date
    ),

    # Diary-side date after harmonizing to the main row date.
    cleaned_diary_date = case_when(
      !is.na(actigraphy_date) ~ actigraphy_date,
      obvious_year_typo ~ year_repaired_diary_date,
      TRUE ~ diary_date
    ),

    # Diary date with only the year repair applied.
    diary_date_after_minimal_fix = case_when(
      obvious_year_typo ~ year_repaired_diary_date,
      TRUE ~ diary_date
    ),

    # Carry the main row date through the cleaned outputs.
    cleaned_actigraphy_date = authoritative_merged_date,
    cleaned_sleep_episode_date = authoritative_merged_date,

    # Keep the existing output column name for the harmonized date.
    cleaned_diary_report_date = authoritative_merged_date,

    # Diary-to-main-date gap after the minimal diary fix.
    cleaned_date_diff_days = as.integer(
      diary_date_after_minimal_fix - authoritative_merged_date
    ),

    # Short label for how the row date was resolved.
    cleaning_rule = case_when(
      !is.na(actigraphy_date) &
        !is.na(diary_date) &
        date_diff_days == 0L ~ "used_actigraphy_date_raw_dates_already_matched",
      !is.na(actigraphy_date) &
        !is.na(diary_date) &
        date_diff_days %in% c(-1L, 1L) ~
        "used_actigraphy_date_to_resolve_one_day_difference",
      !is.na(actigraphy_date) &
        is.na(diary_date) ~ "used_actigraphy_date_because_diary_was_missing",
      !is.na(actigraphy_date) &
        obvious_year_typo ~ "used_actigraphy_date_to_replace_year_typo",
      !is.na(actigraphy_date) ~ "used_actigraphy_date_but_left_pair_for_review",
      TRUE ~ "used_best_available_non_actigraphy_date"
    ),

    # Keep rows with a small gap, a missing diary date, or a clear year typo.
    recommended_keep = case_when(
      !is.na(actigraphy_date) & is.na(diary_date) ~ TRUE,
      !is.na(actigraphy_date) & date_diff_days %in% c(-1L, 0L, 1L) ~ TRUE,
      !is.na(actigraphy_date) & obvious_year_typo ~ TRUE,
      is.na(actigraphy_date) & !is.na(diary_date_after_minimal_fix) ~ TRUE,
      TRUE ~ FALSE
    ),

    # QC label for the cleaned date decision.
    date_qc_flag = case_when(
      !is.na(actigraphy_date) &
        !is.na(diary_date) &
        date_diff_days == 0L ~ "actigraphy_used_raw_dates_matched",
      !is.na(actigraphy_date) &
        !is.na(diary_date) &
        date_diff_days %in% c(-1L, 1L) ~
        "actigraphy_used_one_day_diary_difference",
      !is.na(actigraphy_date) &
        is.na(diary_date) ~ "actigraphy_used_diary_date_missing",
      !is.na(actigraphy_date) &
        obvious_year_typo ~ "actigraphy_used_obvious_year_typo",
      !recommended_keep ~ "actigraphy_used_but_pair_needs_review",
      TRUE ~ "non_actigraphy_fallback_used"
    )
  ) %>%
  # Sort for review.
  arrange(merge_participant_id, merge_study_day)

## Analysis-ready rows
# Analysis-ready subset.
best_cleaned_analysis_ready <- best_cleaned_merged_data %>%
  filter(recommended_keep)

## Review rows
# Rows held for review.
best_cleaned_review_rows <- best_cleaned_merged_data %>%
  filter(!recommended_keep)

## Cleaning summary
# Summary counts by cleaning rule and QC label.
best_cleaned_summary <- best_cleaned_merged_data %>%
  count(cleaning_rule, date_qc_flag, recommended_keep, name = "n_rows") %>%
  arrange(desc(n_rows), cleaning_rule, date_qc_flag)

## Final workbook write
# Write all outputs to one workbook.
write_xlsx(
  list(
    merged_data = merged_data,
    date_alignment_summary = date_alignment_summary,
    diary_only_rows = diary_only_rows,
    actigraphy_only_rows = actigraphy_only_rows,
    best_cleaned_merged_data = best_cleaned_merged_data,
    best_cleaned_analysis_ready = best_cleaned_analysis_ready,
    best_cleaned_review_rows = best_cleaned_review_rows,
    best_cleaned_summary = best_cleaned_summary
  ),
  output_path
)

## Cleaned row counts
# Quick row counts in the console.
cat(
  "Best cleaned analysis-ready rows:",
  nrow(best_cleaned_analysis_ready),
  "\n"
)
cat(
  "Best cleaned review rows:",
  nrow(best_cleaned_review_rows),
  "\n"
)
