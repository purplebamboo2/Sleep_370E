library(readxl)
library(dplyr)
library(stringr)
library(writexl)

diary_path <- "./combined_data.xlsx"
actigraphy_path <- "./PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx"
output_path <- "./merged_diaries_actigraphy.xlsx"

if (!file.exists(diary_path)) {
  stop("combined_data.xlsx was not found. Run Discrepancy.R first.")
}

if (!file.exists(actigraphy_path)) {
  stop("PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx was not found.")
}

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

diary_data <- read_excel(diary_path) %>%
  mutate(
    merge_participant_id = normalize_push_id(`Participant ID`),
    merge_study_day = normalize_study_day(`Study Day`),
    diary_date = as.Date(Date)
  )

actigraphy_data <- read_excel(actigraphy_path) %>%
  mutate(
    merge_participant_id = normalize_push_id(ParticipantID),
    merge_study_day = normalize_study_day(StudyDay),
    actigraphy_date = as.Date(C_ACTI_Date)
  )

# Both files contain repeated daily rows, so joining on participant ID alone
# would create a many-to-many merge. Study day is normalized first and used
# alongside the participant ID as the merge key.
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
  arrange(merge_participant_id, merge_study_day)

date_alignment_summary <- merged_data %>%
  filter(merge_status == "matched") %>%
  count(date_diff_days, name = "n") %>%
  arrange(desc(n), date_diff_days)

diary_only_rows <- merged_data %>%
  filter(merge_status == "diary_only")

actigraphy_only_rows <- merged_data %>%
  filter(merge_status == "actigraphy_only")

write_xlsx(
  list(
    merged_data = merged_data,
    date_alignment_summary = date_alignment_summary,
    diary_only_rows = diary_only_rows,
    actigraphy_only_rows = actigraphy_only_rows
  ),
  output_path
)

cat("Merged file written to:", output_path, "\n")
cat("Total rows in merged_data:", nrow(merged_data), "\n")
cat("Matched rows:", sum(merged_data$merge_status == "matched", na.rm = TRUE), "\n")
cat("Diary-only rows:", nrow(diary_only_rows), "\n")
cat("Actigraphy-only rows:", nrow(actigraphy_only_rows), "\n")

# ============================================================
# Best Cleaned Merged Data ----
# ============================================================

# I wanted this part separated from the raw merge on purpose.
# The objects above still show the merge exactly as it came out once the
# participant IDs and study days were lined up. From a data management
# standpoint, that matters: the raw merge is the audit trail, and the code
# below is the cleaner analytic version. Keeping the two steps apart makes
# it much easier to explain later what was truly in the files and what was
# corrected only after looking at the date patterns.

# This helper is only for the very specific situation where the diary date
# appears to have the correct month and day but the wrong year.
# That was the clearest kind of data entry problem in these files
# (for example, a 2024 actigraphy record paired with a 2014 or 2023 diary
# date for the same participant and study day).
# The function keeps the diary month/day intact and borrows only the year
# from the actigraphy date, which is the more structured date field here.
replace_year_with_reference <- function(date_to_fix, reference_date) {
  repaired_text <- ifelse(
    is.na(date_to_fix) | is.na(reference_date),
    NA_character_,
    paste0(format(reference_date, "%Y"), format(date_to_fix, "-%m-%d"))
  )

  as.Date(repaired_text)
}

# Before filling anything in, I wanted a simple estimate of each
# participant's usual diary-vs-actigraphy date relationship.
# In these data, the diary date is often either the same as the actigraphy
# date or one day later. Both patterns are defensible depending on whether
# the diary is thought of as a report about the prior night's sleep or the
# same calendar day.
#
# The important point is that I only learn this offset from rows that are
# already behaving sensibly. I do not want the obviously messy cases to
# teach the cleaning rule. So I restrict this lookup table to matched rows
# with date differences of 0 or 1 day only, then I take the most common
# value for each participant.
participant_offset_lookup <- merged_data %>%
  # Start with rows where both sources were present. The unmatched rows are
  # a coverage issue, not something date cleaning can repair.
  filter(
    merge_status == "matched",
    !is.na(date_diff_days),
    date_diff_days %in% c(0L, 1L)
  ) %>%
  # Count how often each participant shows each acceptable offset.
  # This gives a very direct summary of the participant's usual pattern.
  count(
    merge_participant_id,
    date_diff_days,
    name = "n_supporting_rows"
  ) %>%
  # Keep the most common offset for each participant.
  # If someone mostly looks like a "same-day" case, use 0.
  # If someone mostly looks like a "next-day diary" case, use 1.
  group_by(merge_participant_id) %>%
  slice_max(order_by = n_supporting_rows, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # I only call the offset reliable when it shows up at least twice.
  # One row by itself feels too thin to justify imputing dates later.
  mutate(offset_is_reliable = n_supporting_rows >= 2L) %>%
  # Rename the column so its role is obvious when it gets joined back in.
  rename(participant_expected_diary_offset_days = date_diff_days)

# This object is the cleaned working table.
# I keep all matched rows here, even the messy ones, because I still want a
# place where every matched pair can be reviewed after the cleaning logic is
# applied. The stricter analysis subset gets created one step later.
best_cleaned_merged_data <- merged_data %>%
  # At this stage I am only cleaning rows where diary and actigraphy were
  # actually paired. The diary-only and actigraphy-only cases belong in
  # their own review tabs because they are missing-data problems, not date
  # repair problems.
  filter(merge_status == "matched") %>%
  # Bring in the participant-level expected offset learned above so it can
  # be used, if warranted, for missing diary dates.
  left_join(participant_offset_lookup, by = "merge_participant_id") %>%
  mutate(
    # Participants with no trustworthy offset estimate should be treated as
    # unreliable by default. Using FALSE here is safer than leaving NA and
    # accidentally letting those rows slip into an imputation rule later.
    offset_is_reliable = coalesce(offset_is_reliable, FALSE),

    # Try the year-only repair up front so we can inspect whether it turns a
    # clearly impossible mismatch into a plausible one. I am not applying it
    # yet; I am just generating the candidate repaired date.
    year_repaired_diary_date = replace_year_with_reference(
      diary_date,
      actigraphy_date
    ),

    # This tells us how close the repaired diary date would be to the
    # actigraphy date. If swapping the year suddenly brings the difference
    # back to 0, 1, or -1 day, that is strong evidence that the original
    # problem was indeed just the year.
    year_repair_diff_days = as.integer(
      year_repaired_diary_date - actigraphy_date
    ),

    # This flag is intentionally strict.
    # I only call something an "obvious year typo" when:
    # 1. the original date gap is huge,
    # 2. the participant ID and study day already match,
    # 3. replacing the year produces a date that is now right on top of the
    #    actigraphy date.
    #
    # The logic here is that a 300+ day mismatch is far too large to be a
    # real nightly alignment issue, but it is exactly the size of problem
    # you see when the year was typed incorrectly.
    obvious_year_typo = !is.na(date_diff_days) &
      abs(date_diff_days) >= 300L &
      !is.na(year_repaired_diary_date) &
      year_repair_diff_days %in% c(-1L, 0L, 1L),

    # I wanted one explicit label that says how each row was handled.
    # That way the final workbook is not just cleaned; it is also legible.
    # The order matters here:
    # - First, keep rows that were already acceptable.
    # - Second, fill in a missing diary date only when a participant-level
    #   offset looks stable enough to trust.
    # - Third, repair the obvious year typos.
    # - Everything else stays unresolved on purpose.
    cleaning_rule = case_when(
      !is.na(diary_date) &
        !is.na(actigraphy_date) &
        date_diff_days %in% c(0L, 1L) ~ "kept_raw_dates",
      is.na(diary_date) &
        !is.na(actigraphy_date) &
        offset_is_reliable ~ "imputed_missing_diary_date_from_actigraphy",
      obvious_year_typo ~ "repaired_obvious_diary_year_typo",
      TRUE ~ "left_unresolved_for_review"
    ),

    # This is the actual cleaned diary date that will travel with the row.
    # I leave the raw diary date untouched elsewhere in the table and create
    # a new cleaned version instead, because it is always better to preserve
    # the original entry for auditing.
    #
    # There are only two situations where I replace the raw diary date:
    # - when the diary date is missing but the participant has a stable,
    #   well-supported offset from other rows;
    # - when the raw diary year is obviously wrong and the year-only repair
    #   brings the date back into a plausible range.
    #
    # I deliberately do not invent fixes for the harder cases such as the
    # PUSH_127 and PUSH_133 patterns. Those rows look inconsistent in ways
    # that could reflect a shifted sequence, skipped days, or a coding issue,
    # and I do not think a one-line rule can settle that responsibly.
    cleaned_diary_date = case_when(
      cleaning_rule == "imputed_missing_diary_date_from_actigraphy" ~
        actigraphy_date + participant_expected_diary_offset_days,
      cleaning_rule == "repaired_obvious_diary_year_typo" ~
        year_repaired_diary_date,
      TRUE ~ diary_date
    ),

    # I keep the actigraphy date unchanged and treat it as the canonical
    # sleep-episode date in the cleaned file. The reason is practical:
    # the actigraphy date is already a dedicated nightly variable, whereas
    # the diary date can behave like either the sleep date or the report
    # date depending on how the diary was completed.
    #
    # To make that distinction explicit, I keep both notions:
    # - cleaned_sleep_episode_date: the actigraphy-centered date for nightly
    #   analyses;
    # - cleaned_diary_report_date: the cleaned diary-side date after any
    #   imputation or obvious typo repair.
    cleaned_actigraphy_date = actigraphy_date,
    cleaned_sleep_episode_date = cleaned_actigraphy_date,
    cleaned_diary_report_date = cleaned_diary_date,

    # This is just the cleaned version of the date difference, which lets us
    # check whether the repair actually improved the alignment.
    cleaned_date_diff_days = as.integer(
      cleaned_diary_date - cleaned_actigraphy_date
    ),

    # This flag is the gatekeeper for the analysis-ready subset.
    # A row is kept when I am comfortable that the date situation is either:
    # - already fine,
    # - missing but recoverable from a participant-specific pattern, or
    # - obviously a year typo that now lines up after repair.
    #
    # I still allow a repaired year typo to end up at -1, 0, or 1 day after
    # cleaning because those are all within the range that already shows up
    # naturally in the better-behaved rows.
    recommended_keep = case_when(
      cleaning_rule == "kept_raw_dates" ~ TRUE,
      cleaning_rule == "imputed_missing_diary_date_from_actigraphy" ~ TRUE,
      cleaning_rule == "repaired_obvious_diary_year_typo" &
        cleaned_date_diff_days %in% c(-1L, 0L, 1L) ~ TRUE,
      TRUE ~ FALSE
    ),

    # This quality-control label is there so that, when looking at the final
    # workbook, I can immediately tell why a row was kept and what kind of
    # date behavior it represents.
    #
    # In other words, `recommended_keep` is the binary decision, and
    # `date_qc_flag` is the explanation attached to that decision.
    date_qc_flag = case_when(
      cleaning_rule == "kept_raw_dates" &
        cleaned_date_diff_days == 0L ~ "same_day_dates",
      cleaning_rule == "kept_raw_dates" &
        cleaned_date_diff_days == 1L ~ "diary_date_is_next_day",
      cleaning_rule == "imputed_missing_diary_date_from_actigraphy" ~
        "missing_diary_date_imputed",
      cleaning_rule == "repaired_obvious_diary_year_typo" ~
        "obvious_year_typo_repaired",
      TRUE ~ "unresolved_date_conflict"
    )
  ) %>%
  # Sorting here just makes the workbook easier to read by hand.
  # When reviewing a participant, it is much easier to scan rows in order
  # of study day than in whatever order the merge happened to return.
  arrange(merge_participant_id, merge_study_day)

# This is the actual analysis file: only rows that passed the conservative
# review rules above.
best_cleaned_analysis_ready <- best_cleaned_merged_data %>%
  filter(recommended_keep)

# These are the rows I was not comfortable forcing into the analytic set.
# I still want them written out, because unresolved data problems are often
# more useful when they are visible than when they disappear silently.
best_cleaned_review_rows <- best_cleaned_merged_data %>%
  filter(!recommended_keep)

# This summary is a quick bookkeeping table.
# It gives a compact count of how many rows were kept as-is, how many needed
# imputation, how many were repaired as year typos, and how many still need
# manual review.
best_cleaned_summary <- best_cleaned_merged_data %>%
  count(cleaning_rule, date_qc_flag, recommended_keep, name = "n_rows") %>%
  arrange(desc(n_rows), cleaning_rule, date_qc_flag)

# I write everything back into the same workbook so the raw merge,
# the cleaned merge, and the review tabs all live together.
# For a project like this, that tends to be much easier to manage than
# scattering slightly different versions across multiple files.
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

# These last two lines are just a quick sanity check in the console.
# They make it obvious, right after the script runs, how many matched rows
# survived into the analysis set and how many were deliberately held back
# for review.
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
