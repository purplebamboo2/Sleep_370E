# Shared helpers for stable Excel date and clock-time handling.
#
# The source workbooks mix Excel serials, POSIXct datetimes, and plain text
# representations. These helpers normalize them right after import so the rest
# of the pipeline works from one consistent version of each field.

# Pull out the columns that should behave like calendar dates.
detect_date_columns <- function(column_names) {
  unique(
    column_names[
      grepl("(^Date$|_date$|^C_ACTI_Date$|^online date\\?$)", column_names, perl = TRUE)
    ]
  )
}

# Pull out the fields that should stay as clock times.
detect_clock_time_columns <- function(column_names) {
  unique(
    column_names[
      grepl("Clock Time$", column_names) |
        grepl("^Sleep Offset Time$", column_names) |
        grepl("^Device OffWrist Time$", column_names) |
        grepl("^Caffeine Consumption Time [0-9]+$", column_names) |
        grepl("^Alcohol Consumption Time$", column_names) |
        grepl("^Marijuana Consumption Time$", column_names) |
        grepl("^Medication [0-9]+ Time$", column_names) |
        grepl("^Melatonin Use Time$", column_names)
    ]
  )
}

# Trim blank-style text values before any parsing work.
trim_missing_text <- function(x) {
  out <- trimws(as.character(x))
  out[out %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  out
}

# Numeric coercion is noisy here, so keep it quiet.
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Try a short list of formats until one works.
parse_with_formats <- function(x, formats, tz = "UTC") {
  out <- rep(as.POSIXct(NA, tz = tz), length(x))

  for (fmt in formats) {
    needs_parse <- !is.na(x) & is.na(out)

    if (!any(needs_parse)) {
      break
    }

    parsed <- suppressWarnings(as.POSIXct(x[needs_parse], format = fmt, tz = tz))
    out[needs_parse] <- parsed
  }

  out
}

# Convert mixed date inputs into plain Date values.
normalize_date_column <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    out <- rep(as.Date(NA), length(x))
    non_missing <- !is.na(x)

    if (any(non_missing)) {
      vals <- x[non_missing]
      temp_dates <- rep(as.Date(NA), length(vals))
      # Big values here are Unix seconds; smaller ones are Excel day counts.
      epoch_idx <- abs(vals) > 100000

      if (any(epoch_idx)) {
        temp_dates[epoch_idx] <- as.Date(
          as.POSIXct(vals[epoch_idx], origin = "1970-01-01", tz = "UTC"),
          tz = "UTC"
        )
      }

      if (any(!epoch_idx)) {
        temp_dates[!epoch_idx] <- as.Date(vals[!epoch_idx], origin = "1899-12-30")
      }

      out[non_missing] <- temp_dates
    }

    return(out)
  }

  if (is.logical(x)) {
    return(as.Date(rep(NA_character_, length(x))))
  }

  text <- trim_missing_text(x)
  out <- rep(as.Date(NA), length(text))

  numeric_vals <- safe_numeric(text)
  numeric_idx <- !is.na(numeric_vals)

  if (any(numeric_idx)) {
    out[numeric_idx] <- normalize_date_column(numeric_vals[numeric_idx])
  }

  remaining_idx <- !numeric_idx & !is.na(text)

  if (any(remaining_idx)) {
    remaining_text <- text[remaining_idx]
    parsed_date <- rep(as.Date(NA), length(remaining_text))

    date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")

    for (fmt in date_formats) {
      needs_parse <- !is.na(remaining_text) & is.na(parsed_date)

      if (!any(needs_parse)) {
        break
      }

      parsed_date[needs_parse] <- suppressWarnings(
        as.Date(remaining_text[needs_parse], format = fmt)
      )
    }

    if (any(is.na(parsed_date) & !is.na(remaining_text))) {
      parsed_dt <- parse_with_formats(
        remaining_text,
        formats = c(
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d %H:%M:%S %Z",
          "%Y-%m-%d %H:%M %Z",
          "%m/%d/%Y %H:%M:%S",
          "%m/%d/%Y %H:%M",
          "%m/%d/%Y %I:%M:%S %p",
          "%m/%d/%Y %I:%M %p"
        )
      )
      parsed_date[is.na(parsed_date)] <- as.Date(parsed_dt[is.na(parsed_date)], tz = "UTC")
    }

    out[remaining_idx] <- parsed_date
  }

  out
}

# Convert clock-time fields into fractions of a day.
normalize_clock_time_column <- function(x) {
  if (inherits(x, "POSIXt")) {
    return((as.numeric(x) %% 86400) / 86400)
  }

  if (is.numeric(x)) {
    out <- rep(NA_real_, length(x))
    non_missing <- !is.na(x)

    if (any(non_missing)) {
      vals <- x[non_missing]
      temp <- rep(NA_real_, length(vals))

      # Keep just the time-of-day piece, whether it came in as epoch seconds
      # or an Excel serial with a date attached.
      epoch_idx <- abs(vals) > 100000
      temp[epoch_idx] <- (((vals[epoch_idx] %% 86400) + 86400) %% 86400) / 86400
      temp[!epoch_idx] <- vals[!epoch_idx] %% 1

      out[non_missing] <- temp
    }

    return(out)
  }

  if (is.logical(x)) {
    return(rep(NA_real_, length(x)))
  }

  text <- trim_missing_text(x)
  out <- rep(NA_real_, length(text))

  numeric_vals <- safe_numeric(text)
  numeric_idx <- !is.na(numeric_vals)

  if (any(numeric_idx)) {
    out[numeric_idx] <- normalize_clock_time_column(numeric_vals[numeric_idx])
  }

  remaining_idx <- !numeric_idx & !is.na(text)

  if (any(remaining_idx)) {
    remaining_text <- text[remaining_idx]
    remaining_out <- rep(NA_real_, length(remaining_text))
    time_only_idx <- grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", remaining_text)

    if (any(time_only_idx)) {
      parts <- strsplit(remaining_text[time_only_idx], ":", fixed = TRUE)
      seconds <- vapply(
        parts,
        function(one_time) {
          hh <- as.numeric(one_time[1])
          mm <- as.numeric(one_time[2])
          ss <- if (length(one_time) >= 3) as.numeric(one_time[3]) else 0
          hh * 3600 + mm * 60 + ss
        },
        numeric(1)
      )
      remaining_out[time_only_idx] <- seconds / 86400
    }

    if (any(!time_only_idx)) {
      parsed_dt <- parse_with_formats(
        remaining_text[!time_only_idx],
        formats = c(
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d %H:%M:%S %Z",
          "%Y-%m-%d %H:%M %Z",
          "%Y-%m-%dT%H:%M:%S",
          "%m/%d/%Y %H:%M:%S",
          "%m/%d/%Y %H:%M",
          "%m/%d/%Y %I:%M:%S %p",
          "%m/%d/%Y %I:%M %p"
        )
      )
      remaining_out[!time_only_idx] <- normalize_clock_time_column(parsed_dt)
    }

    out[remaining_idx] <- remaining_out
  }

  out
}

# Keep decimal-hour style fields as plain numeric values.
normalize_time_numeric_column <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return((as.numeric(x) %% 86400) / 3600)
  }

  if (is.logical(x)) {
    return(as.numeric(x))
  }

  text <- trim_missing_text(x)
  text[text %in% c("L", "-999", "-888", "-555")] <- NA_character_
  safe_numeric(text)
}

# Central map of the paired clock and decimal fields we reconcile.
paired_time_configs <- function() {
  list(
    list(
      base_label = "Sleep Onset",
      clock_col = "Sleep Onset Clock Time",
      decimal_col = "Sleep Onset Decimal Hour",
      clean_minutes_col = "Sleep Onset Clean Minutes Since Midnight",
      clean_clock_col = "Sleep Onset Clean Clock Time",
      clean_decimal_col = "Sleep Onset Clean Decimal Hour",
      clean_relative_col = "Sleep Onset Clean Relative to Midnight Time",
      rule_col = "Sleep Onset Time Reconciliation Rule",
      review_col = "Sleep Onset Time Needs Review"
    ),
    list(
      base_label = "Sleep Offset",
      clock_col = "Sleep Offset Time",
      decimal_col = "Sleep Offset Time_Decimal Hour",
      clean_minutes_col = "Sleep Offset Clean Minutes Since Midnight",
      clean_clock_col = "Sleep Offset Clean Clock Time",
      clean_decimal_col = "Sleep Offset Clean Decimal Hour",
      clean_relative_col = NULL,
      rule_col = "Sleep Offset Time Reconciliation Rule",
      review_col = "Sleep Offset Time Needs Review"
    ),
    list(
      base_label = "Nap Start",
      clock_col = "Nap Start Clock Time",
      decimal_col = "Nap Start Decimal Time",
      clean_minutes_col = "Nap Start Clean Minutes Since Midnight",
      clean_clock_col = "Nap Start Clean Clock Time",
      clean_decimal_col = "Nap Start Clean Decimal Time",
      clean_relative_col = NULL,
      rule_col = "Nap Start Time Reconciliation Rule",
      review_col = "Nap Start Time Needs Review"
    ),
    list(
      base_label = "Nap End",
      clock_col = "Nap End Clock Time",
      decimal_col = "Nap End Decimal Time",
      clean_minutes_col = "Nap End Clean Minutes Since Midnight",
      clean_clock_col = "Nap End Clean Clock Time",
      clean_decimal_col = "Nap End Clean Decimal Time",
      clean_relative_col = NULL,
      rule_col = "Nap End Time Reconciliation Rule",
      review_col = "Nap End Time Needs Review"
    ),
    list(
      base_label = "First Meal",
      clock_col = "First Meal Clock Time",
      decimal_col = "First Meal Decimal Time",
      clean_minutes_col = "First Meal Clean Minutes Since Midnight",
      clean_clock_col = "First Meal Clean Clock Time",
      clean_decimal_col = "First Meal Clean Decimal Time",
      clean_relative_col = NULL,
      rule_col = "First Meal Time Reconciliation Rule",
      review_col = "First Meal Time Needs Review"
    ),
    list(
      base_label = "Last Meal",
      clock_col = "Last Meal Clock Time",
      decimal_col = "Last Meal Decimal Time",
      clean_minutes_col = "Last Meal Clean Minutes Since Midnight",
      clean_clock_col = "Last Meal Clean Clock Time",
      clean_decimal_col = "Last Meal Clean Decimal Time",
      clean_relative_col = NULL,
      rule_col = "Last Meal Time Reconciliation Rule",
      review_col = "Last Meal Time Needs Review"
    )
  )
}

# Just the raw paired columns, without the cleaned outputs.
paired_clock_decimal_columns <- function() {
  lapply(
    paired_time_configs(),
    function(config) c(config$clock_col, config$decimal_col)
  )
}

# Move a clock fraction onto a 0-1439 minute scale.
clock_fraction_to_minutes <- function(clock_frac) {
  frac <- normalize_clock_time_column(clock_frac)
  out <- round(frac * 1440)
  out[out == 1440] <- 0
  out
}

# Convert decimal hours into minutes since midnight.
decimal_hours_to_minutes <- function(decimal_hour) {
  vals <- normalize_time_numeric_column(decimal_hour)
  out <- round(vals * 60)
  out[!is.na(out)] <- ((out[!is.na(out)] %% 1440) + 1440) %% 1440
  out
}

# Some decimal fields were entered as HH.MM, so handle that too.
decimal_clock_notation_to_minutes <- function(decimal_hour) {
  vals <- normalize_time_numeric_column(decimal_hour)
  whole_hours <- floor(vals)
  minute_component <- round((vals - whole_hours) * 100)
  out <- whole_hours * 60 + minute_component
  out[!is.na(out)] <- ((out[!is.na(out)] %% 1440) + 1440) %% 1440
  out
}

# Flag decimal values that look more like clock notation than true decimals.
decimal_looks_like_clock_notation <- function(decimal_hour) {
  vals <- normalize_time_numeric_column(decimal_hour)
  whole_hours <- floor(vals)
  minute_component_raw <- (vals - whole_hours) * 100
  minute_component <- round(minute_component_raw)

  !is.na(vals) &
    abs(minute_component_raw - minute_component) <= 0.05 &
    minute_component >= 0 &
    minute_component < 60
}

# Smallest gap between two times on a 24-hour clock.
circular_minute_diff <- function(min_a, min_b) {
  diff <- abs((min_a - min_b) %% 1440)
  pmin(diff, 1440 - diff)
}

# Convert cleaned minutes back to a fraction of a day.
minutes_to_clock_fraction <- function(minutes_since_midnight) {
  out <- rep(NA_real_, length(minutes_since_midnight))
  non_missing <- !is.na(minutes_since_midnight)

  if (any(non_missing)) {
    out[non_missing] <- (
      ((minutes_since_midnight[non_missing] %% 1440) + 1440) %% 1440
    ) / 1440
  }

  out
}

# Convert cleaned minutes back to decimal hours.
minutes_to_decimal_hours <- function(minutes_since_midnight) {
  out <- rep(NA_real_, length(minutes_since_midnight))
  non_missing <- !is.na(minutes_since_midnight)

  if (any(non_missing)) {
    out[non_missing] <- minutes_since_midnight[non_missing] / 60
  }

  out
}

# Keep sleep-onset style fields on the usual -12 to 12 scale.
decimal_hours_to_relative_midnight <- function(decimal_hour) {
  out <- rep(NA_real_, length(decimal_hour))
  non_missing <- !is.na(decimal_hour)

  if (any(non_missing)) {
    vals <- decimal_hour[non_missing]
    out[non_missing] <- ifelse(vals > 12, vals - 24, vals)
  }

  out
}

# Pick one cleaned time when clock and decimal fields disagree.
reconcile_time_pair <- function(
  clock_value,
  decimal_value,
  small_diff_tolerance_minutes = 5,
  twelve_hour_tolerance_minutes = 6
) {
  clock_minutes <- clock_fraction_to_minutes(clock_value)
  decimal_minutes <- decimal_hours_to_minutes(decimal_value)
  decimal_clock_minutes <- decimal_clock_notation_to_minutes(decimal_value)
  decimal_clock_like <- decimal_looks_like_clock_notation(decimal_value)

  clean_minutes <- rep(NA_real_, length(clock_minutes))
  rule <- rep(NA_character_, length(clock_minutes))
  needs_review <- rep(FALSE, length(clock_minutes))

  clock_missing <- is.na(clock_minutes)
  decimal_missing <- is.na(decimal_minutes)
  both_missing <- clock_missing & decimal_missing
  clock_only <- !clock_missing & decimal_missing
  decimal_only <- clock_missing & !decimal_missing
  both_present <- !clock_missing & !decimal_missing

  clean_minutes[both_missing] <- NA_real_
  rule[both_missing] <- "both_missing"

  clean_minutes[clock_only] <- clock_minutes[clock_only]
  rule[clock_only] <- "clock_only_use_clock"

  clean_minutes[decimal_only] <- decimal_minutes[decimal_only]
  rule[decimal_only] <- "decimal_only_use_decimal"

  if (any(both_present)) {
    direct_diff <- circular_minute_diff(clock_minutes, decimal_minutes)
    decimal_clock_diff <- circular_minute_diff(clock_minutes, decimal_clock_minutes)

    # If the two versions already line up, keep the clock entry.
    matched_idx <- both_present & direct_diff <= small_diff_tolerance_minutes
    clean_minutes[matched_idx] <- clock_minutes[matched_idx]
    rule[matched_idx] <- "matched_within_tolerance_use_clock"

    # Values like 7.45 usually mean 7:45, not 7.45 hours.
    clock_notation_idx <- both_present &
      is.na(rule) &
      decimal_clock_like &
      decimal_clock_diff <= small_diff_tolerance_minutes
    clean_minutes[clock_notation_idx] <- clock_minutes[clock_notation_idx]
    rule[clock_notation_idx] <- "decimal_uses_clock_notation_use_clock"

    # A clean 12-hour gap usually means the clock field flipped AM/PM.
    twelve_hour_idx <- both_present &
      is.na(rule) &
      abs(direct_diff - 720) <= twelve_hour_tolerance_minutes
    clean_minutes[twelve_hour_idx] <- decimal_minutes[twelve_hour_idx]
    rule[twelve_hour_idx] <- "clock_ampm_flip_use_decimal"

    # Anything else is too messy to auto-fix.
    review_idx <- both_present & is.na(rule)
    clean_minutes[review_idx] <- NA_real_
    rule[review_idx] <- "needs_manual_review"
    needs_review[review_idx] <- TRUE
  }

  list(
    clean_minutes = clean_minutes,
    rule = rule,
    needs_review = needs_review
  )
}

# Add cleaned time columns next to the raw paired fields.
add_reconciled_time_columns <- function(data) {
  out <- data

  for (config in paired_time_configs()) {
    if (!all(c(config$clock_col, config$decimal_col) %in% names(out))) {
      next
    }

    reconciliation <- reconcile_time_pair(
      out[[config$clock_col]],
      out[[config$decimal_col]]
    )
    clean_decimal <- minutes_to_decimal_hours(reconciliation$clean_minutes)

    out[[config$clean_minutes_col]] <- reconciliation$clean_minutes
    out[[config$clean_clock_col]] <- minutes_to_clock_fraction(reconciliation$clean_minutes)
    out[[config$clean_decimal_col]] <- clean_decimal
    out[[config$rule_col]] <- reconciliation$rule
    out[[config$review_col]] <- reconciliation$needs_review

    if (!is.null(config$clean_relative_col)) {
      out[[config$clean_relative_col]] <- decimal_hours_to_relative_midnight(clean_decimal)
    }
  }

  out
}

# Pull just the rows that still need a person to look at them.
collect_time_reconciliation_review_rows <- function(data) {
  review_rows <- list()
  id_cols <- intersect(
    c("Participant ID", "ParticipantID", "Study Day", "StudyDay", "Date", "C_ACTI_Date"),
    names(data)
  )

  for (config in paired_time_configs()) {
    needed_cols <- c(
      config$clock_col,
      config$decimal_col,
      config$clean_clock_col,
      config$clean_decimal_col,
      config$rule_col,
      config$review_col
    )

    if (!all(needed_cols %in% names(data))) {
      next
    }

    flagged <- which(!is.na(data[[config$review_col]]) & data[[config$review_col]])

    if (length(flagged) == 0) {
      next
    }

    review_rows[[config$base_label]] <- data.frame(
      data[id_cols][flagged, , drop = FALSE],
      Time_Field = config$base_label,
      Raw_Clock_Time = format_clock_time_for_export(data[[config$clock_col]][flagged]),
      Raw_Decimal_Time = format_time_numeric_for_export(data[[config$decimal_col]][flagged]),
      Clean_Clock_Time = format_clock_time_for_export(data[[config$clean_clock_col]][flagged]),
      Clean_Decimal_Time = format_time_numeric_for_export(data[[config$clean_decimal_col]][flagged]),
      Reconciliation_Rule = data[[config$rule_col]][flagged],
      stringsAsFactors = FALSE
    )
  }

  if (length(review_rows) == 0) {
    data.frame(
      Message = "No rows flagged for time reconciliation review.",
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, review_rows)
  }
}

# Count how each reconciliation rule was used.
collect_time_reconciliation_summary <- function(data) {
  summary_rows <- list()

  for (config in paired_time_configs()) {
    if (!all(c(config$rule_col, config$review_col) %in% names(data))) {
      next
    }

    tmp <- data.frame(
      Time_Field = config$base_label,
      Reconciliation_Rule = data[[config$rule_col]],
      Needs_Review = data[[config$review_col]],
      stringsAsFactors = FALSE
    )

    tmp <- tmp[!is.na(tmp$Reconciliation_Rule), , drop = FALSE]

    if (nrow(tmp) == 0) {
      next
    }

    summary_df <- as.data.frame(
      table(
        Time_Field = tmp$Time_Field,
        Reconciliation_Rule = tmp$Reconciliation_Rule,
        Needs_Review = tmp$Needs_Review,
        useNA = "ifany"
      ),
      stringsAsFactors = FALSE
    )
    names(summary_df)[names(summary_df) == "Freq"] <- "n_rows"
    summary_rows[[config$base_label]] <- summary_df[summary_df$n_rows > 0, , drop = FALSE]
  }

  if (length(summary_rows) == 0) {
    data.frame(
      Message = "No time reconciliation summary available.",
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, summary_rows)
  }
}

# Normalize all recognized date and time fields in one pass.
normalize_date_time_fields <- function(data) {
  out <- data
  date_cols <- intersect(detect_date_columns(names(out)), names(out))
  clock_time_cols <- intersect(detect_clock_time_columns(names(out)), names(out))
  time_numeric_cols <- intersect(
    names(out)[grepl("(Decimal Hour|Decimal Time|Relative to Midnight Time|Latency$)", names(out))],
    names(out)
  )

  for (col_name in date_cols) {
    out[[col_name]] <- normalize_date_column(out[[col_name]])
  }

  for (col_name in clock_time_cols) {
    out[[col_name]] <- normalize_clock_time_column(out[[col_name]])
  }

  for (col_name in setdiff(time_numeric_cols, clock_time_cols)) {
    out[[col_name]] <- normalize_time_numeric_column(out[[col_name]])
  }

  out
}

# Round ordinary numeric columns without touching time fields.
round_non_datetime_numeric <- function(data, digits = 2) {
  out <- data
  time_numeric_cols <- names(out)[
    grepl("(Decimal Hour|Decimal Time|Relative to Midnight Time|Latency$)", names(out))
  ]
  protected_cols <- unique(c(
    detect_date_columns(names(out)),
    detect_clock_time_columns(names(out)),
    time_numeric_cols
  ))
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  round_cols <- setdiff(numeric_cols, protected_cols)

  for (col_name in round_cols) {
    out[[col_name]] <- round(out[[col_name]], digits = digits)
  }

  out
}

# Write dates out as plain text so Excel shows them cleanly.
format_date_for_export <- function(x) {
  out <- normalize_date_column(x)
  ifelse(is.na(out), NA_character_, format(out, "%Y-%m-%d"))
}

# Write clock times as HH:MM instead of raw Excel numbers.
format_clock_time_for_export <- function(x) {
  frac <- normalize_clock_time_column(x)
  seconds <- round(frac * 86400) %% 86400
  stamp <- as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
  ifelse(is.na(frac), NA_character_, format(stamp, "%H:%M"))
}

# Keep decimal time fields readable on export too.
format_time_numeric_for_export <- function(x) {
  vals <- normalize_time_numeric_column(x)
  out <- rep(NA_character_, length(vals))
  non_missing <- !is.na(vals)
  out[non_missing] <- trimws(
    format(vals[non_missing], scientific = FALSE, trim = TRUE, digits = 15)
  )
  out
}

# Apply the export formatting right before writing a workbook.
prepare_excel_export <- function(data) {
  out <- data
  date_cols <- intersect(detect_date_columns(names(out)), names(out))
  clock_time_cols <- intersect(detect_clock_time_columns(names(out)), names(out))
  time_numeric_cols <- intersect(
    names(out)[grepl("(Decimal Hour|Decimal Time|Relative to Midnight Time|Latency$)", names(out))],
    names(out)
  )

  for (col_name in date_cols) {
    out[[col_name]] <- format_date_for_export(out[[col_name]])
  }

  for (col_name in clock_time_cols) {
    out[[col_name]] <- format_clock_time_for_export(out[[col_name]])
  }

  for (col_name in setdiff(time_numeric_cols, clock_time_cols)) {
    out[[col_name]] <- format_time_numeric_for_export(out[[col_name]])
  }

  out
}
