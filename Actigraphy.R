library(readxl)
library(dplyr)
library(ggplot2)

actigraphy_data <- read_excel("./PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx")

View("./PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx")

numeric_cols <- names(actigraphy_data)[sapply(actigraphy_data, is.numeric)]

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

range_text <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  r <- range(x)
  paste0(round(r[1], 2), " to ", round(r[2], 2))
}

stats_table <- data.frame(
  Variable = numeric_cols,
  Mean = sapply(actigraphy_data[numeric_cols], function(x) mean(x, na.rm = TRUE)),
  Median = sapply(actigraphy_data[numeric_cols], function(x) median(x, na.rm = TRUE)),
  Mode = sapply(actigraphy_data[numeric_cols], mode_value),
  SD = sapply(actigraphy_data[numeric_cols], function(x) sd(x, na.rm = TRUE)),
  Range = sapply(actigraphy_data[numeric_cols], range_text),
  stringsAsFactors = FALSE
)

stats_table$Mean <- round(stats_table$Mean, 2)
stats_table$Median <- round(stats_table$Median, 2)
stats_table$Mode <- round(stats_table$Mode, 2)
stats_table$SD <- round(stats_table$SD, 2)

print(stats_table, row.names = FALSE)

actigraphy_data$Sleep.Onset.Decimal.Hour <- actigraphy_data$C_ACTI_SleepOnsetTime_TRM %% 24
actigraphy_data$Sleep.Offset.Time_Decimal.Hour <- actigraphy_data$C_ACTI_WT_DEC %% 24

Onset_sin <- sin(2 * pi * actigraphy_data$Sleep.Onset.Decimal.Hour / 24)
Onset_cos <- cos(2 * pi * actigraphy_data$Sleep.Onset.Decimal.Hour / 24)

plot(Onset_cos, Onset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Onset Time (Circular Encoding)")

Offset_sin <- sin(2 * pi * actigraphy_data$Sleep.Offset.Time_Decimal.Hour / 24)
Offset_cos <- cos(2 * pi * actigraphy_data$Sleep.Offset.Time_Decimal.Hour / 24)

plot(Offset_cos, Offset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Offset Time (Circular Encoding)")

Onset_circ <- atan2(Onset_sin, Onset_cos)

actigraphy_data <- actigraphy_data %>%
  mutate(
    Onset_circ = atan2(Onset_sin, Onset_cos),
    Onset_circ = ifelse(Onset_circ < 0, Onset_circ + 2 * pi, Onset_circ),
    start_hour = Onset_circ * 24 / (2 * pi),

    Offset_circ = atan2(Offset_sin, Offset_cos),
    Offset_circ = ifelse(Offset_circ < 0, Offset_circ + 2 * pi, Offset_circ),
    end_hour = Offset_circ * 24 / (2 * pi)
  )

ggplot(actigraphy_data, aes(x = Onset_circ, y = 1)) +
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi / 2) +
  scale_x_continuous(
    limits = c(0, 2 * pi),
    breaks = (0:11) * 2 * pi / 12,
    labels = c("12AM", "2AM", "4AM", "6AM", "8AM", "10AM",
               "12PM", "2PM", "4PM", "6PM", "8PM", "10PM")
  ) +
  theme_minimal() +
  labs(title = "Sleep Onset Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggplot(actigraphy_data, aes(x = Offset_circ, y = 1)) +
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi / 2) +
  scale_x_continuous(
    limits = c(0, 2 * pi),
    breaks = (0:11) * 2 * pi / 12,
    labels = c("12AM", "2AM", "4AM", "6AM", "8AM", "10AM",
               "12PM", "2PM", "4PM", "6PM", "8PM", "10PM")
  ) +
  theme_minimal() +
  labs(title = "Wake Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

actigraphy_plot_data <- actigraphy_data %>%
  select(Onset_circ, Offset_circ) %>%
  mutate(id = row_number()) %>%
  tidyr::pivot_longer(cols = c(Onset_circ, Offset_circ),
                      names_to = "type", values_to = "angle")

ggplot(actigraphy_plot_data, aes(x = angle, y = 1, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  coord_polar(theta = "x", start = -pi / 2) +
  scale_x_continuous(
    limits = c(0, 2 * pi),
    breaks = (0:11) * 2 * pi / 12,
    labels = c("12AM", "2AM", "4AM", "6AM", "8AM", "10AM",
               "12PM", "2PM", "4PM", "6PM", "8PM", "10PM")
  ) +
  scale_color_manual(values = c("Onset_circ" = "darkblue", "Offset_circ" = "darkgoldenrod2"),
                     labels = c("Onset_circ" = "Onset Time", "Offset_circ" = "Wake Time")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Sleep Onset and Wake Times (Clock View)", color = "")

stats_table_plot <- stats_table
stats_table_plot$Mean <- sprintf("%.2f", stats_table_plot$Mean)
stats_table_plot$Median <- sprintf("%.2f", stats_table_plot$Median)
stats_table_plot$Mode <- sprintf("%.2f", stats_table_plot$Mode)
stats_table_plot$SD <- sprintf("%.2f", stats_table_plot$SD)

header_line <- sprintf(
  "%-45s %9s %9s %9s %9s %20s",
  "Variable", "Mean", "Median", "Mode", "SD", "Range"
)

row_lines <- sprintf(
  "%-45s %9s %9s %9s %9s %20s",
  stats_table_plot$Variable,
  stats_table_plot$Mean,
  stats_table_plot$Median,
  stats_table_plot$Mode,
  stats_table_plot$SD,
  stats_table_plot$Range
)

table_lines <- c(header_line, row_lines)

table_plot_data <- data.frame(
  x = 1,
  y = rev(seq_along(table_lines)),
  label = table_lines,
  stringsAsFactors = FALSE
)

ggplot(table_plot_data, aes(x = x, y = y, label = label)) +
  geom_text(hjust = 0, family = "mono", size = 3) +
  xlim(1, 2.6) +
  theme_void() +
  labs(title = "Actigraphy Summary Statistics")

# -----------------------------
# Outlier Detection
# -----------------------------

# Screen only the nightly actigraphy variables. Repeated participant-level
# summary columns (_mean, _sd) and the duplicated WakeTime field are excluded.
acti_outlier_vars <- c(
  "C_ACTI_SleepOnsetTime_TRM",
  "C_ACTI_SOL",
  "C_ACTI_WT_DEC",
  "C_ACTI_Duration",
  "C_ACTI_Efficiency",
  "C_ACTI_Waso",
  "C_ACTI_NumberOfWakeBouts",
  "C_ACTI_SleepTime",
  "C_ACTI_NumberOfSleepBouts"
)

is_whole_number <- function(x, tol = 1e-8) {
  abs(x - round(x)) < tol
}

flag_within_person_outliers <- function(x, threshold = 2) {
  flags <- rep(FALSE, length(x))
  non_missing <- !is.na(x)

  if (sum(non_missing) < 4) {
    return(flags)
  }

  center <- mean(x, na.rm = TRUE)
  spread <- sd(x, na.rm = TRUE)

  if (!is.finite(spread) || spread == 0) {
    return(flags)
  }

  flags[non_missing] <- abs(x[non_missing] - center) / spread > threshold
  flags
}

rule_flag_df <- data.frame(
  # Relative bedtime should stay within a half-day window.
  C_ACTI_SleepOnsetTime_TRM = !is.na(actigraphy_data$C_ACTI_SleepOnsetTime_TRM) &
    (actigraphy_data$C_ACTI_SleepOnsetTime_TRM < -12 |
       actigraphy_data$C_ACTI_SleepOnsetTime_TRM > 12),
  # Sleep onset latency cannot be negative and 4+ hours is implausible here.
  C_ACTI_SOL = !is.na(actigraphy_data$C_ACTI_SOL) &
    (actigraphy_data$C_ACTI_SOL < 0 | actigraphy_data$C_ACTI_SOL > 240),
  # Clock time should fall within a 24-hour day.
  C_ACTI_WT_DEC = !is.na(actigraphy_data$C_ACTI_WT_DEC) &
    (actigraphy_data$C_ACTI_WT_DEC < 0 | actigraphy_data$C_ACTI_WT_DEC > 24),
  # Sleep interval under 1 hour or over 16 hours is not credible.
  C_ACTI_Duration = !is.na(actigraphy_data$C_ACTI_Duration) &
    (actigraphy_data$C_ACTI_Duration < 60 | actigraphy_data$C_ACTI_Duration > 960),
  # Efficiency is a percentage, so it must stay between 0 and 100.
  C_ACTI_Efficiency = !is.na(actigraphy_data$C_ACTI_Efficiency) &
    (actigraphy_data$C_ACTI_Efficiency < 0 | actigraphy_data$C_ACTI_Efficiency > 100),
  # Wake after sleep onset cannot be negative or exceed the sleep interval.
  C_ACTI_Waso = !is.na(actigraphy_data$C_ACTI_Waso) &
    (actigraphy_data$C_ACTI_Waso < 0 |
       actigraphy_data$C_ACTI_Waso > 720 |
       (!is.na(actigraphy_data$C_ACTI_Duration) &
          actigraphy_data$C_ACTI_Waso > actigraphy_data$C_ACTI_Duration)),
  # Bout counts should be nonnegative whole numbers and not extremely large.
  C_ACTI_NumberOfWakeBouts = !is.na(actigraphy_data$C_ACTI_NumberOfWakeBouts) &
    (actigraphy_data$C_ACTI_NumberOfWakeBouts < 0 |
       actigraphy_data$C_ACTI_NumberOfWakeBouts > 200 |
       !is_whole_number(actigraphy_data$C_ACTI_NumberOfWakeBouts)),
  # Total sleep time cannot be negative or longer than the interval itself.
  C_ACTI_SleepTime = !is.na(actigraphy_data$C_ACTI_SleepTime) &
    (actigraphy_data$C_ACTI_SleepTime < 0 |
       actigraphy_data$C_ACTI_SleepTime > 960 |
       (!is.na(actigraphy_data$C_ACTI_Duration) &
          actigraphy_data$C_ACTI_SleepTime > actigraphy_data$C_ACTI_Duration)),
  # Bout counts should be nonnegative whole numbers and not extremely large.
  C_ACTI_NumberOfSleepBouts = !is.na(actigraphy_data$C_ACTI_NumberOfSleepBouts) &
    (actigraphy_data$C_ACTI_NumberOfSleepBouts < 0 |
       actigraphy_data$C_ACTI_NumberOfSleepBouts > 200 |
       !is_whole_number(actigraphy_data$C_ACTI_NumberOfSleepBouts)),
  check.names = FALSE
)

within_person_flag_df <- setNames(
  data.frame(
    lapply(acti_outlier_vars, function(var_name) {
      unsplit(
        lapply(
          split(actigraphy_data[[var_name]], actigraphy_data$ParticipantID),
          flag_within_person_outliers
        ),
        actigraphy_data$ParticipantID
      )
    }),
    check.names = FALSE
  ),
  acti_outlier_vars
)

combined_flag_df <- setNames(
  data.frame(Map(`|`, rule_flag_df, within_person_flag_df), check.names = FALSE),
  acti_outlier_vars
)

collapse_outlier_reasons <- function(rule_row, within_row, variable_names) {
  rule_hits <- paste0(variable_names[rule_row], " [rule-based]")
  within_hits <- paste0(variable_names[within_row & !rule_row], " [within-person mean +/- 2 SD]")
  all_hits <- c(rule_hits, within_hits)

  if (length(all_hits) == 0) {
    return(NA_character_)
  }

  paste(all_hits, collapse = "; ")
}

actigraphy_data$ACTI_Outlier_Flag <- as.integer(rowSums(combined_flag_df, na.rm = TRUE) > 0)
actigraphy_data$ACTI_Outlier_Count <- rowSums(combined_flag_df, na.rm = TRUE)
actigraphy_data$ACTI_Outlier_Reasons <- vapply(
  seq_len(nrow(actigraphy_data)),
  function(i) {
    collapse_outlier_reasons(
      as.logical(rule_flag_df[i, ]),
      as.logical(within_person_flag_df[i, ]),
      acti_outlier_vars
    )
  },
  character(1)
)

outlier_summary_table <- data.frame(
  Variable = acti_outlier_vars,
  NonMissing_N = vapply(
    acti_outlier_vars,
    function(var_name) sum(!is.na(actigraphy_data[[var_name]])),
    integer(1)
  ),
  RuleBased_Flags = vapply(
    acti_outlier_vars,
    function(var_name) sum(rule_flag_df[[var_name]], na.rm = TRUE),
    integer(1)
  ),
  WithinPerson_Flags = vapply(
    acti_outlier_vars,
    function(var_name) sum(within_person_flag_df[[var_name]], na.rm = TRUE),
    integer(1)
  ),
  AnyFlag_Flags = vapply(
    acti_outlier_vars,
    function(var_name) sum(combined_flag_df[[var_name]], na.rm = TRUE),
    integer(1)
  ),
  PercentFlagged = round(
    100 * vapply(
      acti_outlier_vars,
      function(var_name) sum(combined_flag_df[[var_name]], na.rm = TRUE),
      integer(1)
    ) / pmax(
      vapply(
        acti_outlier_vars,
        function(var_name) sum(!is.na(actigraphy_data[[var_name]])),
        integer(1)
      ),
      1
    ),
    2
  ),
  stringsAsFactors = FALSE
)

overall_outlier_row <- data.frame(
  Variable = "ANY_RAW_METRIC",
  NonMissing_N = nrow(actigraphy_data),
  RuleBased_Flags = sum(rowSums(rule_flag_df, na.rm = TRUE) > 0),
  WithinPerson_Flags = sum(rowSums(within_person_flag_df, na.rm = TRUE) > 0),
  AnyFlag_Flags = sum(actigraphy_data$ACTI_Outlier_Flag, na.rm = TRUE),
  PercentFlagged = round(100 * mean(actigraphy_data$ACTI_Outlier_Flag, na.rm = TRUE), 2),
  stringsAsFactors = FALSE
)

outlier_summary_table <- bind_rows(outlier_summary_table, overall_outlier_row)

cat("\nActigraphy Outlier Summary\n")
print(outlier_summary_table, row.names = FALSE)
