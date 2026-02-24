library(readxl)
library(dplyr)
library(ggplot2)

actigraphy_data <- read_excel("./PUSH_ACTIGRAPH_Long_FINAL_02.23.26.xlsx")

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
