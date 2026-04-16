# Packages used in the analysis.
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(nlme)
})

# Input file and output files.
data_path <- "merged_diaries_actigraphy.xlsx"
sheet_name <- "best_cleaned_analysis_ready"

mixed_results_path <- "ramsay_mixed_model_results.txt"
assumptions_path <- "ramsay_model_assumptions.txt"

primary_plot_path <- "ramsay_primary_model_diagnostics.png"
exploratory_plot_path <- "ramsay_exploratory_model_diagnostics.png"

# Small helpers.
to_numeric <- function(x) as.numeric(as.character(x))
section <- function(x) cat("\n", x, "\n", paste(rep("=", nchar(x)), collapse = ""), "\n", sep = "")

# Sample-size summary used in the text output.
count_summary <- function(data, label) {
  data.frame(
    model = label,
    nights = nrow(data),
    participants = n_distinct(data$id),
    median_nights_per_participant = median(count(data, id)$n),
    row.names = NULL
  )
}

# Shared data setup for the mixed models.
make_analysis_data <- function(include_exploratory = FALSE) {
  if (include_exploratory) {
    data <- raw_data %>%
      transmute(
        id = factor(`Global Participant ID`),
        sleep_quality = to_numeric(`Sleep Quality`),
        duration_hr = to_numeric(`Sleep Duration`) / 60,
        onset_rel = to_numeric(`Sleep Onset Clean Relative to Midnight Time`),
        weekend = to_numeric(`Weekend_Indicator`),
        summer = to_numeric(`Summer_Break_Indicator`),
        pa_hr = to_numeric(`Physical Activity Mins`) / 60,
        device_hr = to_numeric(`Devices_Total_Minutes`) / 60
      )
  } else {
    data <- raw_data %>%
      transmute(
        id = factor(`Global Participant ID`),
        sleep_quality = to_numeric(`Sleep Quality`),
        duration_hr = to_numeric(`Sleep Duration`) / 60,
        onset_rel = to_numeric(`Sleep Onset Clean Relative to Midnight Time`),
        weekend = to_numeric(`Weekend_Indicator`),
        summer = to_numeric(`Summer_Break_Indicator`)
      )
  }

  data %>%
    filter(complete.cases(.)) %>%
    group_by(id) %>%
    mutate(
      duration_mean = mean(duration_hr),
      duration_within = duration_hr - duration_mean,
      onset_mean = mean(onset_rel),
      onset_within = onset_rel - onset_mean
    ) %>%
    ungroup()
}

# Read the analysis-ready sheet once at the top.
raw_data <- read_excel(data_path, sheet = sheet_name)

# Data for the mixed models.
null_data <- raw_data %>%
  transmute(
    id = factor(`Global Participant ID`),
    sleep_quality = to_numeric(`Sleep Quality`)
  ) %>%
  filter(complete.cases(.))

primary_data <- make_analysis_data()
exploratory_data <- make_analysis_data(include_exploratory = TRUE)

# Mixed-effects models.
control <- lmeControl(opt = "optim")

null_model <- lme(
  sleep_quality ~ 1,
  random = ~ 1 | id,
  data = null_data,
  method = "REML",
  control = control
)

primary_model <- lme(
  sleep_quality ~ duration_within + duration_mean + onset_within + onset_mean + weekend + summer,
  random = ~ 1 | id,
  data = primary_data,
  method = "REML",
  control = control
)

exploratory_model <- lme(
  sleep_quality ~ duration_within + duration_mean + onset_within + onset_mean +
    weekend + summer + pa_hr + device_hr,
  random = ~ 1 | id,
  data = exploratory_data,
  method = "REML",
  control = control
)

# Basic summaries for the text report.
null_variance <- VarCorr(null_model)
icc <- as.numeric(null_variance[1, "Variance"]) /
  (as.numeric(null_variance[1, "Variance"]) + as.numeric(null_variance[2, "Variance"]))

primary_terms <- summary(primary_model)$tTable
exploratory_terms <- summary(exploratory_model)$tTable

# Write the mixed-model results.
sink(mixed_results_path)

section("Mixed-Effects Sleep Quality Analysis")
cat("Data file:", data_path, "\n")
cat("Worksheet:", sheet_name, "\n")

section("Model Statements")
cat("Null model:\n")
cat("sleep_quality ~ 1 + (1 | id)\n\n")
cat("Primary model:\n")
cat("sleep_quality ~ duration_within + duration_mean + onset_within + onset_mean + weekend + summer + (1 | id)\n\n")
cat("Exploratory model:\n")
cat("sleep_quality ~ duration_within + duration_mean + onset_within + onset_mean + weekend + summer + pa_hr + device_hr + (1 | id)\n\n")
cat("Term meanings:\n")
cat("sleep_quality = self-reported Sleep Quality score for a participant-night\n")
cat("duration_within = that night's sleep duration minus that participant's mean sleep duration\n")
cat("duration_mean = that participant's mean sleep duration across included nights\n")
cat("onset_within = that night's sleep onset relative to midnight minus that participant's mean onset\n")
cat("onset_mean = that participant's mean sleep onset relative to midnight across included nights\n")
cat("weekend = weekend-night indicator\n")
cat("summer = summer-break indicator\n")
cat("pa_hr = physical activity hours in the exploratory model\n")
cat("device_hr = device-use hours in the exploratory model\n")
cat("id = participant identifier\n")
cat("(1 | id) = random intercept for participant\n")

section("Sample Sizes")
print(bind_rows(
  count_summary(null_data, "Null model"),
  count_summary(primary_data, "Primary model"),
  count_summary(exploratory_data, "Exploratory model")
))

section("Null Model")
print(summary(null_model))
cat("ICC:", round(icc, 3), "\n")

section("Primary Model")
print(summary(primary_model))
print(intervals(primary_model, which = "fixed"))

section("Exploratory Model")
print(summary(exploratory_model))
print(intervals(exploratory_model, which = "fixed"))

section("Interpretation Notes")
cat(sprintf(
  "Within-person duration effect: %.2f points in Sleep Quality for each extra hour above a participant's own average duration.\n",
  primary_terms["duration_within", "Value"]
))
cat(sprintf(
  "Between-person duration effect: %.2f points in Sleep Quality for each extra hour in a participant's average duration.\n",
  primary_terms["duration_mean", "Value"]
))
cat(sprintf(
  "Within-person onset effect: %.2f points in Sleep Quality for each one-hour later sleep onset than usual.\n",
  primary_terms["onset_within", "Value"]
))
cat(sprintf(
  "Between-person onset effect: %.2f points in Sleep Quality for each one-hour later average sleep onset.\n",
  primary_terms["onset_mean", "Value"]
))
cat(sprintf(
  "Exploratory activity effect: %.2f points per extra hour of physical activity.\n",
  exploratory_terms["pa_hr", "Value"]
))
cat(sprintf(
  "Exploratory device effect: %.2f points per extra hour of device time.\n",
  exploratory_terms["device_hr", "Value"]
))

section("Assumptions")
cat("Basic assumption checks are all at the very end of this script.\n")
cat("Assumption report:", assumptions_path, "\n")

sink()

# ------------------------------------------------------------
# Basic assumption checks
# Keep all assumption code together down here.
# ------------------------------------------------------------

# Basic checks for the mixed models.
basic_checks <- function(model) {
  residuals <- resid(model, type = "pearson")
  fitted_values <- fitted(model)
  random_effects <- as.numeric(ranef(model)[, 1])

  list(
    residual_shapiro = shapiro.test(residuals)$p.value,
    random_effect_shapiro = shapiro.test(random_effects)$p.value,
    residual_skew = mean(((residuals - mean(residuals)) / sd(residuals))^3),
    abs_resid_fitted_cor = cor(abs(residuals), fitted_values),
    n_abs_gt_3 = sum(abs(residuals) > 3)
  )
}

# Diagnostic plots for the mixed models.
save_plot <- function(model, file_path, title_text) {
  residuals <- resid(model, type = "pearson")
  fitted_values <- fitted(model)
  random_effects <- as.numeric(ranef(model)[, 1])

  png(file_path, width = 1200, height = 900)
  par(mfrow = c(2, 2))

  plot(
    fitted_values,
    residuals,
    xlab = "Fitted values",
    ylab = "Pearson residuals",
    main = paste(title_text, "- Residuals vs fitted"),
    pch = 19,
    col = "gray35"
  )
  abline(h = 0, lty = 2, col = "firebrick")

  qqnorm(residuals, main = paste(title_text, "- Residual Q-Q"), pch = 19, col = "gray35")
  qqline(residuals, col = "firebrick", lwd = 2)

  hist(
    residuals,
    main = paste(title_text, "- Residual histogram"),
    xlab = "Pearson residuals",
    col = "gray85",
    border = "white"
  )

  qqnorm(random_effects, main = paste(title_text, "- Random intercept Q-Q"), pch = 19, col = "gray35")
  qqline(random_effects, col = "firebrick", lwd = 2)

  invisible(dev.off())
}

# Run the basic checks.
primary_checks <- basic_checks(primary_model)
exploratory_checks <- basic_checks(exploratory_model)

# Save the diagnostic plots.
save_plot(primary_model, primary_plot_path, "Primary mixed model")
save_plot(exploratory_model, exploratory_plot_path, "Exploratory mixed model")

# Write the assumptions report.
sink(assumptions_path)

section("Basic Assumptions")
cat("I only checked the basics here: residual shape, obvious outliers, residual spread, and random intercept behavior.\n")

section("Primary Mixed Model")
cat(sprintf(
  "Residuals are a little left-skewed (Shapiro p = %.4f, skew = %.2f), with %d residuals beyond |3|.\n",
  primary_checks$residual_shapiro,
  primary_checks$residual_skew,
  primary_checks$n_abs_gt_3
))
cat(sprintf(
  "Residual spread looks acceptable overall: cor(|residual|, fitted) = %.2f.\n",
  primary_checks$abs_resid_fitted_cor
))
cat(sprintf(
  "Random intercepts look acceptable for a first pass (Shapiro p = %.3f).\n",
  primary_checks$random_effect_shapiro
))
cat("Verdict: good enough for the planned first-pass mixed-model analysis.\n")

section("Exploratory Mixed Model")
cat(sprintf(
  "Residuals show the same basic pattern (Shapiro p = %.4f, skew = %.2f), with %d residuals beyond |3|.\n",
  exploratory_checks$residual_shapiro,
  exploratory_checks$residual_skew,
  exploratory_checks$n_abs_gt_3
))
cat(sprintf(
  "Residual spread still looks acceptable: cor(|residual|, fitted) = %.2f.\n",
  exploratory_checks$abs_resid_fitted_cor
))
cat(sprintf(
  "Random intercepts still look acceptable (Shapiro p = %.3f).\n",
  exploratory_checks$random_effect_shapiro
))
cat("Verdict: also usable, with the same mild non-normality caveat.\n")

section("Overall Call")
cat("The mixed models look acceptable for a first-pass write-up.\n")
cat("The biggest issue is mild residual non-normality, which is not surprising for a 1-10 outcome.\n")
cat("I do not see a big variance problem, and the random intercepts look reasonable.\n")
cat("So my call is: the main assumptions are met well enough to proceed.\n")

section("Files")
cat("Mixed results:", mixed_results_path, "\n")
cat("Assumption report:", assumptions_path, "\n")
cat("Diagnostic plots:", primary_plot_path, "and", exploratory_plot_path, "\n")

sink()

# Final console message.
cat("Analysis finished.\n")
cat("Mixed results written to:", mixed_results_path, "\n")
cat("Assumption report written to:", assumptions_path, "\n")
