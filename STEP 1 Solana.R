

#Data Input ----
library(readxl)
source("./excel_date_time_utils.R")

step1 <- suppressWarnings(read_excel("./merged_diaries_actigraphy.xlsx", sheet = "best_cleaned_analysis_ready"))
View(step1)
step1$`Sleep Onset Decimal Hour` <- as.numeric(step1$`Sleep Onset Decimal Hour`)
step1$`Sleep Offset Time_Decimal Hour` <- as.numeric(step1$`Sleep Offset Time_Decimal Hour`)

#making means for jules----
library(dplyr)

means <- step1 %>%
  group_by(step1$`Global Participant ID`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
View(means)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(means, "mean_per_person.xlsx")



#All together vers----

#Building blocks----


install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")
install.packages("lmtest")
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(dplyr)
library(lmtest)

#Variability Dataset

sleep_var <- step1 %>%
  group_by(`Global Participant ID`) %>%
  summarise(
    onset_var = sd(`Sleep Onset Decimal Hour`, na.rm = TRUE),
    offset_var = sd(`Sleep Offset Time_Decimal Hour`, na.rm = TRUE),
    mean_alertness = mean(`Alertness Rating`, na.rm = TRUE),
    mean_wake_diff = mean(`Wake Difficulty Rating`, na.rm = TRUE),
    mean_anxiety = mean(`Anxiety Rating`, na.rm = TRUE),
    mean_sleep_quality = mean(`Sleep Quality`, na.rm = TRUE)
  )

step1 <- merge(step1, sleep_var,
               by = "Global Participant ID")

#Alertness Analysis----

#onset & offset
alertness1 <- lm(
  `Alertness Rating` ~
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(alertness1)
bptest(alertness1)

#onset, offset, duration
alertness2 <- lm(
  `Alertness Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(alertness2)
bptest(alertness2)


#onset, offset, duration, variability
alertness3 <- lm(
  `Alertness Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(alertness3)
bptest(alertness3)


#Wake Difficulty Analysis----

#onset & offset
wake_diff1 <- lm(
  `Wake Difficulty Rating` ~
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(wake_diff1)
bptest(wake_diff1)

#onset, offset, duration
wake_diff2 <- lm(
  `Wake Difficulty Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(wake_diff2)
bptest(wake_diff2)


#onset, offset, duration, variability
wake_diff3 <- lm(
  `Wake Difficulty Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(wake_diff3)
bptest(wake_diff3)
#Anxiety Analysis----

#onset & offset
anxiety1 <- lm(
  `Anxiety Rating` ~
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(anxiety1)
bptest(anxiety1)

#onset, offset, duration
anxiety2 <- lm(
  `Anxiety Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(anxiety2)
bptest(anxiety2)


#onset, offset, duration, variability
anxiety3 <- lm(
  `Anxiety Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(anxiety3)
bptest(anxiety3)
#Sleep Quality Analysis----

#onset & offset
sleep_quality1 <- lm(
  `Sleep Quality` ~
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(sleep_quality1)
bptest(sleep_quality1)

#onset, offset, duration
sleep_quality2 <- lm(
  `Sleep Quality` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour`,
  data = step1
)

summary(sleep_quality2)
bptest(sleep_quality2)


#onset, offset, duration, variability
sleep_quality3 <- lm(
  `Sleep Quality` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(sleep_quality3)
bptest(sleep_quality3)



#plots----

#CHECK MULTICOLLINEARITY

vif(model_alertness)

#REGRESSION DIAGNOSTIC PLOTS

plot(model_alertness)

#CORRELATION MATRIX

vars <- step1 %>%
  select(
    `Sleep Duration`,
    `Sleep Offset Time_Decimal Hour`,
    `Sleep Onset Decimal Hour`,
    `Alertness Rating`,
    `Wake Difficulty Rating`,
    `Anxiety Rating`,
    `Sleep Quality`
  )

corrplot(cor(vars, use = "pairwise.complete.obs"),
         method = "circle")

#SCATTERPLOTS WITH REGRESSION LINES

ggplot(step1,
       aes(x = `Sleep Onset Decimal Hour`,
           y = `Alertness Rating`)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(step1,
       aes(x = `Sleep Offset Time_Decimal Hour`,
           y = `Sleep Quality`)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm") +
  theme_minimal()

#VARIABILITY EFFECTS VISUALIZATION

ggplot(sleep_var,
       aes(x = onset_var,
           y = mean_alertness)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(sleep_var,
       aes(x = offset_var,
           y = mean_sleep_quality)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  theme_minimal()

#Pearson Correlation on DV----
install.packages("Hmisc")
install.packages("corrplot")
install.packages("GGally")
library(Hmisc)
library(corrplot)
library(GGally)

#dependent variables
dep_vars <- step1[, c(
  "Alertness Rating",
  "Wake Difficulty Rating",
  "Anxiety Rating",
  "Sleep Quality"
)]

#Pearson correlation matrix
cor_matrix <- cor(dep_vars,
                  use = "pairwise.complete.obs",
                  method = "pearson")

print(cor_matrix)

#Correlations WITH p-values
cor_test_results <- rcorr(as.matrix(dep_vars),
                          type = "pearson")

print(cor_test_results)

#Correlation plot
corrplot(cor_matrix,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9)

#Scatterplot matrix with regression lines
ggpairs(dep_vars)


#running robust standard errors----
library(sandwich)
library(lmtest)

coeftest(alertness3, vcov = vcovHC(alertness3, type = "HC3"))
coeftest(anxiety3, vcov = vcovHC(anxiety3, type = "HC3"))
coeftest(wake_diff3, vcov = vcovHC(wake_diff3, type = "HC3"))
coeftest(sleep_quality3, vcov = vcovHC(sleep_quality3, type = "HC3"))







# graphs for the final presentation----

install.packages("ggplot2")
install.packages("corrplot")
install.packages("broom")
install.packages("dplyr")

library(ggplot2)
library(corrplot)
library(broom)
library(dplyr)

# GRAPH 1: Sleep Duration → Alertness

ggplot(step1,
       aes(x = `Sleep Duration`,
           y = `Alertness Rating`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Sleep Duration Predicting Alertness",
       x = "Sleep Duration",
       y = "Alertness Rating")


# GRAPH 2: Offset Variability → Wake Difficulty

ggplot(step1,
       aes(x = offset_var,
           y = `Wake Difficulty Rating`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Wake-Time Variability Predicting Wake Difficulty",
       x = "Wake-Time Variability",
       y = "Wake Difficulty Rating")


# GRAPH 3: Duration → Anxiety

ggplot(step1,
       aes(x = `Sleep Duration`,
           y = `Anxiety Rating`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Sleep Duration Predicting Anxiety",
       x = "Sleep Duration",
       y = "Anxiety Rating")


# GRAPH 4: Offset Variability → Anxiety

ggplot(step1,
       aes(x = offset_var,
           y = `Anxiety Rating`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Wake-Time Variability Predicting Anxiety",
       x = "Wake-Time Variability",
       y = "Anxiety Rating")


# GRAPH 5: Duration → Sleep Quality

ggplot(step1,
       aes(x = `Sleep Duration`,
           y = `Sleep Quality`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Sleep Duration Predicting Sleep Quality",
       x = "Sleep Duration",
       y = "Sleep Quality")


# GRAPH 6: Offset Time → Sleep Quality

ggplot(step1,
       aes(x = `Sleep Offset Time_Decimal Hour`,
           y = `Sleep Quality`)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Wake Time Predicting Sleep Quality",
       x = "Sleep Offset (Decimal Hour)",
       y = "Sleep Quality")


# GRAPH 7: CORRELATION HEATMAP (DEPENDENT VARIABLES)

library(Hmisc)

dep_vars <- step1[, c(
  "Alertness Rating",
  "Wake Difficulty Rating",
  "Anxiety Rating",
  "Sleep Quality"
)]

cor_results <- rcorr(as.matrix(dep_vars))

corrplot(cor_results$r,
         p.mat = cor_results$P,
         sig.level = 0.05,
         insig = "blank",
         method = "circle",
         type = "upper")





# START UP----
library(readxl)
library(dplyr)
library(ggplot2)

# Prefer using cleaned data from Discrepancy.R if it is already in memory.
if (exists("new_data")) {
  sleep_data <- new_data
} else {
  sleep_data <- read_excel(file.choose(), na = c("-999", "-888"))
}

names(sleep_data) <- make.names(names(sleep_data))

time_cols <- c(
  "Sleep.Onset.Decimal.Hour",
  "Sleep.Offset.Time_Decimal.Hour",
  "Nap.Start.Decimal.Time",
  "Nap.End.Decimal.Time",
  "First.Meal.Decimal.Time",
  "Last.Meal.Decimal.Time"
)

analysis_numeric_cols <- c(
  "Sleep.Duration",
  "Physical.Activity.Mins",
  "Sleep.Quality",
  "Alertness.Rating",
  "Wake.Difficulty.Rating"
)

numeric_cols <- unique(c(time_cols, analysis_numeric_cols))

missing_cols <- setdiff(numeric_cols, names(sleep_data))
if (length(missing_cols) > 0) {
  stop(paste("Missing required numeric columns:", paste(missing_cols, collapse = ", ")))
}

to_numeric_clean <- function(x) {
  x <- as.character(x)
  x[x %in% c("L", "-999", "-888", "")] <- NA
  suppressWarnings(as.numeric(x))
}

sleep_data[numeric_cols] <- lapply(sleep_data[numeric_cols], to_numeric_clean)

summary(sleep_data)

# Check if the structure looks correct now (should see numeric columns)
str(sleep_data)

#ROUNDING TIME----
#--Onset times ----
Onset_sin <- sin(2 * pi * sleep_data$Sleep.Onset.Decimal.Hour / 24)
Onset_cos <- cos(2 * pi * sleep_data$Sleep.Onset.Decimal.Hour / 24)

plot(Onset_cos, Onset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Onset Time (Circular Encoding)")


#--Offset times ----
Offset_sin <- sin(2 * pi * sleep_data$Sleep.Offset.Time_Decimal.Hour / 24)
Offset_cos <- cos(2 * pi * sleep_data$Sleep.Offset.Time_Decimal.Hour / 24)

plot(Offset_cos, Offset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Offset Time (Circular Encoding)")


Onset_circ <- atan2(Onset_sin, Onset_cos)

sleep_data <- sleep_data %>%
  mutate(
    Onset_circ = atan2(Onset_sin, Onset_cos),  
    Onset_circ = ifelse(Onset_circ < 0, Onset_circ + 2*pi, Onset_circ),  
    start_hour  = Onset_circ * 24 / (2*pi),     
    
    Offset_circ = atan2(Offset_sin, Offset_cos),  
    Offset_circ = ifelse(Offset_circ < 0, Offset_circ + 2*pi, Offset_circ),  
    end_hour  = Offset_circ * 24 / (2*pi)     
  )






#Start and end plot----

sleep_plot_data <- sleep_data %>%
  select(Onset_circ, Offset_circ) %>%
  mutate(id = row_number()) %>%
  tidyr::pivot_longer(cols = c(Onset_circ, Offset_circ),
                      names_to = "type", values_to = "angle") 


ggplot(sleep_plot_data, aes(x = angle, y = 1, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  coord_polar(theta = "x", start = -pi/2) +  # rotate so 12AM is top
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM",
               "2PM","4PM","6PM","8PM","10PM")
  ) +
  scale_color_manual(values = c("Onset_circ"="darkblue", "Offset_circ"="darkgoldenrod2"),
                     labels = c("End Time", "Start Time")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Sleep Start and End Times (Clock View)", color = "")



library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

source("./excel_date_time_utils.R")

step1 <- suppressWarnings(read_excel(
  "./merged_diaries_actigraphy.xlsx",
  sheet = "best_cleaned_analysis_ready"
))

step1$`Sleep Onset Decimal Hour` <- as.numeric(step1$`Sleep Onset Decimal Hour`)
step1$`Sleep Offset Time_Decimal Hour` <- as.numeric(step1$`Sleep Offset Time_Decimal Hour`)

# Circular encoding
step1 <- step1 %>%
  mutate(
    Onset_sin = sin(2 * pi * `Sleep Onset Decimal Hour` / 24),
    Onset_cos = cos(2 * pi * `Sleep Onset Decimal Hour` / 24),
    
    Offset_sin = sin(2 * pi * `Sleep Offset Time_Decimal Hour` / 24),
    Offset_cos = cos(2 * pi * `Sleep Offset Time_Decimal Hour` / 24),
    
    Onset_circ = atan2(Onset_sin, Onset_cos),
    Onset_circ = ifelse(Onset_circ < 0, Onset_circ + 2*pi, Onset_circ),
    
    Offset_circ = atan2(Offset_sin, Offset_cos),
    Offset_circ = ifelse(Offset_circ < 0, Offset_circ + 2*pi, Offset_circ),
    
    start_hour = Onset_circ * 24 / (2*pi),
    end_hour = Offset_circ * 24 / (2*pi)
  )

# Clock view data
sleep_plot_data <- step1 %>%
  select(Onset_circ, Offset_circ) %>%
  filter(!is.na(Onset_circ), !is.na(Offset_circ)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(Onset_circ, Offset_circ),
    names_to = "type",
    values_to = "angle"
  )

# Clock plot
ggplot(sleep_plot_data, aes(x = angle, y = 1, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  coord_polar(theta = "x", start = -pi/2) +
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM",
               "2PM","4PM","6PM","8PM","10PM")
  ) +
  scale_color_manual(
    values = c("Onset_circ" = "darkblue",
               "Offset_circ" = "darkgoldenrod2"),
    labels = c("Start Time", "End Time")
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Sleep Start and End Times (Clock View)",
    color = ""
  )



library(readxl)
library(dplyr)

step1 <- suppressWarnings(read_excel(
  "./merged_diaries_actigraphy.xlsx",
  sheet = "best_cleaned_analysis_ready"
))

# Convert to numeric
step1$`Sleep Onset Decimal Hour` <- as.numeric(step1$`Sleep Onset Decimal Hour`)
step1$`Sleep Offset Time_Decimal Hour` <- as.numeric(step1$`Sleep Offset Time_Decimal Hour`)

# Function for circular mean (in hours)
circular_mean_hour <- function(time_vec) {
  time_vec <- time_vec[!is.na(time_vec)]
  
  sin_mean <- mean(sin(2 * pi * time_vec / 24))
  cos_mean <- mean(cos(2 * pi * time_vec / 24))
  
  angle <- atan2(sin_mean, cos_mean)
  if (angle < 0) angle <- angle + 2*pi
  
  hour <- angle * 24 / (2*pi)
  return(hour)
}

# Compute averages
avg_sleep_time <- circular_mean_hour(step1$`Sleep Onset Decimal Hour`)
avg_wake_time  <- circular_mean_hour(step1$`Sleep Offset Time_Decimal Hour`)

avg_sleep_time
avg_wake_time