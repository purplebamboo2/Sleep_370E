library(readxl)
source("./excel_date_time_utils.R")


#Data Input ----
step1 <- suppressWarnings(read_excel("./merged_diaries_actigraphy.xlsx"))
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


#Standard Deviations----


library(dplyr)

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
step1 <- merge(step1, sleep_var, by = "Global Participant ID")
#Alertness Analysis----

alert_onset <- lm(mean_alertness ~ onset_var, data = sleep_var)
summary(alert_onset)
alert_offset <- lm(mean_alertness ~ offset_var, data = sleep_var)
summary(alert_offset)


alert_onset_offset <- lm(step1$`Alertness Rating` ~ step1$`Sleep Offset Time_Decimal Hour` + 
                                         step1$`Sleep Onset Decimal Hour` ,data = step1)
summary(alert_onset_offset)

model_alertness <- lm(step1$`Alertness Rating` ~ step1$`Sleep Duration` + 
                                                 step1$`Sleep Offset Time_Decimal Hour` + 
                                                 step1$`Sleep Onset Decimal Hour`, data = step1)
summary(model_alertness)



#Wake Difficulty----

wake_onset <- lm(mean_wake_diff ~ onset_var, data = sleep_var)
summary(wake_onset)
wake_offset <- lm(mean_wake_diff ~ offset_var, data = sleep_var)
summary(wake_offset)


wake_onset_offset <- lm(step1$`Wake Difficulty Rating` ~ step1$`Sleep Offset Time_Decimal Hour` + 
                           step1$`Sleep Onset Decimal Hour` ,data = step1)
summary(wake_onset_offset)

model_wake <- lm(step1$`Wake Difficulty Rating` ~ step1$`Sleep Duration` + 
                        step1$`Sleep Offset Time_Decimal Hour` + 
                        step1$`Sleep Onset Decimal Hour`, data = step1)
summary(model_wake)

#Anxiety----

anxiety_onset <- lm(mean_anxiety ~ onset_var, data = sleep_var)
summary(anxiety_onset)
anxiety_offset <- lm(mean_anxiety ~ offset_var, data = sleep_var)
summary(anxiety_offset)


anxiety_onset_offset <- lm(step1$`Anxiety Rating` ~ step1$`Sleep Offset Time_Decimal Hour` + 
                           step1$`Sleep Onset Decimal Hour` ,data = step1)
summary(anxiety_onset_offset)

model_anxiety <- lm(step1$`Anxiety Rating` ~ step1$`Sleep Duration` + 
                        step1$`Sleep Offset Time_Decimal Hour` + 
                        step1$`Sleep Onset Decimal Hour`, data = step1)
summary(model_anxiety)
#Sleep Quality----

sleep_onset <- lm(mean_sleep_quality ~ onset_var, data = sleep_var)
summary(sleep_onset)
sleep_offset <- lm(mean_sleep_quality ~ offset_var, data = sleep_var)
summary(sleep_offset)


sleep_onset_offset <- lm(step1$`Sleep Quality` ~ step1$`Sleep Offset Time_Decimal Hour` + 
                           step1$`Sleep Onset Decimal Hour` ,data = step1)
summary(sleep_onset_offset)

model_sleep <- lm(step1$`Sleep Quality` ~ step1$`Sleep Duration` + 
                        step1$`Sleep Offset Time_Decimal Hour` + 
                        step1$`Sleep Onset Decimal Hour`, data = step1)
summary(model_sleep)



#Redo

# INSTALL PACKAGES (run once)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")

# LOAD LIBRARIES

library(dplyr)
library(ggplot2)
library(car)
library(corrplot)

# STEP 1: CREATE VARIABILITY DATASET

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

# STEP 2: MERGE VARIABILITY BACK INTO DATA

step1 <- merge(step1, sleep_var,
               by = "Global Participant ID")

# STEP 3: ALERTNESS MODEL

model_alertness <- lm(
  `Alertness Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(model_alertness)

# STEP 4: WAKE DIFFICULTY MODEL

model_wake <- lm(
  `Wake Difficulty Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(model_wake)

# STEP 5: ANXIETY MODEL

model_anxiety <- lm(
  `Anxiety Rating` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(model_anxiety)

# STEP 6: SLEEP QUALITY MODEL

model_sleep <- lm(
  `Sleep Quality` ~
    `Sleep Duration` +
    `Sleep Offset Time_Decimal Hour` +
    `Sleep Onset Decimal Hour` +
    onset_var +
    offset_var,
  data = step1
)

summary(model_sleep)

# STEP 7: CHECK MULTICOLLINEARITY

vif(model_alertness)

# STEP 8: REGRESSION DIAGNOSTIC PLOTS

plot(model_alertness)

# STEP 9: CORRELATION MATRIX

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

# STEP 10: SCATTERPLOTS WITH REGRESSION LINES

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

# STEP 11: VARIABILITY EFFECTS VISUALIZATION

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

