

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