library(readxl)
source("./excel_date_time_utils.R")

#Data Input ----
step1 <- suppressWarnings(read_excel("./merged_diaries_actigraphy.xlsx"))
View(step1)

#making means for jules----
library(dplyr)

means <- step1 %>%
  group_by(step1$`Global Participant ID`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
View(means)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(means, "mean_per_person.xlsx")