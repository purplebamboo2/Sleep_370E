library(readxl)
source("./excel_date_time_utils.R")

#Data Input ----
kalinka <- suppressWarnings(
  read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_KALIKA_11.24.2025_updated.xlsx")
)
rayaan <- suppressWarnings(
  read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_RAYAAN_12.12.2025.xlsx")
)
kalinka[kalinka == -888] <- NA
rayaan[rayaan == -888] <- NA
kalinka[kalinka == -999] <- NA
rayaan[rayaan == -999] <- NA
kalinka[kalinka == -555] <- NA
rayaan[rayaan == -555] <- NA
# Put dates and clock times on the same scale before comparing files.
kalinka <- normalize_date_time_fields(kalinka)
rayaan <- normalize_date_time_fields(rayaan)



#Discrepancy Analysis ----
all.equal(kalinka, rayaan)

#Fixing Process ----

all(names(kalinka) == names(rayaan))
intersect(names(kalinka), names(rayaan))

#kalinka only columns
setdiff(names(kalinka), names(rayaan))
#rayaan only columns
setdiff(names(rayaan), names(kalinka))

#column name fixes to make them match ----
names(kalinka)[names(kalinka) == "AdditionalComment"] <- "Additional Comment"
names(rayaan)[names(rayaan) == "Sleep Enviornment"] <- "Sleep Environment"
names(rayaan)[names(rayaan) == "Alcoholo Consumption"] <- "Alcohol Consumption"

#columns recheck ----
setdiff(names(kalinka), names(rayaan))
setdiff(names(rayaan), names(kalinka))

all(names(kalinka) == names(rayaan))

rayaan <- rayaan[, names(kalinka)]
identical(names(kalinka), names(rayaan))

type_check <- data.frame(
  column = names(kalinka),
  kalinka_type = sapply(kalinka, function(x) class(x)[1]),
  rayaan_type = sapply(rayaan, function(x) class(x)[1])
)

type_check[type_check$kalinka_type != type_check$rayaan_type, ]

for (col in names(kalinka)) {
  rayaan[[col]] <- as(rayaan[[col]], class(rayaan[[col]])[1])
}

identical(rayaan, kalinka) 
which(!(rayaan == kalinka) | (is.na(rayaan) != is.na(kalinka)), arr.ind = TRUE) 
which(rayaan != kalinka, arr.ind = TRUE)

sum((rayaan != kalinka) | (is.na(rayaan) != is.na(kalinka)), na.rm = TRUE)



#Merged data creation ----
new_data <- kalinka

for (col in names(kalinka)) {
  new_data[[col]][is.na(new_data[[col]])] <- rayaan[[col]][is.na(new_data[[col]])]
}



#Discrepancy analysis between new data and old sets ----
identical(new_data, kalinka)
which(!(new_data == kalinka) | (is.na(new_data) != is.na(kalinka)), arr.ind = TRUE)
which(new_data != kalinka, arr.ind = TRUE)

sum((new_data != kalinka) | (is.na(new_data) != is.na(kalinka)), na.rm = TRUE)


identical(new_data, rayaan)
which(!(new_data == rayaan) | (is.na(new_data) != is.na(rayaan)), arr.ind = TRUE)
which(new_data != rayaan, arr.ind = TRUE)

sum((new_data != rayaan) | (is.na(new_data) != is.na(rayaan)), na.rm = TRUE)




#View Data
View(new_data)
View(kalinka)
View(rayaan)

#Changing Written to Categorical ----
library(dplyr)
library(stringr)

#Sleep Environment
new_data <- new_data %>%
  mutate(
    Sleep_Env_clean = case_when(
      str_detect(`Sleep Environment`, regex("my bed|my room|at my house|in my bed|home|bed|bed-bedroom|bedroom-bed|in the bedroom|bedroom/bed|bedroom|bedroom at home|bed in bedroom (home)|home|my home", ignore_case = TRUE)) ~ 1,
      str_detect(`Sleep Environment`, regex("mom", ignore_case = TRUE)) ~ 2,
      str_detect(`Sleep Environment`, regex("dad", ignore_case = TRUE)) ~ 3,
      str_detect(`Sleep Environment`, regex("aunt|grandma|grandparent|family", ignore_case = TRUE)) ~ 4,
      str_detect(`Sleep Environment`, regex("hotel|dorm", ignore_case = TRUE)) ~ 5,
      str_detect(`Sleep Environment`, regex("friend", ignore_case = TRUE)) ~ 6,
      str_detect(`Sleep Environment`, regex("couch", ignore_case = TRUE)) ~ 7,
      str_detect(`Sleep Environment`, regex("good|decent", ignore_case = TRUE)) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

#Nap Environment
new_data <- new_data %>%
  mutate(
    Nap_Env_clean = case_when(
      str_detect(`Nap Enviornment`, regex("bed|room|home|my room", ignore_case = TRUE)) ~ 1,
      str_detect(`Nap Enviornment`, regex("couch|my couch|on the couch", ignore_case = TRUE)) ~ 2,
      str_detect(`Nap Enviornment`, regex("dorm", ignore_case = TRUE)) ~ 3,
      str_detect(`Nap Enviornment`, regex("movie", ignore_case = TRUE)) ~ 4,
      str_detect(`Nap Enviornment`, regex("class", ignore_case = TRUE)) ~ 5,
      str_detect(`Nap Enviornment`, regex("car", ignore_case = TRUE)) ~ 6,
      TRUE ~ NA_real_
    )
  )

#Additional Comments
new_data <- new_data %>%
  mutate(
    Additonal_Comments_clean = case_when(
      str_detect(`Additional Comment`, regex("actigraph|did not wear", ignore_case = TRUE)) ~ 1,
      str_detect(`Additional Comment`, regex("menstrual|period|I'm sick, started my period", ignore_case = TRUE)) ~ 2,
      str_detect(`Additional Comment`, regex("not feeling well|sick", ignore_case = TRUE)) ~ 3,
      str_detect(`Additional Comment`, regex("difficult day|hard day", ignore_case = TRUE)) ~ 4,
      str_detect(`Additional Comment`, regex("good day|felt better, made cake|good day, better than usual", ignore_case = TRUE)) ~ 5,
      str_detect(`Additional Comment`, regex("exercise|practice|sport|activity|went to art school, walked a lot", ignore_case = TRUE)) ~ 6,
      str_detect(`Additional Comment`, regex("No|None", ignore_case = TRUE)) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


View(new_data)

#Rounding ----

library(dplyr)
new_data <- round_non_datetime_numeric(new_data, digits = 2)
View(new_data)

#Merging hours and minutes ----
new_data <- new_data %>%
  mutate(
    TV_Total_Minutes = coalesce(new_data$`TV Movies Hrs`,0)*60 + coalesce(new_data$`TV Movies Mins`,0),
    Videos_Total_Minutes = coalesce(new_data$`Watch Videos Hrs`,0)*60 + coalesce(new_data$`Watch Videos Mins`,0),
    VideoGames_Total_Minutes = coalesce(new_data$`Video Games Hrs`,0)*60 + coalesce(new_data$`Video Games Mins`,0),
    VideoChat_Total_Minutes = coalesce(new_data$`Video Chat Hrs`,0)*60 + coalesce(new_data$`Video Chat Mins`,0),
    Devices_Total_Minutes = coalesce(new_data$`Devices Hrs`,0)*60 + coalesce(new_data$`Devices Mins`,0),
    SocialMedia_Total_Minutes = coalesce(new_data$`Social Media Hrs`,0)*60 + coalesce(new_data$`Social Media Mins`,0)
  )
View(new_data)

#Online data add/first checks ----
online <- suppressWarnings(
  read_excel("./FINAL ONLINE PUSHAdolescentDailyDiary_3.9.2026.xlsx")
)
# Rename the blank online columns before normalization so the date column gets
# handled like every other date field.
names(online)[names(online) == "...44"] <- "online complete?"
names(online)[names(online) == "...45"] <- "online date?"
online <- normalize_date_time_fields(online)
View(online)


all.equal(new_data, online)

identical(names(new_data), names(online))
intersect(names(new_data), names(online))

#new_data only columns
setdiff(names(new_data), names(online))
#online only columns
setdiff(names(online), names(new_data))


#Fixing online data to match new_data columns ----
#names

names(online)[names(online) == "AdditionalComment"] <- "Additional Comment"
names(online)[names(online) == "Sleep Enviornment"] <- "Sleep Environment"

#check

#new_data only columns
setdiff(names(new_data), names(online))
#online only columns
setdiff(names(online), names(new_data))

#Categorical Changes
library(dplyr)
library(stringr)

#Sleep Environment
online <- online %>%
  mutate(
    Sleep_Env_clean = case_when(
      str_detect(`Sleep Environment`, regex("my bed|my room|at my house|in my bed|home|bed|bed-bedroom|bedroom-bed|in the bedroom|bedroom/bed|bedroom|bedroom at home|bed in bedroom (home)|home|my home", ignore_case = TRUE)) ~ 1,
      str_detect(`Sleep Environment`, regex("mom", ignore_case = TRUE)) ~ 2,
      str_detect(`Sleep Environment`, regex("dad", ignore_case = TRUE)) ~ 3,
      str_detect(`Sleep Environment`, regex("aunt|grandma|grandparent|family", ignore_case = TRUE)) ~ 4,
      str_detect(`Sleep Environment`, regex("hotel|dorm", ignore_case = TRUE)) ~ 5,
      str_detect(`Sleep Environment`, regex("friend", ignore_case = TRUE)) ~ 6,
      str_detect(`Sleep Environment`, regex("couch", ignore_case = TRUE)) ~ 7,
      str_detect(`Sleep Environment`, regex("good|decent", ignore_case = TRUE)) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

#Nap Environment
online <- online %>%
  mutate(
    Nap_Env_clean = case_when(
      str_detect(`Nap Enviornment`, regex("bed|room|home|my room", ignore_case = TRUE)) ~ 1,
      str_detect(`Nap Enviornment`, regex("couch|my couch|on the couch", ignore_case = TRUE)) ~ 2,
      str_detect(`Nap Enviornment`, regex("dorm", ignore_case = TRUE)) ~ 3,
      str_detect(`Nap Enviornment`, regex("movie", ignore_case = TRUE)) ~ 4,
      str_detect(`Nap Enviornment`, regex("class", ignore_case = TRUE)) ~ 5,
      str_detect(`Nap Enviornment`, regex("car", ignore_case = TRUE)) ~ 6,
      TRUE ~ NA_real_
    )
  )

#Additional Comments
online <- online %>%
  mutate(
    Additonal_Comments_clean = case_when(
      str_detect(`Additional Comment`, regex("actigraph|did not wear", ignore_case = TRUE)) ~ 1,
      str_detect(`Additional Comment`, regex("menstrual|period|I'm sick, started my period", ignore_case = TRUE)) ~ 2,
      str_detect(`Additional Comment`, regex("not feeling well|sick", ignore_case = TRUE)) ~ 3,
      str_detect(`Additional Comment`, regex("difficult day|hard day", ignore_case = TRUE)) ~ 4,
      str_detect(`Additional Comment`, regex("good day|felt better, made cake|good day, better than usual", ignore_case = TRUE)) ~ 5,
      str_detect(`Additional Comment`, regex("exercise|practice|sport|activity|went to art school, walked a lot", ignore_case = TRUE)) ~ 6,
      str_detect(`Additional Comment`, regex("No|None", ignore_case = TRUE)) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

#check

#new_data only columns
setdiff(names(new_data), names(online))
#online only columns
setdiff(names(online), names(new_data))


#Rounding

library(dplyr)
online <- round_non_datetime_numeric(online, digits = 2)


#Merging hours and minutes online
online <- online %>%
  mutate(
    TV_Total_Minutes = coalesce(online$`TV Movies Hrs`,0)*60 + coalesce(online$`TV Movies Mins`,0),
    Videos_Total_Minutes = coalesce(online$`Watch Videos Hrs`,0)*60 + coalesce(online$`Watch Videos Mins`,0),
    VideoGames_Total_Minutes = coalesce(online$`Video Games Hrs`,0)*60 + coalesce(online$`Video Games Mins`,0),
    VideoChat_Total_Minutes = coalesce(online$`Video Chat Hrs`,0)*60 + coalesce(online$`Video Chat Mins`,0),
    Devices_Total_Minutes = coalesce(online$`Devices Hrs`,0)*60 + coalesce(online$`Devices Mins`,0),
    SocialMedia_Total_Minutes = coalesce(online$`Social Media Hrs`,0)*60 + coalesce(online$`Social Media Mins`,0)
  )
View(online)

#check

#new_data only columns
setdiff(names(new_data), names(online))
#online only columns
setdiff(names(online), names(new_data))

new_data$DELETE <- NULL
online$"Academic Year" <- NA
new_data$"online complete?" <- NA
new_data$"online date?" <- NA

# Build cleaned time columns before stacking the diary and online files.
new_data <- add_reconciled_time_columns(new_data)
online <- add_reconciled_time_columns(online)

#final check

#new_data only columns
setdiff(names(new_data), names(online))
#online only columns
setdiff(names(online), names(new_data))

#Combined data set/pushing to git ----
combined <- rbind(new_data, online)
# Keep the review tabs in the same workbook as the combined data.
time_reconciliation_review_rows <- collect_time_reconciliation_review_rows(combined)
time_reconciliation_summary <- collect_time_reconciliation_summary(combined)
View(combined)

library(writexl)
write_xlsx(
  list(
    Sheet1 = prepare_excel_export(combined),
    time_reconciliation_review_rows = prepare_excel_export(time_reconciliation_review_rows),
    time_reconciliation_summary = prepare_excel_export(time_reconciliation_summary)
  ),
  "combined_data.xlsx"
)
