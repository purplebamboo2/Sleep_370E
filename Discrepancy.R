install.packages("readxl")
library(readxl)

#Data
kalinka <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_KALIKA_11.24.2025_updated.xlsx")
rayaan <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_RAYAAN_12.12.2025.xlsx")
kalinka[kalinka == -888] <- NA
rayaan[rayaan == -888] <- NA
kalinka[kalinka == -999] <- NA
rayaan[rayaan == -999] <- NA
kalinka[kalinka == -555] <- NA
rayaan[rayaan == -555] <- NA



#Discrepancy Analysis
all.equal(kalinka, rayaan)

#Fixing Process

all(names(kalinka) == names(rayaan))
intersect(names(kalinka), names(rayaan))

#kalinka only columns
setdiff(names(kalinka), names(rayaan))
#rayaan only columns
setdiff(names(rayaan), names(kalinka))

#column name fixes
names(kalinka)[names(kalinka) == "AdditionalComment"] <- "Additional Comment"
names(rayaan)[names(rayaan) == "Sleep Enviornment"] <- "Sleep Environment"
names(rayaan)[names(rayaan) == "Alcoholo Consumption"] <- "Alcohol Consumption"

#kalinka only columns recheck
setdiff(names(kalinka), names(rayaan))
#rayaan only columns recheck
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



#Merged data creation
new_data <- kalinka

for (col in names(kalinka)) {
  new_data[[col]][is.na(new_data[[col]])] <- rayaan[[col]][is.na(new_data[[col]])]
}



#Discrepancy analysis
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

#THURSDAY UPDATE
library(dplyr)
library(stringr)

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
