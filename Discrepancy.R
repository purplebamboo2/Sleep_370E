install.packages("readxl")
library(readxl)

#Data
kalinka <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_KALIKA_11.24.2025_updated.xlsx")
rayaan <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_RAYAAN_12.12.2025.xlsx")
kalinka[kalinka == -888] <- NA
rayaan[rayaan == -888] <- NA

for (col in names(kalinka)) {
  if (is.numeric(kalinka[[col]]) && any(kalinka[[col]] == -999, na.rm = TRUE)) {
    kalinka[[col]] <- as.character(kalinka[[col]])
    kalinka[[col]][kalinka[[col]] == "-999"] <- "L"
  }
}

for (col in names(rayaan)) {
  if (is.numeric(rayaan[[col]]) && any(rayaan[[col]] == -999, na.rm = TRUE)) {
    rayaan[[col]] <- as.character(rayaan[[col]])
    rayaan[[col]][rayaan[[col]] == "-999"] <- "L"
  }
}


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



#Merged data creation
new_data <- kalinka

for (col in names(kalinka)) {
  new_data[[col]][is.na(new_data[[col]])] <- rayaan[[col]][is.na(new_data[[col]])]
}

#Discrepancy analysis
identical(new_data, kalinka)
which(!(new_data == kalinka) | (is.na(new_data) != is.na(kalinka)), arr.ind = TRUE)
which(new_data != kalinka, arr.ind = TRUE)

identical(new_data, rayaan)
which(!(new_data == rayaan) | (is.na(new_data) != is.na(rayaan)), arr.ind = TRUE)
which(new_data != rayaan, arr.ind = TRUE)



#View Data
View(new_data)
View(kalinka)
View(rayaan)


