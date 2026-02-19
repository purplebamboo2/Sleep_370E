install.packages("readxl")
library(readxl)

#Data
kalinka <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_KALIKA_11.24.2025_updated.xlsx", na = c("-999", "-888"))
rayaan <- read_excel("./Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_RAYAAN_12.12.2025.xlsx", na = c("-999", "-888"))

#Discrepancy Analysis
all.equal(kalinka, rayaan)
