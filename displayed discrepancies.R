library(dplyr)
library(tidyr)

install.packages("readxl")
library(readxl)

#Data
kalinka = read_excel("/Users/zaneeldadah/Desktop/STAT 370 Sleep Project/Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_KALIKA_11.24.2025_updated.xlsx", na = c("-999", "-888"))
rayaan = read_excel("/Users/zaneeldadah/Desktop/STAT 370 Sleep Project/Jan 2026 COPY PUSHAdolescentDailyDiary_DATA_LABELS_7.2.2024_CLEANING_JA_RAYAAN_12.12.2025_updated.xlsx", na = c("-999", "-888"))

#Discrepancy Analysis
all.equal(kalinka, rayaan)


id_cols <- c("Participant ID", "Date")  # add any other ID columns you want to keep

kal_long <- kalinka %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "variable",
    values_to = "kalinka_value",
    values_transform = list(kalinka_value = as.character)
  )

ray_long <- rayaan %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "variable",
    values_to = "rayaan_value",
    values_transform = list(rayaan_value = as.character)
  )

discrepancies <- full_join(kal_long, ray_long,
                           by = c(id_cols, "variable")) %>%
  filter(
    !(is.na(kalinka_value) & is.na(rayaan_value)) &
      kalinka_value != rayaan_value |
      is.na(kalinka_value) | is.na(rayaan_value)
  ) %>%
  arrange(across(all_of(id_cols)), variable)

discrepancies
print(discrepancies, n = 25)