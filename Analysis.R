#Presentation analysis

analysis <- suppressWarnings(read_excel("./merged_diaries_actigraphy.xlsx"))

summary(analysis)
View(analysis)

analysis$`Sleep Onset Clean Decimal Hour` <- 
  as.numeric(analysis$`Sleep Onset Clean Decimal Hour`)
analysis$`Sleep Onset Clean Decimal Hour` <- 
  as.numeric(analysis$`Sleep Offset Clean Decimal Hour`)

ggplot(data = analysis, aes(x = `Sleep Duration`, y = `Sleep Quality`)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Sleep Quality",
       x = "Sleep (minutes)",
       y = "Sleep Quality (1-10)") +
  theme_minimal()   

ggplot(data = analysis, aes(x = `Sleep Onset Decimal Hour`, y = `Sleep Quality`)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Sleep Quality",
       x = "Sleep Quality (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   


ggplot(data = analysis, aes(x = `Physical Activity Mins`, y = `Sleep Quality`)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Physical Activity Duration",
       x = "Physical Activity (minutes)",
       y = "Sleep (minutes)") +
  theme_minimal()   

ggplot(data = sleep_data, aes(x = Sleep.Quality, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Sleep Quality",
       x = "Sleep Quality (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   

ggplot(data = sleep_data, aes(x = Alertness.Rating, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Alertness",
       x = "Alertness Rating (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()

ggplot(data = sleep_data, aes(x = Wake.Difficulty.Rating, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Wake Difficulty",
       x = "Wake Difficulty rating (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   



