#Presentation analysis

analysis <- suppressWarnings(read_excel("./merged_diaries_actigraphy.xlsx"))

summary(analysis)
View(analysis)


ggplot(data = analysis, aes(x = `Sleep Quality`, y = `Sleep Duration`)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Sleep Quality",
       x = "Sleep Quality (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   


