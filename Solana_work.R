# START UP----
library(readxl)

# Use read_excel instead of read.csv
sleep_data <- read_excel(file.choose(), na = c("-999", "-888"))
names(sleep_data) <- make.names(names(sleep_data))
summary(sleep_data)

# Check if the structure looks correct now (should see numeric columns)
str(sleep_data)

#ROUNDING TIME----
#--Onset times ----
Onset_sin <- sin(2 * pi * sleep_data$Sleep.Onset.Decimal.Hour / 24)
Onset_cos <- cos(2 * pi * sleep_data$Sleep.Onset.Decimal.Hour / 24)

plot(Onset_cos, Onset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Onset Time (Circular Encoding)")


#--Offset times ----
Offset_sin <- sin(2 * pi * sleep_data$Sleep.Offset.Time_Decimal.Hour / 24)
Offset_cos <- cos(2 * pi * sleep_data$Sleep.Offset.Time_Decimal.Hour / 24)

plot(Offset_cos, Offset_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Sleep Offset Time (Circular Encoding)")


#--Nap times ----
Nap_S_sin <- sin(2 * pi * sleep_data$Nap.Start.Decimal.Time / 24)
Nap_S_cos <- cos(2 * pi * sleep_data$Nap.Start.Decimal.Time / 24)

plot(Nap_S_cos, Nap_S_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Nap Start Time (Circular Encoding)")


Nap_E_sin <- sin(2 * pi * sleep_data$Nap.End.Decimal.Time / 24)
Nap_E_cos <- cos(2 * pi * sleep_data$Nap.End.Decimal.Time / 24)

plot(Nap_E_cos, Nap_E_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Nap End Time (Circular Encoding)")

#--First meal times ----
First_M_sin <- sin(2 * pi * sleep_data$First.Meal.Decimal.Time / 24)
First_M_cos <- cos(2 * pi * sleep_data$First.Meal.Decimal.Time / 24)

plot(First_M_cos, First_M_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "First Meal Time (Circular Encoding)")

#--Last meal times ----
Last_M_sin <- sin(2 * pi * sleep_data$Last.Meal.Decimal.Time / 24)
Last_M_cos <- cos(2 * pi * sleep_data$Last.Meal.Decimal.Time / 24)

plot(Last_M_cos, Last_M_sin,
     asp = 1,
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     pch = 19,
     main = "Last Meal Time (Circular Encoding)")


#ANALYSIS----


#Start plot better----
Onset_circ <- atan2(Onset_sin, Onset_cos)


library(dplyr)

sleep_data <- sleep_data %>%
  mutate(
    Onset_circ = atan2(Onset_sin, Onset_cos),  
    Onset_circ = ifelse(Onset_circ < 0, Onset_circ + 2*pi, Onset_circ),  
    start_hour  = Onset_circ * 24 / (2*pi),     
  
    Offset_circ = atan2(Offset_sin, Offset_cos),  
    Offset_circ = ifelse(Offset_circ < 0, Offset_circ + 2*pi, Offset_circ),  
    end_hour  = Offset_circ * 24 / (2*pi)     
  )

library(ggplot2)

ggplot(sleep_data, aes(x = Onset_circ, y = 1)) +  
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi/2) +  
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,                
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM","2PM","4PM","6PM","8PM","10PM")
  ) +
  theme_minimal() +
  labs(title = "Sleep Start Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        )



#End plot better----

ggplot(sleep_data, aes(x = Offset_circ, y = 1)) +  
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi/2) +  
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,                
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM","2PM","4PM","6PM","8PM","10PM")
  ) +
  theme_minimal() +
  labs(title = "Sleep Start Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
#Start and end plot----

library(ggplot2)


sleep_plot_data <- sleep_data %>%
  select(Onset_circ, Offset_circ) %>%
  mutate(id = row_number()) %>%
  tidyr::pivot_longer(cols = c(Onset_circ, Offset_circ),
                      names_to = "type", values_to = "angle") 


ggplot(sleep_plot_data, aes(x = angle, y = 1, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  coord_polar(theta = "x", start = -pi/2) +  # rotate so 12AM is top
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM",
               "2PM","4PM","6PM","8PM","10PM")
  ) +
  scale_color_manual(values = c("Onset_circ"="darkblue", "Offset_circ"="darkgoldenrod2"),
                     labels = c("End Time", "Start Time")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Sleep Start and End Times (Clock View)", color = "")



#Nap start plot----
Nap_S_circ = atan2(Nap_S_sin, Nap_S_cos)
Nap_E_circ = atan2(Nap_E_sin, Nap_E_cos)
library(dplyr)

sleep_data <- sleep_data %>%
  mutate(
    Nap_S_circ = atan2(Nap_S_sin, Nap_S_cos),  
    Nap_S_circ = ifelse(Nap_S_circ < 0, Nap_S_circ + 2*pi, Nap_S_circ),  
    nap_start_hour  = Nap_S_circ * 24 / (2*pi),     
    
    Nap_E_circ = atan2(Nap_E_sin, Nap_E_cos),  
    Nap_E_circ = ifelse(Nap_E_circ < 0, Nap_E_circ + 2*pi, Nap_E_circ),  
    nap_end_hour  = Nap_E_circ * 24 / (2*pi)     
  )

library(ggplot2)

ggplot(sleep_data, aes(x = Nap_S_circ, y = 1)) +  
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi/2) +  
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,                
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM","2PM","4PM","6PM","8PM","10PM")
  ) +
  theme_minimal() +
  labs(title = "Nap Start Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )


gi
#Nap End plot better----

ggplot(sleep_data, aes(x = Nap_E_circ, y = 1)) +  
  geom_point(size = 3, color = "blue") +
  coord_polar(theta = "x", start = -pi/2) +  
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,                
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM","2PM","4PM","6PM","8PM","10PM")
  ) +
  theme_minimal() +
  labs(title = "Sleep Start Times (Clock View)") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

#Nap Start and end plot----

library(ggplot2)


nap_plot_data <- sleep_data %>%
  select(Nap_S_circ, Nap_E_circ) %>%
  mutate(id = row_number()) %>%
  tidyr::pivot_longer(cols = c(Nap_S_circ, Nap_E_circ),
                      names_to = "type", values_to = "angle") 


ggplot(nap_plot_data, aes(x = angle, y = 1, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  coord_polar(theta = "x", start = -pi/2) +  
  scale_x_continuous(
    limits = c(0, 2*pi),
    breaks = (0:11) * 2*pi/12,
    labels = c("12AM","2AM","4AM","6AM","8AM","10AM","12PM",
               "2PM","4PM","6PM","8PM","10PM")
  ) +
  scale_color_manual(values = c("Nap_E_circ"="darkgoldenrod2", "Nap_S_circ"="darkblue"),
                     labels = c("Nap End Time", "Nap Start Time")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Nap Start and End Times (Clock View)", color = "")



#Mean Onset hr----
mean_onset_sin <- mean(Onset_sin, na.rm = TRUE)
mean_onset_cos <- mean(Onset_cos, na.rm = TRUE)

mean_onset_angle <- atan2(mean_onset_sin, mean_onset_cos)

if(mean_onset_angle < 0) mean_onset_angle <- mean_onset_angle + 2*pi

mean_onset_hour <- mean_onset_angle * 24 / (2*pi)

mean_onset_hour

#Mean Offset hr----
mean_offset_sin <- mean(Offset_sin, na.rm = TRUE)
mean_offset_cos <- mean(Offset_cos, na.rm = TRUE)

mean_offset_angle <- atan2(mean_offset_sin, mean_offset_cos)

if(mean_offset_angle < 0) mean_offset_angle <- mean_offset_angle + 2*pi

mean_offset_hour <- mean_offset_angle * 24 / (2*pi)

mean_offset_hour


#Plots----
physical_model = lm(sleep_data$Sleep.Duration ~ sleep_data$Physical.Activity.Mins, data = sleep_data)
physical_model


ggplot(data = sleep_data, aes(x = Physical.Activity.Mins, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Physical Activity Duration",
       x = "Physical Activity (minutes)",
       y = "Sleep (minutes)") +
  theme_minimal()   

results <- cor.test(sleep_data$Sleep.Duration, sleep_data$Physical.Activity.Mins, method = "pearson")
print(results)


ggplot(data = sleep_data, aes(x = Sleep.Quality, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Sleep Quality",
       x = "Sleep Quality (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   

results_1 <- cor.test(sleep_data$Sleep.Duration, sleep_data$Sleep.Quality, method = "spearman")
print(results_1)

ggplot(data = sleep_data, aes(x = Alertness.Rating, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Alertness",
       x = "Alertness Rating (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()

results_2 <- cor.test(sleep_data$Sleep.Duration, sleep_data$Alertness.Rating, method = "spearman")
print(results_2)

ggplot(data = sleep_data, aes(x = Wake.Difficulty.Rating, y = Sleep.Duration)) +
  geom_point(color = "blue") +          
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Sleep Duration Vs Wake Difficulty",
       x = "Wake Difficulty rating (1-10)",
       y = "Sleep (minutes)") +
  theme_minimal()   

results_3 <- cor.test(sleep_data$Sleep.Duration, sleep_data$Wake.Difficulty.Rating, method = "spearman")
print(results_3)




