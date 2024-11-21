library(plyr)
library(DescTools)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(hrbrthemes)
library(tidyverse)
library(viridis)
library(forcats)
library(readxl)
library(knitr)
library(addinslist)
library(clipr)
library(gmodels)

CrossTable(Module_9_Lab_Data$PrayerFreq, Module_9_Lab_Data$PrayerSchool,
           expected = TRUE, dnn = c("Prayer Frequency",
                                    "Prayer in School"),
           format = c("SPSS"))

# Graph preparation

table(Module_9_Lab_Data$PrayerSchool)

data <- Module_9_Lab_Data

result <- data %>%
  group_by(PrayerFreq, PrayerSchool) %>%
  summarise(Count = n(), .groups = 'drop')

# View the result
print(result, n = 35)

# Data
data <- data.frame(
  Habit = rep(paste("Habit", 0:5), each = 5),
  Response = rep(c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree", "Undecided"), times = 6),
  Count = c(117, 46, 39, 7, 15,   # Habit 0
            41, 56, 98, 35, 26,   # Habit 1
            9, 13, 40, 13, 9,     # Habit 2
            14, 26, 83, 55, 14,   # Habit 3
            17, 25, 108, 97, 20,  # Habit 4
            17, 26, 112, 198, 19) # Habit 5  
)

# Reordering Response levels
data$Response <- factor(data$Response, 
                        levels = c("Undecided", "Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))

# Create the plot
ggplot(data, aes(x = Response, y = Count, fill = Response)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Habit, scales = "free_y") +
  labs(
    title = "Survey Responses for Prayer Habits",
    x = "Prayer should be allowed in public schools",
    y = "Count") +
  theme(
    axis.text.x = element_blank()
    ) +
  scale_fill_manual(values = c("#787878", "#ff1100",
                               "#ff6961", "#92ff8c", "#0dff00"))