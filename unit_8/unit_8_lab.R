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


Module_8_Lab_Data <- read_excel(
  "C:/Users/byrds/Downloads/Module 8 Lab Data.xlsx")

betterList <- data.frame(prayerFrequency = Module_8_Lab_Data$PrayerFreq,
                         attendance = Module_8_Lab_Data$Attendance,
                         gender = Module_8_Lab_Data$Gender)

# Prayer and Attendance

storedVal <- aov(betterList$attendance~betterList$prayerFrequency,
                 betterList)
summary(storedVal)

TukeyHSD(aov(betterList$attendance~as.factor(betterList$prayerFrequency),
             betterList))

# Gender, Prayer, and Attendance

storedVal <- lm(betterList$attendance~betterList$prayerFrequency*betterList$gender,
                data = betterList)
summary(storedVal)

storedVal <- aov(betterList$attendance~betterList$prayerFrequency*betterList$gender,
                 data = betterList)
summary(storedVal)

TukeyHSD(aov(betterList$attendance~as.factor(betterList$prayerFrequency)*as.factor(betterList$gender),
             data = betterList))
# ggplot2 `interaction.plot()`

listInt <- ddply(betterList,.(prayerFrequency,gender),summarise, val = mean(attendance))

ggplot() +
  geom_point(data = betterList, aes(x = prayerFrequency, y = attendance, colour = gender)) +
  geom_point(data = listInt, aes(x = prayerFrequency, y = val, colour = gender)) +
  geom_line(data = listInt, aes(x = prayerFrequency, y = val, group = gender, colour = gender)) +
  ylim(1, 5) +
  theme_bw() +
  labs(title = "Interaction Plot", x = "Prayer Frequency", y = "Mean of Attendance", 
       colour = "Gender")