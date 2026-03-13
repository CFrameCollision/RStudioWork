library(tidyverse)
library(extrafont)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(thematic)
library(extrafontdb)
library(readxl)
library(colorspace)
library(addinslist)
library(clipr)
library(gmodels)
library(Hmisc)
library(RColorBrewer)
library(DescTools)
library(viridis)
library(ggpmisc)

Final_Exam_Data <- read_excel("data/Final Exam Data.xlsx", 
                              col_types = c("numeric", "numeric",
                                            "numeric", "numeric",
                                            "numeric", "numeric",
                                            "numeric", "numeric"))

churchState <- Final_Exam_Data$`Church and State`
attendance <- Final_Exam_Data$`Attendance Intervals 2017`
sex <- Final_Exam_Data$Sex

df <- data.frame(sex, churchState, attendance)
summary(df)

aovResults <- aov(formula = churchState ~ sex + attendance, df)
anova(aovResults)

cor(df[1:3])

lmresult <- lm(formula = churchState ~ sex + attendance, df)
summary(lmresult)

df$fitted <- lmresult$fitted.values
df$residuals <- lmresult$residuals

ggplot(df, aes(x = fitted, y = churchState)) +
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon Fitted Values") +
  xlab("Fitted values") +
  ylab("Opinion on Seperation of Church & State") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

ggplot(df, aes(x = attendance, y = churchState))+
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon attendance") +
  xlab("Attendance") +
  ylab("Opinion on Seperation of Church & State") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

ggplot(df, aes(x = sex, y = churchState))+
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon Sex") +
  xlab("Sex") +
  ylab("Opinion on Seperation of Church & State") +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

# =============================================================

dfs <- subset(df, churchState != 8)

aovResults <- aov(formula = churchState ~ sex + attendance, dfs)
anova(aovResults)

cor(dfs[1:3])

lmresult <- lm(formula = churchState ~ sex + attendance, dfs)
summary(lmresult)

dfs$fitted <- lmresult$fitted.values
dfs$residuals <- lmresult$residuals

median(dfs$fitted)

ggplot(dfs, aes(x = fitted, y = churchState)) +
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon Fitted Values") +
  xlab("Fitted values") +
  ylab("Opinion on Seperation of Church & State") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

ggplot(df, aes(x = attendance, y = churchState))+
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon attendance") +
  xlab("Attendance") +
  ylab("Opinion on Seperation of Church & State") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

ggplot(df, aes(x = sex, y = churchState))+
  geom_count()+
  scale_size_area()+
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State\
       Based Upon Sex") +
  xlab("Sex") +
  ylab("Opinion on Seperation of Church & State") +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))