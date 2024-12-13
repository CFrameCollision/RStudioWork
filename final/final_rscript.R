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

lmresult <- lm(formula = churchState ~ sex + attendance, df)
lmresult
summary(lmresult)

df$fitted <- lmresult$fitted.values
df$residuals <- lmresult$residuals

median(df$fitted)

ggplot(df, aes(x = fitted, y = churchState)) +
  geom_point() +
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Opinion on Seperation of Church & State Based Upon Fitted Values") +
  xlab("Fitted values (see section 2-4 for more info)") +
  ylab("Opinion on Seperation of Church & State") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))


lmresult2 <- lm(formula = churchState ~ attendance, df)
lmresult2
summary(lmresult2)