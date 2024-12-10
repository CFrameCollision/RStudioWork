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

Module_11_Lab_Data <- read_excel("data/Module 11 Lab Data.xlsx", 
                                 col_types = c("numeric", "numeric",
                                               "numeric", "numeric"))

# Re-coded with shorter names because idk I don't like long variable
# names. ¯\_(ツ)_/¯

pf <- Module_11_Lab_Data$`Pray Frequency`
inc <- Module_11_Lab_Data$`Income Intervals`
age <- Module_11_Lab_Data$AGE
regat <- Module_11_Lab_Data$`Regular Attendance`

df <- data.frame(pf, inc, age, regat)

for (i in 2:4) {
  print(paste(median(df[[i]])))
}

lmresult <- lm(formula = pf ~ inc + age + regpr, df)

# Testing automating certain analysis's so I can quickly look through
# output.

for (i in 1:length(lmresult$coefficients)) {
  if (lmresult$coefficients[i] > 0) {
    print(paste("The coefficient for", names(lmresult$coefficients[i]),
          "is", lmresult$coefficients[i], "and direct"))
}   else {
    print(paste("The coefficient for", names(lmresult$coefficients[i]),
              "is", lmresult$coefficients[i], "and indirect"))
  }
}

# Back to normal code...

summary(lmresult)

df$fitted <- lmresult$fitted.values
df$residuals <- lmresult$residuals

ggplot(df, aes(x = fitted, y = pf)) +
  geom_point() +
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Prayer Frequency Based Upon Fitted Values") +
  xlab("Fitted values (see section 2-4 for more info)") +
  ylab("Prayer Frequency") +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))