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

lmresult <- lm(formula = pf ~ inc + age + regat, df)

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
  if (abs(lmresult$coefficients[i]) > 0.2) {
    print("There is a weak to strong relationship.")
  }   else {
    print("There is little to no relationship")
  }
}

# Back to normal code...

summary(lmresult)

testThing <- summary(lmresult)
print(paste("The R^2 value is", testThing[["adj.r.squared"]]))

df$fitted <- lmresult$fitted.values
df$residuals <- lmresult$residuals

median(df$fitted)

x_value <- 2.241326
y_value <- (2.72e-14) + 1 * x_value

ggplot(df, aes(x = fitted, y = pf)) +
  geom_point() +
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "Prayer Frequency Based Upon Fitted Values") +
  xlab("Fitted values (see section 2-4 for more info)") +
  ylab("Prayer Frequency") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")) +
  geom_point(aes(x = x_value, y = y_value), color = "red", size = 2.2) +
  geom_text(aes(x = x_value, y = y_value,
                label = paste("(", round(x_value, 2), ", ",
                              round(y_value, 2), ")", sep = "")),
            vjust = -1, hjust = 1, size = 3.2)