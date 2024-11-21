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

Module_4_Lab_Data <- read_excel(
      "C:/Users/byrds/Downloads/Module 4 Lab Data.xlsx", 
      col_types = c("numeric", "numeric"))

data_2017 <- na.omit(Module_4_Lab_Data[[1]])
data_2007 <- Module_4_Lab_Data[[2]]

# Stat sig of differences in age of consistent prayer

test1 <- t.test(data_2007, data_2017, alternative = c("less"),
       conf.level = 0.95)

# Tests for significance between data sets

# Create a data frame of 21≥
lessthan22_07 <- subset(data_2007, data_2007 < 22)
lessthan22_17 <- subset(data_2017, data_2017 < 22)

# Create a data frame of 69≤
greaterthan68_07 <- subset(data_2007, data_2007 > 68)
greaterthan68_17 <- subset(data_2017, data_2017 > 68)

# Stat sig for 21-
test2 <- t.test(lessthan22_07, lessthan22_17,
                alternative = c("less"), conf.levels = 0.95)

# Stat sig for 69+
test3 <- t.test(greaterthan68_07, greaterthan68_17,
                alternative = c("greater"), conf.levels = 0.95)

format_ttest <- function(test) {
  sprintf("t = %.2f, df = %.2f, p-value = %.4f, 95%% CI = (%.2f, %.2f)",
          test$statistic, test$parameter, test$p.value,
          test$conf.int[1], test$conf.int[2])
}

# Print results with formatting
cat("Statistical significance of differences in
    age of consistent prayer:\n")
cat(format_ttest(test1), "\n\n")
cat("Statistical significance for age 21 or lower:\n")
cat(format_ttest(test2), "\n\n")
cat("Statistical significance for age 69 or higher:\n")
cat(format_ttest(test3), "\n")