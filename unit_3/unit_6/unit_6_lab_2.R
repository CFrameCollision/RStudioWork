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

Module_6_Lab_Data_1_ <- read_excel(
  "C:/Users/byrds/Downloads/Module 6 Lab Data (1).xlsx", 
  col_types = c("numeric"))

incomeData <- na.omit(Module_6_Lab_Data_1_[[1]])

ZTest(incomeData, mu = c(11230), sd_pop = c(679),
      alternative = c("greater"), paired = FALSE)