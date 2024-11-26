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
library(Hmisc)

Module_10_PA_Data_1_ <- read_excel("data/Module 10 PA Data(1).xlsx", 
                                   col_types = c("numeric", "numeric", "numeric", 
                                                 "numeric"))
Module_10_PA_Data_1 <- as.data.frame(Module_10_PA_Data_1_)

cor(Module_10_PA_Data_1)

rcorr(as.matrix(Module_10_PA_Data_1), type = "pearson")

rsq <- function (x, y) cor(x, y) ^ 2

rsq(as.matrix(Module_10_PA_Data_1), as.matrix(Module_10_PA_Data_1))