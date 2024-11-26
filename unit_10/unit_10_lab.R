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

Module_10_Lab_Data <- read_excel("data/Module 10 Lab Data.xlsx", 
                                 col_types = c("numeric", "numeric"))

rcorr(as.matrix(Module_10_Lab_Data), type = "pearson")

