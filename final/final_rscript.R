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

library(readxl)
Final_Exam_Data <- read_excel("data/Final Exam Data.xlsx", 
                              col_types = c("numeric", "numeric",
                                            "numeric", "numeric",
                                            "numeric", "numeric", 
                                            "numeric", "numeric"))

summary(Final_Exam_Data)