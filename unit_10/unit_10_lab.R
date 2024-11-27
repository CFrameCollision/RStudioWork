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

rsq <- function (x, y) cor(as.matrix(x), as.matrix(y)) ^ 2

rsq(Module_10_Lab_Data, Module_10_Lab_Data)

graphdata <- as.data.frame(Module_10_Lab_Data)

ggplot(data = graphdata, aes(x = Age,y = `Income Interval`)) +
  geom_point(shape = 18, color = "darkred") +
  geom_smooth(method = lm) +
  xlim(0, 100) +
  ylim(0, 8) +
  theme_ipsum()