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

data01 <- aov(Module_8_PA_Q2_Data_1_[[1]]~Module_8_PA_Q2_Data_1_[[2]],
              data = Module_8_PA_Q2_Data_1_)
summary(data01)

TukeyHSD(aov(Module_8_PA_Q2_Data_1_[[1]]~as.factor(Module_8_PA_Q2_Data_1_[[2]]),
             data = Module_8_PA_Q2_Data_1_))