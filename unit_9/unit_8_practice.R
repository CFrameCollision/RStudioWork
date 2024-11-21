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

CrossTable(Module_9_PA_Data_1_$MM,
           Module_9_PA_Data_1_$Exercise, expected = TRUE,
           dnn = c("MM's", "Excercise"), format = c("SPSS"))

chisq.test(Module_9_PA_Data_1_$MM, Module_9_PA_Data_1_$Exercise)

practAssig <- c(800, 700, 900)

chisq.test(practAssig)

CrossTable(practAssig, expected = TRUE,
           dnn = c("Votes"), format = c("SPSS"))