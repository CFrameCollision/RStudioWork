library(plyr)
library(DescTools)
library(ggplot2)
library(dplyr)

data <- as.numeric(as.character(unlist(Module_2_Data)))

bay1 <- na.omit(Module_2_Data[[1]])
bay2 <- na.omit(Module_2_Data[[2]])
bay3 <- na.omit(Module_2_Data[[3]])

mean(bay1)
sd(bay1)
var(bay1)
range(bay1)

mean(bay2)
sd(bay2)
var(bay2)
range(bay2)

mean(bay3)
sd(bay3)
var(bay3)
range(bay3)

# Fixing Baylor 2010
bay3_combined <- c(rep(1, 510), rep(2, 179), rep(3, 164), rep(4, 118),
                   rep(5, 66), rep(6, 39), rep(7, 48), rep(8, 21),
                   rep(9, 16), rep(10, 8), rep(11, 11), rep(12,49))
mean(bay3_combined)
sd(bay3_combined)
var(bay3_combined)
range(bay3_combined)

summary(bay1)
summary(bay2)
summary(bay3)
summary(bay3_combined)