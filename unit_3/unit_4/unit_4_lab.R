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

data_2017 <- na.omit(Module_4_Lab_Data[[1]])
data_2007 <- Module_4_Lab_Data[[2]]

summary(data_2007)
sd(data_2007)
summary(data_2017)
sd(data_2017)

t.test(data_2007, data_2017, alternative = c("two.sided"),
       conf.level = 0.95)

# Shows distribution via a histogram
# Build data set with different distributions
# Create the data frame with different-length vectors
data_07 <- data.frame(
  value = data_2007,
  type = "2007 Ages of Those\n Who Regularly Prayer"
)

data_17 <- data.frame(
  value = data_2017,
  type = "2017 Ages of Those\n Who Regularly Prayer"
)

# Combine the two data frames using rbind()
data <- rbind(data_07, data_17)

# Represent it
hist_plot <- ggplot(data, aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef", alpha=c(0.8),
                 position = 'identity') +
  xlim(0, 100) +
  scale_fill_manual(values=c("#19b3a2", "#903080")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Age", y = "Frequency") +
  theme(legend.position = "none")

hist_plot
# ggplot Density plot

# Build data set with different distributions
# Create the data frame with different-length vectors
data_07 <- data.frame(
  value = data_2007,
  type = "2007 Ages"
)

data_17 <- data.frame(
  value = data_2017,
  type = "2017 Ages"
)

# Remove non-finite values
data_07 <- data_07[is.finite(data_07$value), ]
data_17 <- data_17[is.finite(data_17$value), ]

# Combine the two data frames using rbind()
data <- rbind(data_07, data_17)

# Represent it
value_density <- ggplot(data, aes(x=value, fill=type)) +
  geom_density( color="#e9ecef", alpha=0.8, position = 'identity') +
  xlim(0, 100) +
  scale_fill_manual(values=c("#19b3a2", "#903080")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Age", y = "Probability") +
  theme(legend.position = "none") +
  geom_vline(xintercept = c(21, 69), color = "red",
             linetype = "dashed", size = 0.65)

value_density

# Probability for selected values of 21
pdata21_07 <- 100*pnorm(21, mean = mean(data_2007), sd = sd(data_2007),
          lower.tail = TRUE)
pdata21_17 <- 100*pnorm(21, mean = mean(data_2017), sd = sd(data_2017),
          lower.tail = TRUE)

# Probability for selected values of 69
pdata69_07 <- 100*pnorm(69, mean = mean(data_2007), sd = sd(data_2007),
          lower.tail = FALSE)
pdata69_17 <- 100*pnorm(69, mean = mean(data_2017), sd = sd(data_2017),
          lower.tail = FALSE)

pdata21_07
pdata21_17
pdata69_07
pdata69_17

# Calculates Z-scores
z_scores_07 <- (data_2007-mean(data_2007))/sd(data_2007)
z_scores_17 <- (data_2017-mean(data_2017))/sd(data_2017)

# Build data set with different distributions
# Create the data frame with different-length vectors
data_07 <- data.frame(
  value = z_scores_07,
  type = "2007 Z-Score"
)

data_17 <- data.frame(
  value = z_scores_17,
  type = "2017 Z-Score"
)

# Combine the two data frames using rbind()
data <- rbind(data_07, data_17)

# Represent it
z_score_plot <- ggplot(data, aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef", alpha=c(0.8),
                  position = 'identity') +
  xlim(-3, 3) +
  scale_fill_manual(values=c("#19b3a2", "#903080")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Z-Score", y = "Frequency") +
  theme(legend.position = "none")

z_score_plot

# ggplot Density plot

# Build data set with different distributions
# Create the data frame with different-length vectors
data_07 <- data.frame(
  value = z_scores_07,
  type = "2007 Z-Score"
)

data_17 <- data.frame(
  value = z_scores_17,
  type = "2017 Z-Score"
)

# Combine the two data frames using rbind()
data <- rbind(data_07, data_17)

# Represent it
z_score_density <- ggplot(data, aes(x=value, fill=type)) +
  geom_density(color="#e9ecef", alpha=c(0.8),
               position = 'identity') +
  xlim(-3, 3) +
  scale_fill_manual(values=c("#19b3a2", "#903080")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Z-Score", y = "Probability") +
  theme(legend.position = "none") 

z_score_density

# Get probabilities from Z-scores
prob_07 <- pnorm(q = z_scores_07, lower.tail = TRUE)
prob_17 <- pnorm(q = z_scores_17, lower.tail = TRUE)