library(plyr)
library(DescTools)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(hrbrthemes)
library(tidyverse)
library(viridis)
library(forcats)

data_mid <- na.omit(Midterm_Essay_2_Data[[1]])

summary(data_mid)
sd(data_mid)

mid_frame <- data.frame(value = data_mid,
                        type = "Midterm Test Scores")
# Shows distribution via a histogram
# Represent it
hist_plot <- ggplot(mid_frame, aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef", alpha=c(0.8), binwidth = c(1))+
  xlim(50, 100) +
  ylim(0, 13) +
  scale_x_continuous(n.breaks= 13) +
  scale_fill_manual(values=c("#19b3a2")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Score", y = "Frequency") +
  theme(legend.position = "none")

hist_plot

# Represent it
value_density <- ggplot(mid_frame, aes(x=value, fill=type)) +
  geom_density( color="#e9ecef", alpha=0.8) +
  xlim(35, 100) +
  scale_fill_manual(values=c("#903080")) +
  facet_wrap(~type) +
  theme_ipsum() +
  labs(fill="", x = "Score", y = "Probability") +
  theme(legend.position = "none")

value_density

# Probability for selected values of 21
100*pnorm(80, mean = mean(data_mid), sd = sd(data_mid),
          lower.tail = TRUE)

# Probability for selected values of 69
paste(100*pnorm(69, mean = mean(data_2007), sd = sd(data_2007),
          lower.tail = FALSE), "%", sep = "")
paste(100*pnorm(69, mean = mean(data_2017), sd = sd(data_2017),
          lower.tail = FALSE), "%", sep = "")

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