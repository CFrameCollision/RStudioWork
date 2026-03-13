library(dplyr)
library(DescTools)
library(ggplot2)
library(plyr)
library(RColorBrewer)

pract_data <- c(18, 19, 15, 20, 25, 31, 17, 35, 27, 22, 34,
          29, 40, 33, 21)
summary(pract_data)
sd(pract_data)

hist()

# Calculates Z-scores
z_scores <- (pract_data-mean(pract_data))/sd(pract_data)

z_scores

# Get probabilities from Z-scores
prob <- 100 * pnorm(q = z_scores, lower.tail = TRUE)
prob_paste <- paste(prob, "%", sep = "")

prob_paste

# Generate a normal distribution plot
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  labs(title = 'Normal Distribution', x = 'STDEV', y = '') +
  geom_vline(xintercept=c(-3, -2, -1, 0, 1, 2, 3),
             color = 4,
             lwd = 0.6,  
             linetype = "dashed")