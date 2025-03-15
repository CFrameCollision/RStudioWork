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
library(ggpmisc)
library(ggpubr)

data <- read_excel("data/Voting Responses 2.xlsx", 
                                 col_types = c("skip", "numeric", "numeric", 
                                               "text", "numeric", "numeric", "text", 
                                               "numeric", "numeric", "text", "numeric", 
                                               "text", "skip", "skip", "skip", "skip", 
                                               "skip"))

# Initialize a list to store the results
results <- list()

# Define the variables of interest
variables <- c("Are you registered to vote?",
               "Does your major influence/effect your political participation?",
               "How often do you view political news?",
               "How likely are you to vote in the next election?")

# Loop through each variable and calculate mean and standard deviation
for (var in variables) {
  # Check if the variable exists in the data.frame
  if (var %in% colnames(data)) {
    # Calculate mean and standard deviation
    mean_value <- mean(data[[var]], na.rm = TRUE)
    sd_value <- sd(data[[var]], na.rm = TRUE)
    range_value <- range(data[[var]], na.rm = TRUE)
    mode_value <- Mode(data[[var]], na.rm = TRUE)
    
    # Store the results in the list
    results[[var]] <- list(mean = mean_value, sd = sd_value,
                           range = range_value, Mode = mode_value)
  } else {
    # If the variable doesn't exist, print a warning message
    cat(paste("Warning: Variable", var, "not found in the data.\n"))
  }
}

# Print the results
for (var in variables) {
  if (!is.null(results[[var]])) {
    cat(paste("For", var, "- Mean:", paste(format(round(results[[var]]$mean, 4), nsmall = 4)),
              "| Standard Deviation:", paste(format(round(results[[var]]$sd, 4), nsmall = 4)),
              "| Range:", paste(results[[var]]$range, collapse = " to "),
              "| Mode:", paste(results[[var]]$Mode, collapse = " to ")), "\n", "\n")
  }
}

cor.test(data$`How often do you view political news?`,
         data$`How likely are you to vote in the next election?`,
         alternative = "less", method = c("pearson"))