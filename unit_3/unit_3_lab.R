library(plyr)
library(DescTools)
library(ggplot2)

data <- Module_2_Data

# Initialize a list to store the results
results <- list()

# Define the variables of interest
variables <- c("Baylor_2005", "Baylor_2007", "Baylor_2010")

# Loop through each variable and calculate mean and standard deviation
for (var in variables) {
  # Check if the variable exists in the dataframe
  if (var %in% colnames(data)) {
    # Calculate mean and standard deviation
    mean_value <- mean(data[[var]], na.rm = TRUE)
    sd_value <- sd(data[[var]], na.rm = TRUE)
    
    # Store the results in the list
    results[[var]] <- list(mean = mean_value, sd = sd_value)
  } else {
    # If the variable doesn't exist, print a warning message
    cat(paste("Warning: Variable", var, "not found in the data.\n"))
  }
}

# Print the results
for (var in variables) {
  if (!is.null(results[[var]])) {
    cat(paste("For", var, "- Mean:", results[[var]]$mean,
              "Standard Deviation:", results[[var]]$sd, "\n"))
  }
}
