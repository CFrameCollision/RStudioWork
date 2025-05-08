# Run "./data/new_data97-educational-data/new_data97-educational-data.R" first.
# This file runs analyses on two rounds of the NLSY97 data set. The README

library(MASS)
library(tidyverse)
library(extrafont)
library(ggthemes)
library(hrbrthemes)
library(thematic)
library(colorspace)
library(addinslist)
library(gmodels)
library(RColorBrewer)
library(DescTools)
library(viridis)
library(ggpmisc)
library(naniar)
library(broom)
library(mice)
library(survey)

source('data/nlsy97-educational-data/nlsy97-educational-data.R')

# Race key:
# 1 Black
# 2 Hispanic
# 3 Mixed Race (Non-Hispanic)
# 4 Non-Black

# Filter out non-responses

new_data <- new_data %>%
  mutate(degree_label = case_when(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 0 ~ "None",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 1 ~ "GED",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 2 ~ "HS Diploma",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 3 ~ "AA",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 4 ~ "BA",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 5 ~ "MA",
    CV_HIGHEST_DEGREE_EVER_EDT_2017 == 6 ~ "PhD",
    TRUE ~ NA_character_))

# Descriptive characteristics of respondents

# Degree attained

new_data <- new_data %>%
  mutate(degree_label = factor(degree_label,
                               levels = c("None", "GED", "HS Diploma",
                                          "AA", "BA", "MA", "PhD")))

new_data_rmNA <- new_data %>% dplyr::filter(!is.na(degree_label))

# Removes outliers
new_data_rmNA <- new_data_rmNA %>%
  filter(CV_HGC_RES_MOM_1997 <= 20 | is.na(CV_HGC_RES_MOM_1997)) %>%
  filter(CV_HGC_RES_DAD_1997 <= 20 | is.na(CV_HGC_RES_DAD_1997))

#title = "Highest Degree Attained of Parents (Overall)"
ggplot(new_data_rmNA, aes(x = degree_label)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_minimal()

#title = "Highest Degree Attained (Mother)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_MOM_1997)) +
  geom_bar(fill = "lightgreen") +
  labs(x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_minimal()

#title = "Highest Degree Attained (Father)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_DAD_1997)) +
  geom_bar(fill = "lightpink") +
  labs(x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_minimal() 


##########  Start of missing data analysis ##########
# Looking at missing-ness by race/ethnicity
new_data_rmNA %>%
  dplyr::select(KEY_RACE_ETHNICITY_1997, CV_HGC_RES_MOM_1997, CV_HGC_RES_DAD_1997) %>%
  mutate(
    mom_missing = is.na(CV_HGC_RES_MOM_1997),
    dad_missing = is.na(CV_HGC_RES_DAD_1997)
  ) %>%
  group_by(KEY_RACE_ETHNICITY_1997) %>%
  summarise(
    n = n(),
    mom_missing_pct = mean(mom_missing) * 100,
    dad_missing_pct = mean(dad_missing) * 100
  )

# Testing for patterns in missing-ness

gg_miss_upset(new_data_rmNA)

mcar_data <- new_data_rmNA %>%
  dplyr::select(CV_HIGHEST_DEGREE_EVER_EDT_2017,
         CV_HGC_RES_MOM_1997,
         CV_HGC_RES_DAD_1997)

mcar_test(mcar_data)




##########  Imputations ##########
# Multiple imputations 

imp_data <- new_data_rmNA %>%
  dplyr::select(CV_HIGHEST_DEGREE_EVER_EDT_2017,
         KEY_RACE_ETHNICITY_1997, CV_HGC_RES_MOM_1997,
         CV_HGC_RES_DAD_1997, SAMPLING_WEIGHT_CC_2017)

imp <- mice(imp_data, m = 5, method = 'pmm')

imp <- complete(imp, action = "long", include = TRUE)

# Factors imputations
imp$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
  imp$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = 0:6,
  labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

# Re-convert to mids object
imp <- as.mids(imp)

# Runs ordinal log regr on imp data
pom_imp <- with(imp, polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ CV_HGC_RES_MOM_1997 +
    KEY_RACE_ETHNICITY_1997 + CV_HGC_RES_DAD_1997,
  Hess = TRUE
))

# Pool the results
# !!!!!Remember data is logarithmic!!!!!
pom_pooled <- pool(pom_imp)
summary(pom_pooled)

# Converting pooled results to tidy format
pooled_summary <- summary(pom_pooled)

# Adding term names
tidy_pooled <- tidy(pom_pooled, conf.int = TRUE, conf.level = 0.95)

tidy_pooled_sub <- subset(tidy_pooled, tidy_pooled$estimate <= 0.8)

# Plot
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Log Odds Estimate", y = "Predictor Estimate") +
  theme_minimal()

tidy_pooled_sub <- subset(tidy_pooled, tidy_pooled$estimate > 0.8)

# Plot
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Log Odds Estimate", y = "Threshold Estimate") +
  theme_minimal()

# Selects an imp
completed_data <- complete(imp, action = 5L)

# Weighted GLM
completed_data$degree_num <- as.numeric(
  completed_data$CV_HIGHEST_DEGREE_EVER_EDT_2017)

svy_design <- svydesign(
  ids = ~1,
  weights = ~SAMPLING_WEIGHT_CC_2017,
  data = completed_data
)

svy_model <- svyglm(
  degree_num ~ CV_HGC_RES_MOM_1997 +
    KEY_RACE_ETHNICITY_1997 + CV_HGC_RES_DAD_1997,
  design = svy_design
)

summary(svy_model)

# Visualize pooled regression coefficients of imps (not done yet)

library(effects)

# Use one completed data set as demonstration
effect_svy_model <- effects::Effect(c("KEY_RACE_ETHNICITY_1997", "CV_HGC_RES_MOM_1997"),
               svy_model)

# For predicted probabilities
as.data.frame(effect_svy_model)