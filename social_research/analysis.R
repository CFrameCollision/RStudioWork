# Run "./data/new_data97-educational-data/new_data97-educational-data.R" first.
# This file runs analyses on two rounds of the NLSY97 data set. The README

library(MASS)
library(tidyverse)
library(extrafont)
library(ggthemes)
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
library(MissMech)
library(papaja)
library(dplyr)
library(tidyr)
library(effects)
library(rlang)

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

new_data <- new_data %>%
  mutate(race = case_when(
    KEY_RACE_ETHNICITY_1997 == 1 ~ "Black",
    KEY_RACE_ETHNICITY_1997 == 2 ~ "Hispanic",
    KEY_RACE_ETHNICITY_1997 == 3 ~ "Mixed Race",
    KEY_RACE_ETHNICITY_1997 == 4 ~ "Non-Black/Non-Hispanic",
    TRUE ~ NA_character_))

# Descriptive characteristics of respondents

# Degree attained

new_data <- new_data %>%
  mutate(degree_label = factor(degree_label,
                               levels = c("None", "GED", "HS Diploma",
                                          "AA", "BA", "MA", "PhD")))

new_data <- new_data %>%
  mutate(race = factor(race,
                       levels = c("Black", "Hispanic", "Mixed Race",
                                  "Non-Black/Non-Hispanic")))

new_data_rmNA <- new_data %>% dplyr::filter(!is.na(degree_label))

# Removes outliers
new_data_rmNA <- new_data_rmNA %>%
  filter(CV_HGC_RES_MOM_1997 <= 20 | is.na(CV_HGC_RES_MOM_1997)) %>%
  filter(CV_HGC_RES_DAD_1997 <= 20 | is.na(CV_HGC_RES_DAD_1997))

#title = "Highest Degree Attained of Respondents (Overall)"
ggplot(new_data_rmNA, aes(x = degree_label, fill = race)) +
  geom_bar() +
  labs(x = "Degree") +
  facet_wrap(~race, scales = "free_y") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  guides(fill = FALSE) +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "histogram.png", plot = last_plot(), scale = 1,
       device = "png", dpi = "retina", width = 6.5, height = 5.5, units = "in")

#title = "Highest Degree Attained (Mother)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_MOM_1997)) +
  geom_bar(fill = "lightgreen") +
  labs(x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_apa()

#title = "Highest Degree Attained (Father)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_DAD_1997)) +
  geom_bar(fill = "lightpink") +
  labs(x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_apa() 


##########  Start of missing data analysis ##########
# Looking at missing-ness by race/ethnicity
new_data_rmNA %>%
  dplyr::select(KEY_RACE_ETHNICITY_1997, CV_HGC_RES_MOM_1997,
                CV_HGC_RES_DAD_1997) %>%
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
                CV_HGC_RES_DAD_1997) %>%
  mutate(across(everything(), as.numeric))

mcar_test(mcar_data)

# Apply Jamshidian and Jalals test
MCAR_Test_Result <- TestMCARNormality(mcar_data)

# View result
print(MCAR_Test_Result)

# Creating dummy vars
# DV = Dummy Var

new_data_rmNA <- new_data_rmNA %>%
  mutate(
    DV_RACE_BLACK = ifelse(KEY_RACE_ETHNICITY_1997 == 1, 1, 0),
    DV_RACE_HISPANIC = ifelse(KEY_RACE_ETHNICITY_1997 == 2, 1, 0),
    DV_RACE_MIXED = ifelse(KEY_RACE_ETHNICITY_1997 == 3, 1, 0)
  )

##########  Imputations ##########
# Multiple imputations 

imp_data <- new_data_rmNA %>%
  dplyr::select(CV_HIGHEST_DEGREE_EVER_EDT_2017,
         DV_RACE_MIXED, DV_RACE_HISPANIC, DV_RACE_BLACK,
         CV_HGC_RES_MOM_1997, CV_HGC_RES_DAD_1997, SAMPLING_WEIGHT_CC_2017)

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

########## Imp Diagnoses ##########

# Redoing long transformation so as to leave imp unaffected
long_imp <- complete(imp, action = "long", include = TRUE)

# Marks which values were originally missing
long_imp <- long_imp %>%
  mutate(.id = as.integer(.id)) %>%
  group_by(.imp) %>%
  mutate(
    mom_missing = is.na(imp_data$CV_HGC_RES_MOM_1997),
    dad_missing = is.na(imp_data$CV_HGC_RES_DAD_1997)
  ) %>%
  ungroup()

# Reshapes to long format for faceting
long_faceted <- long_imp %>%
  select(.imp, CV_HGC_RES_MOM_1997, CV_HGC_RES_DAD_1997, mom_missing, dad_missing) %>%
  pivot_longer(
    cols = starts_with("CV_HGC_RES_"),
    names_to = "parent_var",
    values_to = "hgc"
  ) %>%
  mutate(
    missing_flag = case_when(
      parent_var == "CV_HGC_RES_MOM_1997" & mom_missing ~ "Imputed",
      parent_var == "CV_HGC_RES_DAD_1997" & dad_missing ~ "Imputed",
      TRUE ~ "Observed"
    )
  )

# Plot
ggplot(long_faceted, aes(x = hgc, fill = missing_flag, color = missing_flag)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~parent_var, scales = "free_y", labeller = as_labeller(
    c("CV_HGC_RES_MOM_1997" = "Mother's Education",
      "CV_HGC_RES_DAD_1997" = "Father's Education"))) +
  scale_fill_manual(values = c("Observed" = "blue", "Imputed" = "red")) +
  scale_color_manual(values = c("Observed" = "blue", "Imputed" = "red")) +
  labs(x = "Highest Grade Completed",
       y = "Density",
       fill = "Data Type",
       color = "Data Type") +
  theme_apa()

ggsave(filename = "densityplot.png", plot = last_plot(), scale = 1, device = "png",
       dpi = "retina", width = 6.5, height = 3, units = "in")

########## POLR ##########

# Runs ordinal log regr on imp data
pom_imp <- with(imp, polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ CV_HGC_RES_MOM_1997 *
    DV_RACE_BLACK + CV_HGC_RES_MOM_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_MOM_1997 * DV_RACE_MIXED + CV_HGC_RES_DAD_1997 *
    DV_RACE_BLACK + CV_HGC_RES_DAD_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_DAD_1997 * DV_RACE_MIXED,
  Hess = TRUE
))

# !!!!!Remember data is logarithmic!!!!!
pom_pooled <- pool(pom_imp)
summary(pom_pooled)

# Converting pooled results to tidy format
pooled_summary <- summary(pom_pooled)

# Adding term names
tidy_pooled <- tidy(pom_pooled, conf.int = TRUE, conf.level = 0.95)

tidy_pooled_sub <- tidy_pooled %>% dplyr::filter(term %in% c(
  "CV_HGC_RES_MOM_1997",
  "DV_RACE_BLACK",
  "DV_RACE_HISPANIC",
  "DV_RACE_MIXED",
  "CV_HGC_RES_DAD_1997",
  "CV_HGC_RES_MOM_1997:DV_RACE_BLACK",
  "CV_HGC_RES_MOM_1997:DV_RACE_HISPANIC",
  "CV_HGC_RES_MOM_1997:DV_RACE_MIXED",
  "DV_RACE_BLACK:CV_HGC_RES_DAD_1997",
  "DV_RACE_HISPANIC:CV_HGC_RES_DAD_1997",
  "DV_RACE_MIXED:CV_HGC_RES_DAD_1997"
))

tidy_pooled_sub <- tidy_pooled_sub %>%
  mutate(term = dplyr::recode(term,
                              "CV_HGC_RES_MOM_1997" = "HGC_Mom",
                              "CV_HGC_RES_DAD_1997" = "HGC_Dad",
                              "DV_RACE_BLACK" = "Black",
                              "DV_RACE_HISPANIC" = "Hispanic",
                              "DV_RACE_MIXED" = "Mixed",
                              "CV_HGC_RES_MOM_1997:DV_RACE_BLACK" = "HGC_Mom:Black",
                              "CV_HGC_RES_MOM_1997:DV_RACE_HISPANIC" = "HGC_Mom:Hispanic",
                              "CV_HGC_RES_MOM_1997:DV_RACE_MIXED" = "HGC_Mom:Mixed",
                              "DV_RACE_BLACK:CV_HGC_RES_DAD_1997" = "Black:HGC_Dad",
                              "DV_RACE_HISPANIC:CV_HGC_RES_DAD_1997" = "Hispanic:HGC_Dad",
                              "DV_RACE_MIXED:CV_HGC_RES_DAD_1997" = "Mixed:HGC_Dad"
  ))

# Plot for predictors
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Log Odds Estimate", y = "Predictor Estimate") +
  theme_apa()

ggsave(filename = "coefficientplot1.png", plot = last_plot(), scale = 1, device = "png",
       dpi = "retina", width = 6.5, height = 4.5, units = "in")

tidy_pooled_sub <- tidy_pooled %>% dplyr::filter(!term %in% c(
  "CV_HGC_RES_MOM_1997",
  "DV_RACE_BLACK",
  "DV_RACE_HISPANIC",
  "DV_RACE_MIXED",
  "CV_HGC_RES_DAD_1997",
  "CV_HGC_RES_MOM_1997:DV_RACE_BLACK",
  "CV_HGC_RES_MOM_1997:DV_RACE_HISPANIC",
  "CV_HGC_RES_MOM_1997:DV_RACE_MIXED",
  "DV_RACE_BLACK:CV_HGC_RES_DAD_1997",
  "DV_RACE_HISPANIC:CV_HGC_RES_DAD_1997",
  "DV_RACE_MIXED:CV_HGC_RES_DAD_1997"
))

# Plot for thresholds
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Log Odds Estimate", y = "Threshold Estimate") +
  theme_apa()

ggsave(filename = "coefficientplot2.png", plot = last_plot(), scale = 1, device = "png",
       dpi = "retina", width = 6.5, height = 4.5, units = "in")

# define threshold cutoffs
thresholds <- c(
  "None|GED" = 1.78574,
  "GED|HS"   = 3.00156,
  "HS|AA"    = 5.05602,
  "AA|BA"    = 5.53610,
  "BA|MA"    = 7.18690,
  "MA|PhD"   = 9.82104
)

# Create a sequence of linear predictor values (e.g., effects of covariates)
x_vals <- seq(-5, 15, length.out = 500)

# Compute cumulative probabilities using logistic (sigmoid) function
logistic <- function(x) 1 / (1 + exp(-x))

cum_probs <- sapply(thresholds, function(cut) logistic(cut - x_vals))

# Turn into a data frame
df <- as.data.frame(cum_probs)
df$x <- x_vals

# Calculate individual category probabilities
df <- df %>%
  mutate(
    None = `None|GED`,
    GED  = `GED|HS`   - `None|GED`,
    HS   = `HS|AA`    - `GED|HS`,
    AA   = `AA|BA`    - `HS|AA`,
    BA   = `BA|MA`    - `AA|BA`,
    MA   = `MA|PhD`   - `BA|MA`,
    PhD  = 1          - `MA|PhD`
  ) %>%
  select(x, None, GED, HS, AA, BA, MA, PhD) %>%
  pivot_longer(-x, names_to = "Education_Level", values_to = "Probability")

# plot
ggplot(df, aes(x = x, y = Probability, color = Education_Level)) +
  geom_line(linewidth = 0.78) +
  labs(x = "Linear Predictor",
       y = "Probability",
       color = "Education Level"
  ) +
  theme_apa()

ggsave(filename = "probabilityplot.png", plot = last_plot(), scale = 1, device = "png",
       dpi = "retina", width = 6.5, height = 4.5, units = "in")

########## Survey ##########

# Selects an imp
completed_data <- complete(imp, action = 1L)


# Fixed after turning in, see svyglm.txt for code used in paper
# Weighted OLR
completed_data$degree_num <- factor(
  completed_data$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

# Define survey design with weights
svy_design <- svydesign(
  ids = ~1,
  weights = ~SAMPLING_WEIGHT_CC_2017,
  data = completed_data
)

# Run weighted ordinal logistic regression using svyolr
svy_model <- svyolr(
  degree_num ~ CV_HGC_RES_MOM_1997 * DV_RACE_BLACK +
    CV_HGC_RES_MOM_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_MOM_1997 * DV_RACE_MIXED +
    CV_HGC_RES_DAD_1997 * DV_RACE_BLACK +
    CV_HGC_RES_DAD_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_DAD_1997 * DV_RACE_MIXED,
  design = svy_design
)

# View results
summary(svy_model)

######## Marginal effects graph ##########
# Currently unused in paper

for (i in c("CV_HGC_RES_MOM_1997", "CV_HGC_RES_DAD_1997")) {

gender <- ifelse(i=="CV_HGC_RES_MOM_1997", "Mother's", "Father's")
  
completed_data <- complete(imp, 5)

# Refit the polr model using that dataset (for effects to work)
polr_fit <- MASS::polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ CV_HGC_RES_MOM_1997 *
    DV_RACE_BLACK + CV_HGC_RES_MOM_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_MOM_1997 * DV_RACE_MIXED + CV_HGC_RES_DAD_1997 *
    DV_RACE_BLACK + CV_HGC_RES_DAD_1997 * DV_RACE_HISPANIC +
    CV_HGC_RES_DAD_1997 * DV_RACE_MIXED,
  data = completed_data,
  Hess = TRUE)

# Get predicted probabilities across HGC_Mom for all race groups
effect_obj <- Effect(
  focal.predictors = c(i, "DV_RACE_BLACK", "DV_RACE_HISPANIC", "DV_RACE_MIXED"),
  mod = polr_fit,
  xlevels = list(i = 0:20))

effect_df <- as.data.frame(effect_obj)

# Construct categorical race var for plotting
effect_df$Race <- with(effect_df, ifelse(DV_RACE_BLACK == 1, "Black",
                                         ifelse(DV_RACE_HISPANIC == 1, "Hispanic",
                                                ifelse(DV_RACE_MIXED == 1, "Mixed", "Non-Black/Non-Hispanic"))))


# Reshape the effect_df to long format for faceting
long_df <- effect_df %>%
  pivot_longer(cols = starts_with("prob."),
               names_to = "degree",
               names_prefix = "prob.",
               values_to = "fit") %>%
  left_join(
    effect_df %>%
      pivot_longer(cols = starts_with("L.prob."),
                   names_to = "degree",
                   names_prefix = "L.prob.",
                   values_to = "lower"),
    by = c(i, "DV_RACE_BLACK", "DV_RACE_HISPANIC", "DV_RACE_MIXED", "degree")
  ) %>%
  left_join(
    effect_df %>%
      pivot_longer(cols = starts_with("U.prob."),
                   names_to = "degree",
                   names_prefix = "U.prob.",
                   values_to = "upper"),
    by = c(i, "DV_RACE_BLACK", "DV_RACE_HISPANIC", "DV_RACE_MIXED", "degree")
  ) %>%
  mutate(Race = case_when(
    DV_RACE_BLACK == 1 ~ "Black",
    DV_RACE_HISPANIC == 1 ~ "Hispanic",
    DV_RACE_MIXED == 1 ~ "Mixed",
    TRUE ~ "Non-Black/Non-Hispanic"
  ))

long_df <- long_df %>%
  group_by(Race, degree, !!sym(i)) %>%
  summarise(
    fit = mean(fit, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(degree = factor(degree,
                         levels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
                         ordered = TRUE))

long_df1 <- long_df %>% filter(degree != "PhD")

# Plot
print(
  ggplot(long_df1, aes(x = !!sym(i), y = fit, color = Race, fill = Race)) +
  geom_line(linewidth = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  facet_wrap(~degree, scales = "free_y") +
  labs(
    title = paste(
      "Predicted Probability of Educational Attainment by Race and", gender,
      "Education"),
    x = paste(gender ,"Highest Grade Completed"),
    y = "Predicted Probability"
  ) +
  theme_apa())

long_df2 <- long_df %>% 
  filter(degree == "PhD")

# Plot
print(
  ggplot(long_df2, aes(x = !!sym(i), y = fit, color = Race, fill = Race)) +
    geom_line(linewidth = 0.6) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    labs(
      title = paste(
        "Predicted Probability of Educational Attainment by Race and", gender,
        "Education"),
      x = paste(gender ,"Highest Grade Completed"),
      y = "Predicted Probability"
    ) +
    theme_apa())
}