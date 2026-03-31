library(FactoMineR)
library(tidyverse)
library(MASS)
library(naniar)
library(car)
library(mice)
library(survey)
library(MissMech)
library(papaja)
library(VIM)
library(effects)
library(visdat)
library(stargazer)
library(corrr)
library(factoextra)
library(ggcorrplot)

options(scipen = 999)

if (
  Sys.info()['sysname'] == "Linux" && basename(getwd()) != "social_research"
) {
  setwd("social_research")
}

source('data/nlsy97-educational-data/nlsy97-educational-data.R')

# Race key:
# 1 Black
# 2 Hispanic
# 3 Mixed Race (Non-Hispanic)
# 4 Non-Black

# Factorise categorical data

# PD = Professional Degree
new_data <- new_data %>%
  mutate(
    degree_label = case_when(
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 0 ~ "None",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 1 ~ "GED",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 2 ~ "HS Diploma",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 3 ~ "AA",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 4 ~ "BA",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 5 ~ "MA",
      CV_HIGHEST_DEGREE_EVER_EDT_2017 %in% c(6, 7) ~ "PhD",
      TRUE ~ NA_character_
    )
  )

new_data <- new_data %>%
  mutate(
    race = case_when(
      KEY_RACE_ETHNICITY_1997 == 1 ~ "Black",
      KEY_RACE_ETHNICITY_1997 == 2 ~ "Hispanic",
      KEY_RACE_ETHNICITY_1997 == 3 ~ "Mixed Race",
      KEY_RACE_ETHNICITY_1997 == 4 ~ "Non-Black/Non-Hispanic",
      TRUE ~ NA_character_
    )
  )

# Descriptive characteristics of respondents

# Degree attained

new_data <- new_data %>%
  mutate(
    degree_label = factor(
      degree_label,
      levels = c(
        "None",
        "GED",
        "HS Diploma",
        "AA",
        "BA",
        "MA",
        "PhD"
      ),
      ordered = TRUE
    )
  )

new_data <- new_data %>%
  mutate(
    race = factor(
      race,
      levels = c(
        "Black",
        "Hispanic",
        "Mixed Race",
        "Non-Black/Non-Hispanic"
      )
    )
  )

new_data_rmNA <- new_data %>% dplyr::filter(!is.na(degree_label))

# Removes outliers. 95 is coded as ungraded. 95 responses ~ n = 6. See below code.
new_data_rmNA <- new_data_rmNA %>%
  dplyr::filter(CV_HGC_RES_MOM_1997 <= 20 | is.na(CV_HGC_RES_MOM_1997)) %>%
  dplyr::filter(CV_HGC_RES_DAD_1997 <= 20 | is.na(CV_HGC_RES_DAD_1997))

new_data_rmNA <- new_data_rmNA %>%
  mutate(
    DV_RACE_BLACK = ifelse(KEY_RACE_ETHNICITY_1997 == 1, 1, 0),
    DV_RACE_HISPANIC = ifelse(KEY_RACE_ETHNICITY_1997 == 2, 1, 0),
    DV_RACE_MIXED = ifelse(KEY_RACE_ETHNICITY_1997 == 3, 1, 0),
    HGCParentEd = pmax(
      CV_HGC_RES_DAD_1997,
      CV_HGC_RES_MOM_1997,
      na.rm = TRUE
    )
  )

##########  Imputations ##########

imp_data <- new_data_rmNA %>%
  dplyr::select(
    KEY_SEX_1997,
    CV_HIGHEST_DEGREE_EVER_EDT_2017,
    DV_RACE_MIXED,
    DV_RACE_HISPANIC,
    DV_RACE_BLACK,
    HGCParentEd,
    VSTRAT_1997,
    VPSU_1997,
    SAMPLING_WEIGHT_CC_2017
  ) %>%
  mutate(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 = case_when(
      CV_HIGHEST_DEGREE_EVER_EDT_2017 == 7 ~ 6,
      .default = as.integer(CV_HIGHEST_DEGREE_EVER_EDT_2017)
    )
  )

impPredictorMatrix <- rbind(
  c(rep(0, 9)), #1
  c(rep(0, 9)), #2
  c(rep(0, 9)), #3
  c(rep(0, 9)), #4
  c(rep(0, 9)), #5
  c(1, 1, 1, 1, 1, 1, 0, 0, 0), #6 - Predictors for ParentEd
  c(rep(0, 9)), #7
  c(rep(0, 9)), #8
  c(rep(0, 9)) #9
)

imp <- mice(
  imp_data,
  m = 20,
  method = 'pmm',
  predictorMatrix = impPredictorMatrix,
  seed = 1234
)

imp_long <- complete(imp, action = "long", include = TRUE)

imp_long$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
  imp_long$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = 0:6,
  labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

imp <- as.mids(imp_long)

########## PCA ##########

eigen(cor(imp_data[-c(7:9)], use = "complete"))

# Check for VIF changes across model specifications
library(car)

# Model 1: Parental ed only
m1_test <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ HGCParentEd,
  data = complete(imp, 1),
  Hess = TRUE
)

# Model 2: Add race main effects
m2_test <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ HGCParentEd +
    DV_RACE_BLACK +
    DV_RACE_HISPANIC +
    DV_RACE_MIXED,
  data = complete(imp, 1),
  Hess = TRUE
)

# Model 3: Add interactions (full model)
m3_test <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ HGCParentEd +
    DV_RACE_BLACK +
    DV_RACE_HISPANIC +
    DV_RACE_MIXED +
    HGCParentEd:DV_RACE_BLACK +
    HGCParentEd:DV_RACE_HISPANIC +
    HGCParentEd:DV_RACE_MIXED,
  data = complete(imp, 1),
  Hess = TRUE
)

# Compare coefficients
coef_compare <- list(
  m1 = coef(m1_test),
  m2 = coef(m2_test),
  m3 = coef(m3_test)
)
coef_compare

# VIF for the full model (note: won't work directly on polr, so use underlying model matrix)
X <- model.matrix(
  ~ HGCParentEd +
    DV_RACE_BLACK +
    DV_RACE_HISPANIC +
    DV_RACE_MIXED +
    HGCParentEd:DV_RACE_BLACK +
    HGCParentEd:DV_RACE_HISPANIC +
    HGCParentEd:DV_RACE_MIXED,
  data = complete(imp, 1)
)
kappa(X, exact = TRUE) # Condition number
