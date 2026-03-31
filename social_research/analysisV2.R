# Complete all new work here.

# Run "./data/new_data97-educational-data/new_data97-educational-data.R" first.
# This file runs analyses on two rounds of the NLSY97 data set. The README
# Things may/will break if you don't clear the data pane between executions.

# After working on this on and off for a year, only now have I checked cor() between mom
# and dad HGC... it's like 0.68 or something (I cleared the
# console by accident and I don't feel like recoding Rubins rule). Notes for later
# when I stop having an existential crisis below...

# Alright, so I'm finding collinearity between mom and dad. What this means is I should
# collapse the data into an average or maximum for both parent. Secondly,
# my POLR model might be incorrect in the way I've specified it.
# I should look into it more. Going back to the first point, cases where dad and mom are
# missing are relatively low at n = 459 / 6% of all valid cases. Look at imputing this or
# case-wise deletion.

# For my POLR model, I've mixed up * and :. x1*x2 is equivalent to x1 + x2 + x1:x2.

# High VIF is likely caused by underspecification. Try every permutation of control variables you can,

library(MASS)
library(tidyverse)
# library(ggthemes)
library(colorspace)
# library(gmodels)
# library(RColorBrewer)
# library(DescTools)
# library(viridis)
# library(ggpmisc)
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
library(Hmisc)
library(VGAM)
library(svyVGAM)

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
      levels = c("Black", "Hispanic", "Mixed Race", "Non-Black/Non-Hispanic")
    )
  )

new_data_rmNA <- new_data %>% dplyr::filter(!is.na(degree_label))

# Removes outliers. 95 is coded as ungraded. 95 responses ~ n = 6. See below code.
# new_data_rmNA %>% dplyr::filter(CV_HGC_RES_DAD_1997 > 20 | CV_HGC_RES_MOM_1997 > 20)
new_data_rmNA <- new_data_rmNA %>%
  dplyr::filter(CV_HGC_RES_MOM_1997 <= 20 | is.na(CV_HGC_RES_MOM_1997)) %>%
  dplyr::filter(CV_HGC_RES_DAD_1997 <= 20 | is.na(CV_HGC_RES_DAD_1997))

#title = "Highest Degree Attained of Respondents (Overall)"
ggplot(new_data_rmNA, aes(x = degree_label, fill = race)) +
  geom_bar(fill = "#2b2b2b") +
  labs(x = "Degree") +
  facet_wrap(~race, scales = "free_y") +
  guides(fill = FALSE) +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "histogram.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 5.5,
  units = "in"
)

#title = "Highest Degree Attained (Mother)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_MOM_1997)) +
  geom_bar(fill = "#2b2b2b") +
  labs(x = "Education Level") +
  facet_wrap(~race, scales = "free_y") +
  theme_apa()

ggsave(
  filename = "histogram2.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 5.5,
  units = "in"
)

#title = "Highest Degree Attained (Father)"
ggplot(new_data_rmNA, aes(x = CV_HGC_RES_DAD_1997)) +
  geom_bar(fill = "#2b2b2b") +
  labs(x = "Education Level") +
  facet_wrap(~race, scales = "free_y") +
  theme_apa()

ggsave(
  filename = "histogram3.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 5.5,
  units = "in"
)

##########  Start of missing data analysis ##########
# Looking at missing-ness by race/ethnicity
new_data_rmNA %>%
  dplyr::select(
    KEY_RACE_ETHNICITY_1997,
    CV_HGC_RES_MOM_1997,
    CV_HGC_RES_DAD_1997
  ) %>%
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

# Look at missingness by vis dat and VIM

vis_dat(new_data_rmNA[c(7, 8)]) +
  theme(axis.text.x = element_text(hjust = 0.2, vjust = 0.3))
aggr(
  new_data_rmNA[c("CV_HGC_RES_DAD_1997", "CV_HGC_RES_MOM_1997", "race")],
  numbers = TRUE,
  prop = FALSE,
  sortVar = TRUE
)

# Testing for patterns in missing-ness

gg_miss_upset(new_data_rmNA)

mcar_data <- new_data_rmNA %>%
  dplyr::select(
    CV_HIGHEST_DEGREE_EVER_EDT_2017,
    CV_HGC_RES_MOM_1997,
    CV_HGC_RES_DAD_1997
  ) %>%
  mutate(across(everything(), as.numeric))

mcar_test(mcar_data)

print("==========")
# Apply Jamshidian and Jalals test
MCAR_Test_Result <- TestMCARNormality(mcar_data[
  sample(6701, 999, replace = FALSE),
])

print(MCAR_Test_Result)

# Creating dummy vars
# DV = Dummy Var

new_data_rmNA <- new_data_rmNA %>%
  mutate(
    DV_RACE_BLACK = ifelse(KEY_RACE_ETHNICITY_1997 == 1, 1, 0),
    DV_RACE_HISPANIC = ifelse(KEY_RACE_ETHNICITY_1997 == 2, 1, 0),
    DV_RACE_MIXED = ifelse(KEY_RACE_ETHNICITY_1997 == 3, 1, 0),
    HGCParentEd = pmax(CV_HGC_RES_DAD_1997, CV_HGC_RES_MOM_1997, na.rm = TRUE)
  )

##########  Imputations ##########
# Multiple imputations

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
  ) %>%
  select(-KEY_RACE_ETHNICITY_1997)

# Turn off/on predictor matrix imputation. Put in a random string to test polr w/out imp
use_predictor_matrix <- TRUE

if (use_predictor_matrix == TRUE) {
  # Imp w/ predictor matrix. Used in paper
  # See notes for matrix definition
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

  impPred <- mice(
    imp_data,
    m = 20,
    method = 'pmm',
    predictorMatrix = impPredictorMatrix,
    seed = 1234
  )
  imp <- complete(impPred, action = "long", include = TRUE)
} else {
  # Imp w/o predictor matrix.
  imp <- mice(imp_data, m = 20, method = 'pmm', seed = 1234)
  imp <- complete(imp, action = "long", include = TRUE)
}

print("No imputation being used!")

imp_data$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
  imp_data$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = 0:6,
  labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

imp_dataNoMixed <- imp_data %>%
  dplyr::filter(
    DV_RACE_MIXED == 0,
    !is.na(HGCParentEd),
    is.finite(HGCParentEd)
  ) %>%
  select(-DV_RACE_MIXED) %>%
  mutate(
    SAMPLING_WEIGHT_CC_2017 = SAMPLING_WEIGHT_CC_2017 /
      mean(SAMPLING_WEIGHT_CC_2017, na.rm = TRUE)
  )

# Provides starting point to allow convergence of model 2.
m1 <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
    HGCParentEd *
    DV_RACE_BLACK +
    HGCParentEd * DV_RACE_HISPANIC +
    HGCParentEd * DV_RACE_MIXED,
  imp_data,
  Hess = TRUE
)
m1.1 <- tidy(m1, conf.int = TRUE, conf.level = 0.95)

startdf <- c(m1$coefficients, m1$zeta)

m2 <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
    HGCParentEd *
    DV_RACE_BLACK +
    HGCParentEd * DV_RACE_HISPANIC +
    HGCParentEd * DV_RACE_MIXED,
  imp_data,
  weights = SAMPLING_WEIGHT_CC_2017,
  start = startdf,
  Hess = TRUE
)
startdf2 <- c(m2$coefficients, m2$zeta)

m2.1 <- tidy(m2, conf.int = TRUE, conf.level = 0.95)

expOR <- c(exp(m2.1$estimate[1:7]), rep(0, 6))
m2.1sum <- cbind(m2.1, expOR)

m2.1sum %>% print()

m3_unw <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
    HGCParentEd * DV_RACE_BLACK + HGCParentEd * DV_RACE_HISPANIC,
  data = imp_dataNoMixed,
  Hess = TRUE,
  method = "logistic",
  na.action = na.omit
)

m3_start <- c(m3_unw$coefficients, m3_unw$zeta)

m3 <- polr(
  CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
    HGCParentEd *
    DV_RACE_BLACK +
    HGCParentEd * DV_RACE_HISPANIC,
  imp_dataNoMixed,
  start = m3_start,
  weights = SAMPLING_WEIGHT_CC_2017,
  Hess = TRUE
)
m3.1 <- tidy(m3, conf.int = TRUE, conf.level = 0.95)

expOR <- c(exp(m3.1$estimate[1:5]), rep(0, 6))
m3.1sum <- cbind(m3.1, expOR)

m3.1sum %>% print()

termFilter <- c(
  "HGCParentEd",
  "DV_RACE_MIXED",
  "DV_RACE_HISPANIC",
  "DV_RACE_BLACK",
  "HGCParentEd:DV_RACE_BLACK",
  "HGCParentEd:DV_RACE_HISPANIC",
  "HGCParentEd:DV_RACE_MIXED"
)

m1.1sub <- m1.1 %>%
  filter(
    term %in% termFilter
  )

plot1 <- ggplot(m1.1sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Log Odds Estimate",
    y = "Predictor Estimate",
    title = "No weight - No imp"
  ) +
  theme_apa()

plot1 %>% print()

m2.1sub <- m2.1 %>%
  filter(
    term %in% termFilter
  )

plot2 <- ggplot(m2.1sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Log Odds Estimate",
    y = "Predictor Estimate",
    title = "Weighted - No Imp"
  ) +
  theme_apa()

plot2 %>% print()

print("End of sensitivity test")


# Factors imputations
imp$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
  imp$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = 0:6,
  labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

imp <- as.mids(imp)

########## Imputation Diagnoses ##########

# Redoing long transformation so as to leave imp unaffected
long_imp <- complete(imp, action = "long", include = TRUE)

# Marks which values were originally missing
long_imp <- long_imp %>%
  mutate(.id = as.integer(.id)) %>%
  group_by(.imp) %>%
  mutate(
    parentMissing = is.na(imp_data$HGCParentEd),
  ) %>%
  ungroup()

# Facilitates faceting
long_flagged <- long_imp %>%
  dplyr::select(
    .imp,
    HGCParentEd,
    parentMissing
  )

p <- ggplot(
  long_flagged,
  aes(
    x = HGCParentEd,
    fill = parentMissing,
    color = parentMissing,
    linetype = parentMissing
  )
) +
  geom_density(alpha = 0.3, size = 0.8) +
  scale_fill_manual(
    labels = c("Casewise Deletion", "Imputed"),
    values = c("TRUE" = "#1e1e1e", "FALSE" = "lightgray")
  ) +
  scale_color_manual(
    labels = c("Casewise Deletion", "Imputed"),
    values = c("TRUE" = "#2c2c2c", "FALSE" = "black")
  ) +
  scale_linetype_manual(
    labels = c("Casewise Deletion", "Imputed"),
    values = c("TRUE" = "twodash", "FALSE" = "solid")
  ) +
  labs(
    x = "Highest Grade Completed",
    y = "Density",
    fill = "Data Type",
    color = "Data Type",
    linetype = "Data Type"
  ) +
  theme_apa()

print(p)

ggsave(
  filename = "densityplot.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 3,
  units = "in"
)

########## Attempt Generalized OLR ##########
golrMod <- with(
  imp,
  vglm(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
      HGCParentEd *
      DV_RACE_BLACK +
      HGCParentEd * DV_RACE_HISPANIC +
      HGCParentEd * DV_RACE_MIXED,
    family = cumulative(
      link = "logitlink",
      parallel = FALSE ~ DV_RACE_MIXED + DV_RACE_BLACK
    ),
    weights = SAMPLING_WEIGHT_CC_2017
  )
)

rrVGAM <- function(model, returnFull = FALSE) {
  # Applies Rubin's Rules to items of mira and vglm class. Used for VGAM with imputations.

  if (
    !inherits(model, "mira") ||
      !inherits(model$analyses[[1]], c("vglm", "svy_vglm"))
  ) {
    stop("Model is either not a mira or vglm object, or both")
  }

  coef_tbl <- purrr::map_dfr(
    model$analyses,
    \(fit) {
      tibble::tibble(
        term = names(coef(fit)),
        q = unname(coef(fit)),
        u = diag(vcov(fit))
      )
    },
    .id = "imp"
  )

  pooled <- coef_tbl %>%
    dplyr::summarise(
      m = dplyr::n(),
      qbar = mean(q),
      ubar = mean(u),
      b = var(q),
      t = ubar + (1 + 1 / m) * b,
      r = ((1 + 1 / m) * b) / ubar,
      df = (m - 1) * (1 + 1 / r)^2,
      std.error = sqrt(t),
      statistic = qbar / std.error,
      p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
      conf.low = qbar - stats::qt(0.975, df = df) * std.error,
      conf.high = qbar + stats::qt(0.975, df = df) * std.error,
      .by = term
    ) %>%
    dplyr::mutate(
      odds_ratio = exp(qbar),
      conf.low.or = exp(conf.low),
      conf.high.or = exp(conf.high)
    )

  if (returnFull) {
    return(pooled)
  }

  pooledReadable <- pooled %>%
    select(-m, -qbar, -ubar, -b, -t, -std.error) %>%
    filter(!grepl(pattern = "^\\(Intercept\\):", term))
  return(pooledReadable)
}

pooled <- rrVGAM(golrMod)

# pooled %>% write.csv(file = "pooledPartialPOLR.csv")

pooled

########## svyVGAM ##########

svygolr <- with(imp, {
  des <- svydesign(
    ids = ~VSTRAT_1997,
    strata = ~VPSU_1997,
    weights = ~SAMPLING_WEIGHT_CC_2017,
    nest = TRUE
  )

  svy_vglm(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
      HGCParentEd *
      DV_RACE_BLACK +
      HGCParentEd * DV_RACE_HISPANIC +
      HGCParentEd * DV_RACE_MIXED,
    design = des,
    family = cumulative(
      link = "logitlink",
      parallel = FALSE ~ DV_RACE_MIXED + DV_RACE_BLACK + DV_RACE_HISPANIC
    )
  )
})

svygolr %>% rrVGAM()

########## POLR ##########

# Runs ordinal logit on imp data
pom_imp <- with(
  imp,
  polr(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
      HGCParentEd *
      DV_RACE_BLACK +
      HGCParentEd * DV_RACE_HISPANIC +
      HGCParentEd * DV_RACE_MIXED,
    start = startdf2,
    Hess = TRUE,
    weights = SAMPLING_WEIGHT_CC_2017
  )
)


# !!!!!Remember data is logarithmic!!!!!
pom_pooled <- pool(pom_imp)
summary(pom_pooled)

oddRatio <- list()
oddRatio$Term <- pom_pooled$pooled$term
oddRatio$Estimate <- pom_pooled$pooled$estimate
oddRatio$OR <- exp(pom_pooled$pooled$estimate)
oddRatio$TValue <- pom_pooled$pooled$t
oddRatio$Sig <- summary(pom_pooled)$p.value

print("OR dataframe")
as.data.frame(oddRatio)

oddRatioPooledSum <- as.data.frame(summary(pom_pooled))
oddRatioPooledSum <- oddRatioPooledSum[, -6]
oddRatioPooledSum <- oddRatioPooledSum[-c(8:13), ]


# Converting pooled results to tidy format
pooled_summary <- summary(pom_pooled)

# Adding term names
tidy_pooled <- tidy(pom_pooled, conf.int = TRUE, conf.level = 0.95)

tidy_pooled <- tidy_pooled %>%
  mutate(
    term = dplyr::recode(
      term,
      "DV_RACE_BLACK" = "Black",
      "DV_RACE_HISPANIC" = "Hispanic",
      "DV_RACE_MIXED" = "Mixed",
    )
  )

termFilter <- c(
  "HGCParentEd",
  "Black",
  "Hispanic",
  "Mixed",
  "HGCParentEd:DV_RACE_BLACK",
  "HGCParentEd:DV_RACE_HISPANIC",
  "HGCParentEd:DV_RACE_MIXED"
)

tidy_pooled_sub <- tidy_pooled %>%
  filter(
    term %in% termFilter
  )

# Plot for predictors
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Log Odds Estimate",
    y = "Predictor Estimate",
    title = "Weighted - Imp"
  ) +
  theme_apa()

ggsave(
  filename = "coefficientplot2.1.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 4.5,
  units = "in"
)

tidy_pooled_sub <- tidy_pooled %>%
  filter(
    !term %in% termFilter
  )
# Plot for thresholds
# title = "Pooled Coefficient Estimates from Imputed polr Model"
ggplot(tidy_pooled_sub, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Log Odds Estimate", y = "Threshold Estimate") +
  theme_apa()

ggsave(
  filename = "coefficientplot2.2.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 4.5,
  units = "in"
)

# define threshold cutoffs
thresholds <- c(
  "None|GED" = 1.29835,
  "GED|HS" = 2.50778,
  "HS|AA" = 4.54876,
  "AA|BA" = 5.02543,
  "BA|MA" = 6.66202,
  "MA|PhD" = 9.28300
)

# Create a sequence of linear predictor values (e.g., effects of covariates)
x_vals <- seq(-5, 20, length.out = 500)

# Compute cumulative probabilities using logistic (sigmoid) function
logistic <- function(x) 1 / (1 + exp(-x))

cum_probs <- sapply(thresholds, function(cut) logistic(cut - x_vals))

dfc <- as.data.frame(cum_probs)
dfc$x <- x_vals

# Calculate individual category probabilities
dfc <- dfc %>%
  mutate(
    None = `None|GED`,
    GED = `GED|HS` - `None|GED`,
    HS = `HS|AA` - `GED|HS`,
    AA = `AA|BA` - `HS|AA`,
    BA = `BA|MA` - `AA|BA`,
    MA = `MA|PhD` - `BA|MA`,
    PhD = 1 - `MA|PhD`
  ) %>%
  dplyr::select(x, None, GED, HS, AA, BA, MA, PhD) %>%
  pivot_longer(-x, names_to = "Education_Level", values_to = "Probability")

# plot
ggplot(dfc, aes(x = x, y = Probability, color = Education_Level)) +
  geom_line(linewidth = 0.78) +
  xlim(-0.5, 20) +
  labs(x = "Linear Predictor", y = "Probability", color = "Education Level") +
  theme_apa()

ggsave(
  filename = "probabilityplot.png",
  plot = last_plot(),
  scale = 1,
  device = "png",
  dpi = "retina",
  width = 6.5,
  height = 4.5,
  units = "in"
)

off <- FALSE

if (off == FALSE) {
  #### Some POLR Diagnostics for parallel slopes assumption ####

  sf <- function(y) {
    c(
      'Y>=1' = qlogis(mean(y >= 1)),
      'Y>=2' = qlogis(mean(y >= 2)),
      'Y>=3' = qlogis(mean(y >= 3)),
      'Y>=4' = qlogis(mean(y >= 4)),
      'Y>=5' = qlogis(mean(y >= 5)),
      'Y>=6' = qlogis(mean(y >= 6)),
      'Y>=7' = qlogis(mean(y >= 7))
    )
  }

  # Get one completed dataset for the summary table
  completed_imp <- complete(imp, 1)

  # Use aggregate + custom function to get the same style of cutpoint logits
  diag_tbl <- completed_imp %>%
    dplyr::mutate(y_num = as.numeric(CV_HIGHEST_DEGREE_EVER_EDT_2017)) %>%
    dplyr::summarise(
      N = dplyr::n(),
      across(y_num, ~ list(sf(.x))),
      .by = c(HGCParentEd, DV_RACE_BLACK, DV_RACE_HISPANIC, DV_RACE_MIXED)
    ) %>%
    tidyr::unnest_wider(y_num)

  diag_tbl
}


########## Survey ##########

# Fixed after turning in, see svyglm.txt for code used in paper
# Weighted OLR
imp_data$degree_num <- factor(
  imp_data$CV_HIGHEST_DEGREE_EVER_EDT_2017,
  levels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
  ordered = TRUE
)

svy_model <- with(imp, {
  svy_design <- svydesign(
    ids = ~VSTRAT_1997,
    strata = ~VPSU_1997,
    weights = ~SAMPLING_WEIGHT_CC_2017,
    nest = TRUE
  )

  # Run weighted ordinal logistic regression using svyolr
  svyolr(
    CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
      HGCParentEd *
      DV_RACE_BLACK +
      HGCParentEd * DV_RACE_HISPANIC +
      HGCParentEd * DV_RACE_MIXED,
    design = svy_design
  )
})

svypooled <- pool(svy_model)

paperModel <- summary(
  svypooled,
  conf.int = TRUE,
  conf.level = 0.95
)

print("Important!!!! ==================")
paperModel <- paperModel %>% as.data.frame()
paperModel <- paperModel %>%
  mutate(OR = exp(estimate)) %>%
  select(term, estimate, OR, std.error, statistic, conf.low, conf.high)
paperModel %>% stargazer(summary = FALSE)
print("Important !!!! ==================")

stop("Done!")

# don't use this stuff \/ \/ \/

svy_model_sum <- summary(svy_model)$coefficient %>% as.data.frame()
svy_model_sum <- cbind(svy_model_sum, ci)
svy_model_sum <- svy_model_sum[-c(8:13), ]
svy_model_sum <- svy_model_sum %>%
  mutate(
    OR = exp(Value)
  ) %>%
  select(Value, OR, everything())
svy_model_sum %>% stargazer(summary = FALSE)

# don't use this stuff /\ /\ /\
