# I need a clean environment to think. Analysis V1 was created when I was still learning to code
# it's structure gives me a headache. This is a continuation of analysis V2, hence the suffix.

# In this file, I'll be subsetting my analyses to include gender. Essentially, I'll run
# a POLR and GOLR model for each gender. Furthermore, I'll attempt a Semi-P OLR.

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
    m = 5,
    method = 'pmm',
    predictorMatrix = impPredictorMatrix,
    seed = 1234
)

imp <- complete(imp, action = "long", include = TRUE)

imp$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
    imp$CV_HIGHEST_DEGREE_EVER_EDT_2017,
    levels = 0:6,
    labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
    ordered = TRUE
)

imp <- as.mids(imp)

########## svyolr ##########
options(survey.lonely.psu = "adjust")
##### Female model - KEY_SEX = 2 #####

mf <- with(imp, {
    dat <- tibble(
        KEY_SEX_1997,
        CV_HIGHEST_DEGREE_EVER_EDT_2017,
        HGCParentEd,
        DV_RACE_BLACK,
        DV_RACE_HISPANIC,
        DV_RACE_MIXED,
        VSTRAT_1997,
        VPSU_1997,
        SAMPLING_WEIGHT_CC_2017
    )

    ds <- svydesign(
        ids = ~VPSU_1997,
        strata = ~VSTRAT_1997,
        nest = TRUE,
        weights = ~SAMPLING_WEIGHT_CC_2017,
        data = dat
    )

    ds_female <- subset(ds, KEY_SEX_1997 == 2)
    svyolr(
        CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
            HGCParentEd *
            DV_RACE_BLACK +
            HGCParentEd * DV_RACE_HISPANIC +
            HGCParentEd * DV_RACE_MIXED,
        design = ds_female
    )
})

pmf <- pool(mf)

pmfdf <- data.frame(
    TermFemale = summary(pmf)$term,
    EstimateFemale = summary(pmf)$estimate,
    P.Value.Female = summary(pmf)$p.value
)
pmfdf[1:7, ]

###### Male model - KEY_SEX = 1 #####

m <- with(imp, {
    dat <- tibble(
        KEY_SEX_1997,
        CV_HIGHEST_DEGREE_EVER_EDT_2017,
        HGCParentEd,
        DV_RACE_BLACK,
        DV_RACE_HISPANIC,
        DV_RACE_MIXED,
        VSTRAT_1997,
        VPSU_1997,
        SAMPLING_WEIGHT_CC_2017
    )

    ds <- svydesign(
        ids = ~VPSU_1997,
        strata = ~VSTRAT_1997,
        nest = TRUE,
        weights = ~SAMPLING_WEIGHT_CC_2017,
        data = dat
    )

    ds_male <- subset(ds, KEY_SEX_1997 == 1)
    svyolr(
        CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
            HGCParentEd *
            DV_RACE_BLACK +
            HGCParentEd * DV_RACE_HISPANIC +
            HGCParentEd * DV_RACE_MIXED,
        design = ds_male
    )
})

pm <- pool(m)

pmdf <- data.frame(
    TermMale = summary(pm)$term,
    EstimateMale = summary(pm)$estimate,
    P.Value.Male = summary(pm)$p.value
)
pmdf[1:7, ]

## Combine to analyse inter-sex differences

combDF <- cbind(pmdf[1:7, ], pmfdf[1:7, ])

combDF <- combDF %>%
    mutate(
        P.Value.Diff = abs(P.Value.Male - P.Value.Female),
        ORMal = exp(EstimateMale),
        ORFem = exp(EstimateFemale),
        ORDiff = ORFem - ORMal
    ) %>%
    select(-TermFemale) %>%
    rename(Term = TermMale)

combDF
