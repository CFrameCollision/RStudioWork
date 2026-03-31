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

### Some descriptives

if (TRUE) {
    df <- table(new_data_rmNA[c("race", "degree_label")]) %>% as.data.frame()
    meanFreq <- df %>%
        group_by(degree_label) %>%
        mutate(Mean = mean(Freq), SD = sd(Freq)) %>%
        ungroup() %>%
        mutate(Race = as.character(race), HGC = as.character(degree_label))
    starMeanFreq <- meanFreq %>%
        select(-degree_label, -race) %>%
        select(HGC, Race, everything()) %>%
        mutate(SD = round(SD, 2))
    stargazer::stargazer(starMeanFreq, summary = FALSE)
}

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

######### Diagnostic Func ##########
# Purpose-built; not useable in other code yet.

run_svyolr_diagnostics_mi <- function(imp, sex_code = 1) {
    imp_list <- complete(imp, action = "all")

    per_imp <- purrr::imap_dfr(imp_list, \(dat, .imp_id) {
        dat <- dat %>%
            as_tibble() %>%
            mutate(
                CV_HIGHEST_DEGREE_EVER_EDT_2017 = if (
                    is.factor(CV_HIGHEST_DEGREE_EVER_EDT_2017)
                ) {
                    CV_HIGHEST_DEGREE_EVER_EDT_2017
                } else {
                    factor(
                        CV_HIGHEST_DEGREE_EVER_EDT_2017,
                        levels = 0:6,
                        labels = c(
                            "None",
                            "GED",
                            "HS",
                            "AA",
                            "BA",
                            "MA",
                            "PhD"
                        ),
                        ordered = TRUE
                    )
                },
                race = case_when(
                    DV_RACE_BLACK == 1 ~ "Black",
                    DV_RACE_HISPANIC == 1 ~ "Hispanic",
                    DV_RACE_MIXED == 1 ~ "Mixed Race",
                    TRUE ~ "Non-Black/Non-Hispanic"
                ) %>%
                    factor(
                        levels = c(
                            "Black",
                            "Hispanic",
                            "Mixed Race",
                            "Non-Black/Non-Hispanic"
                        )
                    )
            )

        ds <- svydesign(
            ids = ~VPSU_1997,
            strata = ~VSTRAT_1997,
            nest = TRUE,
            weights = ~SAMPLING_WEIGHT_CC_2017,
            data = dat
        )

        ds_sub <- subset(ds, KEY_SEX_1997 == sex_code)
        dat_sub <- dat %>% filter(KEY_SEX_1997 == sex_code)

        sparse_tab <- svytable(~ CV_HIGHEST_DEGREE_EVER_EDT_2017 + race, ds_sub)
        sparse_vals <- as.numeric(sparse_tab)

        fit_po <- tryCatch(
            svyolr(
                CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
                    HGCParentEd *
                    DV_RACE_BLACK +
                    HGCParentEd * DV_RACE_HISPANIC +
                    HGCParentEd * DV_RACE_MIXED,
                design = ds_sub
            ),
            error = \(e) NULL
        )

        if (is.null(fit_po)) {
            return(
                tibble(
                    imp = as.integer(.imp_id),
                    min_weighted_cell = min(sparse_vals),
                    n_zero_cells = sum(sparse_vals == 0),
                    cutpoints_strictly_increasing = NA,
                    extreme_prob_rate_95 = NA_real_,
                    extreme_prob_rate_99 = NA_real_,
                    po_lrt_p = NA_real_,
                    condition_number = NA_real_,
                    fit_failed = TRUE
                )
            )
        }

        pred_probs <- predict(fit_po, type = "probs")

        fit_par <- tryCatch(
            svy_vglm(
                CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
                    HGCParentEd *
                    DV_RACE_BLACK +
                    HGCParentEd * DV_RACE_HISPANIC +
                    HGCParentEd * DV_RACE_MIXED,
                design = ds_sub,
                family = VGAM::cumulative(link = "logit", parallel = TRUE)
            ),
            error = \(e) NULL
        )

        fit_nonpar <- tryCatch(
            svy_vglm(
                CV_HIGHEST_DEGREE_EVER_EDT_2017 ~
                    HGCParentEd *
                    DV_RACE_BLACK +
                    HGCParentEd * DV_RACE_HISPANIC +
                    HGCParentEd * DV_RACE_MIXED,
                design = ds_sub,
                family = VGAM::cumulative(link = "logit", parallel = FALSE)
            ),
            error = \(e) NULL
        )

        po_lrt_p <- NA_real_
        if (!is.null(fit_par) && !is.null(fit_nonpar)) {
            cmp <- tryCatch(
                anova(fit_par, fit_nonpar, test = "Chisq"),
                error = \(e) NULL
            )
            if (!is.null(cmp)) {
                cmp_df <- as.data.frame(cmp)
                p_col <- grep("Pr\\(>.*\\)", names(cmp_df), value = TRUE)
                if (length(p_col) > 0) {
                    po_lrt_p <- suppressWarnings(as.numeric(cmp_df[[p_col[
                        1
                    ]]][nrow(cmp_df)]))
                }
            }
        }

        x <- model.matrix(
            ~ HGCParentEd *
                DV_RACE_BLACK +
                HGCParentEd * DV_RACE_HISPANIC +
                HGCParentEd * DV_RACE_MIXED,
            data = dat_sub
        )

        tibble(
            imp = as.integer(.imp_id),
            min_weighted_cell = min(sparse_vals),
            n_zero_cells = sum(sparse_vals == 0),
            cutpoints_strictly_increasing = all(diff(fit_po$zeta) > 0),
            extreme_prob_rate_95 = mean(apply(pred_probs, 1, max) > 0.95),
            extreme_prob_rate_99 = mean(apply(pred_probs, 1, max) > 0.99),
            po_lrt_p = po_lrt_p,
            condition_number = kappa(x, exact = TRUE),
            fit_failed = FALSE
        )
    })

    aggregate <- per_imp %>%
        dplyr::summarize(
            n_imputations = n(),
            n_failed = sum(fit_failed),
            min_weighted_cell_mean = mean(min_weighted_cell, na.rm = TRUE),
            n_zero_cells_mean = mean(n_zero_cells, na.rm = TRUE),
            cutpoints_all_ordered = all(
                cutpoints_strictly_increasing,
                na.rm = TRUE
            ),
            extreme_prob_rate_95_mean = mean(
                extreme_prob_rate_95,
                na.rm = TRUE
            ),
            extreme_prob_rate_99_mean = mean(
                extreme_prob_rate_99,
                na.rm = TRUE
            ),
            po_lrt_p_median = median(po_lrt_p, na.rm = TRUE),
            condition_number_mean = mean(condition_number, na.rm = TRUE)
        )

    list(
        per_imp = per_imp,
        aggregate = aggregate
    )
}

########## svyolr ##########
options(survey.lonely.psu = "adjust")
##### Female model - KEY_SEX = 2 #####

run_svyolr_diagnostics_mi(imp, sex_code = 2)

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

###### Let's see if a binary OLS has any results... ######

long_imp <- complete(imp, action = "long", include = TRUE)

binaryCut <- 1
binaryArg <- switch(
    binaryCut,
    `1` = c("BA", "MA", "PhD"), # Most significant model.
    `2` = c("MA", "PhD"),
    `3` = c("PhD"),
    `4` = c("HS", "BA", "MA", "PhD"),
    `5` = c("GED", "HS", "BA", "MA", "PhD")
)

binaryImp <- long_imp %>%
    mutate(
        binaryHGC = if_else(
            CV_HIGHEST_DEGREE_EVER_EDT_2017 %in% binaryArg,
            1,
            0
        )
    ) %>%
    as.mids()

mod1 <- with(
    binaryImp,
    lm(
        binaryHGC ~ HGCParentEd *
            DV_RACE_BLACK +
            HGCParentEd * DV_RACE_HISPANIC +
            HGCParentEd * DV_RACE_MIXED
    )
)

pmod1 <- pool(mod1)
summary(pmod1)

# Create training & testing data
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
n_obs <- nrow(binaryImp$data)
train_idx <- sample.int(n_obs, size = floor(0.7 * n_obs), replace = FALSE)

### TODO: find and use test statistic for model significance of quasibinomial svy model.
t <- with(binaryImp, {
    d <- tibble(
        binaryHGC,
        HGCParentEd,
        DV_RACE_MIXED,
        DV_RACE_HISPANIC,
        DV_RACE_BLACK,
        VSTRAT_1997,
        VPSU_1997,
        SAMPLING_WEIGHT_CC_2017
    )

    train <- d[train_idx, , drop = FALSE]

    des <- svydesign(
        ids = ~VPSU_1997,
        strata = ~VSTRAT_1997,
        nest = TRUE,
        weights = ~SAMPLING_WEIGHT_CC_2017,
        data = train
    )

    sm <- svyglm(
        binaryHGC ~ HGCParentEd *
            DV_RACE_BLACK +
            HGCParentEd * DV_RACE_HISPANIC +
            HGCParentEd * DV_RACE_MIXED,
        design = des,
        family = quasibinomial(link = "logit")
    )
    sm
})

tp <- pool(t)
paperModel <- summary(tp, conf.int = TRUE)

print("IMPORTANT===========================================")
paperModel <- paperModel %>% as.data.frame()
paperModel <- paperModel %>%
    mutate(OR = exp(estimate)) %>%
    select(term, estimate, OR, std.error, statistic, conf.low, conf.high)
paperModel %>% stargazer(summary = FALSE)
print("IMPORTANT===========================================")


######## Sensitivity Check #########

imp_data$CV_HIGHEST_DEGREE_EVER_EDT_2017 <- factor(
    imp_data$CV_HIGHEST_DEGREE_EVER_EDT_2017,
    levels = 0:6,
    labels = c("None", "GED", "HS", "AA", "BA", "MA", "PhD"),
    ordered = TRUE
)

binaryDat <- imp_data %>%
    mutate(
        binaryHGC = if_else(
            CV_HIGHEST_DEGREE_EVER_EDT_2017 %in% binaryArg,
            1,
            0
        )
    )


svdesign <- svydesign(
    ids = ~VPSU_1997,
    strata = ~VSTRAT_1997,
    nest = TRUE,
    weights = ~SAMPLING_WEIGHT_CC_2017,
    data = binaryDat
)

sm <- svyglm(
    binaryHGC ~ HGCParentEd *
        DV_RACE_BLACK,
    design = svdesign,
    family = quasibinomial(link = "logit")
)

summary(sm)
vif(sm)

#####################################

predmod <- with(binaryImp, {
    d <- tibble(
        binaryHGC,
        HGCParentEd,
        DV_RACE_MIXED,
        DV_RACE_HISPANIC,
        DV_RACE_BLACK,
        VSTRAT_1997,
        VPSU_1997,
        SAMPLING_WEIGHT_CC_2017
    )

    train <- d[train_idx, , drop = FALSE]

    des <- svydesign(
        ids = ~VPSU_1997,
        strata = ~VSTRAT_1997,
        nest = TRUE,
        weights = ~SAMPLING_WEIGHT_CC_2017,
        data = train
    )

    test <- d[-train_idx, , drop = FALSE]

    sm <- svyglm(
        binaryHGC ~ HGCParentEd *
            DV_RACE_BLACK +
            HGCParentEd * DV_RACE_HISPANIC +
            HGCParentEd * DV_RACE_MIXED,
        design = des,
        family = quasibinomial(link = "logit")
    )
    pr <- predict(sm, test, type = "link", se.fit = TRUE)
})

pred_list <- predmod$analyses %>% purrr::map(\(x) as.data.frame(x))

n_pred <- nrow(pred_list[[1]])

pooled_pred <- purrr::map_dfr(seq_len(n_pred), \(i) {
    q_i <- purrr::map_dbl(pred_list, \(x) x$link[i])
    u_i <- purrr::map_dbl(pred_list, \(x) x$SE[i]^2)

    rubin <- mice::pool.scalar(Q = q_i, U = u_i, n = Inf, k = 1)

    tibble::tibble(
        row_id = i,
        eta = rubin$qbar,
        eta_se = sqrt(rubin$t),
        p = plogis(rubin$qbar)
    )
})

pooled_pred
