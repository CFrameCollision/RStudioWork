# Run "./data/new_data97-educational-data/new_data97-educational-data.R" and
# analysis.R first.

library(dplyr)
library(ggplot2)
library(naniar)

# Looking at missing-ness by race/ethnicity
new_data_rmNA %>%
  select(KEY_RACE_ETHNICITY_1997, CV_HGC_RES_MOM_1997, CV_HGC_RES_DAD_1997) %>%
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

gg_miss_upset(new_data_rmNA)

mcar_data <- new_data_rmNA %>%
  select(CV_HIGHEST_DEGREE_EVER_EDT_2017,
         CV_HGC_RES_MOM_1997,
         CV_HGC_RES_DAD_1997)

mcar_test(new_data_rmNA)

library(mice)

imp_data <- new_data_rmNA %>%
  select(CV_HIGHEST_DEGREE_EVER_EDT_2017,
         KEY_RACE_ETHNICITY_1997,
         CV_HGC_RES_MOM_1997,
         CV_HGC_RES_DAD_1997)

imp <- mice(imp_data, m = 5, method = 'pmm', seed = 123)
fit_imp <- with(imp, lm(CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ KEY_RACE_ETHNICITY_1997 + CV_HGC_RES_MOM_1997 + CV_HGC_RES_DAD_1997))

summary(pool(fit_imp))