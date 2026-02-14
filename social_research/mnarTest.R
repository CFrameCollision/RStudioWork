# run analysis.r first.

if (!exists("new_data_rmNA", inherits = TRUE)) {
  stop("Missing key data")
}

testDadMomMNAR <- new_data_rmNA %>%
  select(CV_HGC_RES_DAD_1997, CV_HGC_RES_MOM_1997, KEY_RACE_ETHNICITY_1997)

testDadMomMNAR <- testDadMomMNAR %>%
  mutate(
    dadmiss = case_when(
      is.na(CV_HGC_RES_DAD_1997) ~ 1,
      TRUE ~ 0
    ),
    mommiss = case_when(
      is.na(CV_HGC_RES_MOM_1997) ~ 1,
      TRUE ~ 0
    )
  )

lm(dadmiss ~ KEY_RACE_ETHNICITY_1997, testDadMomMNAR) %>% summary()
lm(mommiss ~ KEY_RACE_ETHNICITY_1997, testDadMomMNAR) %>% summary()

ggplot(
  testDadMomMNAR,
  mapping = aes(x = KEY_RACE_ETHNICITY_1997, y = dadmiss)
) +
  geom_point(position = position_jitter(width = 0.01, height = 0.01)) +
  geom_smooth(method = "lm")

ggplot(
  testDadMomMNAR,
  mapping = aes(x = KEY_RACE_ETHNICITY_1997, y = mommiss)
) +
  geom_point(position = position_jitter(width = 0.01, height = 0.01)) +
  geom_smooth(method = "lm")

# Miss key: missing = 1, present = 0
# Race key:
# 1 Black
# 2 Hispanic
# 3 Mixed Race (Non-Hispanic)
# 4 Non-Black

testDadMomMNAR %>%
  dplyr::count(KEY_RACE_ETHNICITY_1997, dadmiss) %>%
  pivot_wider(names_from = KEY_RACE_ETHNICITY_1997, values_from = n)

testDadMomMNAR %>%
  dplyr::count(KEY_RACE_ETHNICITY_1997, mommiss) %>%
  pivot_wider(names_from = KEY_RACE_ETHNICITY_1997, values_from = n)
