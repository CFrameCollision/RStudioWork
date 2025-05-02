# Run "./data/new_data97-educational-data/new_data97-educational-data.R" first.

library(tidyverse)
library(extrafont)
library(ggthemes)
library(hrbrthemes)
library(thematic)
library(extrafontdb)
library(colorspace)
library(addinslist)
library(clipr)
library(gmodels)
library(Hmisc)
library(RColorBrewer)
library(DescTools)
library(viridis)
library(ggpmisc)

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
    TRUE ~ NA_character_
  ))

new_data_rmNA <- new_data %>% filter(!is.na(degree_label))

ggplot(new_data_rmNA, aes(x = degree_label)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Highest Degree Attained",
       x = "Degree") +
  stat_count(geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 1.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

regression_data <- new_data_rmNA %>%
  filter(!is.na(KEY_RACE_ETHNICITY_1997),
         !is.na(CV_HGC_RES_MOM_1997),
         !is.na(CV_HGC_RES_DAD_1997))

lmresult <- lm(CV_HIGHEST_DEGREE_EVER_EDT_2017 ~ KEY_RACE_ETHNICITY_1997 + CV_HGC_RES_MOM_1997 + CV_HGC_RES_DAD_1997,
               data = regression_data)

summary(lmresult)

df <- regression_data %>%
  mutate(fitted = lmresult$fitted.values,
         residuals = lmresult$residuals,
         hD = CV_HIGHEST_DEGREE_EVER_EDT_2017)

median(df$fitted)

x_value <- 2.747199
y_value <- (2.72e-14) + 1 * x_value

ggplot(df, aes(x = fitted, y = hD)) +
  geom_point() +
  geom_smooth(method = 'lm', color = '#679df5') +
  labs(title = "test") +
  xlab("Fitted values (see section 2-4 for more info)") +
  ylab("Prayer Frequency") +
  stat_poly_eq(use_label(c("eq", "R2")), rr.digits = 4,vjust = 1.3) +
  theme_pander() +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")) +
  geom_point(aes(x = x_value, y = y_value), color = "red", size = 2.2) +
  geom_text(aes(x = x_value, y = y_value,
                label = paste("(", round(x_value, 2), ", ",
                              round(y_value, 2), ")", sep = "")),
            vjust = -1, hjust = 1, size = 3.2)