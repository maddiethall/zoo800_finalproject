library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

rates_by_group = scratch_data %>%
  group_by(group) %>%
  summarise(
    n_samples = n(),
    scratch_rate = mean(scratch, na.rm = TRUE),
    groom_rate = mean(groom, na.rm = TRUE),
    social_rest_rate = mean(social_rest, na.rm = TRUE),
    submission_rate = mean(submission, na.rm = TRUE),
    social_other_rate = mean(social_other, na.rm = TRUE),
    mean_NN_dist = mean(NN_dist, na.rm = TRUE),
    mean_within2 = mean(NN_within_2m, na.rm = TRUE),
    mean_2to5 = mean(NN_2_to_5m, na.rm = TRUE)
  )

rates_overall = scratch_data %>%
  summarise(
    n_samples = n(),
    scratch_rate = mean(scratch, na.rm = TRUE),
    groom_rate = mean(groom, na.rm = TRUE),
    social_rest_rate = mean(social_rest, na.rm = TRUE),
    submission_rate = mean(submission, na.rm = TRUE),
    social_other_rate = mean(social_other, na.rm = TRUE),
    mean_NN_dist = mean(NN_dist, na.rm = TRUE),
    mean_within2 = mean(NN_within_2m, na.rm = TRUE),
    mean_2to5 = mean(NN_2_to_5m, na.rm = TRUE)
  ) %>%
  mutate(group = "Overall")

rates_summary = bind_rows(rates_by_group, rates_overall)
# overall rate: 0.17
# AF: 0.15; AM: 0.21; J: 0.16

# compare rates of other things too

model_scratch = glm(scratch ~ group, data = scratch_data, family = binomial)
summary(model_scratch)
# AMs scratch significantly more than AFs (p = 0.0116)
# No significant difference between Js and AFs (p = 0.68)

emmeans(model_scratch, pairwise ~ group, type = "response")
# AMs are significantly more likely to scratch than AFs (p = 0.0312)
# No difference between AFs and Js (p = 0.91)
# No significant difference between AMs and Js (p = 0.25)