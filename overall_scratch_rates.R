library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

scratch_rates = scratch_data %>%
  group_by(group) %>%
  summarise(
    n_samples = n(),
    scratch_rate = mean(scratch, na.rm = TRUE))
# overall rate: 0.17
# AF: 0.15; AM: 0.21; J: 0.16

model_scratch = glm(scratch ~ group, data = scratch_data, family = binomial)
summary(model_scratch)
# AMs scratch significantly more than AFs (p = 0.0116)
# No significant difference between Js and AFs (p = 0.68)

emmeans(model_scratch, pairwise ~ group, type = "response")
# AMs are significantly more likely to scratch than AFs (p = 0.0312)
# No difference between AFs and Js (p = 0.91)
# No significant difference between AMs and Js (p = 0.25)

