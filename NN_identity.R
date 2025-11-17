library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

NN_identity = scratch_data %>%
  mutate(NN_age_sex = factor(NN_age_sex, levels = c("0", "J", "AF", "AM")))

model_identity = glm(scratch ~ NN_age_sex * group, data = NN_identity, family = binomial)
summary(model_identity)
# AF: sig lower scratching when NN is J (p = 0.02), not with AM or other AF
# J: sig lower scratching when NN is AF (p = 0.002)


