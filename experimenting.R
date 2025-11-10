library(dplyr)
library(ggplot2)
library(ggeffects)

scratch_data = readRDS("cleaned_combined_data.rds")


model1 = glm(
  scratch ~ groom + social_rest + submission + social_other + 
    NN_dist + NN_within_2m + NN_2_to_5m + NN_age_sex + group,
  data = scratch_data,
  family = binomial
)
summary(model1)

model2 <- glm(
  scratch ~ (groom + social_rest + submission + social_other) * group +
    NN_dist + NN_within_2m + NN_2_to_5m + NN_age_sex,
  data = scratch_data,
  family = binomial
)
summary(model2)

model_dist <- glm(
  scratch ~ NN_dist,
  data = scratch_data,
  family = binomial
)
summary(model_dist)

model_dist2 <- glm(
  scratch ~ NN_dist * group,
  data = scratch_data,
  family = binomial
)
summary(model_dist2)

