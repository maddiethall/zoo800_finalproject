library(dplyr)
library(ggplot2)
library(ggeffects)
library(car)

scratch_data = readRDS("data_final.rds")

model_identity = glm(scratch ~ NN_age_sex * group, data = scratch_data, family = binomial)
summary(model_identity)
# AF: sig lower scratching when NN is J (p = 0.02), not with AM or other AF
# J: sig lower scratching when NN is AF (p = 0.002)

Anova(model_identity, type = "III")
# sig interaction effect between focal group and NN identity (p = 0.0006)

emm_identity = emmeans(model_identity, ~ NN_age_sex | group, type = "response")


within_group_identity = contrast(emm_identity, method = "pairwise", by = "group", adjust = "tukey")
within_group_identity

between_group_identity = contrast(emm_identity, method = "pairwise", by = "NN_age_sex", adjust = "tukey")
between_group_identity
