library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(car)

scratch_data = readRDS("cleaned_combined_data.rds")

model_social_NN_simple = glm(scratch ~ NN_dist * social_simple, data = social_data, family = binomial)
summary(model_social_NN_simple)
# the slope of NN_dist changes when animals are social — it becomes steeper and positive, slightly lower baseline
## (p ≈ 0.057, nearly significant)

preds_social_NN_simple = ggpredict(model_social_NN_simple, terms = c("NN_dist [all]", "social_simple"))
ggplot(preds_social_NN_simple, aes(x = x, y = predicted, color = group)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  labs(
    x = "Nearest Neighbor Distance",
    y = "Predicted Probability of Scratching",
    color = "Social Behavior",
    fill = "Social Behavior",
    title = "Effect of Nearest Neighbor Distance and Social Behavior on Scratching"
  ) +
  theme_minimal(base_size = 14)


model_social_NN = glm(scratch ~ NN_dist * social_simple * group, data = social_data, family = binomial)
summary(model_social_NN)
# AF: sig effect of NN distance on scratching when social vs not
# lower scratching prob during social behavior and close proximity 
# and scratching increases during social behavior as NN distance increases (p = 0.029)
# NS for AM or J
# J: strongest distance effect, social line rises faster
# interaction between NN distance and social context across groups is NS

preds_social_NN = ggpredict(model_social_NN, terms = c("NN_dist [all]", "social_simple", "group"))

ggplot(preds_social_NN, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  facet_wrap(~facet) +
  labs(
    x = "Nearest Neighbor Distance",
    y = "Predicted Probability of Scratching",
    color = "Social Behavior",
    fill = "Social Behavior",
    title = "Compounding Effect of Social Behavior and Proximity on Scratching"
  ) +
  theme_minimal(base_size = 14)

model_social_NN_number = glm(scratch ~ NN_total * social_simple * group, data = NN_data, family = binomial)
summary(model_social_NN_number)
# AF: significant interaction effect of NN_total and social_simple (p = 0.01; scratching increases with
## NN_number when nonsocial, decreases when social)
# AM: also significant interaction (p = 0.02; scratching increases with NN_number when social,
## and decreases with NN_number when nonsocial)
# J: also significant (p = 0.01; lower baseline with social behavior vs not, in the nonsocial
## part, scratch decreases with NN_number)

preds_social_NN_number = ggpredict(model_social_NN_number, terms = c("NN_total", "social_simple", "group"))
ggplot(preds_social_NN_number, aes (x = x, y = predicted, color = facet)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = facet), alpha = 0.15, color = NA) +
  facet_wrap(~group)

