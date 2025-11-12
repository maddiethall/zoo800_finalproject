library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

model_social_NN_simple = glm(
  scratch ~ NN_dist * social_simple,
  data = social_data,
  family = binomial)
summary(model_social_NN_simple)
# the slope of NN_dist changes when animals are social — it becomes steeper and positive (p ≈ 0.057, nearly significant)

preds_social_NN_simple = ggpredict(model_social_NN_simple, terms = c("NN_dist [all]", "social_simple"))
ggplot(preds_social_NN_simple, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  labs(
    x = "Nearest Neighbor Distance",
    y = "Predicted Probability of Scratching",
    color = "Social Behavior",
    fill = "Social Behavior",
    title = "Effect of Nearest Neighbor Distance and Social Behavior on Scratching"
  ) +
  theme_minimal(base_size = 14)



model_social_NN = glm(
  scratch ~ NN_dist * social_simple * group, 
  data = social_data, 
  family = binomial)
summary(model_social_NN)
# sig effect of NN distance on scratching when social vs not
# AF: lower scratching prob during social behavior and close proximity 
# and scratching increases during social behavior as NN distance increases (p = 0.029)
# NS for AM
# J: strongest distance effect, social line rises faster, suggesting
# they scratch especially often when social but neighbors are far away (p = 0.045)
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
