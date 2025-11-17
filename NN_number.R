library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

NN_data = social_data %>%
  select(group, scratch, NN_within_2m, NN_2_to_5m, social_simple) %>%
  mutate(NN_total = NN_within_2m + NN_2_to_5m)

model_NN_number = glm(scratch ~ NN_total * group, data = NN_data, family = binomial) 
summary(model_NN_number)
# total NN is sig for AF (p = 0.003) and J (p = 0.002) - negative correlation
# NS for adult male but same pattern
# interaction effect for J (p = 0.008) -  steeper negative correlation

pred_NN_number = ggpredict(model_NN_number, terms = c("NN_total", "group"))

ggplot(pred_NN_number, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Predicted probability of scratching"
  ) +
  theme_minimal(base_size = 13)


