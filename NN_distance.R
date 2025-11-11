library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

model_NN_dist = glm(scratch ~ NN_dist * group, data = scratch_data, family = binomial)
summary(model_NN_dist)

pred_NN_dist = ggpredict(model_NN_dist, terms = c("NN_dist", "group"))

ggplot(pred_NN_dist, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Nearest neighbor distance (m)",
    y = "Predicted probability of scratching",
    title = "Effect of nearest neighbor distance on scratching",
    subtitle = "Predicted from logistic regression with group interaction"
  ) +
  theme_minimal(base_size = 13)

ggplot(scratch_data, aes(x = NN_dist, y = scratch, color = group)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE) +
  labs(x = "Nearest neighbor distance (m)",
       y = "Probability of scratching",
       title = "Effect of NN distance on scratching")


scratch_by_bin = scratch_data %>%
  mutate(dist_bin = cut(NN_dist, breaks = c(0, 2, 5, 10),
                        labels = c("0-2m", "2-5m", "5-10m")))

scratch_by_bin = scratch_by_bin %>%
  group_by(group, dist_bin) %>%
  summarise(scratch_rate = mean(scratch, na.rm = TRUE))

ggplot(scratch_by_bin, aes(x = dist_bin, y = scratch_rate, fill = group)) +
  geom_col(position = "dodge") +
  labs(y = "Scratch rate", x = "NN distance bin",
       title = "Scratching rate across distance categories")

