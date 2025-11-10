library(dplyr)
library(ggplot2)
library(ggeffects)

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



### social behavior

social_data <- scratch_data %>%
  mutate(social_any = groom + social_rest + social_other)
table(social_data$social_any)

model_social <- glm(
  scratch ~ social_any * group,
  data = social_data,
  family = binomial
)

pred_social <- ggpredict(model_social, terms = c("social_any", "group"))

ggplot(pred_social, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Social behavior",
    y = "Predicted probability of scratching",
    title = "Effect of social behavior on scratching",
    subtitle = "Predicted from logistic regression with group interaction"
  ) +
  theme_minimal(base_size = 13)


### NN identity

NN_identity <- scratch_data %>%
  mutate(NN_age_sex = factor(NN_age_sex, levels = c("0", "J", "AF", "AM")))

model_identity <- glm(
  scratch ~ NN_age_sex * group,
  data = NN_identity,
  family = binomial
)
pred_identity <- ggpredict(model_identity, terms = c("NN_age_sex", "group"))
ggplot(pred_identity, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    x = "Nearest neighbor identity",
    y = "Predicted probability of scratching",
    title = "Effect of nearest neighbor identity on scratching"
  )

### social behavior

social_data <- scratch_data %>%
  mutate(social_any = groom + social_rest + social_other)
table(social_data$social_any)

model_social <- glm(
  scratch ~ social_any * group,
  data = social_data,
  family = binomial
)

pred_social <- ggpredict(model_social, terms = c("social_any", "group"))

ggplot(pred_social, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Social behavior",
    y = "Predicted probability of scratching",
    title = "Effect of social behavior on scratching",
    subtitle = "Predicted from logistic regression with group interaction"
  ) +
  theme_minimal(base_size = 13)


### NN identity

NN_identity <- scratch_data %>%
  mutate(NN_age_sex = factor(NN_age_sex, levels = c("0", "J", "AF", "AM")))

model_identity <- glm(
  scratch ~ NN_age_sex * group,
  data = NN_identity,
  family = binomial
)
pred_identity <- ggpredict(model_identity, terms = c("NN_age_sex", "group"))
ggplot(pred_identity, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    x = "Nearest neighbor identity",
    y = "Predicted probability of scratching",
    title = "Effect of nearest neighbor identity on scratching"
  )


### NN distance

model_NN <- glm(
  scratch ~ NN_dist * group,
  data = scratch_data,
  family = binomial
)

pred_NN <- ggpredict(model_NN, terms = c("NN_dist", "group"))

ggplot(pred_NN, aes(x = x, y = predicted, color = group, fill = group)) +
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


pred_dist <- ggpredict(model_dist2, terms = c("NN_dist", "group"))
plot(pred_dist) +
  labs(x = "Nearest neighbor distance (m)",
       y = "Predicted probability of scratching",
       title = "Scratching probability vs. NN distance")


# bin NN_dist into categories
scratch_by_bin <- scratch_data %>%
  mutate(dist_bin = cut(NN_dist, breaks = c(0, 2, 5, 10),
                        labels = c("0-2m", "2-5m", "5-10m")))

scratch_by_bin <- scratch_by_bin %>%
  group_by(group, dist_bin) %>%
  summarise(scratch_rate = mean(scratch, na.rm = TRUE))

ggplot(scratch_by_bin, aes(x = dist_bin, y = scratch_rate, fill = group)) +
  geom_col(position = "dodge") +
  labs(y = "Scratch rate", x = "NN distance bin",
       title = "Scratching rate across distance categories")

