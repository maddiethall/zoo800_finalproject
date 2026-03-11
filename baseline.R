baseline_eff <- ggpredict(
  model_social_NN_number,
  terms = c("group", "social_simple"),
  at = list(NN_total = 0)
)

baseline_emm <- emmeans(
  model_social_NN_number,
  ~ group,
  at = list(
    NN_total = 0,
    social_simple = "Nonsocial"
  ),
  type = "response"
)
pairs(baseline_emm)

scratch_data$NN_avg = mean(scratch_data$NN_total)
scratch_data = scratch_data %>%
  group_by(group) %>%
  mutate(NN_avg = mean(NN_total))

group_means <- scratch_data %>%
  group_by(group) %>%
  summarise(mean_NN = mean(NN_total, na.rm = TRUE))

pred_grid <- expand.grid(
  group = group_means$group,
  social_simple = c("Nonsocial", "Social")
)

pred_grid <- pred_grid %>%
  left_join(group_means, by = "group")


pred <- predict(
  model_social_NN_number,
  newdata = pred_grid %>% rename(NN_total = mean_NN),
  type = "link",
  se.fit = TRUE
)

pred_grid$fit <- pred$fit
pred_grid$SE <- pred$se.fit

pred_grid <- pred_grid %>%
  mutate(
    prob = plogis(fit),
    lower = plogis(fit - 1.96 * SE),
    upper = plogis(fit + 1.96 * SE)
  )

scratch_data$pred_prob <- predict(
  model_social_NN_number,
  type = "response"
)

baseline <- emmeans(
  model_social_NN_number,
  ~ social_simple | group,
  at = list(NN_total = mean(scratch_data$NN_total)),
  type = "response"
)
ggplot(baseline_df,
       aes(x = group,
           y = prob,
           color = social_simple)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  labs(
    x = "Age–Sex Class",
    y = "Predicted Probability of Scratching",
    color = "Social Context"
  ) +
  theme_minimal(base_size = 13)
baseline_df <- as.data.frame(baseline)


ggplot(baseline_df,
       aes(x = group,
           y = prob,
           color = social_simple)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  labs(
    x = "Age–Sex Class",
    y = "Predicted Probability of Scratching",
    color = "Social Context"
  ) +
  theme_minimal(base_size = 13)


baseline <- emmeans(
  model_social_NN_number,
  ~ social_simple | group,
  at = list(NN_total = mean(scratch_data$NN_total)),
  type = "response"
)

baseline_data = scratch_data %>%

baseline_df <- as.data.frame(baseline)

baseline_data = scratch_data %>%
  group_by(group) %>%
  summarize(NN_avg = mean(NN_total, na.rm = TRUE))



scratch_data$pred_prob = predict(model_social_NN_number, type = "response")

ggplot(scratch_data, aes(x = group, y = pred_prob, fill = social_simple)) +
         geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +
         labs(
           x = "Age-Sex Class",
           y = "Predicted Scratching Probability",
           fill = "Social Context"
         ) +
         theme_minimal()

ggplot(baseline_eff_avg,
       aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.35),
    width = 0.2
  ) +
  labs(
    x = "Age–Sex Class",
    y = "Predicted Probability of Scratching",
    color = "Social Context"
  ) +
  theme_minimal(base_size = 13)



baseline_eff_avg <- ggpredict(
  model_social_NN_number,
  terms = c("group", "social_simple"),
  at = list(NN_total = 0)
)

mean_NN <- mean(scratch_data$NN_total, na.rm = TRUE)

ggplot(baseline_eff, aes(x = x, y = predicted, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(0.7), width = 0.15) +
  scale_fill_manual(
    values = c("Nonsocial" = "#5F6F52", "Social" = "#6A0DAD"),
    name = "Social Context") +
  labs(
    x = "Age–Sex Class",
    y = "Predicted Probability of Scratching"
  ) +
  theme_bw(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  ) 


ggplot(baseline_eff_avg, aes(x = x, y = predicted, fill = group)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(0.7), width = 0.15) +
  scale_fill_manual(
    values = c("Nonsocial" = "#5F6F52", "Social" = "#6A0DAD"),
    name = "Social Context") +
  labs(
    x = "Age–Sex Class",
    y = "Predicted Probability of Scratching"
  ) +
  theme_bw(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  ) 
