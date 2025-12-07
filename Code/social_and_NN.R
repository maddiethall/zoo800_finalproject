library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(car)

scratch_data = readRDS("data_final.rds")

model_social_NN_simple = glm(scratch ~ NN_dist * social_simple, data = scratch_data, family = binomial)
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


model_social_NN_dist = glm(scratch ~ NN_dist * social_simple * group, data = scratch_data, family = binomial)
summary(model_social_NN_dist)
# AF: sig effect of NN distance on scratching when social vs not
# lower scratching prob during social behavior and close proximity 
# and scratching increases during social behavior as NN distance increases (p = 0.029)
# NS for AM or J
# J: strongest distance effect, social line rises faster
# interaction between NN distance and social context across groups is NS

Anova(model_social_NN_dist, type = "III")
# between groups is NS

preds_social_NN_dist = ggpredict(model_social_NN_dist, terms = c("NN_dist [all]", "social_simple", "group"))

ggplot(preds_social_NN_dist, aes(x = x, y = predicted, color = group)) +
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

model_social_NN_number = glm(scratch ~ NN_total * social_simple * group, data = scratch_data, family = binomial)
summary(model_social_NN_number)
# AF: significant interaction effect of NN_total and social_simple (p = 0.01; scratching increases with
## NN_number when nonsocial, decreases when social)
# AM: also significant interaction (p = 0.02; scratching increases with NN_number when social,
## and decreases with NN_number when nonsocial)
# J: also significant (p = 0.01; lower baseline with social behavior vs not, in the nonsocial
## part, scratch decreases with NN_number)

preds_social_NN_number = ggpredict(model_social_NN_number, terms = c("NN_total", "social_simple", "group"))


ggplot(preds_social_NN_number, aes (x = x, y = predicted, color = facet)) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Predicted Probability of Scratching"
  ) +
  theme_minimal(base_size = 13) +
  group_scales() +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = facet), alpha = 0.15, color = NA) +
  facet_wrap(~group) +
  scale_color_manual(values = group_colors, name = NULL) +
  scale_fill_manual(values = group_colors, name = NULL) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  ) 
  


library(broom)
library(dplyr)
library(kableExtra)

coef_table_social_number <- tidy(model_social_NN_number) %>%
  mutate(
    signif = p.value < 0.05,
    term = ifelse(signif, paste0(term, "**"), term),
    estimate = ifelse(signif, paste0(round(estimate,3)), round(estimate,3)),
    std.error = ifelse(signif, paste0(round(std.error,3)), round(std.error,3)),
    statistic = ifelse(signif, paste0(round(statistic,3)), round(statistic,3)),
    p.value = ifelse(signif,
                     ifelse(p.value < .001, "<0.001***",
                            paste0(round(p.value,3), "**")),
                     round(p.value,3))
  ) %>%
  select(term, estimate, std.error, statistic, p.value)  # <-- remove 'signif'

kable(coef_table_social_number,
      escape = FALSE,
      caption = "Coefficient Estimates for Best-Fitting Logistic Regression Model",
      col.names = c("Term","Estimate","Std. Error","z","p")) %>%
  kable_classic(full_width = FALSE)




Anova(model_social_NN_number, type = "III")
# sig predictor: total NN, social behavior, and group (p = 0.01)
# sig predictor: total NN and social behavior (p = 0.005)

(emtrends(model_social_NN_number, ~ social_simple * group, var = "NN_total"))
