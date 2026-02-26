library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(car)
library(tidyr)

scratch_data = readRDS("data_final.rds")

model_social_NN_number = glm(scratch ~ NN_total * social_simple * group, data = scratch_data, family = binomial)
summary(model_social_NN_number)

preds_social_NN_number = ggpredict(model_social_NN_number, terms = c("NN_total", "social_simple", "group"))
preds_NN_number = ggpredict(model_social_NN_number, terms = c("NN_total", "group"))
preds_social = ggpredict(model_social_NN_number, terms = c("social_simple", "group"))

baseline_eff <- ggpredict(
  model_social_NN_number,
  terms = c("group", "social_simple"),
  at = list(NN_total = 0)
)

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



heat = ggpredict(
  model_social_NN_number,
  terms = c("NN_total [all]", "social_simple", "group")
)

ggplot(heat, aes(x = x, y = group, fill = predicted)) +
  geom_tile() +
  facet_wrap(~ facet, nrow = 1) +
  scale_fill_gradientn(
    colors = c("#4C9A2A", "#D81B60", "#6A0DAD"),
    name = "p(scratch)"
  ) +
  labs(
    x = "Nearest Neighbor Count",
    y = "Social Context"
  ) +
  theme_bw(base_size = 13)


ggplot(preds_social_NN_number, aes (x = x, y = predicted, color = group)) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Predicted Probability of Scratching"
  ) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  facet_wrap(~facet) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Nonsocial" = "#0B6623", "Social" = "#6A0DAD"),
                     name = "Social Context") +
  scale_fill_manual(values = c("Nonsocial" = "#0B6623", "Social" = "#6A0DAD"),
                    name = "Social Context") +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  ) 






ggplot(preds_social_NN_number, aes (x = x, y = predicted, color = facet)) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Predicted Probability of Scratching"
  ) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = facet), alpha = 0.15, color = NA) +
  facet_wrap(~group) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = group_colors, name = "Age-Sex Class") +
  scale_fill_manual(values = group_colors, name = "Age-Sex Class") +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  ) 






ggplot(preds_NN_number, aes (x = x, y = predicted, color = group)) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Predicted Probability of Scratching"
  ) +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = group_colors, name = "Age-Sex Class") +
  scale_fill_manual(values = group_colors, name = "Age-Sex Class") +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  )


ggplot(model_social_NN_number, aes(x = NN_total, y = scratch, color = group)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "Number of Nearest Neighbors",
    y = "Probability of Scratching",
    color = "Age–Sex Class"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("#D81B60", "#5F6F52", "#6A0DAD")) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 13)
  )



emtrends(
  model_social_NN_number,
  ~ social_simple | group,
  var = "NN_total"
)

pairs(
  emtrends(model_social_NN_number,
           ~ social_simple | group,
           var = "NN_total"))


emtr = emtrends(
  model_social_NN_number,
  ~ social_simple * group,
  var = "NN_total",
  type = "response"
)

emtr_df = as.data.frame(emtr)

ggplot(emtr_df,
       aes(x = social_simple,
           y = NN_total.trend,
           color = social_simple)) +
  geom_point(position = position_dodge(width = 0.4),
             size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL,
                    ymax = asymp.UCL),
                width = 0.2,
                position = position_dodge(width = 0.4),
                size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black") +
  facet_wrap(~ group) +
  labs(
    y = "Change in Probability of Scratching\nper Additional Neighbor",
    color = "Social Context"
  ) +
  scale_color_manual(values = c(
    "Nonsocial" = "#0B6623",
    "Social" = "#6A0DAD"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10))
  )

