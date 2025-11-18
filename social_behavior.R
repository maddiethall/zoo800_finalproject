library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("data_final.rds")

model_social_simple = glm(scratch ~ social_simple * group, family = binomial, data = scratch_data)
summary(model_social_simple)
# AF: scratching probability drops significantly in social contexts (p ≈ 0.0008)
# AM: AMs’ reduction in scratching when social is slightly less than AFs, but not significant
# J: Juveniles’ reduction in scratching when social is even stronger than AFs (p ≈ 0.012)
# overall negative effect of social context on scratching (p = 0.0001)


emm_simple = emmeans(model_social_simple, ~ social_simple | group, type = "response")
summary(emm_simple)
emm_simple_df = as.data.frame(emm_simple)
emm_simple_df = emm_simple_df %>%
  mutate(sig_label = case_when(
    group == "AF" & social_simple == "Social" ~ "*",
    group == "J"  & social_simple == "Social" ~ "*",
    TRUE ~ ""
  ))

ggplot(emm_simple_df, aes(x = social_simple, y = prob, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sig_label, y = prob + 0.05),
            position = position_dodge(width = 0.8), size = 5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(x = "Social context",
       y = "Predicted scratch probability",
       fill = "Group",
       title = "Scratch probability by social context and group",
       subtitle = "* indicates significant drop from nonsocial to social") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")



within_group_social = contrast(emm_simple, method = "pairwise", by = "group", adjust = "tukey")
summary(within_group_social)
# Juveniles scratch much more when nonsocial; huge drop in social context (p < 0.0001)
# AFs scratch significantly more when nonsocial than when social (p = 0.0008)

Anova(model_social_simple, type = "III")
# social behavior is a significant predictor (p = 0.003)
# group is significant (p = 0.002)
# interaction is sig (p = 0.002)