library(dplyr)
library(ggplot2)
library(ggeffects)


social_data = scratch_data %>%
  mutate(social_any = groom + social_rest + social_other)
table(social_data$social_any)


scratch_rates_social = social_data %>%
  group_by(group, social_any) %>%
  summarise(
    n_samples = n(),
    n_scratches = sum(scratch, na.rm = TRUE),
    scratch_rate = mean(scratch, na.rm = TRUE)
  )

ggplot(scratch_rates_social, 
       aes(x = social_any, y = scratch_rate, fill = group)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("Nonsocial", "1 Social Behavior", "2 Social Behaviors")) +
  labs(x = "Behavior context", y = "Scratch rate (proportion of samples)",
       fill = "Group") +
  theme_minimal()


social_data$social_any = factor(social_data$social_any)

model_social = glm(scratch ~ social_any * group, 
                      family = binomial, data = social_data)
summary(model_social)
# AF: scratching significantly decreases with 1 social behavior (p = 0.007) and more with 2 (p = 0.035)
# AM: scratch less with social behavior but NS, also very few data points with social behavior
# J: scratch sig more with no social behavior vs AF (p = 0.001), strong drop in scratching with 1 social behavior (p = 0.024)
# J: not enough data for 2 social behaviors
### Scratching probability decreases as individuals become more socially engaged — both for 1 and 2 social behaviors.
### Juveniles scratch more overall when alone, But their scratching drops even more sharply when social.
### Adult males don’t differ significantly from females.

emm_social = emmeans(model_social, ~ social_any | group, type = "response")


social_data$social_simple = ifelse(social_data$social_any == 0, 0, 1)
social_data$social_simple = factor(social_data$social_simple, labels = c("Nonsocial", "Social"))

model_social_simple = glm(scratch ~ social_simple * group,
                           family = binomial, data = social_data)
summary(model_social_simple)
# AF: scratching probability drops significantly in social contexts (p ≈ 0.0008)
# AM: AMs’ reduction in scratching when social is slightly less than AFs, but not significant
# JL|: Juveniles’ reduction in scratching when social is even stronger than AFs (p ≈ 0.012)

emm_simple = emmeans(model_social_simple, ~ social_simple | group, type = "response")
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



within_group_social = contrast(emm_simple, method = "pairwise", by = "group", adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(sig = ifelse(p.value < 0.05, "*", "")) %>%
  select(group, contrast, sig)
# Juveniles scratch much more when nonsocial; huge drop in social context (p < 0.0001)
# AFs scratch significantly more when nonsocial than when social (p = 0.0008)

