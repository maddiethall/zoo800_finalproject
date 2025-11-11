library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

social_data = scratch_data %>%
  mutate(social_any = groom + social_rest + social_other)

scratch_rates_social = social_data %>%
  group_by(group, social_any) %>%
  summarise(
    n_samples = n(),
    n_scratches = sum(scratch, na.rm = TRUE),
    scratch_rate = mean(scratch, na.rm = TRUE)
  )

social_data$social_any = factor(social_data$social_any)

social_data$social_simple = ifelse(social_data$social_any == 0, 0, 1)
social_data$social_simple = factor(social_data$social_simple, labels = c("Nonsocial", "Social"))

model_social_simple = glm(scratch ~ social_simple * group, family = binomial, data = social_data)
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

 