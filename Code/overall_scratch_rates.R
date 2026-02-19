library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

scratch_rates = scratch_data %>%
  group_by(group) %>%
  summarise(
    n_samples = n(),
    scratch_rate = mean(scratch, na.rm = TRUE))
# overall rate: 0.17
# AF: 0.15; AM: 0.21; J: 0.16

model_scratch = glm(scratch ~ group, data = scratch_data, family = binomial)
summary(model_scratch)
# AMs scratch significantly more than AFs (p = 0.0116)
# No significant difference between Js and AFs (p = 0.68)

emm_scratch = emmeans(model_scratch, pairwise ~ group, type = "response")
# AMs are significantly more likely to scratch than AFs (p = 0.0312)
# No difference between AFs and Js (p = 0.91)
# No significant difference between AMs and Js (p = 0.25)


emm_scratch = emmeans(model_scratch, ~ group, type = "response")

emm_scratch_df = as.data.frame(emm_scratch)


ggplot(emm_scratch_df, aes(x = group, y = prob, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_y_continuous(labels = scales::number_format(accuracy = .1), limits = c(0, 0.5)) +
  labs(
    x = NULL,
    y = "Predicted Probability of Scratching"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13, 
                                margin = margin(r = 10)),
    axis.text.x = element_text(size = 12)
  )
