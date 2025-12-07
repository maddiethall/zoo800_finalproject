library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("data_final.rds")


model_NN_number = glm(scratch ~ NN_total * group, data = scratch_data, family = binomial) 
summary(model_NN_number)
# total NN is sig for AF (p = 0.003) - negative correlation
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
  theme_minimal(base_size = 13) +
  group_scales() +
  scale_color_manual(values = group_colors, name = NULL) +
  scale_fill_manual(values = group_colors, name = NULL) +
  theme(
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10))
  ) 


Anova(model_NN_number, type = "III")
# NN total and group interaction is a sig predictor (p = 0.0002)


emtrends(model_NN_number, ~ group, var = "NN_total")
pairs(emtrends(model_NN_number, ~ group, var = "NN_total"))


coef_table_number <- tidy(model_NN_number) %>%
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

kable(coef_table_number,
      escape = FALSE,
      caption = "Coefficient Estimates for NN Number Logistic Regression Model",
      col.names = c("Term","Estimate","Std. Error","z","p")) %>%
  kable_classic(full_width = FALSE)
