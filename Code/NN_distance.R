library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(car)

scratch_data = readRDS("data_final.rds")


model_NN_dist = glm(scratch ~ NN_dist * group, data = scratch_data, family = binomial)
summary(model_NN_dist)
# AF: scratching increases modestly as neighbors are farther away (p ≈ 0.004)
# AM: effect of distance is NS
# J: as neighbors move away, scratching rises sharply (p = 0.0003)

pred_NN_dist = ggpredict(model_NN_dist, terms = c("NN_dist", "group"))

ggplot(pred_NN_dist, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Predicted Probability of Scratching"
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

emtrends(model_NN_dist, ~ group, var = "NN_dist")
pairs(emtrends(model_NN_dist, ~ group, var = "NN_dist"))


Anova(model_NN_dist, type = "III")
# distance is a significant predictor of scratching (p = 0.003)
# group is a significant predictor of scratching (p = 0.006)
# distance and group are significant (p < 0.001)



coef_table_dist <- tidy(model_NN_dist) %>%
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

kable(coef_table_dist,
      escape = FALSE,
      caption = "Coefficient Estimates for NN Distance Logistic Regression Model",
      col.names = c("Term","Estimate","Std. Error","z","p")) %>%
  kable_classic(full_width = FALSE)
