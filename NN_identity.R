library(dplyr)
library(ggplot2)
library(ggeffects)

NN_identity = scratch_data %>%
  mutate(NN_age_sex = factor(NN_age_sex, levels = c("0", "J", "AF", "AM")))

model_identity = glm(
  scratch ~ NN_age_sex * group,
  data = NN_identity,
  family = binomial
)
pred_identity = ggpredict(model_identity, terms = c("NN_age_sex", "group"))
ggplot(pred_identity, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    x = "Nearest neighbor identity",
    y = "Predicted probability of scratching",
    title = "Effect of nearest neighbor identity on scratching"
  )


