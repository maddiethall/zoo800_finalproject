library(dplyr)
library(ggplot2)
library(ggeffects)
library(emmeans)

scratch_data = readRDS("cleaned_combined_data.rds")

scratch_data = scratch_data %>%
  mutate(social_simple = groom + social_rest + social_other)
scratch_data$social_simple = ifelse(scratch_data$social_simple == 0, 0, 1)
scratch_data$social_simple = factor(scratch_data$social_simple, labels = c("Nonsocial", "Social"))

scratch_data = scratch_data %>%
  mutate(NN_age_sex = factor(NN_age_sex, levels = c("0", "J", "AF", "AM")))

scratch_data = scratch_data %>%
  mutate(NN_total = NN_within_2m + NN_2_to_5m)

scratch_data$group = factor(scratch_data$group, labels = c("AF", "AM", "J"))

saveRDS(scratch_data, "data_final.rds")


group_colors = c(
  "AF" = "#D81B60",
  "AM" = "#5F6F52",
  "J"  = "#6A0DAD"
)
group_scales = function() {
  list(
    scale_color_manual(values = group_colors),
    scale_fill_manual(values = group_colors)
  )
}






