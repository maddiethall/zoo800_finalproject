library(car)
library(DHARMa)
library(mgcv)
library(dplyr)
library(knitr)
library(kableExtra)

## Multicollinearity (VIF)
vif(model_social_simple)
vif(model_social_NN_simple)
vif(model_social_NN_number)
vif(model_NN_number)
vif(model_NN_dist)
# multicollinearity: variance inflation factors
# all values under 5, no problematic multicollinearity


vif_df = as.data.frame(vif(model_social_NN)) |>
  tibble::rownames_to_column("Predictor") |>
  rename(GVIF = GVIF, Df = Df, Adj_GVIF = `GVIF^(1/(2*Df))`)

kable(vif_df, digits = 3, caption = "Variance Inflation Factors for Model") %>%
  kable_classic(full_width = FALSE)


## Overdispersion
overdisp_fun = function(model) {
  rdf = df.residual(model)
  rp = residuals(model, type = "pearson")
  Pearson.chisq = sum(rp^2)
  disp = Pearson.chisq / rdf
  return(disp)
}

overdisp_fun(model_social_simple)
overdisp_fun(model_social_NN_simple)
overdisp_fun(model_social_NN_number)
overdisp_fun(model_NN_number)
overdisp_fun(model_NN_dist)
# all values near 1, no overdispersion

## Full Residual Diagnostics using DHARMa
simres = simulateResiduals(model_social_NN_number)
plot(simres)
par(mfrow = c(3,1))
testDispersion(simres)
testOutliers(simres)
testUniformity(simres)
# assumptions validated for all models

## Linearity of the Logit
gam_test = gam(scratch ~ s(NN_total, k = 4) + social_simple + group,
                family = binomial, data = scratch_data)
summary(gam_test)
# s(NN_total) is not significant, the relationship is sufficiently linear


## AIC Table
model_null  = glm(scratch ~ 1, family = binomial, data = scratch_data)

aic_df = AIC(model_social_simple, model_NN_number, model_NN_dist,
               model_social_NN_dist, model_social_NN_number, model_null) |>
  tibble::rownames_to_column("Model") |>
  rename(AIC = `AIC`) |>
  arrange(AIC) |>
  mutate(deltaAIC = AIC - min(AIC))

kable(aic_df, digits = 1, caption = "AIC Comparison of Candidate Models") %>%
  kable_classic(full_width = FALSE)
