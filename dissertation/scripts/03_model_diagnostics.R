# ============================================================
# 03_model_diagnostics.R
# Diagnostic checks
# ============================================================

library(performance)
library(DHARMa)

check_model(m2_poly3)

check_collinearity(m2_poly3)

sim_m2 <-
  simulateResiduals(
    m2_poly3,
    plot = TRUE
  )

check_normality(m2_poly3)

check_heteroscedasticity(m2_poly3)

# Sensitivity analysis

HousingData$Buffer.Group <-
  relevel(HousingData$Buffer.Group,
          ref = "4")

test_ref <- lmer(
  Log.Price ~
    Buffer.Group +
    ViewIntensity +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData
)

summary(test_ref)

Housing_sens <-
  HousingData %>%
  filter(Buffer.Group != "1")

Housing_sens$Buffer.Group <-
  droplevels(Housing_sens$Buffer.Group)

test_sens <- lmer(
  Log.Price ~
    Buffer.Group +
    ViewIntensity +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = Housing_sens
)

summary(test_sens)