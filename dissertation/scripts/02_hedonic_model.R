# ============================================================
# 02_hedonic_model.R
# Mixed-effects hedonic regression
# ============================================================

library(lme4)
library(lmerTest)
library(MuMIn)
library(car)
library(broom.mixed)

# Baseline
m0_poly3 <- lmer(
  Log.Price ~
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m0_poly3)
AIC(m0_poly3)
r.squaredGLMM(m0_poly3)

# Distance model
m1_poly3 <- lmer(
  Log.Price ~
    Buffer.Group +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m1_poly3)
AIC(m1_poly3)
r.squaredGLMM(m1_poly3)

# Preferred model
m2_poly3 <- lmer(
  Log.Price ~
    Buffer.Group +
    ViewIntensity +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m2_poly3)
AIC(m2_poly3)
r.squaredGLMM(m2_poly3)

# Post-construction model
m3_poly3 <- lmer(
  Log.Price ~
    Buffer.Group +
    ViewIntensity +
    PostConstruction +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m3_poly3)
AIC(m3_poly3)
r.squaredGLMM(m3_poly3)

# Full interaction model
m4_poly3 <- lmer(
  Log.Price ~
    Buffer.Group * PostConstruction +
    ViewIntensity * PostConstruction +
    New.Build +
    poly(Year,3) +
    (1|Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m4_poly3)
AIC(m4_poly3)
r.squaredGLMM(m4_poly3)

# Model comparison
anova(
  m0_poly3,
  m1_poly3,
  m2_poly3,
  m3_poly3,
  m4_poly3
)

# VIF
car::vif(m2_poly3)

# Tidy coefficients
tidy_m2 <-
  broom.mixed::tidy(
    m2_poly3,
    effects = "fixed",
    conf.int = TRUE
  )

tidy_m2$percent_effect <-
  (exp(tidy_m2$estimate)-1)*100

print(tidy_m2)