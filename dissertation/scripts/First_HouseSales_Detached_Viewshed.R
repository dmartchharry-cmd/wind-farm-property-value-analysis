# ============================================================

rm(list = ls())

# ---- Libraries ----
library(ggplot2)
library(ggfortify)
library(dplyr)
library(ggpubr)
library(forecast)
library(zoo)
library(visreg)
library(DHARMa)
library(lme4)
library(MuMIn)
library(GGally)
library(performance)
library(ggeffects)

library(performance)
library(car)
library(see)
library(lmerTest)

# ---- Load data ----
HousingData <- read.csv(file.choose())

# ---- Type conversions / cleaning ----
HousingData$Date <- as.Date(HousingData$Date)

HousingData$Year <- as.numeric(HousingData$Year)
HousingData$Postcode <- as.factor(HousingData$Postcode)
HousingData$New.Build <- as.factor(HousingData$New.Build)

HousingData$Number.of.Turbines.in.View <- as.integer(HousingData$Number.of.Turbines.in.View)
HousingData$Can.View.Turbines. <- as.factor(HousingData$Can.View.Turbines.)
HousingData$Buffer.Group <- as.factor(HousingData$Buffer.Group)

# Rename construction phase column
HousingData <- HousingData %>%
  rename(ConstructionPhase = X2011.2014..1..2014.2017.2..2017.2020.3.)

HousingData$ConstructionPhase <- as.factor(HousingData$ConstructionPhase)

# Remove NA results for view count
HousingData <- HousingData %>%
  filter(!is.na(Number.of.Turbines.in.View))

# ---- NEW: Create explicit Post-Construction indicator ----
# Change 2014 if your commissioning/construction milestone differs.
# This is key for isolating effects "after turbines exist".
HousingData$PostConstruction <- ifelse(HousingData$Year >= 2014, 1, 0)
HousingData$PostConstruction <- as.factor(HousingData$PostConstruction)

# ---- NEW: Reduce collinearity by building a single "view intensity" variable ----
# Convert Can.View.Turbines. into 0/1 (assumes levels include something like 0/1 or No/Yes).
# Adjust mapping if your factor levels differ.
# Quick safe mapping:
HousingData$CanView01 <- ifelse(as.character(HousingData$Can.View.Turbines.) %in% c("1", "Yes", "YES", "Y", "True", "TRUE"), 1, 0)

# View intensity = number of visible turbines, but forced to 0 if you can't see any.
HousingData$ViewIntensity <- HousingData$Number.of.Turbines.in.View * HousingData$CanView01

# ---- Quick sanity checks ----
str(HousingData)
summary(HousingData$ViewIntensity)

# ---- Exploratory: Pair plot (drop IDs; keep key vars) ----
HousingData %>%
  select(
    Log.Price,
    Price,
    Year,
    ViewIntensity,
    Number.of.Turbines.in.View
  ) %>%
  ggpairs()


# ============================================================
# MODEL BUILDING STRATEGY 
# 1) Baseline (controls only)
# 2) Add distance (Buffer.Group)
# 3) Add visibility (ViewIntensity)
# 4) Add PostConstruction
# 5) Add interactions (impact story)
# ============================================================

# ---- Baseline model (controls only) ----

m0_poly3 <- lmer(
  Log.Price ~ New.Build + poly(Year, 3) + (1 | Postcode),
  data = HousingData,
  REML = FALSE
)

summary(m0_poly3)
AIC(m0_poly3)
r.squaredGLMM(m0_poly3)

# ---- Add distance only ----
m1_poly3 <- lmer(
  Log.Price ~ Buffer.Group + New.Build + poly(Year, 3) + (1 | Postcode),
  data = HousingData,
  REML = FALSE
)
summary(m1_poly3)
AIC(m1_poly3)
r.squaredGLMM(m1_poly3)
# ---- Add view exposure (intensity) ----

m2_poly3 <- lmer(
  Log.Price ~ Buffer.Group + ViewIntensity + New.Build + poly(Year, 3) + (1 | Postcode),
  data = HousingData,
  REML = FALSE
)
summary(m2_poly3)
AIC(m2_poly3)
r.squaredGLMM(m2_poly3)

# ---- Add PostConstruction (level shift) ----
m3_poly3 <- lmer(
  Log.Price ~ Buffer.Group + ViewIntensity + PostConstruction +
    New.Build + poly(Year, 3) + (1 | Postcode),
  data = HousingData,
  REML = FALSE
)
summary(m3_poly3)
AIC(m3_poly3)
r.squaredGLMM(m3_poly3)

# ---- Full “impact” model with interactions ----
# This is the key step: do proximity/visibility effects change after construction?
m4_poly3 <- lmer(
  Log.Price ~
    Buffer.Group * PostConstruction +
    ViewIntensity * PostConstruction +
    New.Build +
    poly(Year, 3) +
    (1 | Postcode),
  data = HousingData,
  REML = FALSE
)
summary(m4_poly3)
AIC(m4_poly3)
r.squaredGLMM(m4_poly3)

# ---- Compare models cleanly ----
anova(m0_poly3, m1_poly3, m2_poly3, m3_poly3, m4_poly3)

car::vif(m2_poly3)

# ============================================================
# DIAGNOSTICS (DHARMa + general checks)
# ============================================================

check_model(m2_poly3)  
# overall diagnostics


check_collinearity(m2_poly3)      # multicollinearity (VIF-like)

# DHARMa residual simulation
sim_m2 <- simulateResiduals(m2_poly3, plot = TRUE)

# Extra quick checks
performance::check_normality(m2_poly3)
performance::check_heteroscedasticity(m2_poly3)


# ============================================================
# ============================================================

# Coefficient table (tidy)
tidy_m2 <- broom.mixed::tidy(m2_poly3, effects = "fixed", conf.int = TRUE)
print(tidy_m2)

# Convert key coefficients into approximate % changes (log-linear interpretation)
# % change ≈ (exp(beta) - 1) * 100
tidy_m2$percent_effect <- (exp(tidy_m2$estimate) - 1) * 100
print(tidy_m2)

# ============================================================
# ============================================================

# Predict population-level log prices (no postcode random effects)
library(dplyr)
library(tidyr)
library(ggplot2)

ggplot(plot_area, aes(x = Year)) +
  
  # observed values
  geom_errorbar(aes(ymin = lo_obs, ymax = hi_obs),
                width = 0.15, linewidth = 0.6, colour = "black") +
  geom_point(aes(y = mean_obs),
             shape = 16, size = 2.6, colour = "black") +
  geom_line(aes(y = mean_obs),
            linewidth = 0.75, colour = "black") +
  
  # predicted values
  geom_errorbar(aes(ymin = lo_pred, ymax = hi_pred),
                width = 0.15, linewidth = 0.6,
                linetype = "dashed", colour = "black") +
  geom_point(aes(y = mean_pred),
             size = 1.5, colour = "black") +
  geom_line(aes(y = mean_pred),
            linewidth = 0.7, linetype = "dashed", colour = "black") +
  
  # construction dates
  geom_vline(xintercept = 2014, linetype = "dotted", linewidth = 0.6) +
  geom_vline(xintercept = 2017, linetype = "dotted", linewidth = 0.6) +
  
  scale_x_continuous(breaks = 2011:2020) +
  labs(
    x = "Year of sale",
    y = "Log house price"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(colour = "black", linewidth = 1.4),
    axis.ticks = element_line(colour = "black", linewidth = 1.1),
    axis.text  = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.position = "none"
  )







HousingData$Buffer.Group <- relevel(HousingData$Buffer.Group, ref = "4")

test_ref <- lmer(Log.Price ~ Buffer.Group +
                   (1|Postcode) + ViewIntensity + New.Build + poly(Year,3),
                 data = HousingData)

summary(test_ref)

Housing_sens <- HousingData %>%
  filter(Buffer.Group != "1")

Housing_sens$Buffer.Group <- droplevels(Housing_sens$Buffer.Group)

test_sens <- lmer(Log.Price ~ Buffer.Group +
                    (1|Postcode) + ViewIntensity + New.Build + poly(Year,3),
                  data = Housing_sens)

summary(test_sens)

tidy_m2 <- broom.mixed::tidy(test_ref, effects = "fixed", conf.int = TRUE)
print(tidy_m2)

# Convert key coefficients into approximate % changes (log-linear interpretation)
# % change ≈ (exp(beta) - 1) * 100
tidy_m2$percent_effect <- (exp(tidy_m2$estimate) - 1) * 100
print(tidy_m2)
