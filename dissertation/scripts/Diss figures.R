library(visreg)

visreg(m2_poly3, "ViewIntensity",
       gg = TRUE,
       xlab = "Number of visible turbines",
       ylab = "Log house price",
       main = "Partial effect of turbine visibility on house prices")

library(emmeans)

emm_buffer <- emmeans(m2_poly3, ~ Buffer.Group)
emm_buffer

emm_df <- as.data.frame(emm_buffer)
emm_df

ggplot(emm_df, aes(x = Buffer.Group, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  labs(
    x = "Distance buffer from wind farm",
    y = "Adjusted log house price",
    title = "Model-adjusted house prices by distance from the Goole Fields wind farm"
  ) +
  scale_x_discrete(labels = c("1–2 km", "2–4 km", "4–8 km", "8–14 km")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_blank()
  )

HousingData$TurbineCount_f <- factor(
  HousingData$Number.of.Turbines.in.View,
  levels = sort(unique(HousingData$Number.of.Turbines.in.View))
)
HousingData$TurbineCount_f <- ordered(HousingData$Number.of.Turbines.in.View)

library(emmeans)

library(emmeans)

turbineemm <- emmeans(m2_poly3, ~ ViewIntensity)
turbineemm

turb_df <- as.data.frame(turbineemm)
turb_df

ggplot(turb_df, aes(x = ViewIntensity, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15)
    
library(emmeans)

vals <- sort(unique(HousingData$ViewIntensity))

emm_turb <- emmeans(
  m2_poly3,
  ~ ViewIntensity,
  at = list(ViewIntensity = vals)
)

summary(emm_turb)
emm_df <- as.data.frame(emm_turb)
summary(emm_df)

# Manual t-ratio of each level vs baseline (level 0)
baseline <- emm_df$emmean[1]
SE <- emm_df$SE

t_ratio <- (emm_df$emmean - baseline) / SE
p_value <- 2 * (1 - pnorm(abs(t_ratio)))  # normal approximation since df=Inf

data.frame(
  ViewIntensity = emm_df$ViewIntensity,
  emmean = emm_df$emmean,
  SE = SE,
  t_ratio = t_ratio,
  p_value = p_value
)
ggplot(emm_df, aes(x = ViewIntensity, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Number of visible turbines", y = "Adjusted log house price") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_blank())




HousingData$TurbineCount_f <- factor(
  HousingData$Number.of.Turbines.in.View,
  levels = sort(unique(HousingData$Number.of.Turbines.in.View))
)
HousingData$TurbineCount_f <- ordered(HousingData$Number.of.Turbines.in.View)

