library(visreg)
library(emmeans)
library(ggplot2)

# ------------------------------------
# Partial effect of visibility
# ------------------------------------

visreg(
  m2_poly3,
  "ViewIntensity",
  gg = TRUE,
  xlab = "Number of visible turbines",
  ylab = "Log house price"
)

# ------------------------------------
# Distance figure
# ------------------------------------

emm_buffer <-
  emmeans(
    m2_poly3,
    ~ Buffer.Group
  )

emm_df <- as.data.frame(emm_buffer)

ggplot(
  emm_df,
  aes(
    Buffer.Group,
    emmean
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.15
  ) +
  labs(
    x = "Distance buffer from wind farm",
    y = "Adjusted log house price"
  ) +
  scale_x_discrete(
    labels = c(
      "1–2 km",
      "2–4 km",
      "4–8 km",
      "8–14 km"
    )
  ) +
  theme_minimal()

# ------------------------------------
# Turbine visibility figure
# ------------------------------------

vals <-
  sort(
    unique(HousingData$ViewIntensity)
  )

emm_turb <-
  emmeans(
    m2_poly3,
    ~ ViewIntensity,
    at = list(
      ViewIntensity = vals
    )
  )

emm_df <- as.data.frame(emm_turb)

ggplot(
  emm_df,
  aes(
    ViewIntensity,
    emmean
  )
) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.2
  ) +
  labs(
    x = "Number of visible turbines",
    y = "Adjusted log house price"
  ) +
  theme_classic()

# ------------------------------------
# Observed vs predicted
# ------------------------------------

ggplot(plot_area, aes(x = Year)) +
  
  geom_errorbar(
    aes(ymin = lo_obs,
        ymax = hi_obs),
    width = 0.15
  ) +
  
  geom_point(
    aes(y = mean_obs),
    size = 2.5
  ) +
  
  geom_line(
    aes(y = mean_obs)
  ) +
  
  geom_errorbar(
    aes(
      ymin = lo_pred,
      ymax = hi_pred
    ),
    width = 0.15,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = mean_pred),
    size = 1.5
  ) +
  
  geom_line(
    aes(y = mean_pred),
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = 2014,
    linetype = "dotted"
  ) +
  
  geom_vline(
    xintercept = 2017,
    linetype = "dotted"
  ) +
  
  labs(
    x = "Year of sale",
    y = "Log house price"
  ) +
  
  theme_classic()