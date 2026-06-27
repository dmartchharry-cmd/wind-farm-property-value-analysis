# ============================================================
# 01_data_processing.R
# Load and prepare housing transaction data
# ============================================================

rm(list = ls())

# Libraries
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
library(GGally)

# Load data
HousingData <- read.csv(file.choose())

# Convert variables
HousingData$Date <- as.Date(HousingData$Date)

HousingData$Year <- as.numeric(HousingData$Year)
HousingData$Postcode <- as.factor(HousingData$Postcode)
HousingData$New.Build <- as.factor(HousingData$New.Build)

HousingData$Number.of.Turbines.in.View <-
  as.integer(HousingData$Number.of.Turbines.in.View)

HousingData$Can.View.Turbines. <-
  as.factor(HousingData$Can.View.Turbines.)

HousingData$Buffer.Group <-
  as.factor(HousingData$Buffer.Group)

HousingData <- HousingData %>%
  rename(
    ConstructionPhase =
      X2011.2014..1..2014.2017.2..2017.2020.3.
  )

HousingData$ConstructionPhase <-
  as.factor(HousingData$ConstructionPhase)

HousingData <- HousingData %>%
  filter(!is.na(Number.of.Turbines.in.View))

HousingData$PostConstruction <-
  ifelse(HousingData$Year >= 2014,1,0)

HousingData$PostConstruction <-
  as.factor(HousingData$PostConstruction)

HousingData$CanView01 <-
  ifelse(
    as.character(HousingData$Can.View.Turbines.) %in%
      c("1","Yes","YES","Y","True","TRUE"),
    1,
    0
  )

HousingData$ViewIntensity <-
  HousingData$Number.of.Turbines.in.View *
  HousingData$CanView01

# Exploratory pairs plot
HousingData %>%
  select(
    Log.Price,
    Price,
    Year,
    ViewIntensity,
    Number.of.Turbines.in.View
  ) %>%
  ggpairs()