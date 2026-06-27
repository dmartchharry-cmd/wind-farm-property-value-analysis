# Evaluating the Impact of Onshore Wind Farms on House Prices

## Hedonic Case Study: Goole Fields Wind Farm, East Riding of Yorkshire

## Overview

This project investigates whether onshore wind farm development is capitalised into residential property prices.

Using a hedonic pricing framework, the study evaluates whether exposure to the Goole Fields Wind Farm influences detached house prices through:

* spatial proximity
* turbine visibility
* construction timing
* neighbourhood-level variation

The analysis integrates GIS-based exposure modelling with mixed-effects regression.

---

## Research Question

Does proximity and visual exposure to the Goole Fields Wind Farm influence local house prices?

---

## Objectives

The project aims to:

* Quantify spatial price effects using distance-based buffer zones
* Assess whether turbine visibility independently affects property values
* Evaluate changes across pre- and post-construction periods
* Account for postcode-level housing market variation

---

# Methodology

## Spatial Analysis

GIS analysis was conducted to create:

* Wind farm exposure zones
* Distance buffers (1km–14km)
* DSM-based turbine visibility modelling

Tools:

* ArcGIS Pro
* QGIS
* Spatial analysis

---

## Hedonic Pricing Model

A mixed-effects hedonic regression model was developed:

Dependent variable:

* Log-transformed house price

Exposure variables:

* Distance from wind farm
* Turbine visibility
* Construction period

Control variables:

* Property characteristics
* Temporal trends
* Postcode-level effects

Model selection was performed using Akaike Information Criterion (AIC).

---

# Key Findings

* Spatial proximity was the strongest exposure predictor
* Properties within 2–4km showed an estimated 5.43% lower price compared with baseline properties
* Turbine visibility was not statistically significant after accounting for neighbourhood effects
* No major structural housing market change was identified during construction phases

---

# Tools & Skills Demonstrated

Programming:

* R
* Statistical modelling
* Data processing

Spatial:

* GIS
* Viewshed analysis
* Raster processing
* Spatial exposure modelling

Methods:

* Hedonic pricing
* Mixed-effects modelling
* Environmental valuation

---

# Repository Structure

```
data/        Data inputs and processed datasets
GIS/         Spatial analysis files
scripts/     R analysis workflow
figures/     Outputs and visualisations
results/     Statistical outputs
```

---

## Author

Dylan Martch-Harry
BSc Environmental Science
University of Southampton
