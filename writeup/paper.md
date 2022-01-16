
## Abstract

**Background:**

**Methods:**

**Results:**

**Conclusions:**


## Introduction

**Aim:** Evaluate the performance of a simplified weekly AR(1) forecasting model with and without assumed decay to zero growth.

## Methods

### Data 

- ECDC weekly reported COVID-19 cases

### Models

- AR(1) differenced growth rate model
- AR(1) differenced growth rate model with decay

### Statistical inference

- AR(1) differenced growth rate model with decay used in real-time to produce submissions to the ECDC forecasting hub.
- AR(1) differenced growth rate model fit to retrospectively available data.

### Evaluation

-  We compare our forecasts to those from the ECDC hub median ensemble of all submitted models.
- We first visualise forecasts at the 4 week horizon for several example countries. 
- We evaluate and compare the performance of each forecasting model using proper scoring rules [@scoringutils] on both the natural and the log scale to observed data reported at least 28 days ago across all ECDC nations with reported data. This corresponds to evaluating absolute and relative error. We explore overall scores as well as scores stratified by horizon, by date of forecast, by report date, and by nation. 

### Implementation

- `cmdstanr` and `stan` 
- `forecast.vocs`
- `scoringutils`

## Results 

### Forecasts compared to data

### Overall

### By forecast horizon

### By date of forecast

### By report date

### By location

## Discussion

### Summary

### Strengths and weaknesses

**Limitations:**

- The AR(1) differenced growth rate model was fit to retrospectively available data and so performance may not reflect real-time performance.

### Further work

### Conclusions

## References