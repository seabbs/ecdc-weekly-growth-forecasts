# COVID-19 cases forecasts for the ECDC Forecast Hub

A Bayesian autoregressive model using weekly incidence data designed to run as a Github action. Both cases and the growth rate are assumed to be AR(1) processes with the growth rate being differenced and scaled by a decay parameter. The model is implemented using the [forecast.vocs R package](https://epiforecasts.io/forecast.vocs).
