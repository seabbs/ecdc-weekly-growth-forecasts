format_forecasts <- function(forecasts, forecast_date) {
  forecasts <- forecast.vocs::fv_extract_forecast(
    posterior
  )[value_type == "cases"]
  cols <- grep("q[1-9]", colnames(forecasts), value = TRUE)
  cols <- c("location", "location_name", "date", "horizon", cols)
  forecasts <- forecasts[, ..cols]
  forecasts <- forecasts[, horizon := 1:.N, by = c("location")]
  forecasts <- forecast.vocs::quantiles_to_long(forecasts)
  forecasts <- forecasts[,
    .(
      forecast_date = forecast_date,
      target = paste0(horizon, " wk ahead inc cases"),
      target_end_date = date,
      location = location,
      type = "quantile",
      quantile,
      value = prediction
    )
  ]
  return(forecasts[])
}