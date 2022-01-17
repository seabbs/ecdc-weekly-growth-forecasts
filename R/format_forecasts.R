format_forecasts <- function(forecasts, forecast_date, point = FALSE) {
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
      target = paste0(horizon, " wk ahead inc case"),
      target_end_date = date,
      location = location,
      type = "quantile",
      quantile,
      value = as.integer(prediction)
    )
  ]
  if (point) {
    point <- forecasts[quantile == 0.5]
    point <- point[, type := "point"][, quantile := NA]
    forecasts <- rbind(forecasts, point)
  }
  return(forecasts[])
}