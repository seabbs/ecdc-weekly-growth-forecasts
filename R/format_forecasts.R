format_forecasts <- function(forecasts, forecast_date, type = "cases", point = FALSE, min_horizon = -3, max_horizon = 4) {
  type <- match.arg(type, c("cases", "hospitalizations", "deaths"))
  hub_label <- c(cases = "case", hospitalizations = "hosp", "death")

  forecasts <- forecast.vocs::fv_extract_forecast(
    posterior
  )[value_type == "cases"]
  cols <- grep("q[1-9]", colnames(forecasts), value = TRUE)
  cols <- c("location", "location_name", "date", "horizon", cols)
  forecasts <- forecasts[, ..cols]
  forecasts <- forecasts[, horizon := ceiling((date - forecast_date) / 7),
                         by = c("location")]
  forecasts <- forecasts[horizon >= min_horizon & horizon <= max_horizon]
  forecasts <- forecast.vocs::quantiles_to_long(forecasts)
  forecasts <- forecasts[,
    .(
      forecast_date = forecast_date,
      target = paste0(horizon, " wk ahead inc ", hub_label[type]),
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

adjust_to_max_pop <- function(forecasts, population) {
  population <- population[,c("location", "population")]
  forecasts <- merge(forecasts, population, by = "location")
  forecasts[value >= population, value := population - 1]
  forecasts[, population := NULL]
  return(forecasts[])
}

exclude_missing_forecasts <- function(forecasts) {
  miss_locations <- forecasts[type %in% "point"][is.na(value)]
  miss_locations <- unique(miss_locations[, location])
  if (length(miss_locations) > 0){
    warning("Forecasts are missing for the following locations: ", paste(miss_locations, collapse = ", "))
    forecasts <- forecasts[!(location %in% miss_locations)]
  }
  return(forecasts[])
}
