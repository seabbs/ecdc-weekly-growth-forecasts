get_obs <- function(weeks = 12, type = "cases") {
  type <- match.arg(type, choices = c("cases", "hospitalizations", "deaths"))
  if (type == "cases") {
    url <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/ECDC/truncated_ECDC-Incident%20Cases.csv" # nolint
  } else if (type == "hospitalizations") {
    url <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/OWID/truncated_OWID-Incident%20Hospitalizations.csv" # nolint
  } else if (type == "deaths") {
    url <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/ECDC/truncated_ECDC-Incident%20Deaths.csv" # nolint
  }
  cases <- data.table::fread(url)
  # Format date
  cases[, date := as.Date(date)]
  setnames(cases, "value", "cases")
  # Order data by date and location
  data.table::setkey(cases, location_name, date)
  cases[, cases_available := date]
  cases[, seq_available := date]
  cases[, c("seq_total", "seq_voc", "share_voc") := 0]
  # Filter for the target number of weeks
  cases <- cases[date >= (max(date) - weeks * 7)]
  return(cases[])
}

get_population <- function() {
  pop <- data.table::fread("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") # nolint
  return(pop[])
}
