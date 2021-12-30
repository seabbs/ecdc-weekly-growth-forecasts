get_obs <- function(weeks = 12) {
  cases <- data.table::fread("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") # nolint
  # Format date
  cases[, date := as.Date(date)]
  # Order data by date and location
  data.table::setkey(cases, location_name, date)
  # Summarise to weekly cases starting on Saturday to Sync with the
  # forecast hubs
  cases[, cases := data.table::frollsum(value, n = 7), by = c("location_name")]
  # Keep only Saturdays
  cases <- cases[weekdays(date) %in% "Saturday"]
  cases[, value := NULL]
  cases[, cases_available := date]
  cases[, seq_available := date]
  cases[, c("seq_total", "seq_voc", "share_voc") := 0]
  # Filter for the target number of weeks
  cases <- cases[date >= (max(date) - weeks * 7)]
  return(cases[])
}
