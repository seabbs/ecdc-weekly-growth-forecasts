#' Identify outliers in a time series using a rolling window approach and the
#' median absolute deviation (MAD).
#'
#' @param data A data table with a date and value column.
#' @param date_col The name of the date column in the data table.
#' @param value_col The name of the value column in the data table.
#' @param window_size The size of the rolling window used for outlier detection.
#' @param upper_threshold The upper threshold for outlier detection, as a
#' multiple of the median absolute deviation (MAD).
#' @param lower_threshold The lower threshold for outlier detection, as a
#' multiple of the MAD.
#' @param initial_window_outlier Whether to treat the first window as an
#' outlier. This is useful if outliers are likely in the first few days of a
#' time series.
#' @param difference_period The number of days to difference the time series
#' before outlier detection. This is useful if the time series has periodicity.
#' Note that outliers are detected on the differenced time series and by
#' by definition we assume an outlier cannot reoccur if the last differenced
#' value was an outlier.
#'
#' @return A data table with an additional outlier column, where 1 indicates
#' an outlier and 0 indicates a non-outlier.
#'
#' @importFrom data.table setkeyv frollapply
#' @export
#' @examples
#' # Load the necessary package
#' library(data.table)
#'
#' # Define a function to generate exponential growth
#' generate_exponential_growth <- function(start_value, growth_rate, n) {
#'   return(start_value * growth_rate ^ (0: (n - 1)))
#' }
#'
#' # Generate a data.table with exponential growth
#' set.seed(42)  # For reproducibility
#' n <- 30  # Number of days
#' start_value <- 100
#' growth_rate <- 0.96  # 2% daily growth
#' dt <- data.table(
#'   date = as.Date("2000-01-01") + 0:(n - 1),
#'   value = round(generate_exponential_growth(start_value, growth_rate, n))
#' )
#' dt[, true_outlier := 0]
#' # Introduce some common types of outliers
#' # Spurious zero
#' dt[date == "2000-01-15", `:=`(value = 0, true_outlier = 1)]
#' # Reporting spike
#' dt[date == "2000-01-20", `:=`(value = value * 10, true_outlier = 1)]
#'
#' # Introduce weekly periodicity by reducing the value by 60% every Sunday
#' dt[, weekday := weekdays(date)]
#' dt[weekdays(date) == "Sunday", value := round(value * 0.4)]
#'
#' # Define the identify_group_outliers function (as previously defined)
#'
#' # Run the function
#' results <- identify_outliers_with_mad(dt)
#'
#' # Print the results
#' print(results)
identify_outliers_with_mad <- function(
 data, date_col = "date", value_col = "value", window_size = 7,
 difference_period = 7, upper_threshold = 20, lower_threshold = 20,
 initial_window_outlier = TRUE) {

  data <- data.table::copy(data)

  # Set keys for data.table for rolling joins
  data.table::setkeyv(data, c(date_col))

  # Operate on the log scale
  data[, value := log(get(value_col) + 0.001)]
  if (difference_period > 0) {
    # Calculate the difference between the current and previous value
    data[,
     difference :=
      get(value_col) - shift(value, n = difference_period)
    ]
  }else {
    data[, difference := value]
  }
  data[, value := NULL]

  # Calculate rolling median
  data[,
   roll_median := data.table::frollapply(
      difference, window_size, median, na.rm = TRUE, align = "right"
    )
  ]

  # Calculate rolling MAD
  data[,
     roll_mad := data.table::frollapply(
      abs(difference - roll_median), window_size, median,
      na.rm = TRUE, align = "right"
    )
  ]

  # Calculate outlier thresholds
  data[, `:=`(upper_bound = roll_median + (upper_threshold * roll_mad),
              lower_bound = roll_median - (lower_threshold * roll_mad))]

  # Identify outliers
  data[, outlier := 0]
  data[
    difference > upper_bound | difference < lower_bound, outlier := 1
  ]

  # If the last difference was an outlier current time point is not an outlier
  if (difference_period > 0) {
    data[,
      outlier := ifelse(shift(outlier, n = difference_period) == 1, 0, outlier)
    ]
  }

  if (initial_window_outlier) {
    # Treat the first window as an outlier
    data[1:max(window_size, difference_period), outlier := 1]
  }

  return(data[])
}

identify_outliers_with_std <- function(
 data, date_col = "date", value_col = "value", window_size = 7,
 difference_period = 7, upper_threshold = 20, lower_threshold = 20,
 initial_window_outlier = TRUE, log_scale = TRUE) {

  data <- data.table::copy(data)

  # Set keys for data.table for rolling joins
  data.table::setkeyv(data, c(date_col))

  # Operate on the log scale
  if (log_scale) {
    data[, value := log(get(value_col) + 0.001)]
  } else {
    data[, value := get(value_col)]
  }
  if (difference_period > 0) {
    # Calculate the difference between the current and previous value
    data[,
     difference :=
      get(value_col) - shift(value, n = difference_period)
    ]
  }else {
    data[, difference := value]
  }
  data[, value := NULL]

  # Calculate rolling average
  data[,
   roll_avg := data.table::frollmean(
      shift(difference, 1), window_size, align = "right"
    )
  ]

  # Calculate rolling standard deviation
  data[,
     roll_std := data.table::frollapply(
      shift(difference, 1), window_size, sd,
      na.rm = TRUE, align = "right"
    )
  ]

  # Calculate outlier thresholds
  data[, `:=`(upper_bound = roll_avg + (upper_threshold * roll_std),
              lower_bound = roll_avg - (lower_threshold * roll_std))]

  # Identify outliers
  data[, outlier := 0]
  data[
    difference > upper_bound | difference < lower_bound, outlier := 1
  ]

  # If the last difference was an outlier current time point is not an outlier
  if (difference_period > 0) {
    data[,
      outlier := ifelse(shift(outlier, n = difference_period) == 1, 0, outlier)
    ]
  }

  if (initial_window_outlier) {
    # Treat the first window as an outlier
    data[1:max(window_size, difference_period), outlier := 1]
  }

  return(data[])
}
