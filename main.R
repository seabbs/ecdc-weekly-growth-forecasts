# Packages
library(forecast.vocs)
library(data.table)
library(future)
library(future.apply)
library(future.callr)
library(here)
library(callr)
library(ggplot2)
library(lubridate)

# Create folders we need
if (!dir.exists(here("figures"))) {
  dir.create(here("figures"), recursive = TRUE)
}

unlink(here("data-processed", "epiforecasts-weeklygrowth"), recursive = TRUE)
dir.create(
  here("data-processed", "epiforecasts-weeklygrowth"), recursive = TRUE
)

forecast_date <- lubridate::floor_date(
  lubridate::today() + 2, unit = "week", week_start = 1
)

 # Load functions
source(here("R", "get_obs.R"))
source(here("R", "format_forecasts.R"))
source(here("R", "sample_growth_damped.R"))
source(here("R", "sample_incidence_damped.R"))
source(here("R", "damped_inits.R"))

forecasts <- list()
for (type in c("cases", "hospitalizations", "deaths")) {
  if (!dir.exists(here("logs", type))) {
    dir.create(here("logs", type), recursive = TRUE)
  }

  # Get the data
  cases <- get_obs(weeks = 64, type = type)

  # Set negative cases to last observed
  cases[, shifted_cases := shift(cases, fill = NA), by = "location"]
  cases[, cases := ifelse(cases < 0, shifted_cases, cases), by = "location"]
  cases[, shifted_cases := NULL]
  cases <- cases[!is.na(cases)]

  # Precompile the model
  mod <- fv_model(
    model = "models/incidence-damped.stan", strains = 1, verbose = TRUE
  )

  # Make forecasts
  fit <- forecast(cases[location %in% "AT"],
    fit = sample_incidence_damped,
    inits = damped_inits,
    strains = 1,
    overdispersion = TRUE,
    r_forecast = TRUE,
    r_step = 1,
    keep_fit = TRUE,
    horizon = 8,
    beta = c(0, 0.25),
    probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    adapt_delta = 0.99,
    max_treedepth = 15,
    save_warmup = FALSE,
    debug = FALSE
  )

  # Set up parallel forecasting
  # plan("callr", workers = 16)

  # # Make forecasts
  # fits <- future_lapply(
  #   split(cases, by = "location"),
  #   forecast,
  #   fit = sample_incidence_damped,
  #   strains = 1,
  #   overdispersion = TRUE,
  #   r_forecast = TRUE,
  #   r_step = 1,
  #   keep_fit = TRUE,
  #   horizon = 8,
  #   beta = c(0, 0.25),
  #   probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
  #   parallel_chains = 1,
  #   iter_warmup = 1000,
  #   iter_sampling = 1000,
  #   chains = 2,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   future.seed = TRUE
  # )

  # merge fits and add location names
  fits <- rbindlist(
    fits,
    use.names = TRUE, fill = TRUE, idcol = "location"
  )
  fits <- merge(
    fits, unique(cases[, .(location, location_name)]),
    by = "location"
  )
  # extract the forecast posterior
  posterior <- unnest_posterior(fits)

  # Plot the case forecasts
  plot_cases <- plot(posterior, type = "cases", log = FALSE) +
    facet_wrap(~location_name, scales = "free_y") +
    theme(legend.position = "none")

  ggsave(
    plot = plot_cases,
    filename = here::here("figures", paste0(type, ".png")),
    height = 36, width = 36, dpi = 300
  )

  plot_log_cases <- plot(posterior, type = "cases", log = TRUE) +
    facet_wrap(~location_name, scales = "free_y") +
    theme(legend.position = "none")

  ggsave(
    plot = plot_log_cases,
    filename = here::here("figures", paste0("log-", type, ".png")),
    height = 36, width = 36, dpi = 300
  )

  # Plot the forecast growth rate
  plot_growth <- plot(posterior, type = "growth") +
    facet_wrap(~location_name, scales = "free_y") +
    labs(fill = NULL)

  ggsave(
    plot = plot_growth,
    filename = here::here("figures", paste0(type, "-growth.png")),
    height = 36, width = 36, dpi = 300
  )

  # Format output for the hub
  forecasts[[type]] <- format_forecasts(
    posterior, forecast_date, type, point = TRUE
  )

  # Exclude forecasts that are missing and warn
  forecasts[[type]] <- exclude_missing_forecasts(forecasts[[type]])

  # Adjust forecasts greater than the population
  pop <- get_population()
  forecasts[[type]] <- adjust_to_max_pop(forecasts[[type]], pop)

  # save any forecasts that errored
  if (nrow(fits[is.null(fit)]) > 0) {
    fwrite(
      fits[is.null(fit)],
      here("logs", type, "failed-forecasts.csv")
    )
  }

  class(fits) <- c("fv_forecast", class(fits))
  fwrite(
    summary(fits, target = "diagnostics")[
      ,
      c("fit", "data", "fit_args", "voc_scale", "r_init", "error") := NULL
    ][],
    here("logs", type, "diagnostics.csv")
  )
}

# Save forecasts
fwrite(
  rbindlist(forecasts),
  here(
    "data-processed", "epiforecasts-weeklygrowth",
    paste0(forecast_date, "-epiforecasts-weeklygrowth.csv")
  )
)
