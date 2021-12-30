# Packages
library(forecast.vocs)
library(data.table)
library(future)
library(future.callr)
library(here)
library(callr)
library(ggplot2)

# Create folders we need
if (!dir.exists(here("figures"))) {
  dir.create(here("figures"), recursive = TRUE)
}
unlink(here("data-processed", "epiforecasts-weeklygrowth"), recursive = TRUE)
dir.create(
  here("data-processed", "epiforecasts-weeklygrowth"), recursive = TRUE
)

# Load functions
source(here("R", "get_obs.R"))
source(here("R", "format_forecasts.R"))

# Get the data
cases <- get_obs(weeks = 16)

# Precompile the model
mod <- fv_model(strains = 1)

# Set up parallel forecasting
plan("callr", workers = 2)

# Make forecasts
fits <- future.apply::future_lapply(
  split(cases, by = "location"),
  forecast,
  strains = 1,
  overdispersion = TRUE,
  r_forecast = TRUE,
  r_step = 1,
  keep_fit = TRUE,
  horizon = 2,
  beta = c(-0.5, 0.25),
  probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
  parallel_chains = 1,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 2,
  adapt_delta = 0.95,
  max_treedepth = 15,
  future.seed = TRUE,
  refresh = 0,
  show_messages = TRUE
)

# merge fits and add location names
fits <- rbindlist(
  fits, use.names = TRUE, fill = TRUE, idcol = "location"
)
fits <- merge(
  fits, unique(cases[, .(location, location_name)]), by = "location"
)
# extract the forecast posterior
posterior <- unnest_posterior(fits)

# Plot the case forecasts
plot_cases <- plot(posterior, type = "cases", log = FALSE) +
  facet_wrap(~location_name, scales = "free_y") +
  theme(legend.position = "none")

ggsave(plot = plot_cases,
       filename = here::here("figures", "cases.png"),
       height = 16, width = 16, dpi = 300
)

# Plot the forecast growth rate
plot_growth <- plot(posterior, type = "growth") +
  facet_wrap(~location_name, scales = "free_y") +
  labs(fill = NULL)

ggsave(plot = plot_growth,
       filename = here::here("figures", "growth.png"),
       height = 16, width = 16, dpi = 300
)

# Format output for the hub
forecast_date <- max(cases$date) + 1
forecasts <- format_forecasts(posterior, forecast_date)

# Save forecasts
fwrite(
  forecasts,
  here("data-processed", "epiforecasts-weeklygrowth",
       paste0(forecast_date, "-epiforecasts-weeklygrowth.csv")
  )
)
