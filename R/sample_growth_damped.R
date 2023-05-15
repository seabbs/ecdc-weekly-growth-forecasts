sample_growth_damped <- function(
    data, model = forecast.vocs::fv_model(strains = 2),
    diagnostics = TRUE, ...) {
  cdata <- data
  cdata$start_date <- NULL
  cdata$seq_start_date <- NULL
  custom_model <- fv_model(
    model = "models/growth-damped.stan", strains = 1
  )
  fit <- custom_model$sample(data = cdata, ...)
  out <- data.table(fit = list(fit), data = list(data), fit_args = list(list(...)))
  if (diagnostics) {
    diag <- fit$sampler_diagnostics(format = "df")
    diagnostics <- data.table(
      samples = nrow(diag), max_rhat = round(max(
        fit$summary(
          variables = NULL,
          posterior::rhat, .args = list(na.rm = TRUE)
        )$`posterior::rhat`,
        na.rm = TRUE
      ), 2), divergent_transitions = sum(diag$divergent__),
      per_divergent_transitions = sum(diag$divergent__) / nrow(diag),
      max_treedepth = max(diag$treedepth__)
    )
    diagnostics[, `:=`(no_at_max_treedepth, sum(diag$treedepth__ ==
      max_treedepth))]
    diagnostics[, `:=`(per_at_max_treedepth, no_at_max_treedepth / nrow(diag))]
    out <- cbind(out, diagnostics)
    timing <- round(fit$time()$total, 1)
    out[, `:=`(time, timing)]
  }
  return(out[])
}
