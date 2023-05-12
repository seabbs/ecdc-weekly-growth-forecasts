damped_inits <- function(data, strains = 2) {
  init_fn <- function() {
    inits <- list(
      init_cases = array(log(abs(rnorm(1, data$X[1], data$X[1] * 0.01)))),
      r_init = rnorm(1, data$r_init_mean, data$r_init_sd * 0.1),
      r_scale = abs(rnorm(1, 0, 0.01)),
      eta = rnorm(data$t_nots - 1, 0, 0.01),
      beta = rnorm(1, 0, 0.1),
      sqrt_phi = array(abs(rnorm(2, 0, 0.01))),
      period_eff = numeric(0),
      period_sd = numeric(0)
    )
    if (data$period > 1) {
      inits$period_eff <- array(rnorm(data$period, 0, 0.1))
      inits$period_sd <- array(abs(rnorm(1, 0, 0.1)))
    }
    if (strains == 1) {
      inits$sqrt_phi <- array(inits$sqrt_phi[1])
    } else {
      inits$init_voc_cases <- array(
        log(abs(rnorm(
          1, max(2, data$X[data$t_nseq + 1] * data$Y[1] / data$N[1]),
          max(2, data$X[data$t_nseq + 1] * data$Y[1] / data$N[1]) * 0.01
        )))
      )
      inits$voc_mod <- rnorm(
        1, data$voc_mean,
        data$voc_sd * 0.1
      )
      if (data$relat == 1) {
        inits$voc_beta <- array(rnorm(1, 0, 0.1))
      }else {
        inits$voc_beta <- numeric(0)
      }
      if (data$relat > 0) {
        inits$voc_scale <- array(abs(rnorm(1, 0, 0.01)))
        inits$voc_eta <- array(rnorm(data$voc_eta_n, 0, 0.01))
      } else {
        inits$voc_scale <- numeric(0)
        inits$voc_eta <- numeric(0)
      }
      if (data$relat == 2) {
        inits$L_Omega <- matrix(c(1, runif(1), 0, runif(1)), 2, 2) # nolint
      }
    }

    if (data$overdisp == 0) {
      inits$sqrt_phi <- numeric(0)
    }
    return(inits)
  }
  return(init_fn)
}