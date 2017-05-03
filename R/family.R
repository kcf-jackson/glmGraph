#' #' @keywords internal
#' family2FUN <- function(family) {
#'   if (family == "gaussian") {
#'     df_FUN <- cbind(
#'       like_FUN = dnorm, sim_FUN = rnorm,
#'       invLink_FUN = gaussian()$linkinv,
#'       parameters = list(list(mean = 0, sd = 1))
#'     )
#'   } else if (family == "gamma") {
#'     df_FUN <- cbind(
#'       like_FUN = dgamma, sim_FUN = rgamma,
#'       invLink_FUN = Gamma(link = log)$linkinv,
#'       parameters = list(list(shape = 1, rate = 1))
#'     )
#'   } else if (family == "poisson") {
#'     df_FUN <- cbind(
#'       like_FUN = dpois, sim_FUN = rpois,
#'       invLink_FUN = poisson()$linkinv,
#'       parameters = list(list(lambda = 1))
#'     )
#'   } else if (family == "binomial") {
#'     df_FUN <- cbind(
#'       like_FUN = dbinom, sim_FUN = rbinom,
#'       invLink_FUN = binomial()$linkinv,
#'       parameters = list(list(size = 1, prob = 0.5))
#'     )
#'   }
#'   df_FUN
#' }

#' Return the density function of a family object
#' @keywords internal
family2likeFUN <- function(family, ...) {
  likeFUN <- switch(
    family,
    "gaussian" = dnorm,
    "gamma" = dgamma,
    "poisson" = dpois,
    "binomial" = dbinom
    # "multinomial" = dmultinom,
    # "quasibinomial" = VGAM::dbetabinom.ab,
    # "quasipoisson" = dnbinom
  )
  likeFUN
}

#' Return the sampling function of a family object
#' @keywords internal
family2simFUN <- function(family, ...) {
  simFUN <- switch(
    family,
    "gaussian" = rnorm,
    "gamma" = rgamma,
    "poisson" = rpois,
    "binomial" = rbinom
    # "multinomial" = rmultinom,
    # "quasibinomial" = VGAM::rbetabinom.ab,
    # "quasipoisson" = rnbinom
  )
  simFUN
}

#' Return the parameter of a family object
#' @keywords internal
family2parameters <- function(family, ...) {
  parameters <- switch(
    family,
    "gaussian" = list(mean = 0, sd = 1),
    "gamma" = list(shape = 1, rate = 1),
    "poisson" = list(lambda = 1),
    "binomial" = list(size = 1, prob = 0.5)
    # "multinomial" = list(size = 1, prob = rep(0.5, 5)),
    # "quasibinomial" = list(size = 1, shape1 = 1, shape2 = 1),
    # "quasipoisson" = list(size = 1, mu = 1)
  )
  parameters
}

#' Return the inverse link function of a family object
#' @keywords internal
family2invLinkFUN <- function(family, ...) {
  parameters <- switch(
    family,
    "gaussian" = gaussian()$linkinv,
    "gamma" = Gamma(link = log)$linkinv,
    "poisson" = poisson()$linkinv,
    "binomial" = binomial()$linkinv
    # "multinomial" = VGAM::multinomial()@linkinv,
    # "quasibinomial" = VGAM::betabinomial()@linkinv,
    # "quasipoisson" = quasipoisson()$linkinv
  )
  parameters
}

#' Convert (conditonal) mean value into parameter value
#' @keywords internal
#' @param mu numeric; the mean value
#' @param parameters a named list; the parameters
#' @param family0 characters string; the exponential family.
mean2parameters <- function(parameters, mu, family) {
  if (family == "gaussian") {
    parameters$mean <- mu
  } else if (family == "gamma") {
    parameters$rate <- parameters$shape / mu
  } else if (family == "poisson") {
    parameters$lambda <- mu
  } else if (family == "binomial") {
    parameters$prob <- mu
  }
  # if (family0 == "multinomial") {}
  # else if (family == "quasibinomial") {
  #   # mu = size * a / (a + b) => a = mu / (n - mu) * b
  #   parameters$shape1 <- mu / (parameters$size - mu) * parameters$shape2
  # } else if (family == "quasipoisson") {
  #   parameters$mu <- mu
  # }
  parameters
}

#' @keywords internal
dispersion2parameters <- function(parameters, dispersion, family) {
  if (family == "gaussian") {
    parameters$sd <- sqrt(dispersion)
  } else if (family == "gamma") {
    parameters$shape <- 1 / dispersion
  }
  parameters
}
