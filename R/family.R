#' Return the density function of a family object
#' @keywords internal
family2likeFUN <- function(family, ...) {
  likeFUN <- switch(
    family,
    "gaussian" = dnorm,
    "gamma" = dgamma,
    "poisson" = dpois,
    "binomial" = dbinom,
    "multinomial" = dmultinom,
    "quasibinomial" = VGAM::dbetabinom.ab,
    "quasipoisson" = dnbinom
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
    "binomial" = rbinom,
    "multinomial" = rmultinom,
    "quasibinomial" = VGAM::rbetabinom.ab,
    "quasipoisson" = rnbinom
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
    "binomial" = list(size = 1, prob = 0.5),
    "multinomial" = list(size = 1, prob = rep(0.5, 5)),
    "quasibinomial" = list(size = 1, shape1 = 1, shape2 = 1),
    "quasipoisson" = list(size = 1, mu = 1)
  )
  parameters
}

#' Return the inverse link function of a family object
#' @keywords internal
family2invLinkFUN <- function(family, ...) {
  parameters <- switch(
    family,
    "gaussian" = gaussian()$linkinv,
    "gamma" = Gamma()$linkinv,
    "poisson" = poisson()$linkinv,
    "binomial" = binomial()$linkinv,
    "multinomial" = multinomial()@linkinv,
    "quasibinomial" = VGAM::betabinomial()@linkinv,
    "quasipoisson" = quasipoisson()$linkinv
  )
  parameters
}

#' Convert (conditonal) mean value into parameter value
#' @keywords internal
#' @param mean0 numeric; the mean value
#' @param parameters a named list
#' @param family0 characters string; the exponential family.
mean2parameters <- function(mean0, parameters, family0) {
  parameters <- switch(

  )
  parameters
}
