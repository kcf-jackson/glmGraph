# USE FAMILY BY FAMILY PARTITION
# family_reference_table <- function() {
#   data.frame(
#     family = c("gaussian", "gamma", "poisson", "binomial", "multinomial",
#       "quadibinomial", "quasipoisson"),
#     like_FUN = list(
#       "gaussian" = dnorm,
#       "gamma" = dgamma,
#       "poisson" = dpois,
#       "binomial" = dbinom,
#       "multinomial" = dmultinom,
#       "quasibinomial" = VGAM::dbetabinom.ab,
#       "quasipoisson" = dnbinom
#     ),
#     sim_FUN = list(
#       "gaussian" = rnorm,
#       "gamma" = rgamma,
#       "poisson" = rpois,
#       "binomial" = rbinom,
#       "multinomial" = rmultinom,
#       "quasibinomial" = VGAM::rbetabinom.ab,
#       "quasipoisson" = rnbinom
#     ),
#     invLink_FUN = list(
#       "gaussian" = gaussian()$linkinv,
#       "gamma" = Gamma()$linkinv,
#       "poisson" = poisson()$linkinv,
#       "binomial" = binomial()$linkinv,
#       "multinomial" = multinomial()@linkinv,
#       "quasibinomial" = VGAM::betabinomial()@linkinv,
#       "quasipoisson" = quasipoisson()$linkinv
#     ),
#     parameters = list(
#       list(mean = 0, sd = 1),
#       list(shape = 1, rate = 1),
#       list(lambda = 1),
#       list(size = 1, prob = 0.5),
#       list(size = 1, prob = rep(0.5, 5)),
#       list(size = 1, shape1 = 1, shape2 = 1),
#       list(size = 1, mu = 1)
#     )
#   )
# }


#' Return the density function of a family object
#' @keywords internal
family2likeFUN <- function(family, ...) {
  likeFUN <- switch(
    family,
    "gaussian" = dnorm,
    "gamma" = dgamma,
    "poisson" = dpois,
    "binomial" = dbinom,
    # "multinomial" = dmultinom,
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
    # "multinomial" = rmultinom,
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
    # "multinomial" = list(size = 1, prob = rep(0.5, 5)),
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
#' @param mu numeric; the mean value
#' @param parameters a named list
#' @param family0 characters string; the exponential family.
mean2parameters <- function(parameters, mu, family) {
  if (family == "gaussian") {
    parameters$mean <- mu
  }
  if (family == "gamma") {
    parameters$shape <- mu * parameters$rate
  }
  if (family == "poisson") {
    parameters$lambda <- mu
  }
  if (family == "binomial") {
    parameters$prob <- mu
  }
  # if (family0 == "multinomial") {}
  if (family == "quasibinomial") {
    # mu = size * a / (a + b) => a = mu / (n - mu) * b
    parameters$shape1 <- mu / (parameters$size - mu) * parameters$shape2
  }
  if (family == "quasipoisson") {
    parameters$mu <- mu
  }
  parameters
}
