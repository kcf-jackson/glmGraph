#' @keywords internal
permute_column <- function(data0) {
  data0[,sample(ncol(data0))]
}

#' @keywords internal
create_reference_table <- function() {
  data.frame(
    family = c("gaussian", "gamma", "poisson", "binomial", "multinomial",
               "quadibinomial", "quasipoisson"),
    likelihood_FUN = c("dnorm", "dgamma", "dpois", "dbinom", "dmultinom",
                 "VGAM::dbetabinom.ab", "dnbinom"),
    simulation_FUN = c("rnorm", "rgamma", "rpois", "rbinom", "rmultinom",
                "VGAM::rbetabinom.ab", "rnbinom"),
    inverse_link_FUN = c("gaussian()$linkinv", "Gamma()$linkinv", "poisson()$linkinv",
                         "binomial()$linkinv", "multinomial()@linkinv",
                         "VGAM::betabinomial()@linkinv", "quasipoisson()$linkinv"),
    # parameters = cbind(parameters = list(
    #   list("mean", "sd"), list("shape", "rate"), list("lambda"),
    #   list("size", "prob"), list("size", "prob"), list("size", "shape1", "shape2"),
    #   list("size", "mu")
    # )),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
parameters2text <- function(parameters) {
  parameter_names <- names(parameters)
  parameter_values <- round(unlist(parameters), 4)
  res <- paste(parameter_names, parameter_values, sep = " = ")
  res <- paste(res, collapse = ", ")
  res
}

#' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}
