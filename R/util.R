#' @keywords internal
permute_column <- function(data0) {
  data0[,sample(ncol(data0))]
}

#' @keywords internal
# This function creates a reference table for the 'print_summary" function.
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

#' @keywords internal
normalise <- function(vec0) {
  vec0 / sum(vec0)
}

#' @keywords internal
which_max <- function(vec0) {
  min(which(vec0 == max(vec0)))
}

#' @keywords internal
which_min <- function(vec0) {
  min(which(vec0 == min(vec0)))
}

#' @keywords internal
which_NA <- function(vec0) {
  which(is.na(vec0))
}

#' @keywords internal
is.fitted <- function(table0) {
  attributes(table0)$beta == "fitted"
}

#' @keywords internal
dummy2factor <- function(m0, factor_name, factor_labels) {
  res <- apply(m0, 1, function(x) which(x == 1)) %>%
    factor(labels = factor_labels) %>% data.frame()
  colnames(res) <- factor_name
  res
}

create_dummy <- function(df0) {
  map_chr(df0)
}
