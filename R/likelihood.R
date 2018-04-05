#' Compute the likelihood for a dataset given the complete factorisation table
#' @param table0 dataframe; A factorisation table, output from "build_conditional".
#' @param data0 matrix or dataframe; the data.
#' @param log TRUE / FALSE; returns loglikelihood / likelihood.
#' @note Note that there are some discrepancy between the likelihood extracted from
#' the model and the likelihood computed using this function for the gaussian and gamma
#' family. The reason is that speedglm::speedglm (and stats::glm) computes the likelihood
#' using the ML estimates, but reports the LS estimates for the dispersion parameters.
#' @export
compute_likelihood <- function(table0, data0, log = TRUE) {
  if (!(class(data0) %in% c("matrix", "data.frame"))) {
    stop("Your data is not of class 'matrix' or 'data.frame'.")
  }
  num_components <- nrow(table0)
  loglikelihood <- 0
  complete_index <- c()
  while (!setequal(complete_index, seq(num_components))) {
    current_index <- table0 %>% get_available_rows(exclude = complete_index)
    for (i in current_index) {
      current <- table0[i,]
      resp <- current$fixed[[1]]
      given <- current$given[[1]]
      loglikelihood <- loglikelihood +
        eval_marginal_likelihood(current, data0[,resp], data0[,given])
    }
    complete_index <- c(complete_index, current_index)
  }
  loglikelihood
}


#' @keywords internal
eval_marginal_likelihood <- function(marginal, resp_variable, covariates) {
  #update conditional mean
  invLink <- marginal$invLink_FUN[[1]]
  beta <- marginal$beta[[1]]
  mu <- get_batch_eta(beta, cbind(1, covariates)) %>% invLink()
  #update parameters and compute the likelihood
  parameters <- marginal$parameters[[1]]
  family <- marginal$family[[1]]
  like_FUN <- marginal$likelihood_FUN[[1]]
  purrr::map2_dbl(
    .x = resp_variable, .y = mu,
    .f = ~do.call(
      like_FUN,
      append(mean2parameters(parameters, .y, family), list(x = .x, log = TRUE))
    )
  ) %>% sum()
}


#' Get likelihood from a fitted model
#' @param table0 data.frame; the fitted graph from 'MLE_graph'.
#' @return The loglikelihood value.
#' @export
get_model_likelihood <- function(table0) {
  attr(table0, "loglikelihood")
}


#' @keywords internal
AIC_like <- function(n, k) { -2 * k }


#' @keywords internal
BIC_like <- function(n, k) { -log(n) * k }
