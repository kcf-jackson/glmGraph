#' Compute the likelihood for the data given the complete factorisation table
#' @param table0 dataframe; A factorisation table, output from "build_conditional".
#' @param data0 matrix or dataframe, the data.
#' @param log TRUE / FALSE; returns loglikelihood / likelihood.
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
      print(i)
      print(current$beta[[1]])
      print(eval_marginal_likelihood(current, data0[,resp], data0[,given]))
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

#' Update the conditional mean given the data
#' @keywords internal
#' @param df0 dataframe; A factorisation table, output from "build_conditional".
#' @param x0 vector; single data point.
#' @describeIn This function updates the conditional mean given the data in preparation
#' for the parameters update.
update_conditional_mean <- function(df0, x0) {
  x0 %<>% as.numeric()
  df0$mean <- purrr::pmap_dbl(
    .l = list(pos = df0$given, beta = df0$beta, inv_link = df0$invLink_FUN,
              default = df0$mean),
    .f = compute_mean, x = x0
  )
  df0
}

#' @keywords internal
compute_mean <- function(pos, x, beta, inv_link, default) {
  if (length(x[pos]) == 0) { return(beta) }
  inv_link(sum(c(1, x[pos]) * beta, na.rm = TRUE)) #intercept
}

#' @keywords internal
get_model_likelihood <- function(table0) {
  attr(table0, "loglikelihood")
}

