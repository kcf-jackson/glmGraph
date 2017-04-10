#' Compute the likelihood for a single data-point given the complete
#' factorisation table
#' @param table0 dataframe; A factorisation table, output from "build_conditional".
#' @param data0 matrix or dataframe, the data.
#' @param log TRUE / FALSE; returns loglikelihood / likelihood.
#' @export
compute_likelihood <- function(table0, data0, log = TRUE) {
  if (!(class(data0) %in% c("matrix", "data.frame"))) {
    stop("You have only one datapoint and it is not of class 'matrix' or 'data.frame'.")
  }
  nr <- nrow(data0)
  seq(nr) %>%
    purrr::map_dbl(
      ~compute_datum_likelihood(table0, data0[.x, ], log = log)
    ) %>%
    sum()
}

#' Compute the likelihood for a single data-point given the complete
#' factorisation table
#' @keywords internal
#' @param table0 dataframe; A factorisation table, output from "build_conditional".
#' @param x data vector; one data point of multiple features/covariates.
#' @param log TRUE / FALSE; returns loglikelihood / likelihood.
compute_datum_likelihood <- function(table0, x, log = TRUE) {
  table0 %<>% update_conditional_mean(x)

  num_components <- nrow(table0)
  loglikelihood <- 0
  for (i in seq(num_components)) {
    current <- table0[i, ]

    like_FUN <- current %>%
      magrittr::use_series("likelihood_FUN") %>%
      magrittr::extract2(1)

    parameters <- current %>%
      magrittr::use_series("parameters") %>%
      magrittr::extract2(1)
    # update parameters based on the conditional mean
    parameters %<>%
      mean2parameters(mu = current$mean, family = current$family) %>%
      append(list(x = x[i], log = log))

    loglikelihood <- loglikelihood + do.call(like_FUN, parameters)
  }
  loglikelihood
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
