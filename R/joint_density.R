#' Compute the likelihood for a single data-point given the complete
#' factorisation table
#' @keywords internal
#' @param table0 dataframe; A factorisation table, output from "build_conditional".
#' @param data0 matrix or dataframe, the data.
#' @param log TRUE / FALSE; returns loglikelihood / likelihood.
#' @export
compute_likelihood <- function(table0, data0, log = TRUE) {
  nr <- nrow(data0)
  seq(nr) %>%
    purrr::map_dbl(
      ~compute_datum_likelihood(table0, data0[nr, ], log = log)
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
  for (i in rev(seq(num_components))) {
    current <- table0[i, ]

    like_FUN <- current %>%
      use_series(likelihood) %>%
      extract2(1)

    parameters <- current %>%
      magrittr::use_series(parameters) %>%
      extract2(1)
    # update parameters based on the conditional mean
    parameters %<>%
      mean2parameters(mean0 = table0$mean, family = table0$family) %>%
      append(list(x = x[i], log = log))

    loglikelihood <- loglikelihood + do.call(like_FUN, parameters)
  }
  loglikelihood
}


#' Specify the distribution for each conditional density
#' @param df0 A factorisation table; output from "factorise".
#' @param family Characters string; the exponential family of distribution.
#' It must be one of c("gaussian", "gamma", "poisson", "binomial", "multinomial",
#' "quadibinomial", "quasipoisson")
#' @export
build_conditional <- function(df0, family) {
  df0 %>%
    dplyr::mutate(
      family = family,
      likelihood_FUN = family %>% purrr::map(family2likeFUN),
      simulation_FUN = family %>% purrr::map(family2simFUN),
      invLink_FUN = family %>% purrr::map(family2invLinkFUN),
      parameters = family %>% purrr::map(family2parameters),
      mean = rep(1, nrow(df0)),
      beta = given %>% purrr::map(~rnorm(length(.x)))
    )
}


#' Update the conditional mean given the data
#' @keywords internal
#' @describeIn This function updates the conditional mean given the data in preparation
#' for the parameters update.
update_conditional_mean <- function(df0, x0) {
  x1 <- array(list(x0), nrow(df0))
  df0$mean <- pmap_dbl(
    .l = list(pos = df0$given, x = x1, beta = df0$beta, inv_link = df0$link_FUN),
    .f = compute_mean
  )
  df0
}
#' @keywords internal
compute_mean <- function(pos, x, beta, inv_link) {
  if (length(x[pos]) == 0) { return(NA) }
  inv_link(sum(x[pos] * beta))
}
