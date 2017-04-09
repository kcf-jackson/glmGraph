#' Specify the distribution for each conditional density
#' @param df0 A factorisation table; output from "factorise".
#' @param family Characters string; the exponential family of distribution.
#' It must be one of c("gaussian", "gamma", "poisson", "binomial", "multinomial",
#' "quasibinomial", "quasipoisson")
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
      beta = df0$given %>% purrr::map(~rnorm(length(.x)))
    )
}


#' Update the conditional mean given the data
#' @keywords internal
#' @describeIn This function updates the conditional mean given the data in preparation
#' for the parameters update.
update_conditional_mean <- function(df0, x0) {
  df0$mean <- purrr::pmap_dbl(
    .l = list(pos = df0$given, beta = df0$beta, inv_link = df0$invLink_FUN,
              default = df0$mean),
    .f = compute_mean, x = x0
  )
  df0
}
#' @keywords internal
compute_mean <- function(pos, x, beta, inv_link, default) {
  if (length(x[pos]) == 0) { return(default) }
  inv_link(sum(x[pos] * beta))
}
