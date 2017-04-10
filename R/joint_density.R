#' Specify the distribution for each conditional density
#' @param df0 A factorisation table; output from "factorise".
#' @param family Characters string; the exponential family of distribution.
#' It must be one of c("gaussian", "gamma", "poisson", "binomial", "multinomial",
#' "quasibinomial", "quasipoisson")
#' @export
build_conditional <- function(df0, family) {
  if (missing(family))
    family = rep("gaussian", nrow(df0))
  df0 %>%
    dplyr::mutate(
      family = family,
      likelihood_FUN = family %>% purrr::map(family2likeFUN),
      simulation_FUN = family %>% purrr::map(family2simFUN),
      invLink_FUN = family %>% purrr::map(family2invLinkFUN),
      parameters = family %>% purrr::map(family2parameters),
      mean = rep(1, nrow(df0)),
      beta = df0$given %>% purrr::map(~rnorm(length(.x) + 1)) #intercept
    )
}
