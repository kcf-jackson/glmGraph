#' Specify the distribution for each conditional density
#' @param df0 A factorisation table; output from "factorise".
#' @param family Vector of characters string; names of the exponential family of
#' distribution to be used. It must be one of c("gaussian", "gamma", "poisson",
#' "binomial")
#' @export
# To-do: Add support to "multinomial", "quasibinomial", "quasipoisson"
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
      beta =  purrr::map(
        .x = df0$given,
        .f = ~rnorm(length(.x) + 1, sd = 1 / sqrt(nrow(df0)))
      ) #intercept
    )
}

#' Print out summary of a complete factorisation table
#' @param table0 dataframe; the complete factorisation table; output from
#' build_conditional.
#' @export
# To-do: Join values with parameters names
print_summary <- function(table0) {
  ref_table <- create_reference_table()
  table0 %>%
    magrittr::extract(c("fixed", "given", "family", "beta")) %>%
    dplyr::left_join(ref_table, by = "family")
}

#' #' Optimise the "build_conditional" function for gibbs sampler model selection.
#' #' @keywords internal
#' fast_build_conditional <- function(df0, family_df0) {
#'   data.frame(df0, family_df0) %>%
#'     dplyr::mutate(beta = purrr::map(df0$given, ~rnorm(length(.x) + 1)))  #intercept
#' }
