#' The function generates a factorisation of a joint density based on a graph.
#' @param rgraph matrix; graph represented by an adjacency matrix
#' @param response_node the index of the response variable; simply ignore if there is
#' no response variable.
#' @examples
#' m0 <- random_DAG(5)
#' factorise(m0)
#' @export
factorise <- function(rgraph, response_node = 1) {
  ind_given <- . %>% {which((.) == 1)}
  data.frame(cbind(
    fixed = 1:ncol(rgraph),
    given = map_cols(rgraph, ind_given)
  ))
}


#' Specify the distribution for each conditional density
#' @param df0 A factorisation table; output from "factorise".
#' @param family Vector of characters string; names of the exponential family of
#' distribution to be used. It must be one of c("gaussian", "gamma", "poisson",
#' "binomial")
#' @return A dataframe containing the full specification including the factorisation
#' and all the information about the asssociated GLM family.
#' @export
# To-do: Add support to "multinomial", "quasibinomial", "quasipoisson"
build_conditional <- function(df0, family) {
  if (missing(family))
    family = rep("gaussian", nrow(df0))

  res <- df0 %>%
    dplyr::mutate(
      family = family,
      likelihood_FUN = family %>% purrr::map(family2likeFUN),
      simulation_FUN = family %>% purrr::map(family2simFUN),
      invLink_FUN = family %>% purrr::map(family2invLinkFUN),
      parameters = family %>% purrr::map(family2parameters),
      mean = rep(1, nrow(df0)),
      beta =  purrr::map(
        .x = df0$given,
        .f = ~runif(length(.x) + 1, min = -3 / sqrt(nrow(df0)), max = 3 / sqrt(nrow(df0)))
      ) #intercept
    )

  attributes(res)$beta <- "random"
  class(res) <- c("gglm.data.frame", class(res))
  res
}
