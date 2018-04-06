#' The function generates a factorisation of a joint density based on a graph.
#' @param rgraph matrix; graph represented by an adjacency matrix
#' @examples
#' m0 <- random_DAG(5)
#' factorise(m0)
#' @export
factorise <- function(rgraph) {
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


#' Print out summary of a complete factorisation table
#' @param x dataframe; the complete factorisation table; output from
#' build_conditional.
#' @param ... further arguments passed to or from other methods.
#' @export
print.gglm.data.frame <- function(x, ...) {
  create_reference_table <- function() {
    # This function creates a reference table for the 'print_summary" function.
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
  parameters2text <- function(parameters) {
    parameter_names <- names(parameters)
    parameter_values <- round(unlist(parameters), 4)
    res <- paste(parameter_names, parameter_values, sep = " = ")
    res <- paste(res, collapse = ", ")
    res
  }

  ref_table <- create_reference_table()
  x %>%
    magrittr::extract(c("fixed", "given", "family", "beta")) %>%
    dplyr::left_join(ref_table, by = "family") %>%
    dplyr::mutate(parameters = purrr::map(x$parameters, parameters2text)) %>%
    print(...)
}
