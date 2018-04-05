#' Simulate data given a graphical model.
#' @param table0 dataframe; the graphical model expressed in a complete
#' factorisation table, output from "build_conditional".
#' @param n integer; number of data to simulate.
#' @return n x p matrix; n is the number of data, p is the number of variables.
#' @export
simulate_data <- function(table0, n = 100) {
  num_var <- nrow(table0)
  data0 <- matrix(NA, nrow = n, ncol = num_var)
  table1 <- table0 %>% add_covariates_column()

  completed_index <- c()
  while (any(is.na(data0))) {
    current_index <- table1 %>% get_available_rows(completed_index)
    for (i in current_index) {
      current <- table1[i,]
      c_fixed <- current$fixed[[1]]
      c_given <- current$given[[1]]
      c_family <- current$family[[1]]
      c_beta <- current$beta[[1]]
      c_invLink <- current$invLink_FUN[[1]]
      c_sim_FUN <- current$simulation_FUN[[1]]
      c_parameter <- current$parameter[[1]]

      mu <- get_batch_eta(c_beta, cbind(1, data0[, c_given])) %>% c_invLink()
      data0[,c_fixed] <- get_sim_data(c_parameter, mu, c_family, c_sim_FUN)
    }
    completed_index %<>% c(current_index)
  }
  data.frame(data0)
}

#' @keywords internal
# This function finds the nodes that have all covariates ready to be used.
get_available_rows <- function(table1, exclude = c()) {
  table1 %>%
    magrittr::use_series("given") %>%
    purrr::map_lgl(~setdiff(.x, exclude) %>% purrr::is_empty()) %>%
    which() %>%
    setdiff(exclude)
}

#' @keywords internal
get_batch_eta <- function(beta, data0) {
  colSums(beta * t(data0))
}

#' @keywords internal
# This function samples from the distribution given the conditional mean.
get_sim_data <- function(c_parameter, mu, c_family, c_sim_FUN) {
  c_parameter %<>% append(list(n = 1))
  mu %>%
    purrr::map_dbl(
      ~do.call(c_sim_FUN, mean2parameters(c_parameter, .x, c_family))
    )
}

#' @keywords internal
# This function adds a column to store the covariates which are used to
# compute the conditional mean in the later stage.
add_covariates_column <- function(table0) {
  table0 %<>% dplyr::mutate(X = purrr::map(beta, ~rep(NA, length(.x) - 1)))  #intercept
}
