#' Simulate data given the complete factorisation table
#' @param table0 dataframe; the complete factorisation table, output from "build_conditional".
#' @param n integer; number of data to simulate.
#' @return n x p matrix; n is the number of data, p is the number of variables.
simulate_data <- function(table0, n) {
  seq(n) %>%
    purrr::map(~simulate_datum(table0)) %>%
    do.call(rbind, .)
}


#' Simulate a datapoint given the complete factorisation table
#' @keywords internal
simulate_datum <- function(table0) {
  num_var <- nrow(table0)
  sim_datum <- rep(NA, num_var)

  table1 <- table0 %>% add_covariates_column()
  current_index <- table1 %>% get_standalone_rows()
  has_more_index <- length(current_index) != 0
  while (has_more_index) {
    for (i in current_index) {
      current <- table1[i, ]
      c_fixed <- current$fixed %>% magrittr::extract2(1)
      c_family <- current$family %>% magrittr::extract2(1)
      # update conditional mean from eta, the linear predictor
      current %<>% update_conditional_mean(sim_datum)
      c_mu <- current$mean %>% magrittr::extract2(1)
      # update parameters from conditional mean
      current$parameters[[1]] %<>%
        mean2parameters(mu = c_mu, family = c_family)
      # Simulate data
      sim_x <- simulate_one_data(current)
      sim_datum[c_fixed] <- sim_x
      # Pass the generated X to the rows that need it
      d_index <- table1 %>% find_dependent_rows(c_fixed)
      if (!purrr::is_empty(d_index)) {
        table1[d_index, ] %<>% update_dependent_rows(c_fixed, sim_x)
      }
    }
    table1 <- table1[-current_index, ]
    current_index <- table1 %>% get_standalone_rows()
    has_more_index <- !purrr::is_empty(current_index)
  }

  sim_datum
}


# Helper functions
#' @keywords internal
simulate_one_data <- function(conditional_density, n = 1) {
  do.call(
    conditional_density$simulation_FUN %>% magrittr::extract2(1),
    conditional_density$parameters %>% magrittr::extract2(1) %>%
      append(list(n = n))
  )
}

#' @keywords internal
add_covariates_column <- function(table0) {
  table0 %<>% dplyr::mutate(X = purrr::map(beta, ~rep(NA, length(.x))))
}

#' @keywords internal
get_standalone_rows <- function(table1) {
  table1 %>% magrittr::extract2("X") %>% purrr::map_lgl(check_X) %>% which()
}

#' @keywords internal
check_X <- function(vec0) {
  if (purrr::is_empty(vec0)) return(TRUE)
  all(!is.na(vec0))
}

#' @keywords internal
find_dependent_rows <- function(table0, var_index) {
  table0 %>% extract2("given") %>%
    purr::map_lgl(~has_index(.x, var_index)) %>% which()
}

#' @keywords internal
update_dependent_rows <- function(d_rows, fixed_index, fixed_sim) {
  nr <- max(1, nrow(d_rows))
  for (i in seq(nr)) {
    d_rows[i, ] %<>% update_one_dependent_row(fixed_index, fixed_sim)
  }
  d_rows
}

#' @keywords internal
update_one_dependent_row <- function(d_row, fixed_index, fixed_sim) {
  # Add simulated X
  change_index <- which(d_row$given[[1]] == fixed_index)
  d_row$X[[1]][change_index] <- fixed_sim
  d_row
}

#' @keywords internal
has_index <- function(vec0, index0) {
  if (purrr::is_empty(vec0)) return(FALSE)
  index0 %in% vec0
}
