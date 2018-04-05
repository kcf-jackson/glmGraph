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
  table0 %<>%
    dplyr::mutate(X = purrr::map(beta, ~rep(NA, length(.x) - 1)))  #intercept
}

#' Randomly remove entries from a dataset
#' @param data0 data.frame or matrix; the data
#' @param p probability of missing; for each entry in the data, the
#' missingness follows a Bernoulli(p) distribution.
#' @examples
#' create_missingness(mtcars, 0.1)
#' @export
create_missingness <- function(data0, p = 0.1) {
  nr <- nrow(data0)
  nc <- ncol(data0)
  total <- nr * nc
  missingness <- matrix(
    sample(c(NA, TRUE), size = total, prob = c(p, 1-p), replace = T),
    nrow = nr, ncol = nc
  )
  data0 * missingness
}
