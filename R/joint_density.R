# compute_data_likelihood <- function(likelihood_FUN, data) {
#
# }
#
#
# build_likelihood <- function(df0) {
#   return(
#     function(v0) {
#       #v0 is the data vector, i.e. one data point of k features.
#
#     }
#   )
# }
#

#' @param table0 dataframe; A factorisation table, output from "specify_conditional".
compute_loglikelihood <- function(table0, x) {
  num_components <- nrow(table0)
  loglikelihood <- 0
  for (i in rev(seq(num_components))) {
    current <- table0[i, ]

    like_FUN <- current %>%
      use_series(likelihood) %>%
      extract2(1)

    parameters <- current %>%
      magrittr::use_series(parameters) %>%
      extract2(1) %>%
      append(list(x = x[i], log = TRUE))

    loglikelihood <- loglikelihood + do.call(like_FUN, parameters)
  }
  loglikelihood
}


build_conditional <- function(df0, family) {
  df0 %>%
    dplyr::mutate(
      family = family,
      likelihood_FUN = family %>% purrr::map(family2likeFUN),
      simulation_FUN = family %>% purrr::map(family2simFUN),
      link_FUN = family %>% purrr::map(family2invLinkFUN),
      parameters = family %>% purrr::map(family2parameters),
      mean = rep(1, nrow(df0)),
      beta = given %>% purrr::map(~rnorm(length(.x)))
    )
}


update_conditional_mean <- function(df0, x0) {
  x1 <- array(list(x0), nrow(df0))
  df0$mean <- pmap_dbl(
    .l = list(pos = df0$given, x = x1, beta = df0$beta, inv_link = df0$link_FUN),
    .f = compute_mean
  )
  df0
}
compute_mean <- function(pos, x, beta, inv_link) {
  if (length(x[pos]) == 0) { return(NA) }
  inv_link(sum(x[pos] * beta))
}
