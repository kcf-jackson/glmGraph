#' Perform imputation based on a graphical model
#' @param table0 data.frame; full joint density specification fitted to data.
#' @param data0 data.frame; the data.
#' @param method "mean", "response" or "sample". See 'details' for more information.
#' @param threshold probability threshold to decide the class for the binomial distribution.
#' @details "mean" refers to impute with the conditional mean.
#' "response" refers to impute with the fitted class / value.
#' "sample" refers to impute with a sample from the conditional distribution.
#' A common usage is to do multiple imputation.
#' Note that "mean" and "response" are the same for continuous distribution.
#' @export
imputation <- function(table0, data0, method, threshold) {
  predict.graph(table0, data0, method, threshold)
}


#' Predict based on a graphical model
#' @param table0 data.frame; full joint density specification fitted to data.
#' @param data0 data.frame; the data.
#' @param method "mean" or "response". See 'details' for more information.
#' @param threshold probability threshold to decide the class for the binomial distribution.
#' @details "mean" refers to predicting with the conditional mean.
#' A common usage is to predict the probability for the occurrence of a class.
#' "response" refers to predicting with the fitted class / value.
#' A common usage is to predict the actual class.
#' Note that "mean" and "response" are the same for continuous distribution.
#' @export
predict.graphical_model <- function(table0, data0, method, threshold) {
  predict.graph(table0, data0, method, threshold)
}


#' Predict based on a graphical model
#' @keywords internal
predict.graph <- function(table0, data0, method = "mean", threshold = 0.5) {
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
      if ((method == "response") & (c_family == "binomial")) {
        imputed_values <- as.numeric(mu > threshold)
      } else if ((method == "mean") | (method == "response")) {
        imputed_values <- mu
      } else if (method == "sample") {
        imputed_values <- get_sim_data(c_parameter, mu, c_family, c_sim_FUN)
      }

      impute_index <- which_NA(data0[, c_fixed])
      data0[impute_index, c_fixed] <- imputed_values[impute_index]
    }
    completed_index %<>% c(current_index)
  }
  data.frame(data0)
}


#' Randomly remove entries from a dataset
#' @keywords internal
#' @param data0 data.frame or matrix; the data
#' @param p probability of missing
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
