#' Simulate data given a graphical model.
#' @param table0 dataframe; the graphical model expressed in a complete
#' factorisation table, output from "build_conditional".
#' @param n integer; number of data to simulate.
#' @return n x p matrix; n is the number of data, p is the number of variables.
#' @examples
#' g_struct <- factorise(random_DAG(5))
#' g_model <- build_conditional(g_struct, rep("gaussian", 5))
#' simulate_data(g_model)
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

      X <- as.matrix(cbind(1, data0[, c_given]))
      mu <- drop(X %*% c_beta) %>% c_invLink()
      data0[,c_fixed] <- get_sim_data(c_parameter, mu, c_family, c_sim_FUN)
    }
    completed_index %<>% c(current_index)
  }
  data.frame(data0)
}


#' Perform imputation based on a graphical model
#' @param table0 data.frame; full joint density specification fitted to data.
#' @param data0 data.frame; the data.
#' @param method "mean", "response" or "sample". See 'details' for more information.
#' @param threshold probability threshold to decide the class for the binomial distribution.
#' @details "mean" refers to imputation with the conditional mean.
#' "response" refers to imputation with the fitted class / value.
#' "sample" refers to imputation with a sample from the conditional distribution.
#' "sample" is commonly used to do multiple imputation.
#' Note that "mean" and "response" are the same for continuous distributions.
#' @export
imputation <- function(table0, data0, method = "response", threshold = 0.5) {
  predict_graph(table0, data0, method, threshold)
}


#' Predict based on a graphical model
#' @param object data.frame; full joint density specification fitted to data.
#' @param data0 data.frame; the data.
#' @param resp_var character strings; the name of the variable to be predicted.
#' @param method "mean" or "response". See 'details' for more information.
#' @param threshold probability threshold to decide the class for the binomial distribution.
#' @param ... further arguments passed to or from other methods.
#' @details "mean" refers to predicting with the conditional mean.
#' A common usage is to predict the probability for the occurrence of a class.
#' "response" refers to predicting with the fitted class / value.
#' A common usage is to predict the actual class.
#' Note that "mean" and "response" are the same for continuous distribution.
#' @export
predict.gglm.data.frame <- function(object, data0, resp_var,
                                    method = "mean", threshold = 0.5, ...) {
  assertthat::assert_that(method %in% c("mean", "response"))
  data0[[resp_var]] <- NA
  predict_graph(object, data0, method, threshold) %>%
    dplyr::select(dplyr::one_of(resp_var))
}


#' Predict based on a graphical model
#' @keywords internal
predict_graph <- function(table0, data0, method = "mean", threshold = 0.5) {
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

      X <- as.matrix(cbind(1, data0[, c_given]))
      mu <- drop(X %*% c_beta) %>% c_invLink()
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
