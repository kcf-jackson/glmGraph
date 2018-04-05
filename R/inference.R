#' Fitting the GLM graphical model with MLE.
#' @param table0 dataframe; the complete factorisation table, output from "build_conditional".
#' @param data0 dataframe; the data.
#' @param progress T or F; whether to show progress bar.
#' @return dataframe; the complete factorisation table with fitted beta.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a data generating process randomly and simulate data from it
#' g_struct <- random_DAG(10) %>% factorise()
#' truth <- build_conditional(g_struct, rep("gaussian", 10))
#' sim_data <- simulate_data(truth, n = 1000)
#'
#' # Create a empty model with the given graph structure but with random coefficients
#' g_model <- build_conditional(g_struct, rep("gaussian", 10))
#' is_fitted(g_model)
#' data.frame(rbind(truth = truth$beta, model = g_model$beta)) # compare coefficients before fitting
#'
#' g_model <- MLE_graph(g_model, sim_data, T)
#' data.frame(rbind(truth = truth$beta, model = g_model$beta)) # compare coefficients after fitting
#' }
#' @export
MLE_graph <- function(table0, data0, progress = T) {
  nr <- nrow(table0)
  loglikelihood <- 0
  if (progress) pb <- txtProgressBar(1, nr, style=3)
  for (i in seq(nr)) {
    current <- table0[i, ]
    c_family <- current$family[[1]]
    c_fixed_index <- current$fixed[[1]]
    c_given_index <- current$given[[1]]

    glm_model <- fit_glm(
      x = cbind(intercept = 1, data0[,c_given_index]),  #intercept
      y = data0[,c_fixed_index],
      family = c_family,
      engine = speedglm::speedglm.wfit
    )
    loglikelihood <- loglikelihood + glm_model$logLik
    current$beta[[1]] <- get_parameters(glm_model)
    current$parameters[[1]] %<>%
      dispersion2parameters(glm_model$dispersion, c_family)
    table0[i, ] <- current
    if (progress) setTxtProgressBar(pb, i)
  }
  attr(table0, "loglikelihood") <- loglikelihood
  attr(table0, "beta") <- "fitted"
  table0
}

#' @keywords internal
fit_glm <- function(x, y, family, engine, ...) {
  if (family == "gamma") {
    family <- Gamma(link = log)
  } else {
    family = match.fun(family)()
  }
  engine(as.matrix(x), y = y, family = family, ...)
}

#' @keywords internal
get_parameters <- function(model_obj) {
  model_obj$coefficients
}
