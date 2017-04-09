#' Fitting the GLM graphical model with MLE.
#' @param table0 dataframe; the complete factorisation table, output from "build_conditional".
#' @param data0 dataframe; the data.
#' @param engine function; the function to fit a GLM.
#' @export
MLE_graph <- function(table0, data0, engine = glm.fit) {
  nr <- nrow(table0)
  pb <- txtProgressBar(1, nr, style=3)
  for (i in seq(nr)) {
    current <- table0[i, ]
    c_family <- current$family %>% extract2(1)
    c_fixed_index <- current$fixed %>% extract2(1)
    c_given_index <- current$given %>% extract2(1)

    glm_model <- fit_glm(
      x = cbind(intercept = 1, data0[,c_given_index]), #intercept
      y = data0[,c_fixed_index],
      family = c_family, engine = engine
    )

    current$beta[[1]] <- get_parameters(glm_model)
    table0[i, ] <- current
    setTxtProgressBar(pb, i)
  }
  table0
}


# Helper functions
#' @keywords internal
fit_glm <- function(x, y, family, engine) {
  engine(x, y, family = match.fun(family)())
}

#' @keywords internal
get_parameters <- function(model_obj) {
  model_obj$coefficients
}
