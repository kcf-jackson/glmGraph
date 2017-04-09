#' A package for simulating data from a GLM graphical model
#' @description This packages provides functions to simulate from and fit graphical models
#' where all conditional distributions follow the generalised linear model. This is
#' mainly used to construct an approximation to a multivariate distribution.
#' @docType package
#' @name glmGraph
#' @author Jackson Kwok, Felix leung
NULL


#' Import functions from other packages.
#' @name Imported functions
#' @keywords internal
#' @importFrom stats cov2cor gaussian glm model.matrix rbinom rexp rnbinom
#' rnorm rpois rt runif var
#' @importFrom utils head tail
#' @importFrom magrittr %>% %<>%
#' @importFrom graphics plot
#' @importFrom stats dbinom dgamma dmultinom dnbinom dnorm dpois rbinom
#' rgamma rmultinom rnbinom rnorm rpois runif Gamma binomial poisson
#' quasipoisson
NULL

