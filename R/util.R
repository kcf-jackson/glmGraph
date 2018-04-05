# Assign names0 to columns and rows of m0
add_names <- function(m0, names0) {
  if (missing(names0)) names0 <- paste0("x_{", 1:ncol(m0), "}")
  assertthat::assert_that(ncol(m0) == nrow(m0))
  colnames(m0) <- rownames(m0) <- names0
  m0
}


# Apply f to each column of m
map_cols <- function(m, f, mapfun = purrr::map) {
  mapfun(seq_len(ncol(m)), ~f(m[,.x]))
}


# Apply f to each row of m
map_rows <- function(m, f, mapfun = purrr::map) {
  mapfun(seq_len(nrow(m)), ~f(m[.x,]))
}


#' Check if the model has been fitted to some data.
#' @param df0 Data.frame; the graphical GLM.
#' @export
is_fitted <- function(df0) {
  status <- attr(df0, "beta")
  if (status == "fitted") {
    print("The model has been fitted to some data.")
    return(T)
  } else if (status == "random") {
    print("The model has not been fitted; the coefficients are set randomly.")
    return(F)
  } else {
    stop("Wrong status.")
  }
}


#' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


#' @keywords internal
normalise <- function(vec0) {
  vec0 / sum(vec0)
}


# Return the index of the maximum element (first index if tied)
#' @keywords internal
which_max <- function(vec0) {
  min(which(vec0 == max(vec0)))
}


# Return the index of the minimum element (first index if tied)
#' @keywords internal
which_min <- function(vec0) {
  min(which(vec0 == min(vec0)))
}


# Return the indexes of all the NA elements.
#' @keywords internal
which_NA <- function(vec0) {
  which(is.na(vec0))
}
