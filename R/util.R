# if_else <- function(test, yes, no) {
#   if (test) return(yes)
#   no
# }


add_names <- function(m0, names0) {
  if (missing(names0)) names0 <- paste0("x_{", 1:ncol(m0), "}")
  assertthat::assert_that(ncol(m0) == nrow(m0))
  colnames(m0) <- rownames(m0) <- names0
  m0
}


map_cols <- function(m, f, mapfun = purrr::map) {
  mapfun(seq_len(ncol(m)), ~f(m[,.x]))
}


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


#' @keywords internal
which_max <- function(vec0) {
  min(which(vec0 == max(vec0)))
}


#' @keywords internal
which_min <- function(vec0) {
  min(which(vec0 == min(vec0)))
}


#' @keywords internal
which_NA <- function(vec0) {
  which(is.na(vec0))
}


#' #' @keywords internal
#' is.fitted <- function(table0) {
#'   attributes(table0)$beta == "fitted"
#' }


#' #' @keywords internal
#' dummy2factor <- function(m0, factor_name, factor_labels) {
#'   res <- apply(m0, 1, function(x) which(x == 1)) %>%
#'     factor(labels = factor_labels) %>% data.frame()
#'   colnames(res) <- factor_name
#'   res
#' }


# create_dummy <- function(df0) {
#   map_chr(df0)
# }


#' #' @keywords internal
#' permute_column <- function(data0) {
#'   data0[,sample(ncol(data0))]
#' }
