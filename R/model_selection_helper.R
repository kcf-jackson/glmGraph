#' Detect variable type and decide what family of distribution to use
#' @keywords internal
#' @param x0; data vector
analyze_variable <- function(x0, threshold = 5) {
  variable_range <- length(unique(x0))
  if (variable_range == 2) {
    return("binomial")   #binary data
  } else {
    if (is.factor(x0) | (variable_range < threshold)) {
      return("multinomial")
    } else if (is.numeric(x0)) {
      if (all(is.wholenumber(x0))) {
        return("poisson")  #count data
      } else {
        if (all(x0 > 0)) {
          return("gamma")
        } else {
          return("gaussian")
        }
      }
    }
  }
  return("unknown")
}


#' @keywords internal
check_family <- function(family) {
  unknown_column <- which(family == "unknown")
  warning_msg <- paste(
    "I have problems figuring out what type of variables the columns",
    paste(unknown_column, collapse = ","), "have.", sep = " "
  )
}


#' @keywords internal
flip_bit <- function(x) { abs(x - 1) }
