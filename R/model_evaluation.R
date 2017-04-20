#' Edge-count tests
#'
#' Test if `df1` and `df2` come from the same distribution. This is a wrapper
#' of `g.tests` in which the edge matrix of the similarity graph is based on a
#' minimum spanning tree of the combined data frame.
#'
#' @param df1,df2 dataframes
#' @param method distance measure. Must be one of "euclidean" (default),
#'   "maximum", "manhattan", "canberra", "binary" or "minkowski". See `method`
#'   in `?dist`.
#' @examples
#' ## Expect failure to reject null.
#' g_tests(data.frame(x = rnorm(100)), data.frame(x = rnorm(100)))
#'
#' ## Expect rejection of the null.
#' g_tests(data.frame(x = rnorm(100)), data.frame(x = runif(100)))
#'
#' @export
g_tests <- function(df1, df2, method = "euclidean") {
  df <- dplyr::bind_rows(df1, df2)
  dis <- stats::dist(df, method = method)
  mst <- vegan::spantree(dis)

  n <- nrow(df1)
  E <- matrix(c(2:nrow(df), mst$kid), ncol = 2, byrow = FALSE)
  gTests::g.tests(E, sample1ID = 1:(n - 1), sample2ID = n:nrow(df))
}
