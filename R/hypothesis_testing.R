#' Graph-based two-sample tests
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
#' mst_test(data.frame(x = rnorm(100)), data.frame(x = rnorm(100)))
#'
#' ## Expect rejection of the null.
#' mst_test(data.frame(x = rnorm(100)), data.frame(x = runif(100)))
#'
#' @export
#' @references Friedman J. and Rafsky L. Multivariate generalizations of the
#' WaldWolfowitz and Smirnov two-sample tests. The Annals of Statistics,
#' 7(4):697-717, 1979.
#'
#' Chen, H. and Friedman, J. H. A new graph-based two-sample test for
#' multivariate and object data. Journal of the American Statistical
#' Association, 2016.
#'
#' Chen, H., Chen, X. and Su, Y. A weighted edge-count two sample test for
#' multivariate and object data. arXiv:1604.06515.
mst_test <- function(df1, df2, method = "euclidean") {
  df <- dplyr::bind_rows(df1, df2)
  dis <- stats::dist(df, method = method)
  mst <- vegan::spantree(dis)

  n <- nrow(df1)
  E <- matrix(c(2:nrow(df), mst$kid), ncol = 2, byrow = FALSE)
  gTests::g.tests(E, sample1ID = 1:(n - 1), sample2ID = n:nrow(df))
}
