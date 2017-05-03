#' Copula correlation function
#' @references Copula Correlation: An Equitable Dependence Measure and
#' Extension of Pearson’s Correlation. https://arxiv.org/pdf/1312.7214.pdf
#' @references https://github.com/felixleungsc/copulr
#' @keywords internal
copula_cor <- function(data0) {
  proxy::dist(x = t(data0), method = ccore) %>% as.matrix()
}

#' @references https://github.com/felixleungsc/copulr
#' @keywords internal
ccore <- function(xin, yin) {
  n <- length(xin)
  maxc <- ccore0(1:n, 1:n, 200)
  minc <- minfc(n)

  s4 <- (ccore0(xin, yin, 200)$s2 - minc$s2) / (maxc$s2 - minc$s2)

  return(s4)
}

#' @references https://github.com/felixleungsc/copulr
#' @keywords internal
ccore0 <- function(x, y, m = 200) {
  n <- length(x)

  u <- rank(x, ties.method = "average") / (n + 1)
  v <- rank(y, ties.method = "average") / (n + 1)

  h <- 0.25 * n ^ (-1 / 4)
  l <- 0.25 * n ^ (-1 / 4)

  A <- matrix(0, m, m)
  pos <- (1:m) / (m + 1)

  for(k in 1:n) {
    ind_u <- (abs(u[k] - pos) <= h);
    ind_v <- (abs(v[k] - pos) <= l);
    A[ind_u, ind_v] <- A[ind_u, ind_v] + 1;
  }

  A <- A / (n * h * l * 4)

  s1 <- mean(mean(max(1 - A , 0)))
  s2 <- sum(sum(abs(A - 1))) / (2 * m ^ 2)

  return(list(s2 = s2, s1 = s1))
}

#' @references https://github.com/felixleungsc/copulr
#' @keywords internal
minfc <- function(n) {
  bw <- 0.25 * n ^ (-1 / 4)
  h <- bw
  k <- ceiling(2 * h * (n + 1))
  x0 <- floor(n / k)
  xin <- 1:n

  yin <- c()
  for(i in 1:k) {
    yin <- c(yin, i + k * (0:x0))
  }

  yin <- yin[yin <= n]
  s3 <- ccore0(xin, yin, 200)

  return(s3)
}

#' @keywords internal
basic_check <- function(x, y) {
  print("Expect same values")
  print(ccore(x, y))
  print(ccore(x^2, y))
  print(ccore(x, y^2))
  if (all(x > 0))
    print(ccore(log(x), exp(y)))
  if (all(y > 0))
    print(ccore(sqrt(x), exp(y + log(y))))
  print("Expect different values")
  print(ccore(x^2, sin(y)))
}
