#' Model selection for GLM graphical model using cross-entropy method
#' @param data0 dataframe; the data.
#' @param rho Between 0 and 1, samples with a "score" above the (1-rho)-quantile are
#' considered as good samples.
#' @param batch_size Number of samples to generate in each iteration.
#' @param tol Tolerence of the threshold for convergence
#' @param alpha Smooth-update parameter between 0 and 1. When alpha = 1, the update uses
#' only the current estimate; when alpha = 0.5, the update uses half of the current estimate
#' and half of the last estimate.
#' @param graph_init characters string; one of "random", "correlation" and "mutual".
#'"random" generates a graph randomly. "correlation" computes the pairwise correlation
#'between variables and keeps the ones above the third quartile. "mutual" is similar to
#'"correlation" except it uses pairwise mutual information instead.
#' @param threshold if TRUE, the function returns a graph; if FALSE, the function returns
#' the underlying Bernoulli probabilities.
#' @export
learn_graph_by_CE <- function(data0, rho = 0.1, batch_size = 50, tol = 1e-05,
                              alpha = 1, graph_init = "random", threshold = T) {
  if (!all(sapply(head(data0), is.numeric))) {
    stop("data has to be all numerics at the moment.")
  }
  num_var <- ncol(data0)
  family <- apply(data0, 2, analyze_variable)
  if ("unknown" %in% family) {
    stop(check_family(family))
  }
  empty_matrix <- matrix(0, nrow = num_var, ncol = num_var)
  upper_index <- upper.tri(empty_matrix)
  random_samples <- matrix(0, nrow = batch_size, ncol = length(prob_vec))
  score <- numeric(batch_size)

  # Cross-entropy method
  prob_vec <- rep(0.5, (num_var * (num_var - 1) / 2))
  for (i in 1:batch_size) {
    rgraph <- sample_random_matrix(empty_matrix, prob_vec)
    random_samples[i,] <- rgraph[upper_index]
    score[i] <- get_model_likelihood(fit_graph(rgraph, family, data0)) - sum(rgraph)
  }
  last_threshold <- compute_threshold(score, rho)
  prob_vec <- (1 - alpha) * prob_vec +
    alpha * update_prob(prob_vec, random_samples, score, last_threshold)
  # Iterate until convergence
  not_converged <- TRUE
  count <- 0
  converge_count <- 0
  while (not_converged) {
    for (i in 1:batch_size) {
      rgraph <- sample_random_matrix(empty_matrix, prob_vec)
      random_samples[i,] <- rgraph[upper_index]
      score[i] <- get_model_likelihood(fit_graph(rgraph, family, data0)) - sum(rgraph)
    }
    threshold <- compute_threshold(score, rho)
    prob_vec <- (1 - alpha) * prob_vec +
      alpha * update_prob(prob_vec, random_samples, score, threshold)
    # update looping information
    not_converged <- (abs(threshold - last_threshold) > tol) | (converge_count <= 3)
    converge_count <- ifelse(abs(threshold - last_threshold) <= tol,
                             converge_count + 1, 0)
    last_threshold <- threshold
    count <- count + 1
    print(sprintf(
      "This is iteration %d. The %.2f-quantile of the samples' likelihood is %.2f.",
      count, 1 - rho, threshold
    ))
  }
  print("Converged.")
  list(graph = vec2mat(prob_vec > 0.5, num_var), family = family)
}


#' Sample based on a matrix of probabilities
#' @param matrix0 empty_matrix
#' @param prob_vec Should have length (d * (d-1) / 2)
#' @keywords internal
sample_random_matrix <- function(matrix0, prob_vec) {
  entries <- purrr::map_int(prob_vec, ~sample(0:1, 1, prob = c(1 - .x, .x)))
  matrix0[upper.tri(matrix0)] <- entries
  matrix0 + t(matrix0)
}

#' @keywords internal
compute_threshold <- function(score_vec, rho) {
  quantile(score_vec, 1 - rho)
}

#' @keywords internal
update_prob <- function(prob_vec, samples_matrix, score_vec, threshold) {
  #assume the goal is maximisation
  keep <- which(score_vec > threshold)
  if (purrr::is_empty(keep)) return(prob_vec)
  colMeans(samples_matrix[keep, ])
}

#' @keywords internal
vec2mat <- function(vec0, dim) {
  res <- matrix(0, nrow = dim, ncol = dim)
  res[lower.tri(res)] <- vec0
  res + t(res)
}
