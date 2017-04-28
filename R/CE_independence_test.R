#' Learn graph structure using the independence-based approach
#' @param data0 dataframe; the data.
#' @param method One of 'random', 'correlation', 'mutual' and 'copula".
#' @param rho Between 0 and 1, samples with a "score" above the (1-rho)-quantile are
#' considered as good samples.
#' @param batch_size Number of samples to generate in each iteration.
#' @param tol Tolerence of the threshold for convergence
#' @param alpha Smooth-update parameter between 0 and 1. When alpha = 1, the update uses
#' only the current estimate; when alpha = 0.5, the update uses half of the current estimate
#' and half of the last estimate.
#' @param reg Information criteria for regularisation; "AIC" or "BIC".
#' @keywords internal
learn_graph_by_independence_CE <- function(data0, method, batch_size = 500,
                                           rho = 0.1, tol = 1e-05, alpha = 1,
                                           reg = "BIC") {
  r_matrix <- data0 %>%
    compute_distance_matrix(method = method) %>%
    convert_distance_to_rank()
  num_data <- nrow(data0)
  reg_FUN <- initialise_reg_FUN(reg)
  num_var <- ncol(data0)
  family <- apply(data0, 2, analyze_variable)
  multinom_prob <- purrr::map(
    .x = rev(seq(num_var - 1)), .f = initialise_multinom_prob
  )
  # One row of 'num_var' thresholds for 'batch_size' many of samples
  threshold_samples <- matrix(0, nrow = batch_size, ncol = num_var - 1)
  score <- numeric(batch_size)

  last_threshold <- 0
  converged <- F
  converge_count <- 0
  count <- 0
  while (!converged) {
    for (i in 1:batch_size) {
      threshold_samples[i,] <- sample_rank_threshold(multinom_prob)
      rgraph <- threshold_rank_matrix(r_matrix, threshold_samples[i,])
      score[i] <- get_model_likelihood(fit_graph(rgraph, family, data0)) +
        reg_FUN(n = num_data, k = sum(rgraph) / 2)
    }
    # Take the threshold to be the 1 - rho quantile of the scores
    # Retain all the samples with score abaove threshold and update the multinomial
    # probabilities
    threshold <- compute_threshold(score, rho)
    best_sample <- which(score >= threshold)
    multinom_prob <- update_multinom_prob(
      multinom_prob, threshold_samples[best_sample, ], alpha
    )
    # Update looping information
    count <- count + 1
    print(sprintf(
      "This is iteration %d. The %.2f-quantile of the samples' likelihood is %.2f.",
      count, 1 - rho, threshold
    ))
    check_converged <- abs(threshold - last_threshold) <= tol
    converge_count <- ifelse(check_converged, converge_count + 1, 0)
    converged <- check_converged & (converge_count > 3)
    last_threshold <- threshold
  }
  rgraph <- threshold_rank_matrix(
    r_matrix, get_final_rank(multinom_prob)
  )
  rgraph
}


# ========== Functions for initialisation =====================================
#' @keywords internal
initialise_multinom_prob <- function(K) {
  rep(1 / K, K)
}

#' @keywords internal
convert_distance_to_rank <- function(dist_matrix) {
  for (i in 1:nrow(dist_matrix)) {
    dist_matrix[i,] <- rank(-dist_matrix[i,])
  }
  dist_matrix
}

# ========== Helper Functions =================================================
#' Sample a threshold for the rank
#' @keywords internal
sample_rank_threshold <- function(list0) {
  purrr::map_int(list0, ~sample(length(.x), 1, prob = .x))
}

#' Create a graph by applying the threshold to the rank matrix
#' @keywords internal
threshold_rank_matrix <- function(m0, threshold0) {
  nr <- nrow(m0)
  nc <- ncol(m0)
  for (i in 1:(nr-1)) {
    m0[i,1:i] <- 0
    m0[i,(i+1):nc] <- (m0[i,(i+1):nc] < threshold0[i])
  }
  m0[nr, ] <- 0
  m0 + t(m0)
}

#' Take the threshold samples, marginalise them and turn them into multinomial
#' probabilities. One row is one sample.
#' @keywords internal
update_multinom_prob <- function(prob_list0, t_matrix, alpha) {
  # take each column of the threshold matrix and turn it into a multinomial distribution
  num_var <- ncol(t_matrix)
  for (i in seq(num_var)) {
    last_prob <- prob_list0[[i]]
    new_prob <- head(column_to_prob(t_matrix[,i], num_var + 1), -i)
    prob_list0[[i]] <- (1 - alpha) * last_prob + alpha * normalise(new_prob)
  }
  prob_list0
}

#' Find the empirical multinomial distribution from a vector
#' @keywords internal
#' @param s number of classes
column_to_prob <- function(vec0, s) {
  as.numeric(normalise(table(c(vec0, seq(s))) - 1))
}

#' @keywords internal
get_final_rank <- function(prob_list0) {
  purrr::map_int(prob_list0, which_max)
}
