#' Learn graph structure using the independence-based approach (coordinate descent)
#' @param data0 dataframe; the data.
#' @param method One of 'random', 'correlation', 'mutual' and 'copula".
#' @param reg Information criteria for regularisation; "AIC" or "BIC".
#' @export
learn_graph_by_independence_CD <- function(data0, method, reg = "BIC") {
  r_matrix <- data0 %>%
    compute_distance_matrix(method = method) %>%
    convert_distance_to_rank()
  num_data <- nrow(data0)
  reg_FUN <- initialise_reg_FUN(reg)
  num_var <- ncol(data0)
  family <- apply(data0, 2, analyze_variable)

  coordinates <- num_var:2
  best_graph <- threshold_rank_matrix(r_matrix, coordinates)
  best_score <- get_model_likelihood(fit_graph(best_graph, family, data0)) +
    reg_FUN(n = num_data, k = sum(rgraph) / 2)

  for (i in 1:(nrow(r_matrix) - 1)) {
    score <- numeric(num_var + 1 - i)
    for (j in 1:(num_var + 1 - i)) {
      coordinates[i] <- j
      tmp_graph <- threshold_rank_matrix(r_matrix, coordinates)
      score[j] <- get_model_likelihood(fit_graph(tmp_graph, family, data0)) +
        reg_FUN(n = num_data, k = sum(rgraph) / 2)
      if (score[j] > best_score) {
        best_score <- score[j]
        best_graph <- tmp_graph
        cat("Improved! Loglikelihood:", best_score, "\n")
      }
    }
    coordinates[i] <- which_max(score)
    rgraph <- threshold_rank_matrix(r_matrix, coordinates)
  }
  best_model = list(rgraph = best_graph, score = best_score)
}
