#' Learn graph structure using the independence-based approach
#' @param data0 dataframe; the data.
#' @param method One of 'random', 'correlation', 'mutual' and 'copula".
#' @param lambda Tuning parameter for gibbs sampler.
#' @param num_iter number of iterations for gibbs sampler.
#' @keywords internal
learn_graph_by_independence_Gibbs <- function(data0, method, lambda,
                                              num_iter = 100) {
  if (missing(lambda)) lambda <- 1 / sqrt(nrow(data0))
  num_var <- ncol(data0)
  family <- apply(data0, 2, analyze_variable)
  r_matrix <- data0 %>%
    compute_distance_matrix(method = method) %>%
    convert_distance_to_rank()
  nc <- ncol(r_matrix)

  coordinates <- num_var:2
  rgraph <- threshold_rank_matrix(r_matrix, coordinates)
  current_score <- get_model_likelihood(fit_graph(rgraph, family, data0)) - sum(rgraph)
  print(current_score)
  best_score <- current_score
  best_graph <- rgraph
  coordinates_table <- matrix(0, nrow = num_iter, ncol = num_var - 1)

  pb <- txtProgressBar(1, num_iter, style = 3)
  for (iter in 1:num_iter) {
    for (i in seq_along(coordinates)) {
      # check the number of possible transitions
      possible_threshold <- 1:(num_var + 1 - i)
      score <- numeric(length(possible_threshold))
      for (j in possible_threshold) {
        threshold <- coordinates
        threshold[i] <- j
        # For each possible threshold, create a graph and compute score
        temp_graph <- threshold_rank_matrix(r_matrix, threshold)
        score[j] <- get_model_likelihood(fit_graph(temp_graph, family, data0)) - sum(temp_graph)
        #---------- Keep track of the best graph ------------------------------
        if (score[j] > best_score) {
          best_score <- score[j]
          best_graph <- temp_graph
          cat("Improved! Loglikelihood:", best_score, "\n")
        }
        #----------------------------------------------------------------------
      }
      # Update probability and do gibbs update
      coordinates[i] <- gibbs_update(lambda, score)
    }
    setTxtProgressBar(pb, iter)
    coordinates_table[iter,] <- coordinates
  }

  final_rank <- apply(coordinates_table, 2, get_highest_count_element)
  rgraph <- threshold_rank_matrix(r_matrix, final_rank)
  list(estimated_graph = rgraph,
       best_model = list(rgraph = best_graph, score = best_score),
       freq_table = coordinates_table)
}

get_highest_count_element <- function(vec0) {
  which_max(table(vec0))
}
