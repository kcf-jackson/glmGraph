#' Model selection for GLM graph
#' @param data0 dataframe; the data.
#' @param p Between 0 and 1; prior brief of how connected the graph is. If init_rgraph is
#' specified, then this is not used.
#' @param lambda Tuning parameter for gibbs sampler.
#' @param num_iter number of iterations for gibbs sampler.
#' @param burn_ins number of burn-ins for gibbs sampler.
#' @param init_rgraph adjacency matrix (optional); the graph to begin estimation with.
#' @param reg Information criteria for regularisation; "AIC" or "BIC".
#' @param ... Extra parameters to be passed to speedglm::speedglm.
#' @export
learn_graph <- function(data0, p = 0.2, init_rgraph, lambda, reg = "BIC",
                        num_iter = 100, burn_ins = 10, ...) {
  # ---------- Setup and safeguarding ----------
  num_data <- nrow(data0)
  num_var <- ncol(data0)
  nr <- nrow(rgraph)
  nc <- ncol(rgraph)
  family <- apply(data0, 2, analyze_variable)
  if ("unknown" %in% family) {
    stop(check_family(family))
  }
  if (!all(sapply(head(data0), is.numeric))) {
    stop("Data have to be all numerics.")
  }
  if (missing(init_rgraph)) {
    rgraph <- random_DAG(num_var, p = p)
  } else {
    rgraph <- init_rgraph
  }
  if (missing(lambda)) {
    lambda <- 1 / sqrt(nrow(data0))
  }
  if (reg == "BIC") {
    reg_FUN <- BIC_like
    IC_factor <- log(num_data)
  } else if (reg == "AIC") {
    reg_FUN <- AIC_like
    IC_factor <- 2
  } else {
    stop("Regularisation function must be one of 'AIC' and 'BIC'.")
  }
  current_likelihood <- fit_graph(rgraph, family, data0) %>%
    get_model_likelihood() %>%
    magrittr::add(reg_FUN(n = num_data, k = sum(rgraph) / 2))
  current_factorisation <- bind_graph_with_family(rgraph, family)
  # Variables for model selection
  best_measure_graph <- list(rgraph = rgraph, score = current_likelihood)
  frequency_graph <- list(rgraph = matrix(0, nr, nc), score = NA)

  #----------------------------------------------------------------
  # Gibbs model selection
  pb <- txtProgressBar(1, num_iter, style = 3)
  for (iter in 1:num_iter) {
    for (i in 1:(nr - 1)) {
      for (j in (i+1):nc) {
        # Evaluate model M0, M1, M2
        jump_likelihood <- evaluate_graphs(data0, rgraph, i, j, current_likelihood,
                                           current_factorisation, IC_factor, ...)
        # Keep track of the best model (for potential future use and comparison)
        best_measure_graph <- update_best(best_measure_graph, rgraph, i, j, jump_likelihood)

        # Gibbs transition
        jump <- gibbs_update(lambda, jump_likelihood)
        rgraph <- gibbs_graph(jump, rgraph, i, j)
        current_likelihood <- jump_likelihood[jump]
        current_factorisation <- bind_graph_with_family(rgraph, family)
      }
    }
    #-------------------Update frequency graph---------------------
    if (iter > burn_ins) {
      frequency_graph$rgraph <- frequency_graph$rgraph + rgraph
    }
    #--------------------------------------------------------------
    setTxtProgressBar(pb, iter)
  }
  best_measure_graph$family <- family
  frequency_graph$rgraph <- frequency_graph$rgraph / (num_iter - burn_ins)
  frequency_graph$family <- family
  list(best_model = best_measure_graph, freq_graph = frequency_graph)
}

