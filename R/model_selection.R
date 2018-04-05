#' Model selection for GLM graph
#' @param data0 dataframe; the data.
#' @param p Between 0 and 1; prior brief of how connected the graph is.
#' @param lambda Tuning parameter for gibbs sampler.
#' @param num_iter number of iterations for gibbs sampler.
#' @param burn_ins number of burn-ins for gibbs sampler.
#' @param graph_init characters string; one of "random", "correlation" and "mutual".
#' "random" generates a graph randomly. "correlation" computes the pairwise correlation
#' between variables and keeps the ones above the third quartile. "mutual" is similar to
#' "correlation" except it uses pairwise mutual information instead.
#' @param reg Information criteria for regularisation; "AIC" or "BIC".
#' @param ... Extra parameters to be passed to speedglm::speedglm.
#' @export
learn_graph <- function(data0, p = 0.2, lambda, num_iter = 100, burn_ins = 10,
                         graph_init = "random", reg = "BIC", ...) {
  if (!all(sapply(head(data0), is.numeric))) {
    stop("data has to be all numerics at the moment.")
  }
  if (missing(lambda)) {
    lambda <- 1 / sqrt(nrow(data0))
  }
  num_data <- nrow(data0)
  if (reg == "BIC") {
    reg_FUN <- BIC_like
    IC_factor <- log(num_data)
  } else if (reg == "AIC") {
    reg_FUN <- AIC_like
    IC_factor <- 2
  } else {
    stop("Regularisation function must be one of 'AIC' and 'BIC'.")
  }
  num_var <- ncol(data0)
  rgraph <- random_DAG(num_var, p = p)
  nr <- nrow(rgraph)
  nc <- ncol(rgraph)
  family <- apply(data0, 2, analyze_variable)
  if ("unknown" %in% family) {
    stop(check_family(family))
  }

  # Helper functions
  essential_spec <- function(rgraph, family) {
    data.frame(factorise(rgraph), family, stringsAsFactors = F)
  }
  evaluate_graphs <- function(rgraph, i, j, current_likelihood, current_fac, ...) {
    likelihood <- numeric(3)
    eval_like_if_DAG <- function(direction, i, j, ...) {
      ifelse(
        check_graph(rgraph, i, j, direction),
        add_new_likelihood(current_fac[i,], j, ...),
        -Inf
      )
    }
    if ((rgraph[i,j] == 0) && (rgraph[j,i] == 0)) {
      likelihood[1] <- current_likelihood
      likelihood[2] <- likelihood[1] + eval_like_if_DAG(1, j, i, ...)
      likelihood[3] <- likelihood[1] + eval_like_if_DAG(2, i, j, ...)
    } else if (rgraph[i,j] == 1) {
      likelihood[2] <- current_likelihood
      likelihood[1] <- likelihood[2] + add_new_likelihood(current_fac[j,], i, ...)
      likelihood[3] <- likelihood[1] + eval_like_if_DAG(2, i, j, ...)
    } else if (rgraph[j,i] == 1) {
      likelihood[3] <- current_likelihood
      likelihood[1] <- likelihood[3] + add_new_likelihood(current_fac[i,], j, ...)
      likelihood[2] <- likelihood[1] + eval_like_if_DAG(1, j, i, ...)
    }
    likelihood
  }
  check_graph <- function(rgraph, i, j, direction) {
    not_DAG <- . %>% add_names() %>% gRbase::topoSort() %>% purrr::is_empty()
    new_graph <- gibbs_graph(direction + 1, rgraph, i, j)
    !not_DAG(new_graph)
  }
  add_new_likelihood <- function(current, j, ...) {
    fixed <- current$fixed[[1]]
    given <- current$given[[1]]
    family <- current$family[[1]]
    current_marginal_likelihood <- fit_glm(
      y = data0[,fixed], x = cbind(intercept = 1, data0[,given]),
      family = family, engine = speedglm::speedglm.wfit, ...
    )$logLik

    state <- j %in% given
    if (state) {
      new_given <- setdiff(given, j)
    } else {
      new_given <- sort(c(given, j))
    }
    new_marginal_likelihood <- fit_glm(
      y = data0[,fixed], x = cbind(intercept = 1, data0[,new_given]),
      family = family, engine = speedglm::speedglm.wfit, ...
    )$logLik

    edge_num_adjustment <- ifelse(state, 1, -1) * IC_factor
    new_marginal_likelihood - current_marginal_likelihood + edge_num_adjustment
  }

  current_likelihood <- rgraph %>%
    fit_graph(family, data0) %>%
    get_model_likelihood() %>%
    magrittr::add(reg_FUN(n = num_data, k = sum(rgraph) / 2))
  current_factorisation <- essential_spec(rgraph, family)
  # print(current_likelihood)
  #---------------Variables for model selection--------------------
  best_measure_graph <- list(rgraph = rgraph, score = current_likelihood)
  frequency_graph <- list(rgraph = matrix(0, nr, nc), score = NA)
  #----------------------------------------------------------------

  # Gibbs model selection
  pb <- txtProgressBar(1, num_iter, style = 3)
  for (iter in 1:num_iter) {
    for (i in 1:(nr - 1)) {
      for (j in (i+1):nc) {
        # Evaluate model M0, M1, M2
        jump_likelihood <- evaluate_graphs(rgraph, i,j, current_likelihood, current_factorisation, ...)
        # keep track with the best model (for potential future use and comparison)
        best_measure_graph <- update_best(best_measure_graph, rgraph, i, j, jump_likelihood)
        jump <- gibbs_update(lambda, jump_likelihood)
        rgraph <- gibbs_graph(jump, rgraph, i, j)
        current_likelihood <- jump_likelihood[jump]
        current_factorisation <- essential_spec(rgraph, family)
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


gibbs_update <- function(lambda, score_vec) {
  score_vec <- score_vec - max(score_vec)   #protect against divide by 0
  prob_vec <- exp(lambda * score_vec)
  prob_vec <- prob_vec / sum(prob_vec)
  sample(seq_along(score_vec), size = 1, prob = prob_vec)
}

gibbs_graph <- function(jump, rgraph, i, j) {
  rgraph[i,j] <- rgraph[j,i] <- 0
  if (jump == 2) {
    rgraph[i,j] <- 1
  } else if (jump == 3) {
    rgraph[j,i] <- 1
  }
  rgraph
}

update_best <- function(best_measure_graph, rgraph, i, j, jump_likelihood) {
  max_ind <- which_max(jump_likelihood)
  new_likelihood <- jump_likelihood[max_ind]
  new_graph <- gibbs_graph(max_ind, rgraph, i, j)
  has_improved <- (new_likelihood > best_measure_graph$score)
  if (has_improved) {
    # cat("Improved! Loglikelihood:", new_likelihood, "\n")
    # plot_graph(rgraph)
    best_measure_graph$rgraph <- new_graph
    best_measure_graph$score <- new_likelihood
  }
  best_measure_graph
}


#' Wrapper function for 'factorise', 'build_conditional' and 'MLE_graph'
#' @description This function takes a graph and fits the data with the family
#' specification.
#' @param rgraph matrix; a graph encoded in adjacency matrix.
#' @param family vector of characters string; the family for each variable.
#' @param data0 dataframe; the data.
#' @export
fit_graph <- function(rgraph, family, data0) {
  full_spec <- build_conditional(factorise(rgraph), family)
  MLE_graph(full_spec, data0)
}


# #' @keywords internal
# compute_GLM_full_class_likelihood <- function(data0) {
#   "gamma", gamma_deriv2, gamma_deriv3,
#   "poisson", poisson_deriv2, poisson_deriv3,
#   "binomial", binomial_deriv2, binomial_deriv3
#   return(list(family_name, likelihood))
# }


#' #' @keywords internal
#' initialise_graph <- function(data0, method = "random", threshold = 0.75) {
#'   method <- tolower(method)
#'   if (!(method %in% c("random", "correlation", "mutual", "copula"))) {
#'     stop("The method must be one of 'random', 'correlation', 'mutual' and 'copula.")
#'   }
#'   num_nodes <- ncol(data0)
#'   if (method == "random") {
#'     g <- random_DAG(num_nodes, p = min(0.5, 2 / num_nodes^2))
#'     return(g)
#'   } else {
#'     g <- compute_distance_matrix(data0, method)
#'   }
#'   g <- (g > quantile(g, threshold))
#'   diag(g) <- 0
#'   g
#' }


#' #' @keywords internal
#' compute_distance_matrix <- function(data0, method) {
#'   if (method == "correlation") {
#'     g <- cor(data0)
#'     diag(g) <- -1
#'   } else if (method == "mutual") {
#'     g <- data0 %>% infotheo::discretize() %>% infotheo::mutinformation()
#'     diag(g) <- 0
#'   } else if (method == "copula") {
#'     g <- copula_cor(data0)
#'   }
#'   g
#' }
