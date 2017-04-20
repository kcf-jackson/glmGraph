#' Model selection for GLM graph
#' @param data0 dataframe; the data.
#' @param p Between 0 and 1; prior brief of how connected the graph is.
#' @param lambda Tuning parameter for gibbs sampler.
#' @param num_iter number of iterations for gibbs sampler.
#' @param graph_init characters string; one of "random", "correlation" and "mutual".
#'"random" generates a graph randomly. "correlation" computes the pairwise correlation
#'between variables and keeps the ones above the third quartile. "mutual" is similar to
#'"correlation" except it uses pairwise mutual information instead.
#' @export
learn_graph <- function(data0, p = 0.2, lambda, num_iter = 100,
                         graph_init = "random") {
  if (!all(sapply(head(data0), is.numeric))) {
    stop("data has to be all numerics at the moment.")
  }
  if (missing(lambda)) {
    lambda <- 1 / sqrt(nrow(data0))
  }
  num_var <- ncol(data0)
  rgraph <- create_random_graph(num_var, p = p)
  nr <- nrow(rgraph)
  nc <- ncol(rgraph)
  family <- apply(data0, 2, analyze_variable)
  if ("unknown" %in% family) {
    stop(check_family(family))
  }
  current_model <- fit_graph(rgraph, family, data0)
  current_likelihood <- get_model_likelihood(current_model) - sum(rgraph)
  current_factorisation <- essential_spec(rgraph, family)
  print(current_likelihood)
  #---------------Variables for model selection--------------------
  best_measure_graph <- list(rgraph = rgraph, score = current_likelihood)
  frequency_graph <- list(
    rgraph = matrix(0, nrow = nr, ncol = nc), score = NA
  )
  #----------------------------------------------------------------
  # Gibbs model selection
  pb <- txtProgressBar(1, num_iter, style = 3)
  for (iter in 1:num_iter) {
    for (i in 1:(nr - 1)) {
      for (j in (i+1):nc) {
        rgraph[i,j] %<>% flip_bit()
        rgraph[j,i] <- rgraph[i,j]
        new_likelihood <- current_likelihood +
          add_new_likelihood(current_factorisation[i,], j, rgraph[i,j], data0)
        #-------------------Update best graph----------------------
        has_improved <- (new_likelihood > best_measure_graph$score)
        if (has_improved) {
          cat("Improved! Loglikelihood:", new_likelihood, "\n")
          plot_graph(rgraph)
          best_measure_graph$rgraph <- rgraph
          best_measure_graph$score <- new_likelihood
        }
        #----------------------------------------------------------
        jump <- gibbs_update(lambda, c(current_likelihood, new_likelihood))
        if (jump == 1) {
          rgraph[i,j] %<>% flip_bit()
          rgraph[j,i] <- rgraph[i,j]
        } else {
          current_likelihood <- new_likelihood
          current_factorisation <- essential_spec(rgraph, family)
        }
      }
    }
    #-------------------Update frequency graph---------------------
    frequency_graph$rgraph <- frequency_graph$rgraph + rgraph
    #--------------------------------------------------------------
    setTxtProgressBar(pb, iter)
  }
  best_measure_graph$family <- family
  frequency_graph$rgraph <- frequency_graph$rgraph / num_iter
  list(best_model = best_measure_graph, freq_graph = frequency_graph)
}


#' @keywords internal
initialise_graph <- function(data0, method = "random", threshold = 0.75) {
  method <- tolower(method)
  if (!(method %in% c("random", "correlation", "mutual", "copula"))) {
    stop("The method must be one of 'random', 'correlation', 'mutual' and 'copula.")
  }
  num_nodes <- ncol(data0)
  if (method == "random") {
    g <- create_random_graph(num_nodes, p = min(0.5, 2 / num_nodes^2))
  } else if (method == "correlation") {
    g <- cor(data0)
    diag(g) <- -1
    g <- (g > quantile(g, threshold))
    diag(g) <- 0
  } else if (method == "mutual") {
    g <- data0 %>% infotheo::discretize() %>% infotheo::mutinformation()
    diag(g) <- 0
    g <- (g > quantile(g, threshold))
    diag(g) <- 0
  } else if (method == "copula") {
    g <- copula_cor(data0)
    g <- (g > quantile(g, threshold))
  }
  g
}


#' @keywords internal
gibbs_update <- function(lambda, score_vec) {
  score_vec <- score_vec - score_vec[1]   #protect against divide by 0
  prob_vec <- exp(lambda * score_vec)
  prob_vec <- prob_vec / sum(prob_vec)
  sample(seq_along(score_vec), size = 1, prob = prob_vec)
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
essential_spec <- function(rgraph, family) {
  data.frame(factorise(rgraph), family, stringsAsFactors = F)
}


#' @keywords internal
fit_graph <- function(rgraph, family, data0) {
  full_spec <- build_conditional(factorise(rgraph), family)
  MLE_graph(full_spec, data0)
}


#' @keywords internal
flip_bit <- function(x) {
  abs(x - 1)
}


# #' @keywords internal
# compute_GLM_full_class_likelihood <- function(data0) {
#   "gamma", gamma_deriv2, gamma_deriv3,
#   "poisson", poisson_deriv2, poisson_deriv3,
#   "binomial", binomial_deriv2, binomial_deriv3
#   return(list(family_name, likelihood))
# }


#' Detect variable type and decide what family of distribution to use
#' @keywords internal
#' @param x0; data vector
analyze_variable <- function(x0) {
  variable_range <- length(unique(x0))
  if (variable_range == 2) {
    return("binomial")   #binary data
  } else {
    if (is.factor(x0)) {
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
add_new_likelihood <- function(current, j, state, data0) {
  fixed <- current$fixed[[1]]
  given <- current$given[[1]]
  family <- current$family[[1]]
  current_marginal_likelihood <- fit_glm(
    y = data0[,fixed], x = cbind(intercept = 1, data0[,given]),
    family = family, engine = speedglm::speedglm.wfit
  )$logLik

  if (state == 1) {
    new_given <- sort(c(given, j))  #Include a new edge in the graph
  } else {
    new_given <- setdiff(given, j)  #Exclude an existing edge from the graph
  }
  new_marginal_likelihood <- fit_glm(
    y = data0[,fixed], x = cbind(intercept = 1, data0[,new_given]),
    family = family, engine = speedglm::speedglm.wfit
  )$logLik

  edge_num_adjustment <- ifelse(state == 1, -2, 2)
  new_marginal_likelihood - current_marginal_likelihood + edge_num_adjustment
}

