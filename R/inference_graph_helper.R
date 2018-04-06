#' Detect variable type and decide what family of distribution to use
#' @keywords internal
#' @param x0; data vector
analyze_variable <- function(x0, threshold = 5) {
  variable_range <- length(unique(x0))
  if (variable_range == 2) {
    return("binomial")   #binary data
  } else {
    if (is.factor(x0) | (variable_range < threshold)) {
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
check_family <- function(family) {
  unknown_column <- which(family == "unknown")
  warning_msg <- paste(
    "I have problems figuring out what type of variables the columns",
    paste(unknown_column, collapse = ","), "have.", sep = " "
  )
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


#' Get likelihood from a fitted model
#' @param table0 data.frame; the fitted graph from 'MLE_graph'.
#' @return The loglikelihood value.
#' @export
get_model_likelihood <- function(table0) {
  attr(table0, "loglikelihood")
}


bind_graph_with_family <- function(rgraph, family) {
  data.frame(factorise(rgraph), family, stringsAsFactors = F)
}


evaluate_graphs <- function(data0, rgraph, i, j, current_likelihood, current_fac, IC_factor, ...) {
  likelihood <- numeric(3)
  eval_like_if_DAG <- function(direction, i, j, ...) {
    ifelse(
      check_graph(rgraph, i, j, direction),
      add_new_likelihood(current_fac[i,], j, ...),
      -Inf
    )
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


check_graph <- function(rgraph, i, j, direction) {
  not_DAG <- . %>% add_names() %>% gRbase::topoSort() %>% purrr::is_empty()
  new_graph <- gibbs_graph(direction + 1, rgraph, i, j)
  !not_DAG(new_graph)
}
