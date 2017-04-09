#' The function generates a factorisation based on a graph
#' @param rgraph matrix; graph represented by an adjacency matrix
#' @export
factorise <- function(rgraph) {
  num_nodes <- ncol(rgraph)
  joint <- seq(num_nodes)
  full_factorisation  <- array(list(), num_nodes + 1)
  # Iterative factorisation
  i <- 1
  fully_factorised <- (length(joint) == 0)
  while (!fully_factorised) {
    res <- joint %>% factorise_density()
    resc <- res$conditional
    full_factorisation[[i]] <- resc %>%
      simplify_conditional(rgraph[resc$fixed, ])
    joint <- res$joint
    # update loop conditions
    fully_factorised <- (length(joint) == 0)
    i <- i + 1
  }
  full_factorisation %>% do.call(rbind, .) %>% data.frame()
}


#==============================================================================
# Conditional X_A | X_B is expressed using two sets of indexes,
# where X_A is a single variable, and X_B is a vector of variables.
#==============================================================================
#' Factorise the joint density of k+1 variables into
#' conditional x joint density of k variables
#' @param joint_vec The indexes of variables in a joint density
#' @param keep_index The position of the variable to be kept
#' @keywords internal
factorise_density <- function(joint_vec, keep_pos = 1) {
  conditional <- list(
    fixed = joint_vec[keep_pos],
    given = joint_vec[-keep_pos]
  )
  joint <- joint_vec[-keep_pos]
  list(conditional = conditional, joint = joint)
}


#' This function simplifies conditionanl given the dependence information
#' @param conditional_list The conditional density. It should be a list of two vectors.
#' @param edges The vector containing the dependence information.
simplify_conditional <- function(conditional_list, edges) {
  conditional_list$given %<>% setdiff(which(edges == 0))
  conditional_list
}
