#' This function generates a random directed acyclic graph (DAG) in adjacency matrix notation.
#' @param num_nodes Integer; number of nodes available to be connected..
#' @param p p Between 0 and 1; a parameter to control number of edges to be generated.
#' The higher this number is, the more connected the graph will be.
#' @export
random_DAG <- function(num_nodes, p = 0.5) {
  mp <- c(1 - p, p / 2, p / 2)  # multinomial probabilities
  draws <- sample(0:2, num_nodes * (num_nodes - 1) / 2, prob = mp, replace = T)
  m0 <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  m0[upper.tri(m0)] <- draws
  m0 <- to_nominal_form(m0)

  not_DAG <- . %>% add_names() %>% gRbase::topoSort() %>% purrr::is_empty()
  while (not_DAG(m0)) {
    m0 <- random_DAG(num_nodes, p)
  }
  m0
}


# Nominal form refers to a full matrix with only 0, 1 entries.
# This function converts upper-triangular matrix with entries 0,1,2 to full matrix with 0,1.
to_nominal_form <- function(m0) {
  m1 <- m0
  m1[m1 > 0] <- m1[m1 > 0] - 1
  m0 - 2 * m1 + t(m1)
}


# This function converts full matrix with 0,1 to upper-triangular matrix with entries 0,1,2.
from_nominal_form <- function(m0) {
  m1 <- m0
  m0[lower.tri(m0)] <- 0
  m1[upper.tri(m1)] <- 0
  m0 + 2 * t(m1)
}


#' @keywords internal
table_to_edgelist <- function(table0) {
  edgelist <- c()
  for (i in 1:nrow(table0)) {
    current <- table0[i,]
    fixed <- current$fixed[[1]]
    given <- current$given[[1]]
    if (!purrr::is_empty(given)) {
      edgelist <- rbind(edgelist, cbind(fixed, given))
    }
  }
  edgelist
}


#' Compare two graphs
#' @description This function compares two graphs and labels the matched / unmatched
#' edges with different colors.
#' @param mode If "directed", the function uses only the upper triangular part of the
#' adjacency matrix; otherwise, the function uses the full matrix.
#' @param m1 adjacency matrix
#' @param m2 adjacency matrix
#' @param color_vec characters vector; see description for details.
#' @details By default, green color applies to edges that are present in both graphs;
#' black color applies to edges that are present in the first graph but not the second
#' graph; red color applies to edges that are present in the second graph but
#' not the first graph.
#' @export
compare_graphs <- function(m1, m2,  mode = "undirected",
                           color_vec = c("green", "black", "red")) {
  num_nodes <- nrow(m1)

  m3 <- m1 + m2
  m3[m3 != 2] <- 0  #matched edges
  matched_graph <- adj_to_graph(m3 / 2, mode)
  igraph::V(matched_graph)$name <- paste("X", seq(num_nodes), sep = "")
  igraph::E(matched_graph)$color <- "green"

  m4 <- m1 + m3 - m2
  m4[m4 != 1] <- 0
  unmatched_graph <- adj_to_graph(m4, mode)  #unmatched edges
  igraph::V(unmatched_graph)$name <- paste("X", seq(num_nodes), sep = "")
  igraph::E(unmatched_graph)$color <- "black"

  m5 <- m2 + m3 - m1
  m5[m5 != 1] <- 0
  extra_graph <- adj_to_graph(m5, mode)  #extra edges
  igraph::V(extra_graph)$name <- paste("X", seq(num_nodes), sep = "")
  igraph::E(extra_graph)$color <- "red"

  res_graph <- matched_graph + unmatched_graph + extra_graph
  merge_color_attr(res_graph)
}
#' @keywords internal
merge_color_attr <- function(graph) {
  res <- igraph::E(graph)$color
  c1 <- igraph::E(graph)$color_1
  replace_index <- which(!is.na(c1))
  res[replace_index] <- c1[replace_index]

  c2 <- igraph::E(graph)$color_2
  replace_index <- which(!is.na(c2))
  res[replace_index] <- c2[replace_index]

  igraph::E(graph)$color <- res
  graph
}
#' Convert a adjacency matrix to graph.
#' @description This is used mainly for converting the estimated matrix to a graph.
#' Using the lower triangular matrix corresponds to following the factorisation
#' in the natural order, i.e. from the first column to the last column of the data.
#' @keywords internal
adj_to_graph <- function(adj_matrix, mode) {
  igraph::graph_from_adjacency_matrix(
    adj_matrix * lower.tri(adj_matrix), mode = mode
  )
}
