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


#' Plot a DAG
#' @param adj_mat An adjacency matrix with columns named after the variables.
#' @examples
#' \dontrun{
#' m0 <- random_DAG(num_nodes = 6)
#' plot_DAG(m0)
#' }
#' @export
plot_DAG <- function(adj_mat) {
  num_nodes <- nrow(adj_mat)
  nodes_id <- seq(num_nodes)
  nodes_name <- paste0("node_", nodes_id)
  labels <- paste0("x_{", nodes_id, "}")

  if (is.null(rownames(adj_mat)) && is.null(colnames(adj_mat)))
    adj_mat <- add_names(adj_mat, nodes_name)

  make_edge <- function(A) {
    to_B <- names(which(adj_mat[A, ] == 1)) %>% paste(collapse = ", ")
    ifelse(to_B == "", "", paste(A, " -> ", to_B, ";", sep = ""))
  }
  join <- . %>% paste(collapse = "\n")

  list_of_nodes <- nodes_name %>% paste0(" [label = '$", labels, "$'];") %>% join()
  list_of_edges <- nodes_name %>% purrr::map_chr(make_edge) %>% join()

  g <- sprintf(
    "digraph boxes_and_circles { rankdir='LR'; node [shape = circle fontsize = 8] \n%s \n\n%s }",
    list_of_nodes, list_of_edges
  )
  DiagrammeR::add_mathjax(DiagrammeR::grViz(g))
}
