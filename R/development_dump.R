# DUMP GRAPH
#' #' This function generates a random graph in adjacency matrix notation.
#' #' @param num_nodes Integer; number of nodes available to be connected..
#' #' @param min_edge Integer or a vector of integers; minimum number(s) of edges for each node.
#' #' @param max_edge Integer or a vector of integers; maximum number(s) of edges for each node.
#' #' @export
#' create_random_graph <- function(num_nodes, min_edge = 1, max_edge) {
#'   # safeguard
#'   if (missing(max_edge)) max_edge <- num_nodes
#'   min_l <- length(min_edge)
#'   max_l <- length(max_edge)
#'   if (min_l != max_l) {
#'     stop("min_edge and max_edge must have the same length.")
#'   }
#'   if ((min_l != 1) & (min_l != num_nodes)) {
#'     stop("min_edge can only have 1 or num_nodes elements.")
#'   }
#'   if ((max_l != 1) & (max_l != num_nodes)) {
#'     stop("max_edge can only have 1 or num_nodes elements.")
#'   }
#'   # setup
#'   if (min_l == 1) {
#'     min_edge <- rep(min_edge, num_nodes)
#'   }
#'   if (max_l == 1) {
#'     max_edge <- rep(max_edge, num_nodes)
#'   }
#'   # generate graph
#'   seq(num_nodes - 1) %>%
#'     purrr::map(
#'       ~create_random_edges(
#'         num_nodes = num_nodes + 1 - .x, zero_pos = .x,
#'         min_edge = min_edge[.x], max_edge = max_edge[.x]
#'       )
#'     ) %>%
#'     do.call(rbind, .)
#' }
#'
#'
#' # Experimental functions
# expand_edges <- function(edges, zero_pos, min_edge = 1) {
#   # safeguard
#   if (length(edges) - 1 < min_edge) {
#     stop("It is impossible to have more edges than the available nodes.")
#   }
#   # expand edges
#   lack_edges <- (sum(edges) < min_edge)
#   while (lack_edges) {
#     num_to_add <- min_edge - sum(edges)
#     index_to_add <- which(edges == 0) %>% sample(num_to_add)
#     edges[index_to_add] <- 1
#     # Make sure a node doesn't have a edge to itself
#     edges[zero_pos] <- 0
#     lack_edges <- (sum(edges) < min_edge)
#   }
#   edges
# }
#
#
# trim_edges <- function(edges, zero_pos, max_edge) {
#   # safeguard
#   if (missing(max_edge)) {
#     max_edge <- length(edges) - 1
#   } else if (max_edge < 0) {
#     warning("It is impossible to have less than 0 edges. max_edge is set to 0.")
#     max_edge <- 0
#   }
#   # trim edges
#   too_many_edges <- (sum(edges) > max_edge)
#   while (too_many_edges) {
#     num_to_cut <- min_edge - sum(edges)
#     index_to_add <- which(edges == 1) %>% sample(num_to_cut)
#     edges[index_to_add] <- 0
#     too_many_edges <- (sum(edges) > max_edge)
#   }
#   edges
# }

#'
#' #' This function creates restricted number of edges randomly
#' #' @param num_nodes Integer; number of nodes available to be connected.
#' #' @param zero_pos The position of the node that shouldn't have an edge.
#' #' @param min_edge Integer; minimum number of edges.
#' #' @param max_edge Integer; maximum number of edges.
#' #' @keywords internal
#' create_random_edges <- function(num_nodes, zero_pos, min_edge, max_edge) {
#'   edges <- create_random_edges_core(num_nodes, zero_pos)
#'   total_edges <- sum(edges)
#'   while ( (total_edges > max_edge) | (total_edges < min_edge) ) {
#'     edges <- create_random_edges_core(num_nodes, zero_pos, p = max_edge / num_nodes)
#'     total_edges <- sum(edges)
#'   }
#'   edges
#' }
