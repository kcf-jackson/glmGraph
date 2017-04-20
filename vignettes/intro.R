## ---- eval = F, echo = F-------------------------------------------------
#  # codes to produce the example graph
#  rm(list = ls())
#  library(glmGratph)
#  num_nodes <- 10
#  rgraph <- create_random_graph(num_nodes, p = 0.2)
#  plot_graph(rgraph, vertex.size = 30, vertex.label = paste("X", 1:10, sep = ""))

## ---- echo = F, out.width = "400px", fig.cap='Figure 1. An example of a graph.'----
knitr::include_graphics("graph_example_1.png")

## ---- eval=T, fig.height=3, fig.width=6.5--------------------------------
library(magrittr)
library(glmGraph)

# Create a random graph
num_nodes <- 10
graph_connectedness <- 0.2  #probability of having an edge between two nodes
rgraph <- create_random_graph(num_nodes, p = graph_connectedness)
print(rgraph)

# Plot graph
plot_graph(rgraph, vertex.size = 50)  #vertex.size is the plot-size of the nodes

## ---- eval=T, fig.height=4, fig.width=4----------------------------------
# Specify the GLM family for the dependencies 
# 1. factorise the graph
table0 <- factorise(rgraph)
print(table0)

# 2. specify a GLM family for each component
family <- c(rep("gaussian", 7), rep("gamma", 3))
table0 <- build_conditional(table0, family = family)

## ---- eval=T, fig.height=4, fig.width=4----------------------------------
# Simulate data
data0 <- simulate_data(table0, n = 2000)  # generate 2000 datapoints
dim(data0)
head(data0)

## ---- eval=T, fig.height=4, fig.width=4----------------------------------
# Create a new table to fit the data 
table1 <- rgraph %>% factorise() %>% build_conditional(family)
table1 <- MLE_graph(table1, data0)

## ---- eval = F-----------------------------------------------------------
#  g <- select_graph(data0)
#  # extract graph
#  best <- g$best_model
#  est_graph <- best$rgraph
#  # refit the data with known graph structure
#  table2 <- est_graph %>% factorise() %>% build_conditional(best$family) %>% MLE_graph(data0)

## ------------------------------------------------------------------------
rm(list = ls())    #clear up the workspace
library(magrittr)
library(glmGraph)

# Create a graph randomly
g <- create_random_graph(10, 0.2)

# Give full specification
family <- c(rep("gaussian", 7), rep("gamma", 3))
t0 <- g %>% factorise() %>% build_conditional(family = family)

# Simulate data
data0 <- simulate_data(t0, n = 1000)

## ------------------------------------------------------------------------
# Fitting the data
t1 <- g %>% factorise() %>% build_conditional(family = family) %>% MLE_graph(data0)

## ---- eval = FALSE-------------------------------------------------------
#  # Comparing the estimated model to the true model
#  data.frame(cbind(t0$beta, t1$beta))

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(data.frame(cbind(
  true_beta = t0$beta %>% purrr::map(~round(.x, 4)), 
  fitted_beta = t1$beta %>% purrr::map(~round(.x, 4))
)))

## ------------------------------------------------------------------------

## ---- eval = F-----------------------------------------------------------
#  # Create graph
#  num_nodes <- 10
#  rgraph <- create_random_graph(num_nodes, p = 0.2)
#  
#  m <- 8
#  family <- c(rep("gaussian", m), rep("gamma", num_nodes - m))
#  
#  table0 <- rgraph %>% factorise() %>% build_conditional(family = family)
#  data0 <- simulate_data(table0, n = 1000)
#  
#  s <- learn_graph(data0, num_iter = 200, graph_init = "mutual")
#  plot(compare_graphs(rgraph, s$best_model$rgraph))
#  plot(compare_graphs(rgraph, s$freq_graph$rgraph > 0.75))
#  
#  s2 <- learn_graph_by_CE(data0)
#  plot(compare_graphs(rgraph, s2$graph > 0.5))

