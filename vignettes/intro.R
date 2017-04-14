## ---- eval=T, results='asis', fig.height=4, fig.width=4------------------
library(magrittr)
library(glmGraph)

# Create a random graph
num_nodes <- 10
graph_connectedness <- 0.2  #probability of having an edge between two nodes
rgraph <- create_random_graph(num_nodes, p = graph_connectedness)
knitr::kable(rgraph)

# Plot graph
plot_graph(rgraph, vertex.size = 30)  #vertex.size is the plot-size of the nodes

## ---- eval=T, results='asis', fig.height=4, fig.width=4------------------
# Specify the dependence with the GLM family 
# 1. factorise the graph
table0 <- factorise(rgraph)
knitr::kable(table0)
# 2. specify a GLM family for each component
family <- c(rep("gaussian", 7), rep("gamma", 3))
table0 <- build_conditional(table0, family = family)

## ---- eval=T, results='asis', fig.height=4, fig.width=4------------------
# Simulate data
data0 <- simulate_data(table0, n = 2000)

## ---- eval=T, results='asis', fig.height=4, fig.width=4------------------
# Create a new table to fit the data
table1 <- rgraph %>% factorise() %>% build_conditional(family)
table1 %<>% MLE_graph(data0)

# Compare the truth and the fitted model
knitr::kable(data.frame(cbind(
    true_beta = table0$beta %>% purrr::map(~round(.x, 3)), 
    fitted_beta = table1$beta %>% purrr::map(~round(.x, 3))
)))

## ---- eval = F-----------------------------------------------------------
#  s <- select_graph(data0, lambda = 1 / sqrt(nrow(data0)))

