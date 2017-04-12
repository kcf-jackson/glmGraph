## ---- eval=F-------------------------------------------------------------
#  library(magrittr)
#  library(glmGraph)
#  
#  # Create graph
#  rgraph <- create_random_graph(10, p = 0.2)
#  plot_graph(rgraph, vertex.size = 30)
#  
#  # Factorise the joint density into products of conditionals
#  table0 <- factorise(rgraph)
#  
#  # Specify the distribution of the conditionals
#  family <- rep("gaussian", nrow(table0))
#  table0 %<>% build_conditional(family = family)
#  
#  # Simulate data
#  data0 <- simulate_data(table0, n = 1000)

## ---- eval = F-----------------------------------------------------------
#  # Create a new table to fit data
#  table1 <- rgraph %>% factorise() %>% build_conditional(family)
#  table1 %<>% MLE_graph(data0)
#  
#  # Compare the truth and the fitted model
#  data.frame(cbind(true_beta = table0$beta, fitted_beta = table1$beta))
#  compute_likelihood(table0, data0)
#  get_model_likelihood(table1)

