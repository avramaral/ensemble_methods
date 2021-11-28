
ind_model_2 <- function (data, geom, data_size, initial_test, n_sim) {
  
  # Neighbors
  neigh_structure <- poly2nb(geom)
  names(neigh_structure) <- geom$region
  
  # Data preparation
  n_last_weeks <- data_size - initial_test + 1
  
  data <- data %>% mutate(Region = factor(Region))
  pop <- c(rep(x = geom$pop_2014, each = (53)),
           rep(x = geom$pop_2015, each = (52)),
           rep(x = geom$pop_2016, each = (52)),
           rep(x = geom$pop_2017, each = (52)),
           rep(x = geom$pop_2018, each = (52)),
           rep(x = geom$pop_2019, each = (52)),
           rep(x = geom$pop_2020, each = (53))) # Make it automatic
  data <- data %>% add_column(pop = pop)
  for (i in 1:n_last_weeks) {
    data[data$Week_seq == initial_test - 1 + i, 6] <- NA
  }
  
  # Model fitting
  result <- gam(Count ~ s(Region, bs = 'mrf', xt = list(nb = neigh_structure)) + s(Week) + s(Week_seq) + offset(log(pop)),
                data = data,
                family = poisson,
                subset = which(!is.na(data$Count))) 
  
  
  # Simulation
  simulated_values <- simulate.gam(object = result, nsim = n_sim, seed = 1, newdata = data)
  simulated_values <- as_tibble(simulated_values) %>% add_column(region = data$Region, .before = 1)
  
  temp_simulated_values <- array(data = NA, dim = c(tail(data$Week_seq, 1) - 1, nrow(geom), ncol(simulated_values) - 1))
  
  for (i in 1:(ncol(simulated_values) - 1)) {
    temp_tibble <- simulated_values %>% 
      dplyr::select(c(1, (i + 1))) %>% 
      group_by(region) %>% 
      mutate(row = row_number()) %>% 
      pivot_wider(names_from = 1, values_from = 2) %>% 
      dplyr::select(-row)
    temp_matrix <- as.matrix(temp_tibble)[2:nrow(temp_tibble), ]
    
    temp_simulated_values[ , , i] <- temp_matrix
  }
  
  simulated_values <- temp_simulated_values
  rm(temp_simulated_values)
  
  return(simulated_values)
  
}