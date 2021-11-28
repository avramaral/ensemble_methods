
fit_ens_models <- function (data, geom, ind_models_result, data_size, initial_test, n_sim, n_sim_post, stepsize) {
  
  ks <- c()
  threshold <- initial_test - 1 + stepsize
  while (threshold <= data_size) {
    ks <- c(ks, threshold)
    threshold <- threshold + stepsize
  }
  
  data <- data %>% 
            group_by(Region, Week) %>% 
            mutate(Row = row_number()) %>% 
            select(Region, Year, Week, Count, Row) %>% 
            pivot_wider(names_from = Region, values_from = Count) %>% 
            ungroup %>% 
            select(-Week, -Row)
  
  # Initial settings
  ens_models_path <- list.files(path = 'ens_models', pattern = '*.R', full.names = TRUE)
  ens_models <- unname(sapply(X = ens_models_path, FUN = gsub, pattern = '.*/|\\.R.*', replacement = ''))
  n_ens_models <- length(ens_models)
  
  # Neighborhood structure
  nb <- poly2nb(geom)
  nb2INLA('data_dengue/Brazil/map.adj', nb)
  gr <- inla.read.graph(filename = 'data_dengue/Brazil/map.adj')
  
  dim_ind_models <- dim(ind_models_result[[1]])
  J <- dim_ind_models[1] # e.g., 366 - 1 = 365
  I <- dim_ind_models[2] # e.g., 27
  pop <- c(rep(x = geom$pop_2014, times = (53 - 1)),
           rep(x = geom$pop_2015, times = (52)),
           rep(x = geom$pop_2016, times = (52)),
           rep(x = geom$pop_2017, times = (52)),
           rep(x = geom$pop_2018, times = (52)),
           rep(x = geom$pop_2019, times = (52)),
           rep(x = geom$pop_2020, times = (53))) # Make it automatic
  
  # Fit models
  ens_model_result <- list()
  
  for (e in 1:n_ens_models) { ens_model_result[[e]] <- array(data = NA, dim = c(n_sim_post, I, n_sim))}
  for (e in 1:n_ens_models) {
    for (k in ks) {#for (k in initial_test:data_size) {
      observed <- c(t(as.matrix(data[2:(k - 1), 2:(I + 1)])), rep(x = NA, times = ((J - k + 2) * I)))
      wght_avg <- compute_weights(m1 = t(ind_models_result[[1]][(k - 1), , ]), m2 = t(ind_models_result[[2]][(k - 1), , ]), m3 = t(ind_models_result[[3]][(k - 1), , ]), y_obs = unlist(data[k, 2:ncol(data)]))

      for (n in 1:n_sim) {
        print(paste('e = ', e, ' k = ', k, ' n = ', n, sep = ''))
        
        df <- data.frame(observed = observed, 
                         population = pop,
                         m1 = c(t(ind_models_result[[1]][ , , n])), 
                         m2 = c(t(ind_models_result[[2]][ , , n])), 
                         m3 = c(t(ind_models_result[[3]][ , , n])),
                         id_area   = rep(x = 1:I, times = J),
                         id_area_1 = rep(x = 1:I, times = J),
                         id_area_2 = rep(x = 1:I, times = J),
                         id_area_3 = rep(x = 1:I, times = J),
                         week = rep(x = 2:(J + 1), each = I)) # Make it independent from the number of ind. models
        
        f <- get(x = ens_models[e])
        partial_result <- f(data = data, df = df, initial_test = k, stepsize = stepsize, wght_avg = wght_avg, gr = gr)
        
        if (partial_result$INLA) {
          sim <- inla.posterior.sample(n = n_sim_post, result = partial_result$result)
          for (m in 1:n_sim_post) {
            ens_model_result[[e]][m, , n] <- exp(sim[[m]]$latent[partial_result$tbp]) * geom$pop_2020 # most recent population
          }
        } else {
          for (m in 1:n_sim_post) {
            ens_model_result[[e]][m, , n] <- partial_result$result
          }
        }
      }
    }
  }
  
  return(ens_model_result)
  
}