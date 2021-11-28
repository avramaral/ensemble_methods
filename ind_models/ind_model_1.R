
ind_model_1 <- function (data, geom, data_size, initial_test, n_sim) {
  
  data <- data %>% 
            group_by(Region, Week) %>% 
            mutate(Row = row_number()) %>% 
            select(Region, Year, Week, Count, Row) %>% 
            pivot_wider(names_from = Region, values_from = Count) %>% 
            ungroup %>% 
            select(-Week, -Row)
  
  n_years <- length(unique(data$Year))
  
  # Neighbors
  k_order_neighbors <- nbOrder(poly2adjmat(geom), maxlag = nrow(geom)) 
  rownames(k_order_neighbors) <- geom$region
  colnames(k_order_neighbors) <- geom$region
  
  # Observed
  observed <- data %>% select(-Year)
  
  # Geom object
  rownames(geom) <- geom$region
  geom_df <- as.data.frame(geom)
  geom_df$geom <- NULL
  geom_sp <- SpatialPolygonsDataFrame(as_Spatial(st_geometry(geom), IDs = geom$region), data = geom_df)
  
  # Population
  total_pop_frac <- matrix(data = NA, nrow = nrow(observed), ncol = ncol(observed))
  colnames(total_pop_frac) <- geom$region
  
  years <- unique(data$Year)
  n_weeks <- rep(x = NA, times = length(years))
  for (i in 1:length(years)) { n_weeks[i] <- nrow(data[data$Year == years[i], ]) }
  
  current_row <- 1
  for (i in 1:length(years)) {
    total_pop_frac[current_row:(current_row + n_weeks[i] - 1), ] <- matrix(data = geom_df[, n_years + 1 + i], nrow = n_weeks[i], ncol = nrow(geom_df), byrow = TRUE)
    current_row <- current_row + n_weeks[i]
  }
  
  # Create an 'sts' object
  geom_sts <- sts(observed = observed, 
                  start = c(unique(data$Year)[1], 1),
                  frequency = 52,
                  neighbourhood = k_order_neighbors,
                  map = geom_sp, 
                  populationFrac = total_pop_frac) 
  
  # Fit the model
  n_last_weeks <- data_size - initial_test + 1
  subset_data <- 2:(nrow(geom_sts) - n_last_weeks) 
  all_weeks <- 2:nrow(geom_sts)
  
  control <- list(end = list(f = addSeason2formula(f = ~ 1 + t, period = geom_sts@freq), offset = population(geom_sts)),
                  ar  = list(f = ~ 1),
                  ne  = list(f = ~ 1 + log(pop), weights = W_powerlaw(maxlag = max(neighbourhood(geom_sts)))),
                  family = 'NegBin1', 
                  subset = subset_data,
                  data = list(pop = population(geom_sts)))
  
  result <- hhh4(stsObj = geom_sts, control = control)
  
  # Simulation
  simulated_values <- simulate(object = result, nsim = n_sim, seed = 1, subset = all_weeks, ystart = observed(geom_sts)[1, ])

  return(simulated_values)
  
}