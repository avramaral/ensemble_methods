
ind_model_3 <- function (data, geom, data_size, initial_test, n_sim) {
  
  data <- data %>% 
    group_by(Region, Week) %>% 
    mutate(Row = row_number()) %>% 
    select(Region, Year, Week, Count, Row) %>% 
    pivot_wider(names_from = Region, values_from = Count) %>% 
    ungroup %>% 
    select(-Week, -Row)
  
  # Neighbors
  neigh_structure <- poly2nb(geom)
  
  # Data preparation
  n_last_weeks <- data_size - initial_test + 1

  time_series <- array(data = NA, dim = c(nrow(data) - 1, 2, nrow(geom)))
  for (i in 1:(nrow(geom))) {
    n_neigh <- length(neigh_structure[[i]])
    neighbs <- neigh_structure[[i]]
    
    temp_data <- data %>% select((neighbs + 1))
    temp_average <- rowSums(temp_data) / n_neigh
    temp_average <- diff(temp_average, differences = 1) # Take the difference
    
    temp_selected_data <- diff(unname(unlist(data[, (i + 1)])), differences = 1) # Take the difference
    
    time_series[ , , i] <- as.matrix(cbind(temp_selected_data, temp_average))
  }
  
  mixed_data <- list()
  train_data <- list()
  test_data <- list()
  
  for (i in 1:nrow(geom)) {
    temp_orig_data <- lag_transform(time_series[, 1, i])
    temp_orig_data <- cbind(time_series[, 2, i], temp_orig_data)
    colnames(temp_orig_data) <- c('cov', colnames(temp_orig_data)[2:3])
    mixed_data[[i]] <- temp_orig_data
    
    train_data[[i]] <- mixed_data[[i]][1:(nrow(data) - n_last_weeks - 1), ]
    test_data[[i]] <- mixed_data[[i]][(nrow(data) - n_last_weeks):(nrow(data) - 1), ]
  }
  
  # Data splitting
  scaled_data <- list()
  y_train <- list()
  x_train <- list()
  y_test <- list()
  x_test <- list()
  
  for (i in 1:nrow(geom)) {
    scaled_data[[i]] <- scale_data(train_data[[i]], test_data[[i]], c(-1, 1))
    
    y_train[[i]] <- scaled_data[[i]]$scaled_data_train[,   3]
    x_train[[i]] <- scaled_data[[i]]$scaled_data_train[, 1:2]
    
    y_test[[i]] <- scaled_data[[i]]$scaled_data_test[,   3]
    x_test[[i]] <- scaled_data[[i]]$scaled_data_test[, 1:2]
  }
  
  # Model fitting
  result <- list()
  fitted_values <- list()
  prediction <- list()
  total_values <- list()
  
  for (i in 1:nrow(geom)) {
    print(paste(i, ' (out of ', nrow(geom), ')', sep = ''))
    
    # Reshape the input to a  3-dimensional object
    ## 1st: sample size
    ## 2nd: time stamps
    ## 3rd: number of features (or covariates)
    x_train[[i]] <- array(data = unname(unlist(x_train[[i]])), dim = c(nrow(x_train[[i]]), 1, 2))
    
    # Specify required arguments
    x_shape2 <- dim(x_train[[i]])[2]
    x_shape3 <- dim(x_train[[i]])[3]
    batch_size <- 1 # Must be a common factor of both the train and test samples
    units <- 1 # Can be adjusted as a tuning parameter
    
    # Specify the model
    result[[i]] <- keras_model_sequential() 
    result[[i]] %>% layer_lstm(units, batch_input_shape = c(batch_size, x_shape2, x_shape3), stateful = TRUE) %>%
      layer_dense(units = 1)
    
    # Compile the model
    result[[i]] %>% compile(loss = 'mean_squared_error',
                            optimizer = optimizer_adam(learning_rate = 0.02, decay = 1e-6),  
                            metrics = c('accuracy'))
    
    # Fit the model
    epochs <- 35 
    for (j in 1:epochs) {
      result[[i]] %>% fit(x_train[[i]], y_train[[i]], epochs = 1, batch_size = batch_size, verbose = 0, shuffle = FALSE)
      result[[i]] %>% reset_states()
    }
    
    # Compute fitted values (train data)
    l <- nrow(x_train[[i]])
    scaler <- scaled_data[[i]]$scaler_train
    fitted <- numeric(l)
    
    for (j in 1:l) {
      x <- x_train[[i]][j, , ]
      dim(x) <- c(1, 1, 2)
      yhat <- result[[i]] %>% predict(x, batch_size = batch_size)
      yhat <- invert_scaling(yhat, scaler,  c(-1, 1)) # Invert the scaling
      yhat <- yhat + data[j, (i + 1)] # Invert the difference
      if (yhat < 0 ) { yhat <- 0 } # IMPORTANT!
      yhat <- as.integer(round(yhat)) # IMPORTANT!
      fitted[j] <- yhat
    }
    
    fitted_values[[i]] <- fitted
    
    # Do predictions (test data)
    m <- nrow(x_test[[i]])
    scaler <- scaled_data[[i]]$scaler_test
    pred <- numeric(m)
    
    for (j in 1:m) {
      x <- as.matrix(x_test[[i]][j, , ])
      dim(x) <- c(1, 1, 2)
      yhat <- result[[i]] %>% predict(x, batch_size = batch_size)
      yhat <- invert_scaling(yhat, scaler,  c(-1, 1)) # Invert the scaling
      yhat <- yhat + data[l + j, (i + 1)] # Invert the difference
      if (yhat < 0 ) { yhat <- 0 } # IMPORTANT!
      yhat <- as.integer(round(yhat)) # IMPORTANT!
      pred[j] <- yhat
    }
    
    prediction[[i]] <- pred
    
    total_values[[i]] <- c(fitted_values[[i]], prediction[[i]])
    
  }
  
  # Save predicted values, so that we can create synthetic data from it
  for (i in 1:nrow(geom)) {
    predicted_values <- as_tibble(c(total_values[[i]], total_values[[i]][length(total_values[[i]])]))  
    write_csv(x = predicted_values, file = paste('python/data/predicted_value_', sprintf("%02d", i), '.csv', sep = ''))
  }
  
  start.time <- Sys.time()
  # TimeGAN script
  n_training_steps = 10
  # Reference: https://stackoverflow.com/a/54112656
  system2('/bin/bash', args = c('-ic', shQuote(paste('python39 python/TimeGAN.py', (nrow(data) - 1), n_sim, nrow(geom), n_training_steps))))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  # Simulation
  simulated_values <- array(data = NA, dim = c(nrow(data) - 1, nrow(geom), n_sim))
  
  for (i in 1:nrow(geom)) {
    synthetic_data <- read_csv(file = paste('python/data/synthetic_data/region_', i, '.csv', sep = ''), col_names = FALSE)
    synthetic_data <- apply(X = apply(X = synthetic_data, MARGIN = c(1, 2), FUN = round, digits = 0), MARGIN = c(1, 2), FUN = function (X) {sapply(X = X, FUN = function (x) {max(0, x)})})
    simulated_values[, i, ] <- as.matrix(synthetic_data)
  }
  
  return(simulated_values)
  
}

# z = 1
# plot(NA, ylim = c(min(simulated_values[, z, ]), max(simulated_values[, z, ])), xlim = c(1, 366))
# for (i in 1:n_sim) {
#   lines(simulated_values[, z, i], col = i)
# }
# lines(total_values[[z]])
