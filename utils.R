
# Compute weights based on the scores to be used in the weighted average approach
compute_weights <- function (m1, m2, m3, y_obs) {
  n_reg <- ncol(m1)
  
  crps_comp <- array(data = NA, dim = c(3, n_reg))
  
  crps_comp[1, ] <- crps_sample(y = y_obs, dat = matrix(data = c(t(m1)), nrow = length(y_obs), byrow = FALSE))
  crps_comp[2, ] <- crps_sample(y = y_obs, dat = matrix(data = c(t(m2)), nrow = length(y_obs), byrow = FALSE))
  crps_comp[3, ] <- crps_sample(y = y_obs, dat = matrix(data = c(t(m3)), nrow = length(y_obs), byrow = FALSE))
  
  for (i in 1:nrow(crps_comp)) {
    for (j in 1:ncol(crps_comp)) {
      if (crps_comp[i, j] == 0) {crps_comp[i, j] <- crps_comp[i, j] + 1e-06}
    }
  }
  
  weights <- apply(X = crps_comp, MARGIN = 2, FUN = function (x) {
    for (i in 1:(length(x))) { x[i] <- (x[i] ^ 2) ^ (-1) }
    x <- x / sum(x)
    x
  })
  
  return(weights)
  
}

# Return the lagged (by k) time series for the LSTM approach
lag_transform <- function (x, k = 1) {
  lagged <- c(rep(NA, k), x[1:(length(x) - k)])
  df <- as.data.frame(cbind(lagged, x))
  colnames(df) <- c(paste('x-', k, sep = ''), 'x')
  df[is.na(df)] <- 0
  return(df)
}

# Return the scaled data for the LSTM approach
scale_data <- function (data_train, data_test, feature_range = c(0, 1)) {
  fr_min <- feature_range[1]
  fr_max <- feature_range[2]
  std_data_train <- ((data_train - min(data_train)) / (max(data_train) - min(data_train)))
  std_data_test <- ((data_test - min(data_test)) / (max(data_test) - min(data_test)))
  scaled_data_train <- std_data_train * (fr_max - fr_min) + fr_min
  scaled_data_test <- std_data_test * (fr_max - fr_min) + fr_min
  
  return(list(scaled_data_train = as.vector(scaled_data_train), 
              scaled_data_test = as.vector(scaled_data_test), 
              scaler_train = c(min = min(data_train), max = max(data_train)), 
              scaler_test = c(min = min(data_test), max = max(data_test))))
}

# Return the inverted scaled data for the LSTM approach
invert_scaling <- function (scaled, scaler, feature_range = c(0, 1)) {
  min <- scaler[1]
  max <- scaler[2]
  l <- length(scaled)
  mins <- feature_range[1]
  maxs <- feature_range[2]
  inverted_dfs <- numeric(l)
  
  for(i in 1:l) {
    x <- (scaled[i] - mins) / (maxs - mins)
    rawValues <- x * (max - min) + min
    inverted_dfs[i] <- rawValues
  }
  
  return(inverted_dfs)
}
