
fit_ind_models <- function (data, geom, data_size, initial_test, n_sim) {
  
  # Initial settings
  ind_models_path <- list.files(path = 'ind_models', pattern = '*.R', full.names = TRUE)
  ind_models <- unname(sapply(X = ind_models_path, FUN = gsub, pattern = '.*/|\\.R.*', replacement = ''))
  n_ind_models <- length(ind_models)
  
  # Fit models
  ind_model_result <- list()
  
  for (i in 1:n_ind_models) {
    f <- get(x = ind_models[i])
    ind_model_result[[i]] <- f(data = data, geom = geom, data_size = data_size, initial_test = initial_test, n_sim = n_sim)
  }
  
  return(ind_model_result)
  
}
