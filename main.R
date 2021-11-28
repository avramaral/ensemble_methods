setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load other relevant files
source(file = 'libraries.R')
source(file = 'utils.R')
source(file = 'ind_models.R')
source(file = 'ens_models.R')
sapply(X = list.files(path = 'ind_models', pattern = '*.R', full.names = TRUE), FUN = source)
sapply(X = list.files(path = 'ens_models', pattern = '*.R', full.names = TRUE), FUN = source)

# Load data
country <- 'Brazil'
data_path <- paste('data_dengue/', country, '/', tolower(country), '.csv', sep = '')
geom_path <- paste('data_dengue/', country, '/', tolower(country), '.rds', sep = '')

data <- read_csv(file = data_path)
geom <- readRDS(file = geom_path)

# Initial settings
data_size <- tail(data$Week_seq, 1)
K <- 2
S <- c(1, 2) # c(1, 2)
n_sim <- 250 # number of data sets simulated from each individual model
n_sim_post <- 50 # 15 # sample size for the posterior distribution of the ensemble model

ind_models_result <- list()
ens_models_result <- list()

# Main loop for model fitting
if (max(S) > K) { 
  stop('Step-size greater than test data.')
} else {
  for (k in 1:K) {
    ind_idx <- paste('k_', k, sep = '')
    initial_test <- data_size - K + k
    ind_models_result[[ind_idx]] <- fit_ind_models(data = data, geom = geom, data_size = data_size, initial_test = initial_test, n_sim = n_sim)
    # saveRDS(ind_models_result[[ind_idx]], file = paste('rdata/', ind_idx, '.rds', sep = ''))
    # readRDS(file = paste('rdata/', ind_idx, '.rds', sep = ''))
    for (s in S) {
      if (s <= (data_size - initial_test + 1)) {
        ens_idx <- paste('k_', k, '_s_', s, sep = '')
        print(ens_idx)
        ens_models_result[[ens_idx]] <- fit_ens_models(data = data, geom = geom, ind_models_result = ind_models_result[[ind_idx]], data_size = data_size, initial_test = initial_test, n_sim = n_sim, n_sim_post = n_sim_post, stepsize = s)
      }
    }
  }
}

