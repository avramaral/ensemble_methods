setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('tidyverse')
library('scoringRules')

source(file = 'assessment_functions.R')

rdatafile <- 'rdata/test.RData'
load(rdatafile)

region_name <- 'Brazil' 

# Individual models

n_ind_models <- length(ind_models_result[[1]])

# Made for test purpose ##################################
# Exclude it later
ind_models_result[["k_2"]] <- ind_models_result[["k_1"]]
ind_models_result[["k_3"]] <- ind_models_result[["k_1"]]
ind_models_result[["k_4"]] <- ind_models_result[["k_1"]]
##########################################################

# Ensemble models

# Made for test purpose ##########################################
# Exclude it later
ens_models_result[['k_2_s_1']] <- ens_models_result[['k_1_s_1']]
ens_models_result[['k_2_s_2']] <- ens_models_result[['k_1_s_1']]
ens_models_result[['k_3_s_1']] <- ens_models_result[['k_1_s_1']]
ens_models_result[['k_3_s_2']] <- ens_models_result[['k_1_s_1']]
ens_models_result[['k_4_s_1']] <- ens_models_result[['k_1_s_1']]
##################################################################

##########################################################################################################################################################################################

# Preliminaries
data <- data %>% 
  group_by(Region, Week) %>% 
  mutate(Row = row_number()) %>% 
  select(Region, Year, Week, Count, Row) %>% 
  pivot_wider(names_from = Region, values_from = Count) %>% 
  ungroup %>% 
  select(-Year, -Week, -Row)

##########################################################################################################################################################################################

# Computing errors

ind_error <- list()
for (s in S) {
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) { 
      idx <- paste('k_', k, sep = '')
      ind_error[[idx]] <- compute_ind_error(K = K,
                                            k = k,
                                            s = s,
                                            data = data,
                                            pred = ind_models_result[[idx]])
    }
  }
}

ens_error <- list()
for (s in S) {
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) { 
      idx <- paste('k_', k, '_s_', s, sep = '')
      ens_error[[idx]] <- compute_ens_error(K = K,
                                            k = k,
                                            s = s,
                                            data = data,
                                            pred = ens_models_result[[idx]])
    }
  }
}

# Plotting errors

relative_path_error <- paste('plots/', region_name, '/error/abs_error', sep = '')
unlink(relative_path_error, recursive = TRUE)
dir.create(relative_path_error) 
for (s in S) {
  relative_path_error_s <- paste(relative_path_error, '/s_', s, sep = '')
  dir.create(relative_path_error_s)
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) {
      filepath = paste(relative_path_error_s, '/s_', s, '_k_', initial_test, '.png', sep = '')
      plot_errors(ind_error = ind_error[[paste('k_', k, sep = '')]], ens_error = ens_error[[paste('k_', k, '_s_', s, sep = '')]], filepath = filepath)
    }
  }
}

# Computing MSEs

ind_mse <- list()
for (k in 1:K) {
  initial_test <- data_size - K + k
  idx <- paste('k_', k, sep = '')
  ind_mse[[idx]] <- compute_mse(K = K,
                                k = k,
                                errors = ind_error[[idx]])
}

ens_mse <- list()
for (s in S) {
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) { 
      idx <- paste('k_', k, '_s_', s, sep = '')
      ens_mse[[idx]] <- compute_mse(K = K,
                                    k = k,
                                    errors = ens_error[[idx]])
    }
  }
}


# mse
# -> list of size length(S)
# --> list with all combinations of (k_s)
# ---> matrix(n_mod_total x n_regions)
mse <- format_mse(K = K, S = S, ind_mse = ind_mse, ens_mse = ens_mse)

mse_summary <- computing_mse_summary()
saveRDS(mse_summary, file = paste('plots/', region_name, '/summary/mse_summary.rds', sep = ''))
group_mse_r <- lapply(X = mse_summary, FUN = rowSums)

# Plotting MSEs 

plot_mse()


# Computing scores

ind_score <- list()
for (s in S) {
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) { 
      idx <- paste('k_', k, sep = '')
      ind_score[[idx]] <- compute_ind_score(K = K,
                                            k = k,
                                            s = s,
                                            data = data,
                                            pred = ind_models_result[[idx]])
    }
  }
}

ens_score <- list()
for (s in S) {
  for (k in 1:K) {
    initial_test <- data_size - K + k
    if (s <= (data_size - initial_test + 1)) { 
      idx <- paste('k_', k, '_s_', s, sep = '')
      ens_score[[idx]] <- compute_ens_score(K = K,
                                            k = k,
                                            s = s,
                                            data = data,
                                            pred = ens_models_result[[idx]])
    }
  }
}

# score
# -> list of size length(S)
# --> list with all combinations of (k_s)
# ---> matrix(n_mod_total x n_regions)
score <- format_score(K = K, S = S, ind_score = ind_score, ens_score = ens_score)

score_summary <- computing_score_summary() # generalize it
saveRDS(score_summary, file = paste('plots/', region_name, '/summary/score_summary.rds', sep = ''))
group_score_r <- lapply(X = score_summary, FUN = rowSums)

# Plotting scores 

plot_score() # generalize it



























