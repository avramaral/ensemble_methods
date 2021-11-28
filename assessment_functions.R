# Error

compute_ind_error <- function (K, k, s, data, pred) {
  
  n_models <- length(pred)
  d_models <- dim(pred[[1]])
  
  n_regions  <- d_models[2]
  n_sim      <- d_models[3]
  
  test_value <- nrow(data) - K + k + s - 1
  
  errors <- list()
  for (m in 1:n_models) { errors[[m]] <- matrix(data = NA, nrow = n_sim, ncol = n_regions) }
  for (m in 1:n_models) {
    for (i in 1:n_sim) {
      errors[[m]][i, ] <- unname(unlist(c(data[test_value, ] - pred[[m]][(test_value - 1), , i])))
    }
  }
  errors
}

compute_ens_error <- function (K, k, s, data, pred) {
  
  n_models <- length(pred)
  d_models <- dim(pred[[1]])
  
  n_sim_post <- d_models[1]
  n_regions  <- d_models[2]
  n_sim      <- d_models[3]
  
  test_value <- nrow(data) - K + k + s - 1
  
  errors <- list()
  for (m in 1:n_models) { errors[[m]] <- matrix(data = NA, nrow = n_sim_post * n_sim, ncol = n_regions) }
  for (m in 1:n_models) {
    count <- 1
    for (i in 1:n_sim_post) {
      for (j in 1:n_sim) {
        errors[[m]][count, ] <- unname(unlist(c(data[test_value, ] - pred[[m]][i, , j])))
        count <- count + 1
      }
    }
  }
  errors
}

# Reference: https://stackoverflow.com/questions/59140960/remove-outliers-and-reduce-ylim-appropriately-for-each-facet-in-ggplot2
filter_lims <- function(x){
  l <- boxplot.stats(x, coef = 2.5)$stats[1]
  u <- boxplot.stats(x, coef = 2.5)$stats[5]
  
  for (i in 1:length(x)) {
    x[i] <- ifelse(x[i] > l & x[i] < u, x[i], NA)
  }
  x
}

plot_errors <- function (ind_error, ens_error, filepath, models_names = c('M1', 'M2', 'M3', 'EM1', 'EM2', 'EM3', 'EM4', 'EM5')) {
  
  n_ind_models <- length(ind_error)
  n_ens_models <- length(ens_error)
  
  d_ind_errors <- dim(ind_error[[1]]) # n_sim x n_regions
  d_ens_errors <- dim(ens_error[[1]]) # (n_sim_post x n_sim) x n_regions
  
  n_points_ind <- d_ind_errors[1]
  n_points_ens <- d_ens_errors[1]
  n_regions    <- d_ens_errors[2]
  
  
  df1 <- as_tibble(data.frame(
    index  = 1:(n_ind_models * n_regions * n_points_ind),
    region = rep(x = rep(x = geom$region, each = n_points_ind), times = n_ind_models),
    model  = rep(models_names[1:n_ind_models], each = n_regions * n_points_ind),
    errors = unlist(ind_error)
  ))
  
  df2 <- as_tibble(data.frame(
    index  = 1:(n_ens_models * n_regions * n_points_ens),
    region = rep(x = rep(x = geom$region, each = n_points_ens), times = n_ens_models),
    model  = rep(models_names[(n_ind_models + 1):(length(models_names))], each = n_regions * n_points_ens),
    errors = unlist(ens_error)
  ))
  
  df <- bind_rows(df1, df2)
  
  df <- df %>% mutate(errors2 = filter_lims(errors)) # Eliminate the outliers
  
  pt <- ggplot(data = df, mapping = aes(x = as.factor(model), y = errors2)) +
    geom_boxplot(
      outlier.color = 'red',
      outlier.fill  = 'red',
      outlier.size  = 2,
      outlier.alpha = 0.5,
      na.rm = TRUE,
      coef = 1e2
    ) + 
    geom_hline(yintercept = 0, color = 'blue') +
    facet_wrap(~ region, scales = 'free_y', ncol = 4) + 
    labs(title = paste0('Errors for all models (in time ', nrow(data) - K + k, ') with no outliers'), 
         x = '', y = 'Error') + 
    theme_bw() +
    theme(text = element_text(family = 'LM Roman 10'))
  ggsave(filename = filepath, plot = pt, width = 20, height = 14, units = c('in'))
  
}

# MSE

compute_mse <- function (K, k, errors) {
  
  n_models <- length(errors)
  
  mse <- list()
  for (m in 1:n_models) {mse[[m]] <- matrix(data = NA, nrow = 1, ncol = nrow(geom))}
  for (m in 1:n_models) {
    mse[[m]][1, ] <- apply(X = errors[[m]], MARGIN = 2, FUN = function (x) { sum(x ^ 2) / length(x) })
  }
  mse
}

data_mse <- function (k, s, n_ind_mod, n_ens_mod) {
  result <- rep(x = 0, times = nrow(geom))
  for (m in 1:(n_ind_mod + n_ens_mod)) {
    if (m <= n_ind_mod) {
      result <- rbind(result, c(ind_mse[[paste('k_', k, sep = '')]][[m]])) 
    } else {
      result <- rbind(result, c(ens_mse[[paste('k_', k, '_s_', s, sep = '')]][[(m - n_ind_models)]]))
    }
  }
  as.matrix(result[2:nrow(result), ])
}

format_mse <- function (K, S, ind_mse, ens_mse) {
  
  n_regions <- dim(ind_mse[[1]][[1]])[2]
  n_ind_mod <- length(ind_mse[[1]])
  n_ens_mod <- length(ens_mse[[1]])
  
  mse <- list()
  for (s in S) {
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        mse[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]] <- data_mse(k = k, s = s, n_ind_mod = n_ind_mod, n_ens_mod = n_ens_mod)
      }
    }
  }
  mse
}

computing_mse_summary <- function () {
  
  result <- list()
  
  cum_sum <- matrix(data = 0, nrow = (length(ind_error[[1]]) + length(ens_error[[1]])), ncol = nrow(geom))
  for (s in S) {
    count <- 0
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        count <- count + 1
        cum_sum <- cum_sum + mse[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]]
      }
    }
    result[[paste('s_', s, sep = '')]] <- cum_sum / count
  }
  result
}

plot_mse <- function (models_names = c('M1', 'M2', 'M3', 'EM1', 'EM2', 'EM3', 'EM4', 'EM5')) {
  
  ks <- rep(x = 0, times = length(S))
  count <- 1
  for (s in S) {
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        ks[count] <- ks[count] + 1
      }
    }
    count <- count + 1
  }
  
  result <- list()
  count <- 1
  for (s in S) { 
    result[[paste('s_', s, sep = '')]] <- array(data = NA, dim = c((length(ind_error[[1]]) + length(ens_error[[1]])), nrow(geom), ks[count]))
    count <- count + 1
  }
  
  count <- 1
  for (s in S) { 
    for (m in 1:(length(ind_error[[1]]) + length(ens_error[[1]]))) {
      for (i in 1:nrow(geom)) {
        for (k in 1:K) {
          if (k <= ks[count]) {
            result[[paste('s_', s, sep = '')]][m, i, k] <- mse[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]][m, i]
          }
        }
      }
    }
    count <- count + 1
  }
  
  count <- 1
  for (s in S) {
    
    df <- as_tibble(data.frame(
      index  = 1:((length(ind_error[[1]]) + length(ens_error[[1]])) * nrow(geom) * ks[count]),
      region = rep(rep(x = geom$region, each = length(ind_error[[1]]) + length(ens_error[[1]])), times = ks[count]),
      model  = rep(x = models_names, times = ks[count] * nrow(geom)),
      time   = rep(x = (nrow(data) - ks[count] + 1):nrow(data), each = nrow(geom) * (length(ind_error[[1]]) + length(ens_error[[1]]))),
      mse   = c(unname(unlist(mse[[paste('s_', s, sep = '')]])))
    ))
    
    count <- count + 1
    
    pt <- ggplot(data = df) + 
      geom_line(mapping = aes(x = time, y = mse, color = model), alpha = 0.5, size = 1) + 
      geom_point(mapping = aes(x = as.numeric(time), y = mse, color = model, shape = model), size = 3, alpha = 0.75) + 
      facet_wrap(~ region, scales = 'free_y', ncol = 4) + 
      scale_x_continuous(breaks = unique(df$time), labels = unique(df$time)) + 
      scale_color_discrete(name = 'Models') + 
      scale_shape_manual(name = 'Models', values = 1:(length(unique(df$model)))) + 
      theme_bw() +
      theme(text = element_text(family = 'LM Roman 10'), panel.grid.minor = element_blank())
    
    p1 <- pt + labs(title = 'Mean Squared Errors for the response variable', 
                    x = 'Windows', y = 'MSE (log-scale)') + 
      scale_y_continuous(trans = 'log10')
    
    p2 <- pt + labs(title = 'Mean Squared Errors for the response variable', 
                    x = 'Windows', y = 'MSE')
    
    ggsave(filename = paste('plots/', region_name, '/error/mse/mse_log_s_', s, '.png', sep = ''), plot = p1, width = 20, height = 14, units = c('in'))
    ggsave(filename = paste('plots/', region_name, '/error/mse/mse_s_', s, '.png', sep = ''), plot = p2, width = 20, height = 14, units = c('in'))
  }
}

# Score

score_computation <- function (obs, sample, score_name) {
  switch (score_name,
          log = {
            return(logs_sample(y = obs, dat = sample))
          }, crps = {
            return(crps_sample(y = obs, dat = sample))
          }, {
            stop('No valid score function was chosen.')
          })
}


compute_ind_score <- function (K, k, s, data, pred, score_name = 'crps') {
  
  n_models <- length(pred)

  test_value <- nrow(data) - K + k + s - 1
  
  scores <- list()
  for (m in 1:n_models) { scores[[m]] <- matrix(data = NA, nrow = 1, ncol = nrow(geom)) }
  for (m in 1:n_models) {
    for (i in 1:nrow(geom)) {
      scores[[m]][1, i] <- score_computation(obs    = unname(unlist(data[test_value, i])), 
                                             sample = pred[[m]][(test_value - 1), i, ], 
                                             score_name  = score_name)
    }
  }
  scores
}

compute_ens_score <- function (K, k, s, data, pred, score_name = 'crps') {
  
  n_models <- length(pred)

  test_value <- nrow(data) - K + k + s - 1
  
  scores <- list()
  for (m in 1:n_models) { scores[[m]] <- matrix(data = NA, nrow = 1, ncol = nrow(geom)) }
  for (m in 1:n_models) {
    count <- 1
    for (i in 1:nrow(geom)) {
      scores[[m]][count, ] <- score_computation(obs    = unname(unlist(data[test_value, i])), 
                                                sample = c(pred[[m]][, i, ]), 
                                                score_name = score_name)
    }
  }
  scores
}

data_score <- function (k, s, n_ind_mod, n_ens_mod) {
  result <- rep(x = 0, times = nrow(geom))
  for (m in 1:(n_ind_mod + n_ens_mod)) {
    if (m <= n_ind_mod) {
      result <- rbind(result, c(ind_score[[paste('k_', k, sep = '')]][[m]])) 
    } else {
      result <- rbind(result, c(ens_score[[paste('k_', k, '_s_', s, sep = '')]][[(m - n_ind_models)]]))
    }
  }
  as.matrix(result[2:nrow(result), ])
}

format_score <- function (K, S, ind_score, ens_score) {
  
  n_regions <- dim(ind_score[[1]][[1]])[2]
  n_ind_mod <- length(ind_score[[1]])
  n_ens_mod <- length(ens_score[[1]])
  
  score <- list()
  for (s in S) {
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        score[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]] <- data_score(k = k, s = s, n_ind_mod = n_ind_mod, n_ens_mod = n_ens_mod)
      }
    }
  }
  score
}


computing_score_summary <- function () {
  
  result <- list()
  
  cum_sum <- matrix(data = 0, nrow = (length(ind_score[[1]]) + length(ens_score[[1]])), ncol = nrow(geom))
  for (s in S) {
    count <- 0
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        count <- count + 1
        cum_sum <- cum_sum + score[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]]
      }
    }
    result[[paste('s_', s, sep = '')]] <- cum_sum / count
  }
  result
}

plot_score <- function (models_names = c('M1', 'M2', 'M3', 'EM1', 'EM2', 'EM3', 'EM4', 'EM5'), score_name = 'crps') {
  
  ks <- rep(x = 0, times = length(S))
  count <- 1
  for (s in S) {
    for (k in 1:K) {
      initial_test <- data_size - K + k
      if (s <= (data_size - initial_test + 1)) {
        ks[count] <- ks[count] + 1
      }
    }
    count <- count + 1
  }
  
  result <- list()
  count <- 1
  for (s in S) { 
    result[[paste('s_', s, sep = '')]] <- array(data = NA, dim = c((length(ind_error[[1]]) + length(ens_error[[1]])), nrow(geom), ks[count]))
    count <- count + 1
  }
  
  count <- 1
  for (s in S) { 
    for (m in 1:(length(ind_score[[1]]) + length(ens_score[[1]]))) {
      for (i in 1:nrow(geom)) {
        for (k in 1:K) {
          if (k <= ks[count]) {
            result[[paste('s_', s, sep = '')]][m, i, k] <- score[[paste('s_', s, sep = '')]][[paste('k_', k, '_s_', s, sep = '')]][m, i]
          }
        }
      }
    }
    count <- count + 1
  }
  
  count <- 1
  for (s in S) {
    
    df <- as_tibble(data.frame(
      index  = 1:((length(ind_error[[1]]) + length(ens_error[[1]])) * nrow(geom) * ks[count]),
      region = rep(rep(x = geom$region, each = length(ind_error[[1]]) + length(ens_error[[1]])), times = ks[count]),
      model  = rep(x = models_names, times = ks[count] * nrow(geom)),
      time   = rep(x = (nrow(data) - ks[count] + 1):nrow(data), each = nrow(geom) * (length(ind_error[[1]]) + length(ens_error[[1]]))),
      score  = c(unname(unlist(score[[paste('s_', s, sep = '')]])))
    ))
    
    count <- count + 1
    
    pt <- ggplot(data = df) + 
      geom_line(mapping = aes(x = time, y = score, color = model), alpha = 0.5, size = 1) + 
      geom_point(mapping = aes(x = as.numeric(time), y = score, color = model, shape = model), size = 3, alpha = 0.75) + 
      facet_wrap(~ region, scales = 'free_y', ncol = 4) + 
      scale_x_continuous(breaks = unique(df$time), labels = unique(df$time)) + 
      scale_color_discrete(name = 'Models') + 
      scale_shape_manual(name = 'Models', values = 1:(length(unique(df$model)))) + 
      theme_bw() +
      theme(text = element_text(family = 'LM Roman 10'), panel.grid.minor = element_blank())
    
    p1 <- pt + labs(title = paste('Scores (', toupper(score_name) , ')', sep = ''),
                    x = 'Time', y = paste(toupper(score_name), ' (log-scale)', sep = '')) + 
      scale_y_continuous(trans = 'log10')
    
    p2 <- pt + labs(title = paste('Scores (', toupper(score_name) , ')', sep = ''), 
                    x = 'Time', y = toupper(score_name))
    
    ggsave(filename = paste('plots/', region_name, '/score/score_log_s_', s, '.png', sep = ''), plot = p1, width = 20, height = 14, units = c('in'))
    ggsave(filename = paste('plots/', region_name, '/score/score_s_', s, '.png', sep = ''), plot = p2, width = 20, height = 14, units = c('in'))
  }
}
























