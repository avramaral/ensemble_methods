
# Weighted average ensemble model
ens_model_2 <- function (data, df, initial_test, stepsize, wght_avg = wght_avg, ...) {
  
  tbp <- initial_test
  df  <- df[(df$week == tbp) | (!is.na(df$observed)), ]
  tbp <- which(is.na(df$observed))
  
  result <- c()
  
  col_id <- 1
  for (i in tbp) {
    result <- c(result, stats::weighted.mean(x = c(df$m1[i], df$m2[i], df$m3[i]), w = wght_avg[, col_id]))
    col_id <- col_id + 1
  }
  
  return(list(result = result, INLA = FALSE))
  
}