
# Equal weights ensemble model
ens_model_1 <- function (data, df, initial_test, stepsize, ...) {
  
  tbp <- initial_test
  df  <- df[(df$week == tbp) | (!is.na(df$observed)), ]
  tbp <- which(is.na(df$observed))
  result <- (df$m1 + df$m2 + df$m3) / 3
  
  return(list(result = result[tbp], INLA = FALSE))
  
}
