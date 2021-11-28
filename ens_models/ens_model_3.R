
# GLMM 1 ensemble model
ens_model_3 <- function (data, df, initial_test, stepsize, gr = NA, ...) {
  
  tbp <- initial_test
  df  <- df[(df$week == tbp) | (!is.na(df$observed)), ]
  tbp <- which(is.na(df$observed))
  
  if (!is.na(gr)) {
    
    formula <- observed ~ 1 + m1 + m2 + m3
    resINLA <- inla(formula = formula, family = 'poisson', data = df, E = population, control.predictor = list(compute = TRUE, link = 1), control.compute = list(config = TRUE))
    
  } else { stop('Provide a graph for the neighborhood structure.') }
  
  return(list(result = resINLA, tbp = tbp, INLA = TRUE))

}
