calc_kappa <- function(x, obs_values, pred_values) {
  #require(SDMTools)
  mat <- SDMTools::confusion.matrix(obs=obs_values, pred=pred_values, threshold=plogis(x))
  SDMTools::Kappa(mat)
}

threshold_kappa <- function(x=0.5, obs_values, pred_values, ...){
  fn <- function(x) {
    calc_kappa(x, obs_values = obs_values, pred_values = pred_values)
  }
  optimize(fn, interval=c(0,1), maximum=TRUE)
}

