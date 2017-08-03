calc_kappa <- function(x, obs_values, pred_values) {
  #require(SDMTools)
  mat <- SDMTools::confusion.matrix(obs=obs_values, pred=pred_values, threshold=plogis(x))
  SDMTools::Kappa(mat)
}



