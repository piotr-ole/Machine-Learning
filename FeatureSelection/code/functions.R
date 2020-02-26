sample.fraction <- function(n, frac) {
  return(sample(n, n * frac))
}

normalization <- function(dat) {
  return ( sapply(dat, function(x) {return((x - min(x)) / (max(x) - min(x))) }) )
}

balanced_acc <- function(pred_classes, true_classes) {
  t <- table(pred_classes, true_classes)
  BA <- 0.5 * (t[2,2] / sum(t[, 2]) + t[1,1] / sum(t[, 1]))
  return(BA)
}

indices_fun <- function(dat, features_names)
{
  len <- length(features_names)
  indices <- numeric(len)
  for (j in seq(len)) {
       indices[j] <- which(colnames(dat) == features_names[j])
  }
  return(indices)
}
