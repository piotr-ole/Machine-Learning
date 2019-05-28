dropCols <- function(dataframe, colsVect) {
  return(dataframe[, !names(dataframe) %in% colsVect])
}

mostFreq <- function(named_vector) {
  t <- table(named_vector)
  return( list(value = names(which(t == max(t))), freq = max(t)))
}

trainTestSet <- function(dat, frac = 0.8) {
  samp <- sample(nrow(dat), frac * nrow(dat))
  return(list( train = dat[samp, ], test = dat[-samp, ]))
}

numerize <- function(dat, colnames) {
  for (name in colnames){
    values <- levels(as.factor(dat[, name]))
    mapped <- seq(0, length(values))
    dat[ , name] <- sapply(dat[ , name], function(x, vals, maps) { ind <- which(vals == x); return(maps[ind]) },
                          values, mapped) 
  }
  return(dat)
}


metrics <- function(model, testSet, class_index, type) {
  if (type == 'glm') {
    pred = predict(model, newdata = testSet, type = 'response')
  }
  else if (type == 'rpart') {
    pred = predict(model, newdata = testSet, type = 'prob')[, 2]
  }
  predicted <- ifelse(pred > 0.5, 1 , 0)
  t <- table(predicted, true = testSet[, class_index])
  acc <- sum(diag(t)) / sum(t)
  recall <- t[2, 2] / sum(t[ , 2])
  precision <- t[2, 2] / sum(t[2 , ])
  print(t)
  return(list(accuracy = acc, recall = recall, precision = precision))
}

