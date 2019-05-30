require(ggplot2)

# drop Columns in dataFrame
# colsVect : vector of column names to drop
dropCols <- function(dataframe, colsVect) {
  return(dataframe[, !names(dataframe) %in% colsVect])
}

# Returns list (name, value) of most frequent value in named vector
mostFreq <- function(named_vector) {
  t <- table(named_vector)
  return( list(value = names(which(t == max(t))), freq = max(t)))
}

# Returns list of datasets (train, set) generated from dat (data.frame) with given fraction (frac) to train set
trainTestSet <- function(dat, frac = 0.8) {
  samp <- sample(nrow(dat), frac * nrow(dat))
  return(list( train = dat[samp, ], test = dat[-samp, ]))
}

# Encodes categorical values as integers from given vector of colnames
numerize <- function(dat, colnames) {
  for (name in colnames){
    values <- levels(as.factor(dat[, name]))
    mapped <- seq(0, length(values))
    dat[ , name] <- sapply(dat[ , name], function(x, vals, maps) { ind <- which(vals == x); return(maps[ind]) },
                          values, mapped) 
  }
  return(dat)
}

# calculates metrics after model evaluation on training set
# recall == tpr
metrics <- function(model, testSet, class_index, type, treshold = 0.5) {
  if (type == 'glm') {
    pred = predict(model, newdata = testSet, type = 'response')
  }
  else if (type == 'rpart') {
    pred = predict(model, newdata = testSet, type = 'prob')[, 2]
  }
  # Other metrics
  predicted <- ifelse(pred > treshold, 1 , 0)
  t <- table(predicted, true = testSet[, class_index])
  if (dim(t)[1] == 2) {
    acc <- sum(diag(t)) / sum(t)
    recall <- t[2, 2] / sum(t[ , 2])
    precision <- t[2, 2] / sum(t[2 , ])
    fpr <- t[2, 1] / sum(t[, 1])
  }
  else if (sum(predicted) == 0) {
      acc <- t[1] / sum(t)
      recall <- 0
      precision <- 0
      fpr <- 0
    }
  else {
    acc <- t[2] / sum(t)
    recall <- 1
    precision <- t[2] / sum(t)
    fpr <- 1
  }
  return(list(table = t, accuracy = acc, recall = recall, precision = precision, fpr = fpr))
}

prec_at_k <- function(model, testSet, class_index, type,  prec_k = 0.1) {
  if (type == 'glm') {
    pred = predict(model, newdata = testSet, type = 'response')
  }
  else if (type == 'rpart') {
    pred = predict(model, newdata = testSet, type = 'prob')[, 2]
  }
  # precision at k
  rows <- order(pred, decreasing = TRUE)
  cl <- testSet[rows, class_index]
  cl <- cl[1:floor(prec_k * length(cl))]
  pred_prec <- sort(pred, decreasing = TRUE)[1:floor(prec_k * length(pred))]
  predicted_prec <- ifelse(pred_prec > treshold, 1 , 0)
  t2 <- table(predicted_prec, true = cl)
  precision_at_k <- t2[2, 2] / sum(t2[2 , ])
  return(precision_at_k)
}

ROC <- function(model, testSet, class_index, type) {
  n <- 100
  x <- numeric(n)
  y <- numeric(n)
  tresholds <- seq(0, 1, length.out = n)
  i <- 1
  for ( tresh in tresholds) {
    m <- metrics(model.glm, test, 1, type = 'glm', treshold = tresh)
    x[i] <- m$fpr
    y[i] <- m$recall
    i <- i + 1
  }
  df <- data.frame(x = x , y = y)
  ggplot(data = df, aes(x = x , y = y)) + geom_line(size = 1.5, col = 'red') +
    ggtitle('ROC Curve') +
    xlab('FPR') +
    ylab('TPR') +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = '#d9d9d9'),
          plot.title = element_text(hjust = 0.5))
}
