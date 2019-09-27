datakNN <- preprocessed_train
df <- datakNN
dmy <- dummyVars(" ~ .", data = df, fullRank= TRUE)
df <- data.frame(predict(dmy, newdata = df))
folds <- 5
cvrepeats <- 1
n = dim(df)[1]
frac = 1 / folds
non_dummy <- setdiff(dmy$vars, dmy$facVars)

for (j in 1:cvrepeats) {
  df <- df[sample(nrow(df)),]
  df$class <- as.factor(df$class)
  for (i in 1:folds) {
    indices_test <- ((i - 1) * (n / folds) + 1):(i * (n / folds))
    train = df[-indices_test,]
    test= df[indices_test, ]
    train[, non_dummy] <- normalization(train[, non_dummy], random = TRUE)
    train_class <- df[-indices_test, dim(df)[2]]
    test[, non_dummy] <- normalization(test[, non_dummy], random = TRUE) # tutaj dodac normalizacje po kolumnach
    test_class <- df[indices_test, dim(df)[2]]
    train = train[, -dim(train)[2]]
    test = test[, -dim(test)[2]]
    fit = knn(train, test, cl = train_class, k = 200)
    CrossTable(x = test_class, y = fit,
               prop.chisq=FALSE)
  }
}