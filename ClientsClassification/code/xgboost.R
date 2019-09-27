##xgboost
dat <- preprocessed_train
folds <- 10
cvrepeats <- 5
experiments <- 1
n = dim(dat)[1]
frac = 1 / folds
b_score = 0.92

is_feature_selection = FALSE
for (k in 1:experiments) {
  print(paste0("best_score:", b_score))
  means <- numeric(cvrepeats)
  feat <- list()
  for (j in 1:cvrepeats) 
  {
    print(paste0("Cross-validation number: ", j))
    df <- dat
    df <- df[sample(nrow(df)),]
    precyzja <- numeric(folds)
    if (is_feature_selection == TRUE)
    {
      features <- sort(sample(dim(df)[2] - 1, 50))
      ind <- c(features, dim(df)[2])
      df <- df[, sort(ind)]
    }
    df <- numeric_encoding(df)
    for (i in 1:folds) 
    {
      indices_test <- ((i - 1) * (n / folds) + 1):(i * (n / folds))
      train = df[-indices_test, ]
      test= df[indices_test, ]
      train <- normalization(train, random = TRUE)
      test <- normalization(test, random = TRUE)
      fit = xgboost(data = data.matrix(train[, -ncol(train)]), label = train$class,
                    objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.04, base_score = b_score)
      p1 = predict(fit, newdata = data.matrix(test[, -ncol(test)]))
      names(p1) <- rownames(test)
      precyzja[i] <- precision_rf(p1, test$class, rownames(test))
    }
    if (is_feature_selection == TRUE)
    {
      feat[j] <- list(features)
    }
    means[j] <- mean(precyzja)
    print(paste0(": Precyzja 10% po kroswalidacji: ", means[j]))
  }
  print(paste0("Finalna precyzja 10%: ", mean(means)))
}