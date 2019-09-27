#rpartSolution

folds <- 10
cvrepeats <- 10
n = nrow(dat)
frac = 1 / folds

dat <- preprocessed_train
means <- numeric(cvrepeats)
for (j in 1:cvrepeats) 
{
  print(paste0("Cross-validation number: ", j))
  df <- dat[sample(nrow(dat)),]
  df$class <- as.factor(df$class)
  precision <- numeric(folds)
  for (i in 1:folds) {
    indices_test <- ((i - 1) * (n / folds) + 1):(i * (n / folds))
    train = df[-indices_test, ]
    test= df[indices_test, ]
    train <- normalization(train, random = TRUE)
    test <- normalization(test, random = TRUE)
    fit = rpart(class ~ ., data = train, control=rpart.control(cp=0.01))
    p1 = predict(fit, newdata = test, type = "prob")
    precision[i] <- precision_rpart(p1[, 2], test$class, rownames(test))
  }
  means[j] <- mean(precision)
  print(paste0("Precyzja 10% po kroswalidacji: ", means[j]))
}
print(paste0("Finalna precyzja 10%: ", mean(means)))