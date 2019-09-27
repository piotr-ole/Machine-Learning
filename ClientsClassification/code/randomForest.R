##randomForest

dat <- preprocessed_train

folds <- 10
cvrepeats <- 5
trees_number <- 60
df <- dat
n = nrow(df)
frac = 1 / folds


means <- numeric(cvrepeats)
feat <- list()
  
for (j in 1:cvrepeats) 
{
  print(paste0("Cross-validation number: ", j))
  df <- dat
  df <- df[sample(nrow(df)),]
  df$class <- as.factor(df$class)
  precision <- numeric(folds)
  for (i in 1:folds) 
  {
    indices_test <- ((i - 1) * (n / folds) + 1):(i * (n / folds))
    train = df[-indices_test, ]
    test= df[indices_test, ]
    train <- normalization(train, random = TRUE)
    test <- normalization(test, random = TRUE)
    fit = randomForest(class ~., data = train, ntrees = trees_number)
    p1 = predict(fit, newdata = test, type = "prob")
    precision[i] <- precision_rf(p1[, 2], test_classes = test$class, test_rownames = rownames(test))
  }
  means[j] <- mean(precision)
  print(paste0("Precyzja po kroswalidacji: ", means[j]))
}
print(paste0("Finalna precyzja 10%: ", mean(means)))


# na testowym

train <-  preprocessed_train
train$class <- as.factor(train$class)
test <-  preprocessed_test
train <- normalization(train, random = TRUE)
test <- normalization_test(test, random = TRUE)
fit = randomForest(class ~., data = train, ntrees = 60)
p1 = predict(fit, newdata = test, type = "prob")
result <- p1[, 2]
names(result) <- c()
write.csv(result, file = "PIOOLE.txt", row.names = FALSE)
