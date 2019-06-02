setwd("~/R files/Machine Learning/Laboratoria")
rm(list = ls())
sample.fraction <- function(n, frac) {
  return(sample(n, n * frac))
}

# LDA i QDA

# W przypadku lda, zak³adam, ¿e rozk³ad cech wektora objaœniaj¹cego ma rozk³ad wielowymiarowy normalny
# Macierze kowariancji obu klas (tutaj zakladam dla dwoch) s¹ takie same, srednie sa rozne

# Dane Default (metody z pakietu MASS)
library(ISLR)
library(MASS)

# Test wbudowanych metod lda i qda na danych Default

dat <- Default
dat$default <- ifelse(dat$default == "No", 0, 1)
dat$default <- as.factor(dat$default)
dat$student <- ifelse(dat$student == "No", 0, 1)

samp <- sample.fraction(nrow(dat), 0.8)

train <- dat[samp, ]
test <- dat[-samp, ]

head(dat)

base.lda <- lda(default ~ ., data = train)
pred.lda <- predict(base.lda, newdata = test[ , -1])
table(pred.lda$class, test$default)

base.qda <- qda(default ~. , data = train)
pred.qda <- predict(base.qda, newdata = test[, -1])
table(pred.qda$class, test$default)

# W³asna implementacja lda
# Tutaj zalozylem, ze lda.fit zwraca nam funkcje klasyfikacyjna, wiec w funkcji predict,
# Beda przewidywane klasy na podstwie tej funkcji


lda.fit <- function(train, response) {
  train_0 <- train[which(response == 0), ]
  train_1 <- train[which(response == 1), ]
  # Pstwa apriori
  prior0 <- nrow(train_0) / nrow(train)
  prior1 <- nrow(train_1) / nrow(train)
  # srednie kazdej cechy
  means_0 <- sapply(train_0, mean)
  means_1 <- sapply(train_1, mean)
  # macierze kowariancji i macierz kowariancji wewnatrz grupowej
  cov_0 <- cov(train_0)
  cov_1 <- cov(train_1)
  n0 <- nrow(train_0)
  n1 <- nrow(train_1)
  W <- ( cov_0 * (n0 - 1) + cov_1 * (n1 - 1) ) / (n0 + n1 - 2)
  # Model
  model <- function(x) {
      a <- t(means_0 - means_1) %*% solve(W) %*% t(as.matrix(x))
      b <- -0.5 * t(means_0 - means_1) %*% solve(W) %*% (means_0 + means_1) + log(prior0 / prior1)
      fitted_model <- sapply(a, function(x, y) { x + y}, b)
     return ( fitted_model)
  }
}

predict.lda <- function(model, test) {
  f_val <- model(test)
  classes <- ifelse(f_val < 0, 1, 0)
  return(classes)
}

train_clean <- train[ , -1]
train_lab <- train[ , 1]
test_clean <- test[ , -1]
test_lab <- test[ , 1]

mylda <- lda.fit(train_clean, train_lab)

classes <- predict.lda(mylda, test_clean)

table(classes, test[, 1])


## W³asna implementacja QDA
# Tutaj robie inaczej, czyli estymuje prawdopodobienstwa aposteriori przynaleznosci do klas ( w ramach treiningu)

qda.fit <- function(train, response) {
  train0 <- train[which(response == 0), ]
  train1 <- train[which(response == 1), ]
  # Pstwa apriori
  prior0 <- nrow(train_0) / nrow(train)
  prior1 <- nrow(train_1) / nrow(train)
  # srednie kazdej cechy
  means0 <- sapply(train_0, mean)
  means1 <- sapply(train_1, mean)
  # macierze kowariancji i macierz kowariancji wewnatrz grupowej
  cov0 <- cov(train_0)
  cov1 <- cov(train_1)
  # Model
  return(list(train0, train1, prior0, prior1, means0, means1, cov0, cov1))
}

predict.qda <- function(model, test) {
  detCov0 = det(cov0)
  detCov1 = det(cov1)
  norm_const0 = 1 / ((2*pi)^(0.5*length(model$means0)) * detCov0^(0.5))
  norm_const1 = 1 / ((2*pi)^(0.5*length(model$means1)) * detCov1^(0.5))
  dense0 = exp(-(test))
}