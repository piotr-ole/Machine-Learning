setwd("C:/Users/piotr/Desktop/Github- projects/Machine-Learning/FeatureSelection")

# Projekt 2

# libraries & sources

library(caret)
library(e1071)
library(klaR)
library(mlbench)
library(Hmisc)
library(randomForest)
library(devtools)
library(bounceR)
library(xgboost)
library(FSelector)

rm(list = ls())


source("functions.R")

## wersja podstawowa bez selekcji cech

dat <- read.table('./data/artificial_train.data')
dat_labels <- read.table('./data/artificial_train.labels')
dat_labels <- dat_labels$V1
dat_labels[dat_labels == -1] = 0

samp <- sample.fraction(nrow(dat), 0.8)

train <- dat[samp, ]
train_lab <- dat_labels[samp]
test <- dat[-samp, ]
test_lab <- dat_labels[-samp]

#train <- normalization(train)
#test <- normalization(test)


fit_rf = randomForest(as.factor(class) ~. , data=cbind(dat, class = dat_labels))
vars = importance(fit_rf)
vars_names <- rownames(vars)
ord = order(vars, decreasing = TRUE)
vars <- vars[ord]
vars_names <- vars_names[ord]

vars_names[1:10]

for (i in seq(2,15)) {
  
glm.model <- glm(class ~. , data = data.frame(train[,vars_names[1:i]], class = train_lab))
pred <- predict(glm.model, newdata = test[, vars_names[1:i]], type = 'response')
classes_pred <- ifelse(pred > 0.5, 1, 0)
t <- table(classes_pred, test_lab)
print(paste0('Acc: ', (t[1,1] + t[2,2]) / sum(t)))

}

for (i in seq(2,50)) {
  
  samp <- sample.fraction(nrow(dat))
  train <- dat[sample]
  
  for i in se

  model.xgb <- xgboost(data = data.matrix(train[, vars_names[1:i]]), label = train_lab,
                      objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.4)
  
  pred_xgb <-  predict(model.xgb, newdata = data.matrix(test[, vars_names[1:i]]))
  predicted_classes_xgb <- ifelse( pred_xgb > 0.5, 1, 0)
  bcs_xgb <- balanced_acc(predicted_classes_xgb, test_lab)
  print(paste0(i, ": Balanced accuraccy (xgb) after ",bcs_xgb))

}
