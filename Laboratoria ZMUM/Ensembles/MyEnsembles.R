setwd("C:/Users/piotr/Desktop/Github- projects/Machine-Learning/Laboratoria ZMUM/Ensembles")
rm(list = ls())
source('../Functions/functions.R')

require(rpart)
require(rpart.plot)
library(foreign)
dane <- read.arff("../data/diabetes.arff")

bagging <- function(formula, data, B = 100, ...) {
  trees <- list()#vector("list", B)
  for (i in 1:B) {
    samp <- sample(x = 1:nrow(data), size = nrow(data), replace = TRUE)
    bootstrap <- data[ samp , ]
    trees[[i]] <- rpart(formula = formula, data = bootstrap, ...)
  }
  return(trees)
}


set <- trainTestSet(dane)

# Metoda oparta na poledynczym drzewie

tree.model <- rpart(as.factor(class) ~ . , set$train)

pred.tree <- predict(tree.model, newdata = set$test, type = 'prob')
pred.tree2 <- predict(tree.model, newdata = set$test, type = 'class')
pred.classes <- ifelse(pred.tree[, 1] > 0.5 , 'tested_negative', 'tested_positive')
table(pred.classes, set$test$class)

# Metoda oparta o bagging

bagging.model <- bagging(as.factor(class) ~ . , set$train, B = 1000)


predict.bagging <- function(bag.model, test) {
  pred.tree = list()
  dt <- sapply(bag.model, function(x, test) { predict(x, newdata = test, type = 'class')}, test)
  pred  <- apply(dt, MARGIN = 1, FUN = function(x) { if (sum(x == 'tested_negative') > 50) return('tested_negative')
                                                    else return('tested_positive')})                                        
}

p <- predict.bagging(bagging.model, set$test)
table(p, set$test$class)

#### ZADANIE 2 ####

library(randomForest)
library(xgboost)
library(ROCR)
library(adabag)

dane <- read.arff("../data/diabetes.arff")

set <- trainTestSet(dane)
Labels <- ifelse(set$test$class == "tested_positive", 1, 0)

# drzewo

model.tree <- rpart(as.factor(class) ~. , set$train)
pred.tree <- predict(model.tree, newdata = set$test, type = 'prob')[, "tested_positive"]
prediction.tree <- prediction(pred.tree, Labels)
perf.tree <- performance(prediction.tree , 'tpr', 'fpr')
plot(perf.tree)

# bagging

bagging <- function(formula, data, B = 100, ...) {
  trees <- list()#vector("list", B)
  for (i in 1:B) {
    samp <- sample(x = 1:nrow(data), size = nrow(data), replace = TRUE)
    bootstrap <- data[ samp , ]
    trees[[i]] <- rpart(formula = formula, data = bootstrap, ...)
  }
  return(trees)
}

predict.bagging <- function(bag.model, test) {
  pred.tree = list()
  dt <- sapply(bag.model, function(x, test) { predict(x, newdata = test, type = 'class')}, test)
  pred  <- apply(dt, MARGIN = 1, FUN = function(x) { if (sum(x == 'tested_negative') > 50) return('tested_negative')
    else return('tested_positive')})                                        
}

model.bagg <- bagging(as.factor(class) ~. , set$train)
pred.bagg <- sapply(model.bagg, function(x, test) { predict(x, newdata = test, type = 'class')}, set$test)
# pstwo to tutaj suma pozytywnych wynikow glosowania / ilosc wszystkich glosowan
pred.bagg <- apply(pred.bagg, MARGIN = 1, FUN = function(x){sum(x == "tested_positive")/length(x)})
prediction.bagg <- prediction(pred.bagg, Labels)
perf.bagg <- performance(prediction.bagg, 'tpr', 'fpr')
plot(perf.bagg)


?boosting
