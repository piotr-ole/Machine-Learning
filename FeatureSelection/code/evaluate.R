setwd("C:/Users/Asus/Documents/Studia IAD/ZMUM/Machine-Learning/FeatureSelection")

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

train <- read.table('./data/artificial_train.data')
train_labels <- read.table('./data/artificial_train.labels')
train_labels <- train_labels$V1
train_labels[train_labels == -1] = 0

validate <- read.table('./data/artificial_valid.data')

fit_rf = randomForest(as.factor(class) ~. , data=cbind(train, class = train_labels))
vars = importance(fit_rf)
vars_names <- rownames(vars)
ord = order(vars, decreasing = TRUE)
vars <- vars[ord]
vars_names <- vars_names[ord]
important_vars <- vars_names[1:15]

model.xgb <- xgboost(data = data.matrix(train[, important_vars]), label = train_labels,
                     objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.3)

pred_xgb <-  predict(model.xgb, newdata = data.matrix(validate[, important_vars]))

important_vars

imp <- sapply(important_vars, function(x) { return(unlist(strsplit(x, 'V'))[2])})
imp <- as.numeric(imp)
imp <- sort(imp)
write.csv(imp, file = './results/PIOOLE_artificial_features.txt', row.names = FALSE)
write.csv(pred_xgb, file = './results/PIOOLE_artificial_prediction.txt', row.names = FALSE)


