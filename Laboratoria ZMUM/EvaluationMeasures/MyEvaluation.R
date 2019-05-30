setwd("C:/Users/piotr/Desktop/Github- projects/Machine-Learning/Laboratoria ZMUM/EvaluationMeasures")
rm(list = ls())
source('../Functions/functions.R')
library(ISLR)
library(rpart)
library(rpart.plot)

dat <- Default
head(dat)
#dat <- numerize(dat, c('default', 'student'))
set <- trainTestSet(dat, 0.5)
train <- set$train
test <- set$test

model.glm <- glm(as.factor(default) ~. , data = train, family = 'binomial')
model.rpart <- rpart(as.factor(default) ~., data = train,
                     control = rpart.control(minsplit = 2, cp = 0.01))

rpart.plot(model.rpart)

glm.metrics <- metrics(model.glm, test, 1, type = 'glm')
rpart.metrics <- metrics(model.rpart, test, 1, type = 'rpart')

glm.metrics$precision_at_k
glm.metrics$precision
rpart.metrics$precision
rpart.metrics$precision_at_k

ROC(model = model.rpart, testSet = test, class_index = 1, type = 'rpart')
ROC(model = model.glm, testSet = test, class_index = 1, type = 'rpart')
