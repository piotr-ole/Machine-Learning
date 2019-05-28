setwd("C:/Users/piotr/Desktop/Github- projects/Machine-Learning/Laboratoria ZMUM/EvaluationMeasures")
rm(list = ls())
source('../Functions/functions.R')
library(ISLR)
library(rpart)
library(rpart.plot)

dat <- Default
head(dat)
dat <- numerize(dat, c('default', 'student'))
set <- trainTestSet(dat, 0.5)

model.glm <- glm(default ~. , data = set$train, family = 'binomial')
model.rpart <- rpart(as.factor(default) ~., data = set$train,
                     control = rpart.control(minsplit = 2, cp = 0.01))

rpart.plot(model.rpart)

glm.metrics <- metrics(model.glm, set$test, 1, type = 'glm')
rpart.metrics <- metrics(model.rpart, set$test, 1, type = 'rpart')


