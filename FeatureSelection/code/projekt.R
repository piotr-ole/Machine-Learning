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

# Wczytanie danych

dat <- read.table('./data/artificial_train.data')
dat_labels <- read.table('./data/artificial_train.labels')
dat_labels <- dat_labels$V1
dat_labels[dat_labels == -1] = 0


############## wersja podstawowa bez selekcji cech ######################

#train <- normalization(train)
#test <- normalization(test)

samp <- sample(nrow(dat), replace = FALSE)
dt <- dat[samp, ]
dt_labels <- dat_labels[samp]
bcs_xgb = numeric(cv_num)
acc = numeric(cv_num)

for (i in seq(0, cv_num - 1)) {
  ind <- (nsize*i + 1):(nsize*(i+1))
  train <- dt[-ind, ]
  test <- dt[ind, ]
  train_lab <- dt_labels[-ind]
  test_lab <- dt_labels[ind]
  model.xgb <- xgboost(data = data.matrix(train[, vars_names]), label = train_lab,
                       objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.4)
  
  pred_xgb <-  predict(model.xgb, newdata = data.matrix(test[, vars_names]))
  predicted_classes_xgb <- ifelse( pred_xgb > 0.5, 1, 0)
  bcs_xgb[i + 1] <- balanced_acc(predicted_classes_xgb, test_lab)
  t <- table(predicted_classes_xgb, test_lab)
}
print(paste0(": Balanced accuraccy (xgb) after cross-val: ",mean(bcs_xgb)))

# Balanced accuraccy: 79,63%

########### RANDOM FOREST FEATURE SELECTION #################

fit_rf = randomForest(as.factor(class) ~. , data=cbind(dat, class = dat_labels))
varImpPlot(fit_rf, main = 'Feature Importance')
vars = importance(fit_rf)
vars_names <- rownames(vars)
ord = order(vars, decreasing = TRUE)
vars <- vars[ord]
vars_names <- vars_names[ord]

cv_num <- 5
nsize <- nrow(dat) / cv_num

for (j in seq(2,20)) {
  
  samp <- sample(nrow(dat), replace = FALSE)
  dt <- dat[samp, ]
  dt_labels <- dat_labels[samp]
  bcs_xgb = numeric(cv_num)
  acc = numeric(cv_num)
  for (i in seq(0, cv_num - 1)) {
  ind <- (nsize*i + 1):(nsize*(i+1))
  train <- dt[-ind, ]
  test <- dt[ind, ]
  train_lab <- dt_labels[-ind]
  test_lab <- dt_labels[ind]
  model.xgb <- xgboost(data = data.matrix(train[, vars_names[1:j]]), label = train_lab,
                      objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.3)
  
  pred_xgb <-  predict(model.xgb, newdata = data.matrix(test[, vars_names[1:j]]))
  predicted_classes_xgb <- ifelse( pred_xgb > 0.5, 1, 0)
  bcs_xgb[i + 1] <- balanced_acc(predicted_classes_xgb, test_lab)
  t <- table(predicted_classes_xgb, test_lab)
  }
  print(paste0(j, ": Balanced accuraccy (xgb) after cross-val: ",mean(bcs_xgb)))
}

########### FEATURE FILTERING + GLM IMPORTANCE SELECTION ##############
library(bounceR)
features_names <- featureFiltering(data = cbind(y = dat_labels, dat),
                                   target = "y",
                                   method = "cc",
                                   returning = "names")

indices <- indices_fun(train, features_names)

model.glm <- glm(as.factor(class) ~. , data = cbind(class = dat_labels, dat), family = 'binomial')
vars <- varImp(model.glm)
vars$names <- rownames(vars)
vars <- vars[order(vars$Overall, decreasing = TRUE), ]
vars_names <- vars$names


cv_num <- 5
nsize <- nrow(dat) / cv_num

for (j in seq(2,20)) {
  
  samp <- sample(nrow(dat), replace = FALSE)
  dt <- dat[samp, ]
  dt_labels <- dat_labels[samp]
  bcs_xgb = numeric(cv_num)
  acc = numeric(cv_num)
  for (i in seq(0, cv_num - 1)) {
    ind <- (nsize*i + 1):(nsize*(i+1))
    train <- dt[-ind, ]
    test <- dt[ind, ]
    train_lab <- dt_labels[-ind]
    test_lab <- dt_labels[ind]
    model.xgb <- xgboost(data = data.matrix(train[, vars_names[1:j]]), label = train_lab,
                         objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.3)
    
    pred_xgb <-  predict(model.xgb, newdata = data.matrix(test[, vars_names[1:j]]))
    predicted_classes_xgb <- ifelse( pred_xgb > 0.5, 1, 0)
    bcs_xgb[i + 1] <- balanced_acc(predicted_classes_xgb, test_lab)
    t <- table(predicted_classes_xgb, test_lab)
  }
  print(paste0(j, ": Balanced accuraccy (xgb) after cross-val: ",mean(bcs_xgb)))
}





################## INFORMATION GAIN FEATURE SELECTION ################################
library(dplyr)
df = cbind(dat, class = dat_labels)
inf = information.gain(class ~ ., df)
inf$vars <- rownames(inf)
inf_good <- inf %>% filter(attr_importance > 0)

ind <- sample(nrow(dat), 0.8 * nrow(dat))
train <- dat[ind, inf_good$vars]
test <- dat[-ind, inf_good$vars]

fit <- glm(class ~ . , data = cbind(train, class = dat_labels[ind]))
fit <- xgboost(data = data.matrix(train), label = dat_labels[ind],
          objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.3)

pred = predict(fit , newdata = test)
pred <-  predict(fit , newdata = data.matrix(test))
balanced_acc(pred_classes = ifelse(pred > 0.5, 1, 0), dat_labels[-ind])


cv_num <- 5
nsize <- nrow(dat) / cv_num
# xgboost
bcs_xgb <- numeric(cv_num)
samp <- sample(nrow(dat))
dt <- dat[samp,]
dt_labels <- dat_labels[samp]
for (i in seq(0, cv_num - 1)) {
  ind <- (nsize*i + 1):(nsize*(i+1))
  train <- dt[-ind, inf_good$vars]
  test <- dt[ind, inf_good$vars]
  train_lab <- dt_labels[-ind]
  test_lab <- dt_labels[ind]
  model.xgb <- xgboost(data = data.matrix(train), label = train_lab,
                       objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.3)
  
  pred_xgb <-  predict(model.xgb, newdata = data.matrix(test))
  predicted_classes_xgb <- ifelse( pred_xgb > 0.5, 1, 0)
  bcs_xgb[i + 1] <- balanced_acc(predicted_classes_xgb, test_lab)
  t <- table(predicted_classes_xgb, test_lab)
}
print(paste0("Balanced accuraccy (xgb) after cross-val: ", mean(bcs_xgb)))

# rpart
bcs_rpart <- numeric(cv_num)
samp <- sample(nrow(dat))
dt <- dat[samp,]
dt_labels <- dat_labels[samp]
for (i in seq(0, cv_num - 1)) {
  ind <- (nsize*i + 1):(nsize*(i+1))
  train <- dt[-ind, inf_good$vars]
  test <- dt[ind, inf_good$vars]
  train_lab <- dt_labels[-ind]
  test_lab <- dt_labels[ind]
  model.rpart <- rpart(formula = as.factor(class) ~. , data = cbind(train, class = train_lab))
  pred_rpart <-  predict(model.rpart, newdata = test)[, 2]
  predicted_classes_rpart <- ifelse( pred_rpart > 0.5, 1, 0)
  bcs_rpart[i + 1] <- balanced_acc(predicted_classes_rpart, test_lab)
  #t <- table(predicted_classes_rpart, test_lab)
}
print(paste0("Balanced accuraccy (rpart) after cross-val: ", mean(bcs_rpart)))


# glm

bcs_glm <- numeric(cv_num)
samp <- sample(nrow(dat))
dt <- dat[samp,]
dt_labels <- dat_labels[samp]
for (i in seq(0, cv_num - 1)) {
  ind <- (nsize*i + 1):(nsize*(i+1))
  train <- dt[-ind, inf_good$vars]
  test <- dt[ind, inf_good$vars]
  train_lab <- dt_labels[-ind]
  test_lab <- dt_labels[ind]
  df = cbind(train, class = train_lab)
  model.glm <- glm(as.factor(class) ~. , data = df, family = 'binomial')
  pred_glm <-  predict(model.glm, newdata = test)
  predicted_classes_glm <- ifelse( pred_glm > 0.5, 1, 0)
  bcs_glm[i + 1] <- balanced_acc(predicted_classes_glm, test_lab)
  #t <- table(predicted_classes_rpart, test_lab)
}
print(paste0("Balanced accuraccy (rpart) after cross-val: ", mean(bcs_glm)))

