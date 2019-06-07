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

cv_num <- 5
rep <- 5
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

################## BORUTA FEATURE SELECTION ################################
library(Boruta)


boruta_output <- Boruta(as.factor(class) ~. , data=cbind(dat, class = dat_labels), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif) 



