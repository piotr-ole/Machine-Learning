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

rm(list = ls())
setwd("~/R files/Machine Learning/Projekt 2 - ZMUM")

source("functions.R")

## wersja podstawowa bez selekcji cech

dat <- read.table('./data/artificial_train.data')
dat_labels <- read.table('./data/artificial_train.labels')
dat_labels <- dat_labels$V1
dat_labels[dat_labels == -1] = 0
thresh = 0.5
cv_num <- 5
bcs_xgb <- numeric(cv_num)
bcs_glm <- numeric(cv_num)

for (i in seq(cv_num)) {

samp <- sample.fraction(nrow(dat), 0.8)

train <- dat[samp, ]
train_lab <- dat_labels[samp]
test <- dat[-samp, ]
test_lab <- dat_labels[-samp]

train <- normalization(train)
test <- normalization(test)

# feature selection

## filtering

features_names <- featureFiltering(data = cbind(y = train_lab, train),
                            target = "y",
                            method = "cc",
                            returning = "names")

indices <- integer(length(features_names))
for (j in seq(length(features_names))) {
  indices[j] <- which(colnames(train) == features_names[i])
}
indices <- sort(indices)

train <- train[ , -indices]
test <- test[ , -indices]


# fitting model

fit_xgb <-  xgboost(data = data.matrix(train), label = train_lab,
              objective = 'binary:logistic', verbose = 0, nrounds = 10, eta = 0.4)

d <- data.frame(train)
d$y <- as.factor(train_lab)
fit_glm <- glm(data = d, formula = y ~ . , family = "binomial")

# prediction

p_xgb <-  predict(fit_xgb, newdata = data.matrix(test))
predicted_classes_xgb <- ifelse( p_xgb > thresh, 1, 0)
bcs_xgb[i] <- balanced_acc(predicted_classes_xgb, test_lab)
print(paste0("Balanced accuraccy (xgb) after ", i , "/", cv_num, " fold CV: ", bcs_xgb[i]))

pred_glm <- predict(fit_glm, newdata = data.frame(test), type = "response")
predicted_classes_glm <- ifelse( p_xgb > thresh, 1, 0)
bcs_glm[i] <- balanced_acc(predicted_classes_glm, test_lab)
print(paste0("Balanced accuraccy (glm) after ", i , "/", cv_num, " fold CV: \n", bcs_glm[i]))
}

print(paste0("Balanced [xgb] accuraccy after ", cv_num, " fold CV: ", mean(bcs_xgb)))

print(paste0("Balanced [glm] accuraccy after ", cv_num, " fold CV: ", mean(bcs_glm)))



### ----------- PONIZEJ PLAYGROUND -------------- ####


test_cc <- featureFiltering(data = cbind(y = dat_labels, dat),
                            target = "y",
                            method = "cc",
                            returning = "names")

indices <- integer(length(test_cc))
for (i in seq(length(test_cc))) {
  indices[i] <- which(colnames(dat) == test_cc[i])
}
indices <- sort(indices)

test_mr <- featureFiltering(data = cbind(y = dat_labels, dat),
                            target = "y",
                            method = "mrmr",
                            returning = "names")


cbind(y = dat_labels, dat)



dat_labels <- as.factor(dat_labels)

descrCor <-  cor(dat)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
highlyCorDescr
dat <- dat[-highlyCorDescr]

comboInfo <- findLinearCombos(dat)
comboInfo

filterCtrl <- sbfControl(functions = caretSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(dat, as.factor(dat_labels), sbfControl = filterCtrl)
rfWithFilter

str(dat)
str(dat_labels)
str(mdrrClass)

subsets <- c(1:5, 10, 15, 20, 25)

set.seed(10)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(dat, dat_labels,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile



test <- sim_data()
head(test)
