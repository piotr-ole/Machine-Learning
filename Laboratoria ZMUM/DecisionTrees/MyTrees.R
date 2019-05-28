library(rpart)
library(rpart.plot)

data <- read.table('SAheart.data', sep = ',', header = TRUE, row.names = 1)
head(data)

samp <- sample(nrow(data), 0.8 *nrow(data))
train <- data[samp, ]
test <- data[-samp, ]

tree <- rpart(formula = chd ~ ., 
              data = train, 
              control = rpart.control(minsplit = 5, cp = 0.01))

rpart.plot(tree)

pred <- predict(tree, newdata =  test[, -ncol(test)])

classes <- ifelse(pred > 0.5, 1, 0)
t <- table(test$chd, classes)

acc <- (t[1, 1] + t[2 , 2]) / sum(t)
acc

head(data)


dropCols <- function(dataframe, colsVect) {
  return(dataframe[, !names(dataframe) %in% colsVect])
}

mostFreq <- function(named_vector) {
  t <- table(named_vector)
  return( list(value = names(which(t == max(t))), freq = max(t)))
}

dat <- dropCols(train, c('famhist', 'chd'))
f <- mostFreq(train$famhist)
f$value

mean_record <- sapply(dat , mean)
mean_record <- cbind(data.frame(t(mean_record)), famhist = f$value)

predict(tree, mean_record)


plotcp(tree)
tree$cptable


z <- prune.rpart(tree, cp=0.038)
rpart.plot(z)
