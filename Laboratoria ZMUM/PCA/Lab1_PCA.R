rm(list = ls())
library(ggplot2)
library(ggthemes)
data("USArrests")
head(USArrests)
dat <- data.frame(sapply(USArrests, scale))
rownames(dat) <- rownames(USArrests)
head(dat)

# PCA metod¹ na piechote

cov_matrix <- cov(dat)
eig <- eigen(cov_matrix)
eig_vectors <- eig$vectors
eig_values <- eig$values
#sk³adowe g³ówne
pc_components1 <- as.matrix(dat) %*% eig_vectors
pc_components1

# Sk³adowe g³ówne wersja R

pc_components2 <- princomp( ~. , data = USArrests, cor = TRUE)
pc_components2$scores
print(summary(pc_components2))


vars <- (pc_components2$sdev)^2
vars <- vars / sum(vars)
data = data.frame(vars = vars,
                  PC = seq(vars))

ggplot(data = data, aes(x = PC, y = vars)) + 
  geom_point() + 
  geom_line(group = 1, col = "red") +
  theme_test() +
  scale_y_continuous(labels = scales::percent)


biplot(pc_components2, choices = 1:2, pc.biplot = TRUE, cex = 0.6)
pc_components2$loadings


## Zadanie nr 3
library(ISLR)
dat2 <- Hitters
dat2 <- na.omit(dat2)

response <- dat2$Salary
datPCA <- dat2[, -which(colnames(dat2) == "Salary")]
datPCA$League <- ifelse(datPCA$League == 'N', 1, 0)
datPCA$Division <- ifelse(datPCA$Division == 'W', 1, 0)
datPCA$NewLeague <- ifelse(datPCA$NewLeague == 'N', 1, 0)
hitterspca <- princomp( ~ ., data = datPCA, cor = TRUE)

vars <- hitterspca$sdev^2
vars <- vars / sum(vars)

data = data.frame(vars = vars,
                  PC = seq(vars))

ggplot(data = data, aes(x = PC, y = vars)) + 
  geom_point() + 
  geom_line(group = 1, col = "red") +
  theme_test() +
  scale_y_continuous(labels = scales::percent)


accumulated_var <- cumsum(vars)

data = data.frame(vars = accumulated_var,
                  PC = seq(accumulated_var))

ggplot(data = data, aes(x = PC, y = vars)) + 
  geom_point() + 
  geom_line(group = 1, col = "red") +
  theme_gdocs() +
  scale_y_continuous(labels = scales::percent)

components <- hitterspca$scores

determination_coef <- function(y_pred, y_real) {
  R <- 1 - sum((y_real - y_pred)^2) / sum((y_real - mean(y_real))^2)
  return(R)
}

Rs <- numeric(ncol(components))
for (i in 1:ncol(components)) {
  train <- data.frame(components[, seq(i)])
  train$y <- response
  model <- lm(y ~ . , data = train)
  Rs[i] <- determination_coef(model$fitted.values, train$y)
}

plot(Rs, col ='red')

train <- data.frame(datPCA)
train$y <- response
model <- lm(y ~. , data = train)

s <- summary(model)
variables <- rownames(s$coefficients)
variables <- variables[order(s$coefficients[ , "Pr(>|t|)"], decreasing = TRUE)]
variables <- variables[variables != '(Intercept)']

Rs2 <- numeric(length(variables))
for (i in 1:ncol(datPCA)) {
  train <- data.frame(datPCA[, variables[seq(i)]])
  train$y <- response
  model <- lm(y ~ . , data = train)
  Rs2[i] <- determination_coef(model$fitted.values, train$y)
}

plot(Rs2, col ='red')
points(Rs, col ='blue')
