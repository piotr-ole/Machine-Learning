# SVM
library(ggplot2)

# Generowanie danych
rm(list = ls())
n <- 500

noise <- function(n) {
  return(rnorm(n, 0, 0.1))
}

x1_1 <- runif(n, min = -1, max = 1)
x2_1 <- sapply(x1_1, function(x) { sample(c(-1, 1), size = 1, prob = c(0.5, 0.5)) * sqrt(1 - x^2)}) + noise(n)
x1_0 <- runif(n, min = -2, max = 2)
x2_0 <- sapply(x1_0, function(x) { sample(c(-1, 1), size = 1, prob = c(0.5, 0.5)) * sqrt(4 - x^2)}) + noise(n)

dt = data.frame(x1 = c(x1_1, x1_0),
                x2 = c(x2_1, x2_0),
                y = c(rep(1, n), rep(0, n)))

ggplot(dt, aes(x = x1, y = x2, color = as.factor(y))) + geom_point() +
  scale_color_discrete(name = "class")


#SVM

library(e1071)

# liniowe j¹dro

model.linear <- svm(as.factor(y) ~. , data = dt, kernel = 'linear')
pred.linear <- predict(model.linear, newdata = dt)
pred.linear
#
#x1 <- seq(min(dt$x1), max(dt$x1), length.out = 100)
#x2 <- seq(min(dt$x2), max(dt$x2), length.out = 100)
#grd <- expand.grid(x1 = x1, x2 =  x2)
#z <- predict(model.linear, newdata = grd, type = 'response')
#z


p.linear <-  ggplot(dt, aes(x = x1, y = x2, color = as.factor(pred.linear), shape = as.factor(y))) + geom_point() +
  scale_color_discrete(name = "predicted class") + 
  scale_shape_discrete(name = "real class") +
  ggtitle('Linear kernel') +
  theme(plot.title = element_text(hjust = 0.5))
                

# radialne j¹dro

model.radial = svm(as.factor(y) ~. , data = dt, kernel = 'radial')
pred.radial = predict(model.radial, newdata = dt)

p.radial <- ggplot(dt, aes(x = x1, y = x2, color = as.factor(pred.radial), shape = as.factor(y))) + geom_point() +
  scale_color_discrete(name = "predicted class") + 
  scale_shape_discrete(name = "real class") +
  ggtitle('Radial kernel') +
  theme(plot.title = element_text(hjust = 0.5))

# wielomianowe j¹dro

model.poly = svm(as.factor(y) ~. , data = dt, kernel = 'polynomial')
pred.poly = predict(model.poly, newdata = dt)

p.poly <- ggplot(dt, aes(x = x1, y = x2, color = as.factor(pred.poly), shape = as.factor(y))) + geom_point() +
  scale_color_discrete(name = "predicted class") + 
  scale_shape_discrete(name = "real class") +
  ggtitle('Polynomial kernel') +
  theme(plot.title = element_text(hjust = 0.5))

library(patchwork)

p.linear + p.radial + p.poly
