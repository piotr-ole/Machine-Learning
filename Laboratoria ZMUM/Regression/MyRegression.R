# Regresja
library(ggplot2)
# Zadanie 1

rm(list = ls())
dat <- read.table('../data/realest.txt', header = TRUE)
head(dat)

model.linear <- lm(Price ~. , data = dat)
model.linear
summary(model.linear)

dat2 <- dat
dat2$Bedroom <- dat2$Bedroom + 1

model.linear2 <- lm(Price ~. , data = dat2)
summary(model.linear2)

model.linear_simple1 <- lm(Price ~ ., data = dat[c('Price', 'Bedroom')])
summary(model.linear_simple1)

model.linear_simple2 <- lm(Price ~ ., data = dat2[c('Price', 'Bedroom')])
summary(model.linear_simple2)

myhouse <- data.frame(Bedroom = 3, Space = 1500, Room = 8, Lot = 40,
                      Tax = 1000, Bathroom = 2, Garage = 1, Condition = 1)

# predict
price <- predict(model.linear, myhouse)

# Z definicji
coefs = model.linear$coefficients
price = coefs[1] + sum(coefs[2:length(coefs)] * myhouse)
price

# Zadanie 2

library(car)

dat <- USPop
head(USPop)
summary(USPop)
# model
time <- 0:21
popn <- nls(population ~ beta1 / (1 + exp(beta2 + beta3*time)) , data = dat, 
            start = list(beta1 = 350, beta2 = 4.5, beta3 = -0.3),
            trace = TRUE)

# estymacja wspolczynnikow
popn
beta1 <- 440.8332
beta2 <- 4.0324
beta3 <- -0.2161

# wrysowanie krzywej nls
library (dplyr)

dat <- dat %>% mutate(yi = beta1 / (1 + exp(beta2 + beta3*time))) %>% data.frame
head(dat)

ggplot(data = dat, aes(x = year, y = population)) + geom_point() +
  geom_line(data = dat, aes(x = year, y = yi), col= 'red')

pop_2015 <- beta1 / (1 + exp(beta2 + beta3*22.5)) # 22.5 bierze sie z tego, ze 1 to jest 10 lat, wiec jak 21 to 2000 to 2015 odpowiada 22.5

#analogicznie predictem
predict(popn, list(time = 22.5))

# ró¿nica wynika prawdopodobnie z dok³adniejszego przybli¿enia wewn¹trz popn

### Zadanie 3


fun <- function(x){
  y = 4.26 * (exp(-x) - 4 * exp(-2 * x) + 3 * exp(-3 * x))
}

ISE <- function(y , y_hat) {
  return(mean((y - y_hat)^2))
}


x <- sort(runif(n, 0, 3.5))
yteor <- sapply(x, fun)
y <- yteor + rnorm(n, mean = 0, sd = 0.1)

plot(x, y)
lines(x, yteor, col = 'orange', lwd = 2)

model.ksmooth <- ksmooth(x, y, x.points = x)
lines(model.ksmooth$x, model.ksmooth$y, col = 'blue', lwd = 2)

model.loess <- loess(formula = y ~ x, list(x, y))
pred.loess <- predict(model.loess, data = list(x,y))
lines(x, pred.loess, col = 'red', lwd = 2)

model.spline <- smooth.spline(x, y)
pred.spline <- predict(model.spline, data = list(x,y))
lines(pred.spline$x, pred.spline$y , col = 'magenta', lwd = 2)


## To samo tylko w ggplocie z patchworkiem

p1 <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) + geom_point() +
  geom_line(data = data.frame(x = x, y = yteor), col = 'orange', size = 1.5) +
  ggtitle('Teoretical curve') + 
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) + geom_point() +
  geom_line(data = data.frame(x = model.ksmooth$x, y = model.ksmooth$y), col = 'red', size = 1.5) +
  ggtitle('Ksmooth estimator') + 
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) + geom_point() +
  geom_line(data = data.frame(x = x, y = pred.loess), col = 'blue', size = 1.5) +
  ggtitle('Loess estimator') + 
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) + geom_point() +
  geom_line(data = data.frame(x = pred.spline$x, y = pred.spline$y), col = 'magenta', size = 1.5) +
  ggtitle('Smooth.spline estimator') + 
  theme(plot.title = element_text(hjust = 0.5))

library(patchwork)

(p1 + p2) / (p3 + p4)

# Pe³na profeska

ISE <- function(y , y_hat) {
  return(mean((y - y_hat)^2))
}

ISE.ksmooth = ISE(y, model.ksmooth$y)
ISE.spline = ISE(y, pred.spline$y)
ISE.loess = ISE(y, pred.loess)


#Zaleznosc ISE od n

n <- 300

ISE.k = list()
ISE.s = list()
ISE.l = list()
for(i in 1:n) {
  
  x <- sort(runif(n, 0, 3.5))
  yteor <- sapply(x, fun)
  y <- yteor + rnorm(n, mean = 0, sd = 0.1)
  model.ksmooth <- ksmooth(x, y, x.points = x)
  model.loess <- loess(formula = y ~ x, list(x, y))
  model.spline <- smooth.spline(x, y)
  ISE.k[[i]] = ISE(y, model.ksmooth$y)
  ISE.s[[i]] = ISE(y, pred.spline$y)
  ISE.l[[i]] = ISE(y, pred.loess)
}

plot(1:n, ISE.s)
