# MARS

library(earth)
rm(list = ls())
# Przyklad 1

n <- 1000
p <- 50
x <- matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
x[ , 1] <- runif(n, min = 0, max = 4)
y <- rbinom(n = 1, 1, 0.5) * sqrt(x[, 1]) + rnorm(n, mean = 0, sd = 1)
dat <- data.frame(x = x,
                  y = y)
head(dat)
earth(y ~ . , data = dat)
