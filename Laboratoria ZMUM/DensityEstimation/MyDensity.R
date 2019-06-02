
# 200 - elementowa mieszanka rozkladow
set.seed(7777)
n <- 200
p <- 0.9
w <- rbinom(n, 1, p)
x <- w * rnorm(n, 5, sd = 1) + (1-w) * rnorm(n, 10, sd = 1)
hist(x)
