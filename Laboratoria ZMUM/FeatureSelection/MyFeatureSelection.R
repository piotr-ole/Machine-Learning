# Feature Selection
rm(list = ls())

n <- 100
p <- 10000
L <- 100

simulation <- function(n, p , L) {
  
  max_cors <- replicate(L , {
    x <- matrix( rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p )
    y <- rnorm(n, mean = 0, sd = 1)
    max_cor <- max(apply(x, MARGIN = 2, FUN = function(t, y) { cor(t , y)}, y))
    return(max_cor)
  })
  
  p <- ggplot(data = data.frame(x = rep(1, length(max_cors)),
                                y = max_cors),
              aes(x = x, y = y)) +
    geom_boxplot()
  
  return(list(corelations = max_cors, boxplot = p))
}

s1 <- simulation(100, 1000, 100)
s1$boxplot
s2 <- simulation(100, 10000, 100)
s2$boxplot
s3 <- simulation(100, 50000, 100)
s3$boxplot
s4 <- simulation(100, 100000, 100)
s4$boxplot

(s1$boxplot + s2$boxplot) / (s3$boxplot + s4$boxplot)


# Porownanie filtrow (korelacja vs. informacja wzajemna)
n <- 100
sigm <- 1

choice = '1'

generate_pair <- function(choice, sigm) {
  
eps = rnorm(n, mean = 0, sd = sigm)

if (choice == '1') {
  x <- runif(n = n , min = 0, max = 1)
  y <- 2 * x + eps
} else if (choice == '2') {
  x <- runif(n = n , min = 0, max = 1)
  y <-  rbinom(n, 1, 0.5) * sqrt(x) + eps
} else if (choice == '3') {
  x <- runif(n = n , min = -1, max = 1)
  y <- x^2 + eps
} else if (choice == '4') {
  x <- runif(n = n , min = 0, max = 6)
  y <- sin(x) + eps
}

return(list(x = x, y = y))

}

set <- generate_pair('1', 1)
set$x

#simulation2 <- function()

choice = '2'
L <- 50
corelations <- list()
inf.gains <- list()
i = 1
for (s in seq(0 ,5, by = 0.1)) {
  
  cors <- replicate(L, 
  {
    set <- generate_pair(choice, s)
    cor(set$x, set$y)
  })
  
  infgs <- replicate(L, 
                     {
                       set <- generate_pair(choice, s)
                       df <- data.frame(y = set$y, x = set$x)
                       return(as.numeric(information.gain(y ~ x, df)))
                     })
  inf.gains[[i]] = mean(infgs)
  corelations[[i]] = mean(cors)
  i = i + 1
}

plot(seq(0, 5, by = 0.1), corelations)
plot(seq(0, 5, by = 0.1), inf.gains)

## Zadanie 3 
n = 1000
p = 500
true = c(1,2,3) # indeksy istotnych zmiennych:
x = matrix(0,nrow=n,ncol=p)
for(j in 1:p)x[,j]=rnorm(n)
b=numeric(p)
b[1:3] = 1
eta = x%*%b
probs = exp(eta)/(1+exp(eta))
y = rbinom(n,1,probs)
d = data.frame(x,y)

library(glmnet)

model = glmnet(x, y, family = 'binomial')
model$lambda
