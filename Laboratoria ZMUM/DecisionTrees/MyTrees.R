# used libraries

library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)

source('../Functions/functions.R')

# Zadanie nr 1
setwd("C:/Users/piotr/Desktop/Github- projects/Machine-Learning/Laboratoria ZMUM/DecisionTrees")

data <- read.table('../data/SAheart.data', sep = ',', header = TRUE, row.names = 1)
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

# Zadanie 2
rm(list = ls())
library(ggplot2)
library(dplyr)

dat <- read.table('../data/earthquake.txt', header = TRUE)

plt <- dat %>% mutate(cl = ifelse(dat$popn == 'equake', 'Q', 'X')) %>% ggplot(aes(x = body, y = surface, color = popn, label = cl)) + 
    geom_text(aes(label = cl))
plt
head(dat)
#dat$popn <- ifelse(dat$popn == 'equake', 1, 0)
tree <-  rpart(formula = as.factor(popn) ~ body + surface, data = dat,
               control = rpart.control(minsplit = 5))
summary(tree)
rpart.plot(tree)

# Tutaj robimy jak (chyba regresji logistycznej)
#siatka
head(dat)
tree.grid = expand.grid(
                        body = seq(min(dat$body), max(dat$body), length = 50),
                        surface = seq(min(dat$surface), max(dat$surface), length = 50)
                        )

pred = predict(tree, newdata = tree.grid, type = 'prob')
tree
# To jest po to, ze de facto rysujemy kontur w 3d i to co jest na wykresie to przeciecie tego konturu z plaszczyzna z = 0
zpred <- ifelse(pred[,1] == 1, 1, -1)

ggplot() +
  geom_contour(data = cbind(tree.grid, zpred), aes(x = body, y = surface, z = zpred), bins = 1) +
  geom_point(data = dat, aes(x = body, y =surface))

#Okey czyli mamy taki algorytm rysowania krzywych rozdzielaj¹cych
'
1. Tworzymy model na danych treningowych
2. Tworzymy siatkê punktów na których chcemy dokonaæ predykcji modelu
3. Dokonujemy predict na siatce punktów
4. Tworzymy nowy set, cbind(siatka, predict)
5. Robimy contour tego zbioru, wa¿ne to daæ bins = 1, bo inaczej to on wrysowywuje dodatkowe wartstwice
6. Mozna sie pobawic w estatyke wykresu
'

## Zadanie nr 3
rm(list  = ls())
source('../Functions/functions.R')
fitness <- read.table('../data/fitness.txt', header = TRUE)

# Dopasowuje drzewo regresyjne do danych

regression.tree <- rpart(formula = Oxygen ~ . ,
                         data  = fitness,
                         control = rpart.control(minsplit = 2, cp = 0.01))
# Wykres drzewa
rpart.plot(regression.tree)
# Najwiekszy pobor tlenu dla biegacza, ktory ma najkrotszy czas

# Rekord testowy
medians <- data.frame(t(sapply(dropCols(fitness, 'Oxygen'), median)))
medians
pred <- predict(regression.tree, medians)
pred
# Zgadza sie z drzewem

# Podpunt d

plotcp(regression.tree)
printcp(regression.tree)
regression.tree$cptable

t <- prune.rpart(regression.tree, cp = 0.048)
rpart.plot(t)

# podpunkt e
rm(list = ls())
tree.reg <- rpart(Oxygen ~ RunTime + Age,
                  data = fitness,
                  control = rpart.control(minsplit = 2, cp = 0.02))
rpart.plot(tree.reg, digits = 4)

library(plotly)

tree.grid = expand.grid(
  RunTime = seq(min(fitness$RunTime), max(fitness$RunTime), length = 50),
  Age = seq(min(fitness$Age), max(fitness$Age), length = 50)
)

pred = predict(tree.reg, newdata = tree.grid)
head(pred)

dat <- cbind(tree.grid, pred)
head(dat)
# To ponizej cos nie dziala dobrze
persp(x = dat$RunTime, y = dat$Age, z = dat$pred)

?plot_ly
plot_ly(x = dat$RunTime, y = dat$Age, z = matrix(dat$pred, nrow = nrow(dat), ncol = ncol(dat))) %>% add_surface()

head(dat)

kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
p

