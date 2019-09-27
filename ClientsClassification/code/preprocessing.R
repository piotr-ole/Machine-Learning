rm(list = ls())
source("functions.R")
train <- read.table("train.txt")
test <- read.table("testx.txt")

train_class <- train$class

train <- train[, -ncol(train)]
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
n <- 40000
p <- 0.95
to_cut <- p * n
indices_na_to_cut <- which(na_count > to_cut)
train <- train[, -indices_na_to_cut]
test <- test[, -indices_na_to_cut]

obj <- factor_repair_eval(train)
train <- obj$dataset
cols <- obj$columns_repaired
test <- factor_repair_columns(test, cols)

write_factors(train, "factors_train.txt")
write_factors(test, "factors_test.txt")

fac_train = c()
fac_test = c()
for(i in 1:ncol(train))
{
  if (class(train[, i]) == "factor") {
    fac_train = c(fac_train, i)
  }
  if (class(test[, i]) == "factor")
  {
    fac_test = c(fac_test, i)
  }
}

sum(fac_test == fac_train) == length(fac_train)

## Kod do testowania

#table_fun(train, fac_train[8:15], train_class)
res <- return_table(train, fac_train[17], train_class)
sum(res[res$`1_perc` < 0.02 & res$`0` > 10 , ]$all_perc)
#for (i in fac_train[1:10]) 
#{
#  print(sort(table(train[, i]), decreasing = TRUE))
#}

res <- return_table(train, fac_train[22], train_class)
table(test[, fac_test[22]])
n <- 1
res <- return_table(train, fac_train[n], train_class)
sort(table(test[, fac_test[n]]), decreasing = TRUE)
sum(names(table(test[, fac_test[n]])) == rownames(res)) == nrow(res)

# Zdefiniowanie przeksztalcen zmiennych

v1 <- c("7", "0", "Nas") # reszta Other
v2 <- c("0", "Nas") # reszta Other
v3 <- c("Nas", "3") # reszta Other
v4 <- c("0", "Nas") #reszta Other
v5 <- c("0", "Nas") #reszta Other
v6 <- c("0","Nas") #reszta Other
v7 <- c("Empty") # reszta Other
v9 <- c("RO12", "2Knk1KF") #reszta  Other
v16 <- c("Empty") # reszta Other
v19 <- c("9_Y1", "HLqf", "F3hy") # reszta Other
v20 <- c("cyKH", "DFSe", "em8I", "KfYH") # reszta Other
v23 <- c("me75fM6ugJ","7M47J5GA0pTYIFxg5uy", "DHn_WUyBhW_whjA88g9bvA64_", "Kxdu", "NKv3VA1BpP",
         "GjJ35utlTa_GNSvxxpb9ju") # reszta Other
v25 <- c("uKAI") #reszta Other
v27 <- c("NhsEn4L", "XfqtO3UdzaXh_") # reszta Other
v29 <- c("Empty") #reszta do Other
v34 <- c("FzaX", "Empty") # reszta Other
v43 <- c("F2FyR07IdsN7I") # reszta do Other

to_delete <- c(8, 10, 11, 12, 13, 14, 15, 17, 18, 30, 31, 32, 35, 37, 39)


## Przeksztalcenia

numbers <- c(1, 2, 3, 4, 5, 6, 7, 9, 16, 19, 20, 23, 25, 27, 29, 34, 43)
stay_list <- list(v1, v2, v3, v4, v5, v6, v7, v9, v16, v19, v20, v23, v25, v27, v29, v34, v43)

modify <- function(df, col_number, stays) {
  
  df[, col_number] <- sapply(df[, col_number], function(x, y) { if(sum( x == y) == 0) {return('Other')}
    else {return(as.character(x))}}, y = stays)
  df[, col_number] <- as.factor(df[, col_number])
  return(df)
}

for (i in 1:length(numbers)){
  train <- modify(train, fac_train[numbers[i]], stay_list[[i]])
}

for (i in 1:length(numbers)){
  test <- modify(test, fac_test[numbers[i]], stay_list[[i]])
}


## Teraz usuwanie
train <- train[, -fac_train[to_delete]]
test <- test[, -fac_test[to_delete]]

# dodanie labeli do train
train <- cbind(train, class = train_class)

preprocessed_train <- train
preprocessed_test <- test

write_factors(preprocessed_train, "factors_train.txt")
write_factors(preprocessed_test, "factors_test.txt")
