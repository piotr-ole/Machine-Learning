library(caret)
library(randomForest)
library(dplyr)
library(rpart)
library(xgboost)
library(rpart.plot)

sample_fraction <- function(n, frac) {
  return(sample(n, n * frac))
}

write_factors <- function(df, filename = "factors.txt") {
  factors_num = vector(length = ncol(df))
  varnames <- colnames(df)
  for(i in 1:ncol(df)) {
    factors_num[i] <- paste0(varnames[i] ," : ", length(levels(as.factor(df[, i]))))
  }
  write.table(factors_num, file = filename)
}


normalize <- function(x, random = FALSE, median = TRUE) { #t = 0.03 jako trim
  if (random == TRUE)
  {
    median = sample(c(0,1), 1)
  }
  if (median == TRUE)
  {
    x[is.na(x) | x == ""] <- median(x[!is.na(x) & x != ""])
  } 
  else
  {
    x[is.na(x) | x == ""] <- mean(x[!is.na(x) & x != ""], trim = runif(1, 0.01, 0.03))
  }
  x <- (x - min(x)) / (max(x) - min(x)) #x <- x / max(x)
  return(x)
}

normalization <- function(x, random = FALSE) { ####UWAGA NA ZBIOR TESTOWY
  for (i in 1:(dim(x)[2] - 1)) {
    if (class(x[, i]) != "factor")
    {
      x[, i] <- normalize(x[, i], random = random, median = TRUE)
    }
  }
  return(x)
}

normalization_test <- function(x, random = FALSE) { ####UWAGA NA ZBIOR TESTOWY
  for (i in 1:ncol(x)) {
    if (class(x[, i]) != "factor")
    {
      x[, i] <- normalize(x[, i], random = random, median = TRUE)
    }
  }
  return(x)
}

encoding <- function(dataset, min_factor = 0) {
  encoded <- dataset[, 1]
  for (i in 1:(ncol(dataset) - 1)) {
    if (class(dataset[, i]) == "factor" & length(levels(dataset[, i])) > min_factor)
    {
      m <- t(matrix(as.integer(intToBits(as.integer(as.factor(dataset[, i])))), nrow = 32))
      encoded <- cbind(encoded, data.frame(m))
    }
    else
    {
      encoded <- cbind(encoded, dataset[, i])
    }
  }
  encoded <- cbind(encoded, class = dataset[, ncol(dataset)])
  empty <- sapply(encoded, function(x) { sum(as.integer(x))})
  ind <- which(empty == 0)
  encoded <- encoded[, -c(1, ind)]
  rownames(encoded) <- rownames(dataset)
  colnames(encoded) <- c(paste0("Var", seq(1, ncol(encoded) - 1)), "class")
  return(encoded)
}

one_hot_encoding <- function(dataset)
{
  encoded <- dataset[, 1]
  for (i in 1:(ncol(dataset) - 1)) {
    if (class(dataset[, i]) == "factor")
    {
      m <- model.matrix(~ dataset[, i]  + 0, data = dataset[, i])
      encoded <- cbind(encoded, data.frame(m))
    }
    else
    {
      encoded <- cbind(encoded, dataset[, i])
    }
  }
  encoded <- cbind(encoded, class = dataset[, ncol(dataset)])
  encoded <- encoded[, -c(1)]
  rownames(encoded) <- rownames(dataset)
  colnames(encoded) <- c(paste0("Var", seq(1, ncol(encoded) - 1)), "class")
  return(encoded)
}

numeric_encoding <- function(dataset) {
  for (i in 1:(ncol(dataset) - 1)) {
    if (class(dataset[, i]) == "factor")
    {
      dataset[, i] <- as.numeric(dataset[, i])
    }
  }
  return(dataset)
}

factor_repair <- function(df, treshold = 10) 
{
  for (i in 1:(ncol(df) - 1)) {
    if (length(levels(as.factor(df[, i]))) < treshold || class(df[, i]) == "factor") 
    {
      df[, i] <- as.character(df[, i])
      if (anyNA(df[, i]) == TRUE)
      {
        df[which(is.na(df[, i])) , i] <-  'Nas'
      }
      if (any(df[, i] == ''))
      {
        df[which(df[,i] == ''), i] <- 'Empty'
      }
      df[, i] <- as.factor(df[, i])
    }
  }
  return(df)
}

factor_repair_eval <- function(df, treshold = 10) 
{
  columns_indices <- c()
  for (i in 1:(ncol(df))) {
    if (length(levels(as.factor(df[, i]))) < treshold || class(df[, i]) == "factor") 
    {
      df[, i] <- as.character(df[, i])
      if (anyNA(df[, i]) == TRUE)
      {
        df[which(is.na(df[, i])) , i] <-  'Nas'
      }
      if (any(df[, i] == ''))
      {
        df[which(df[,i] == ''), i] <- 'Empty'
      }
      df[, i] <- as.factor(df[, i])
      columns_indices <- c(columns_indices, i)
    }
  }
  return(list(dataset = df, columns_repaired = columns_indices))
}

factor_repair_columns <- function(df, columns)
{
  for (i in columns) 
  {
    df[, i] <- as.character(df[, i])
    if (anyNA(df[, i]) == TRUE)
    {
      df[which(is.na(df[, i])) , i] <-  'Nas'
    }
    if (any(df[, i] == ''))
    {
      df[which(df[,i] == ''), i] <- 'Empty'
    }
    df[, i] <- as.factor(df[, i])
  }
  return(df)
}


delete_factors <- function(df, max_factor = Inf)
{
  indices <- c()
  for (i in 1:(ncol(df) - 1)) 
  {
    if (class(df[, i]) == "factor" && length(levels(df[, i])) > max_factor)
    {
      indices <- c(indices, i)
    }
  }
  df <- df[, -indices]
  return(df)
}

delete_and_encode <- function(df, delete_thresh = 1000, encode_tresh = 53) {
  df <- delete_factors(df, delete_thresh)
  df <- encoding(df, encode_tresh)
  return(df)
}

precision_rpart <- function(prob_from_first_class, test_classes, test_rownames) {
  p1 <- prob_from_first_class
  k = 0.1
  p1 <- sort(p1, decreasing = TRUE)
  p1 <- p1[1:(k*length(p1))]
  ids <- names(p1)
  p1 <- rep(1, length(p1))
  true_classes <- test_classes
  names(true_classes) <- test_rownames
  true_classes <- true_classes[ids]
  tab = table(p1, true_classes)
  print(paste0("Precyzja 10%: ", (tab[2] / (k * n * frac))))
  return(tab[2] / (k * n * frac))
}

precision_rf <- function(prob_from_first_class, test_classes, test_rownames) {
  p1 <- prob_from_first_class
  k = 0.1
  p1 <- sort(p1, decreasing = TRUE)
  p1 <- p1[1:(k*length(p1))]
  ids <- names(p1)
  p1 <- rep(1, length(p1))
  true_classes <- test_classes
  names(true_classes) <- test_rownames
  true_classes <- true_classes[ids]
  tab = table(p1, true_classes)
  print(paste0("Precyzja 10%: ", (tab[2] / (k * n * frac))))
  return(tab[2] / (k * n * frac))
}


save_features <- function(model_name, fetures, precision_val)
{
  write.table(unlist(features), file = paste0("./wyniki/", model_name, "_features_prec_0_",
                                              substr(precision_val, 3, nchar(precision_val))))
}

save_best_features <- function(model_name, features, means) {
  i <- which(means == max(means))
  save_features(model_name, features[i], means[i])
}

table_fun <- function(df, variables, class_label)
{
  n <- nrow(df)
  for (i in variables) 
  {
    t <- table(df[, i], class_label)
    t <- cbind(t, "1_perc" = (t[, 2] / (t[ , 1] + t[, 2])))
    t <- cbind(t, "0_perc" = (t[, 1] / (t[ , 1] + t[, 2])))
    t <- cbind(t, "all_perc" = ((t[ , 1] + t[, 2]) / n))
    t <- cbind(t, "0_perc_from_all_zeros" = ((t[ , 1] / sum(t[ , 1]))))
    t <- cbind(t, "1_perc_from_all_ones" = ((t[ , 2] / sum(t[ , 2]))))
    print(t[order(t[,1], decreasing = TRUE), ])
    nam <- colnames(t)
    res <- data.frame(t)
    colnames(res) <- nam 
  }
}

return_table <- function(df, var, class_label)
{
  n <- nrow(df)
  i <-  var
  t <- table(df[, i], class_label)
  t <- cbind(t, "1_perc" = (t[, 2] / (t[ , 1] + t[, 2])))
  t <- cbind(t, "0_perc" = (t[, 1] / (t[ , 1] + t[, 2])))
  t <- cbind(t, "all_perc" = ((t[ , 1] + t[, 2]) / n))
  t <- cbind(t, "0_perc_from_all_zeros" = ((t[ , 1] / sum(t[ , 1]))))
  t <- cbind(t, "1_perc_from_all_ones" = ((t[ , 2] / sum(t[ , 2]))))
  print(t[order(t[,1], decreasing = TRUE), ])
  nam <- colnames(t)
  res <- data.frame(t)
  colnames(res) <- nam 
  return(res)
}

