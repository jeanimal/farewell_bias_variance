# Functions to illustrate the bias-variance trade-off and double descent.

library(tidyverse)
library(ggplot2)

# Function for computing Moore-Penrose pseudo-inverse via SVD.
# From Ben at
# https://stats.stackexchange.com/questions/461423/moore-penrose-pseudo-inverse-fast-algorithm-in-r
pseudo_inv <- function(matrix) {
  SVD <- svd(matrix);
  DDD <- rep(0, length(SVD$d));
  for (i in 1:length(DDD)) { DDD[i] <- ifelse(SVD$d[i] == 0, 0, 1/SVD$d[i]);  }
  SVD$v %*% diag(DDD) %*% t(SVD$u); 
}

# We need a form of linear regression that can handle more parameters than rows-- 
# that is, the overparameterized case.
#
# Usually regularization is used, but that requires a hyperparameter.  Instead we use
# tidgeless regression from:
# Hastie, Trevor, Andrea Montanari, Saharon Rosset, and Ryan J Tibshirani. 2019. 
#   “Surprises in High-Dimensional Ridgeless Least Squares Interpolation.”
#   arXiv Preprint arXiv:1903.08560
# Code from https://m-clark.github.io/posts/2021-10-30-double-descent/
fit_ridgeless = function(X_train, y, X_test, y_test){
  # get the coefficient estimates
  b = pseudo_inv(crossprod(X_train)) %*% crossprod(X_train, y)
  
  # get training/test predictions
  predictions_train = X_train %*% b
  predictions_test  = X_test %*% b
  
  # get training/test error
  rmse_train = sqrt(mean((y - predictions_train[,1])^2))
  rmse_test  = sqrt(mean((y_test - predictions_test[,1])^2))
  
  # return result
  list(
    b = b,
    predictions_train = predictions_train,
    predictions_test  = predictions_test,
    rmse_train = rmse_train,
    rmse_test  = rmse_test
  )
}

