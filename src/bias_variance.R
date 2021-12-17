# Functions to illustrate the bias-variance trade-off and double descent.

library(psych) # For pseudo-inverse

# TODO(jean): Use and test this.
splitTrainTest <- function(X, y, n_train, p) {
  train_idx <- sample(nrow(X), size=n_train, replace=FALSE)
  train_col <- sample((2:ncol(X)), size=p, replace=FALSE)
  X_train <- X[train_idx, train_col]
  X_train <- cbind(1, X_train)  # Add intercept.
  y_train <- y[train_idx]
  X_test  <- X[-(1:n_train), train_col]
  X_test <- cbind(1, X_test)  # Add intercept.
  y_test  <- y[-(1:n_train)]
  list (
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test
  )
}

# We need a form of linear regression that can handle more parameters than rows-- 
# that is, the overparameterized case.
#
# Usually regularization is used, but that requires a hyperparameter.  Instead we use
# ridgeless regression from:
# Hastie, Trevor, Andrea Montanari, Saharon Rosset, and Ryan J Tibshirani. 2019. 
#   “Surprises in High-Dimensional Ridgeless Least Squares Interpolation.”
#   arXiv Preprint arXiv:1903.08560
# Code from https://m-clark.github.io/posts/2021-10-30-double-descent/
fit_ridgeless = function(X_train, y, X_test, y_test){
  # get the coefficient estimates
  # Note: The pseudo-inverse function can have a lot of rounding error.
  # If you do not use the one from the psych package, choose wisely.
  b <- psych::Pinv(crossprod(X_train)) %*% crossprod(X_train, y)
  
  # get training/test predictions
  predictions_train <- X_train %*% b
  predictions_test <- X_test %*% b
  
  # get training/test error
  rmse_train <- sqrt(mean((y - predictions_train[,1])^2))
  rmse_test <- sqrt(mean((y_test - predictions_test[,1])^2))
  
  # return result
  list(
    # Commented out unneeded outputs
    # b = b,
    # predictions_train = predictions_train,
    # predictions_test  = predictions_test,
    rmse_train = rmse_train,
    rmse_test  = rmse_test
  )
}

