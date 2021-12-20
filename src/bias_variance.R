# Functions to illustrate the bias-variance trade-off and double descent.

library(MASS) # For pseudo-inverse.

# Samples n_train rows and p columns from X for X_train and y_train.
# The remaining rows are used for X_test and y_test.
# If add_intercept is true, p+1 columns will be returned for X_train
#  because the intercept will be included.
splitTrainTest <- function(X, y, n_train, p, add_intercept) {
  train_idx <- sample(nrow(X), size=n_train, replace=FALSE)
  col <- sample((2:ncol(X)), size=p, replace=FALSE)
  X_train <- X[train_idx, col]
  if (add_intercept) {
    X_train <- cbind(1, X_train)
  }
  y_train <- y[train_idx]
  X_test  <- X[-(1:n_train), col]
  if (add_intercept) {
    X_test <- cbind(1, X_test)
  }
  y_test  <- y[-(1:n_train)]
  list (
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test
  )
}

# Simple unit tests of splitTrainTest
X <- data.frame(a=c(1,2,3), b=c(11, 12, 13), c=c(101, 102, 103),
                d=c(1001, 1002, 1003), e=c(1001, 1002, 1003))
y <- c(1.1, 2.2, 3.3)

out <- splitTrainTest(X, y, n_train=2, p=3, add_intercept=FALSE)
stopifnot(ncol(out$X_train)==3)
stopifnot(nrow(out$X_train)==2)
stopifnot(length(out$y_train)==2)

stopifnot(ncol(out$X_test)==3)
stopifnot(nrow(out$X_test)==1)
stopifnot(length(out$y_test)==1)

out <- splitTrainTest(X, y, 2, 3, add_intercept=TRUE)
stopifnot(ncol(out$X_train)==3+1) # +1 for intercept.
stopifnot(nrow(out$X_train)==2)
stopifnot(length(out$y_train)==2)

stopifnot(ncol(out$X_test)==3+1) # +1 for intercept.
stopifnot(nrow(out$X_test)==1)
stopifnot(length(out$y_test)==1)


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
  # If you do not use the one from the MASS package, choose wisely.
  b <- MASS::ginv(crossprod(X_train)) %*% crossprod(X_train, y)
  
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

