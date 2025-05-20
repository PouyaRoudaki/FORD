## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(FORD)   
library(FOCI)

## ----example-1----------------------------------------------------------------
set.seed(42)
n <- 2000
p <- 100
X <- matrix(rnorm(n * p), ncol = p)
colnames(X) <- paste0("X", seq_len(p))
Y <- X[, 1] * X[, 2] + sin(X[, 1] * X[, 3]) + X[, 4]^2

## ----foci-result1-------------------------------------------------------------
result_foci_1 <- foci(Y, X, numCores = 1)
result_foci_1

## ----ford-result1-------------------------------------------------------------
result_ford_1 <- ford(Y, X, numCores = 1)
result_ford_1

## ----foci-result2-------------------------------------------------------------
result_foci_2 <- foci(Y, X, num_features = 5, stop = FALSE, numCores = 1)
result_foci_2

## ----ford-result2-------------------------------------------------------------
result_ford_2 <- ford(Y, X, num_features = 5, stop = FALSE, numCores = 1)
result_ford_2

