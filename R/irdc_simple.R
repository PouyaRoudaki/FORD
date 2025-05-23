# irdc_simple ----
#' Simple Estimator of the Integrated R-squared Dependence Coefficient (irdc_simple) for 1 Dimensional continuous X and Y
#'
#' The Simple Integrated R-squared Dependence Coefficient (irdc_simple) is a measure of dependence between
#' a continuous random variables Y and X, based on an i.i.d. sample of (Y, X).
#' The estimated coefficient is asymptotically guaranteed to lie between 0 and 1.
#' The measure is asymmetrical; that is, irdc_simple(X, Y) != irdc_simple(Y, X).
#' The measure equals 0 if and only if X is independent of Y, and it equals 1 if and only if
#' Y is a measurable function of X.
#' This coefficient has several applications; for example, it can be used for independence test.
#' This coefficient only implemented for the 1-Dimensional continuous random variable X and Y.
#' @param Y A vector of length n.
#' @param X A vector of length n.
#' @param na.rm Logical; if TRUE, missing values (NAs) will be removed. Default is TRUE.
#'
#' @details The value returned by `irdc_simple` can be positive or negative for finite samples,
#' but asymptotically, it is guaranteed to be between 0 and 1.
#' A small value indicates low dependence between Y and X, while a high value indicates strong dependence.
#' The `irdc_simple` function is used for testing the independence of variables.
#'
#' @return The Simple Integrated R-squared Dependence Coefficient (irdc_simple) between Y and X.
#' @importFrom stats complete.cases sd
#' @export
#' @author Mona Azadkia, Pouya Roudaki
#' @references Azadkia, M. and Roudaki, P. (2025). A NEW MEASURE OF DEPENDENCE: INTEGRATED R2
#' @seealso \code{\link{ford}},\code{\link{irdc}}
#' @examples
#' n = 1000
#' x <- matrix(runif(n * 3), nrow = n)
#' y <- (x[, 1] + x[, 2])
#' irdc_simple(y, x[, 1])
#' irdc_simple(y, x[, 2])
#' irdc_simple(y, x[, 3])
irdc_simple <- function(Y, X, na.rm = TRUE){

  # if inputs are not in proper matrix format change if possible
  # otherwise send error
  if(!is.vector(Y)) {
    Y = as.vector(Y)
  }
  if(!is.vector(X)) {
    X = as.vector(X)
  }

  # rearrenge both X and Y based on the ascending order of X
  ord <- order(X)
  X <- X[ord]
  Y <- Y[ord]

  if((length(Y) != length(X))) stop("The length of Y and X should be equal.")

  if (na.rm == TRUE) {
    # cases without na
    wo_na = complete.cases(Y,X)
    # NAs are removed here:
    X = X[wo_na]
    Y = Y[wo_na]
  }

  n = length(Y)

  if (n < 3) {
    stop("Not enough points to calculate the coefficient.")
  }

  # Find Y's ranks
  R_Y <- rank(Y, ties.method = "max")

  # vector of denominators:  (r_j-1)*(n-r_j)
  D <- (R_Y - 1) * (n - R_Y)

  # initials vector of numerator:  \sum_{j, r_j != 1 or n} \sum_{i!=j, i != j-1, i!=n} \bone{ r_{j} \in K_{i}} where K_{i} = [min(r_i, r_{i+1}),max(r_i, r_{i+1})]
  C_Y <- rep(0, n)

  # Find the maximum and minimum which is required for K(i)
  # 1 Right Shifted R_Y: c(R_Y[-1], R_Y[1]), Adjacent intervals (i, i+1), inclusive
  r_i <- R_Y[-n]
  r_ip1 <- R_Y[-1]
  L <- pmin(r_i, r_ip1)
  R <- pmax(r_i, r_ip1)

  # Find the vector of numerator:  \sum_{i!=j, i != j-1, i!=n } \bone{ Y_{j} \in K_{i}} where K_{i} = [min(r_i, r_{i+1}),max(r_i, r_{i+1})]
  # Difference array trick to count how many intervals contain each rank.
  # The idea here is to that we have an interval stabbing problem which can be solved in O(n) using a Difference Array Method
  Dvec <- numeric(n + 1L)
  Dvec <- Dvec + tabulate(L, nbins = n + 1L)
  Dvec <- Dvec - tabulate(R + 1L, nbins = n + 1L)
  C_rank <- cumsum(Dvec)[1:n]

  # C_Y[j] is number of intervals that contain R_Y[j]
  C_Y <- C_rank[R_Y]
  C_Y[2:(n - 1)] <- C_Y[2:(n - 1)] - 2L  # subtract intervals (j-1,j) and (j,j+1)
  C_Y[1] <- C_Y[1] - 1L  # only interval (1,2)
  C_Y[n] <- C_Y[n] - 1L  # only interval (n-1,n)

  # remove bad indices: first rank and last rank of Y.
  bad_id <- which(R_Y == 1 | R_Y == n)

  # Find irdc_simple: 1 - 1/2 \sum_{j, R_j != 1 or n} 1/ ((r_j-1) * (n-r_j))  * \sum_{i!=j, i != j-1, i!=n} \bone{ Y_{j} \in K_{i}} where K_{i} = (min(r_i, r_{i+1}),max(r_i, r_{i+1}))
  nu_n_dim_1 <- 1 - sum(C_Y[-bad_id] / D[-bad_id]) / 2

  # Return the irdc_simple
  return(nu_n_dim_1)
}
