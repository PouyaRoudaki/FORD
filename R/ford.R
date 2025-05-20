####################################################################
# MAIN FUNCTIONS:
# ford_main: the core function for implementing the ford algorithm
# ford:  Performs feature ordering by Integrated R square dependence coefficient
####################################################################

# ford_main -------------------------------------------------------------------------
#' Variable selection by the ford algorithm
#
#' ford_main is the core function for ford, implementing the
#' variable selection algorithm based on the Integrated R square dependence coefficient \code{\link{irdc}}.
#
#' @param Y Vector of responses (length n)
#' @param X Matrix of predictors (n by p)
#' @param dist.type.X A string specifying the distribution type of X: either "continuous" or "discrete". Default is "continuous".
#' @param num_features Number of variables to be selected, cannot be larger than p. The default value is NULL and in that
#' case it will be set equal to p. If stop == TRUE (see below), then num_features is irrelevant.
#' @param stop Stops at the first instance of negative irdc, if TRUE.
#' @param numCores number of cores that are going to be used for parallelizing the process.
#' @details ford is a forward stepwise algorithm that uses the Integrated R square dependence coefficient (\code{\link{irdc}})
#' at each step, instead of the multiple correlation coefficient
#' as in ordinary forward stepwise. If stop == TRUE, the process is stopped at the first instance of
#' nonpositive irdc, thereby selecting subset of variables. Otherwise, a set of covariates of size
#' num_features, ordered according to predictive power (as measured by irdc) is produced.
#' @return An object of class "ford", with three attributes a vector of selected covariates,
#' their names and their cumulative dependence value with Y in decreasing order of predictive power.
#' @author Mona Azadkia, Pouya Roudaki
#' @references Azadkia, M. and Roudaki, P. (2025). A NEW MEASURE OF DEPENDENCE: INTEGRATED R2
#' \url{https://arxiv.org/pdf/???}.
#' @seealso \code{\link{irdc}}
ford_main <- function(Y, X, dist.type.X = "continuous", num_features = NULL, stop = TRUE, numCores = parallel::detectCores()){

  namesX <- colnames(X)
  if (is.null(num_features)) num_features = dim(X)[2]
  n = length(Y)
  p = ncol(X)
  nu = rep(0, num_features)
  index_select = rep(0, num_features)
  # select the first variable
  if (is.null(dim(X))) {
    seq_nu = irdc(Y, X, dist.type.X)
  } else {
    estimate_nu_Fixed_Y <- function(id){
      return(irdc(Y, X[, id], dist.type.X))
    }
    seq_nu = parallel::mclapply(seq(1, p), estimate_nu_Fixed_Y, mc.cores = numCores)
    seq_nu = unlist(seq_nu)
  }

  nu[1] = max(seq_nu)
  if (nu[1] <= 0 & stop == TRUE) return(NULL)
  index_max = min(which(seq_nu == nu[1]))
  index_select[1] = index_max
  count = 1

  # select rest of the variables
  while (count < num_features) {
    seq_nu = rep(0, p - count)
    # indices that have not been selected yet
    index_left = setdiff(seq(1, p), index_select[1:count])

    # find the next best feature
    estimate_nu_Fixed_Y_and_Sub_X <- function(id){
      return(irdc(Y, cbind(X[, c(index_select[1:count], id)]), dist.type.X))
    }

    if (length(index_left) == 1) {
      seq_nu = estimate_nu_Fixed_Y_and_Sub_X(index_left[1])
    } else {
      seq_nu = parallel::mclapply(index_left, estimate_nu_Fixed_Y_and_Sub_X, mc.cores = numCores)
      seq_nu = unlist(seq_nu)
    }
    nu[count + 1] = max(seq_nu)
    index_max = min(which(seq_nu == nu[count + 1]))

    if (nu[count + 1] <= nu[count] & stop == TRUE) break
    index_select[count + 1] = index_left[index_max]
    count = count + 1
  }

  selectedVar = data.table::data.table(index = index_select[1:count], names = namesX[index_select[1:count]])
  step_nu = nu
  result = list(selectedVar = selectedVar, step_nu = step_nu[1:count])
  class(result) = "ford"
  return(result)
}


# ford -------------------------------------------------------------------------
#' Variable selection by the ford algorithm
#'
#' ford is a variable selection algorithm based on  Integrated R square dependence coefficient \code{\link{irdc}}.
#'
#' @param Y Vector of responses (length n)
#' @param X Matrix of predictors (n by p)
#' @param dist.type.X A string specifying the distribution type of X: either "continuous" or "discrete". Default is "continuous".
#' @param num_features Number of variables to be selected, cannot be larger than p. The default value is NULL and in that
#' case it will be set equal to p. If stop == TRUE (see below), then num_features is irrelevant.
#' @param stop Stops at the first instance of negative irdc, if TRUE.
#' @param na.rm Removes NAs if TRUE.
#' @param standardize Standardize covariates if set equal to "scale" or "bounded". Otherwise will use the raw inputs.
#' The default value is "scale" and normalizes each column of X to have mean zero and variance 1. If set equal to "bounded"
#' map the values of each column of X to [0, 1].
#' @param numCores Number of cores that are going to be used for
#' parallelizing the variable selecction process.
#' @param parPlat Specifies the parallel platform to chunk data by rows.
#' It can take three values:
#' 1- The default value is set to 'none', in which case no row chunking
#' is done;
#' 2- the \code{parallel} cluster to be used for row chunking;
#' 3- "locThreads", specifying that row chunking will be done via
#' threads on the host machine.
#' @param printIntermed The default value is TRUE, in which case print intermediate results from the cluster nodes before final processing.
#' @details ford is a forward stepwise algorithm that uses the conditional dependence coefficient (\code{\link{irdc}})
#' at each step, instead of the multiple correlation coefficient
#' as in ordinary forward stepwise. If \code{stop} == TRUE, the process is stopped at the first instance of
#' nonpositive irdc, thereby selecting a subset of variables. Otherwise, a set of covariates of size
#' \code{num_features}, ordered according to predictive power (as measured by irdc) is produced.
#'
#' \emph{Parallel computation:}
#'
#' The computation can be lengthy, so the package offers two kinds of
#' parallel computation.
#'
#' The first, controlled by the argument \code{numCores},
#' specifies the number of cores to be used on the host
#' machine. If at a given step there are k candidate variables
#' under consideration for inclusion, these k tasks are assigned
#' to the various cores.
#'
#' The second approach, controlled by the argument \code{parPlat}
#' ("parallel platform"), involves the user first setting up a cluster via
#' the \pkg{parallel} package. The data are divided into chunks by rows,
#' with each cluster node applying ford to its data chunk.  The
#' union of the results is then formed, and fed through ford one more
#' time to adjust the discrepancies. The idea is that that last step
#' will not be too lengthy, as the number of candidate variables has
#' already been reduced.  A cluster size of r may actually
#' produce a speedup factor of more than r (Matloff 2016).
#'
#' Potentially the best speedup is achieved by using the two approaches
#' together.
#'
#' The first approach cannot be used on Windows platforms, as
#' \code{parallel::mcapply} has no effect.  Windows users should thus
#' use the second approach only.
#'
#' In addition to speed, the second approach is useful for diagnostics, as
#' the results from the different chunks gives the user an
#' idea of the degree of sampling variability in the
#' ford results.
#'
#' In the second approach, a random permutation is applied to the
#' rows of the dataset, as many datasets are sorted by one or more
#' columns.
#'
#' Note that if a certain value of a feature is rare in the
#' full dataset, it may be absent entirely in some chunk.
#' @return An object of class "ford", with attributes
#' \code{selectedVar}, showing the selected variables in decreasing
#' order of predictive power, and \code{step_nu}, listing
#' the 'irdc' values. Typically the latter will begin to level off at
#' some point, with additional marginal improvements being small.
#' @import data.table
#' @export
#' @author Mona Azadkia, Pouya Roudaki
#' @references Azadkia, M. and Roudaki, P. (2025). A NEW MEASURE OF DEPENDENCE: INTEGRATED R2
#' \url{https://arxiv.org/pdf/???}.
#' @references Matloff, N. (2016). Software Alchemy: Turning Complex
#' Statistical Computations into Embarrassingly-Parallel Ones.
#' \emph{J. of Stat. Software.}
#' @seealso \code{\link{irdc}}, \code{\link[XICOR]{xicor}}
#' @examples
#' # Example 1
#' n = 1000
#' p = 100
#' x <- matrix(rnorm(n * p), nrow = n)
#' colnames(x) = paste0(rep("x", p), seq(1, p))
#' y <- x[, 1] * x[, 10] + x[, 20]^2
#' # with num_features equal to 3 and stop equal to FALSE, ford will give a list of
#' # three selected features
#' result1 = ford(y, x, num_features = 3, stop = FALSE, numCores = 1)
#' result1
#' # Example 2
#' # same example, but stop according to the stopping rule
#' result2 = ford(y, x, numCores = 1)
#' result2
#' \dontrun{
#' # Windows use of multicore
#' library(parallel)
#' cls <- makeCluster(parallel::detectCores())
#' ford(y, x, parPlat = cls)
#' # run on physical cluster
#' cls <- makePSOCKcluster('machineA','machineB')
#' ford(y, x, parPlat = cls)
#' }
ford <- function(Y, X, dist.type.X = "continuous", num_features = NULL, stop = TRUE, na.rm = TRUE,
                 standardize = "scale", numCores = parallel::detectCores(),
                 parPlat = 'none', printIntermed = TRUE) {

  if(!(dist.type.X %in% c("continuous", "discrete"))){
    stop("Please set dist.type.X to either 'continuous' or 'discrete'.")
  }

  if (is.null(colnames(X))) {
    colnames(X) <- paste0('V',1:ncol(X))
    warning('X lacked column names, has been assigned V1, V2,...')
  }
  namesX <- colnames(X)

  # if inputs are not in proper format change if possible
  # otherwise send error
  if(!is.vector(Y)) {
    Y <- as.vector(unlist(Y))
  }
  if(!is.matrix(X)) {
    X <- as.matrix(X)
  }

  if (is.null(num_features)) num_features <- dim(X)[2]

  if((length(Y) != nrow(X))) stop("Number of rows of Y and X should be equal.")

  if (na.rm == TRUE) {
    # NAs are removed here:
    ok <- complete.cases(Y,X)
    X <- as.matrix(X[ok,])
    Y <- Y[ok]
  }


  n <- length(Y)

  if(n < 3) stop("Number of rows with no NAs should be bigger than 3.")

  p = ncol(X)

  if (num_features > p) stop("Number of features should not be larger than maximum number of original features.")
  if ((floor(num_features) != num_features) || (num_features <= 0)) stop("Number of features should be a positive integer.")

  if (!is.numeric(Y)) stop("currently ford does not handle factor Y")

  if (standardize == "scale") {
    for (i in 1:p) {
      if(length(unique(X[, i])) > 1) {
        X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])
      }else{
        stop(paste0("Column ", i, " of X is constant."))
      }
    }
  }

  if (standardize == "bounded") {
    for (i in 1:p) {
      if(length(unique(X[, i])) > 1) {
        X[,i] <- (X[,i] - min(X[,i])) / (max(X[, i]) - min(X[, i]))
      }else{
        stop(paste0("Column ", i, " of X is constant."))
      }
    }
  }

  if (parPlat[1] == 'none') {
    return(ford_main(Y, X, dist.type.X, num_features = num_features,
                     stop = stop, numCores = numCores))
  }

  # Many datasets are ordered by one or more columns; to
  # preserve iid-ness, randomize the row order; if we get here, we will
  # be chunking
  nr <- nrow(X)
  permRowNums <- sample(1:nr,nr,replace=FALSE)
  X <- X[permRowNums,]
  Y <- Y[permRowNums]

  rowNums <- parallel::splitIndices(length(Y), numCores)
  selectFromChunk <- function(nodeNum) {
    myRows <- rowNums[[nodeNum]]
    sel <- ford_main(Y[myRows], X[myRows,], dist.type.X, stop = stop,
                     numCores = numCores)$selectedVar$index
  }

  if(inherits(parPlat,'cluster')) {
    cls <- parPlat
  }else if(parPlat == 'locThreads') {
    # set up the cluster (in multicore case, it's virtual)
    cls <- parallel::makeCluster(numCores)
  } else stop('invalid parPlat')

  # worker nodes load library
  parallel::clusterEvalQ(cls, library(ford))
  # ship data to workers
  parallel::clusterExport(cls, c('Y', 'X','dist.type.X', 'rowNums', 'selectFromChunk'),
                          envir = environment())
  # drop-in replacement for mclapply
  slc <- parallel::parLapply(cls, seq(1, length(cls)), selectFromChunk)

  if (printIntermed) print(slc)

  slc <- Reduce(union, slc)
  ## Check whether windows of mac
  numCores <- if (.Platform$OS.type == 'windows') 1 else parallel::detectCores()

  res <- ford_main(Y, X[, slc], dist.type.X, num_features, stop, numCores=numCores)
  # must translate indices in reduced system to those of original
  newIdxs <- res$selectedVar$index
  origIdxs <- slc[newIdxs]
  res$selectedVar$index <- origIdxs

  res$step_nu <- res$step_nu[1:num_features]
  parallel::stopCluster(cls)

  return(res)
}
