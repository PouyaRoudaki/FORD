# .shuffle ----
#' Shuffle a Vector If It Has More Than One Element
#'
#' This helper function shuffles a vector only if it contains more than one element.
#' If the input is a single value (length 1), it simply returns it unchanged.
#'
#' @param x A vector to shuffle.
#'
#' @return A shuffled version of the input vector, or the original value if `x` has length 1.
#' @keywords internal
#'
#' @examples
#' .shuffle(c(1, 2, 3))   # Might return c(3, 1, 2)
#' .shuffle(7)            # Returns 7
.shuffle <- function(x) {
  if (length(x) > 1) sample(x) else x
}


# .get_neighbors ----
#' Core Logic for Randomized Neighbor Extraction in the Presence of Repetition (which occurs with high probability for discrete data)
#'
#' This function finds the indices of the first two nearest neighbors for the i-th row in matrix X.
#' To handle duplicate distances (e.g., from repeated points), it groups neighbors based on their distance:
#'   - First group: all neighbors with the smallest non-zero distance (excluding the point itself if the distance is zero)
#'   - Second group: all neighbors with the next smallest distance
#' Both groups are randomly shuffled before selecting the first two overall neighbors.
#'
#' @param i Integer, the index of the i-th observation in X
#' @param dists_mat A matrix of distances between all X_i and X_j
#' @param idxs_mat A matrix of all neighbor indices sorted by their distances for each observation
#' @param n Integer, the number of observations in the random variable X
#'
#' @return A list including a vector of first two groups of neighbors for X_i and a vector of the first two neighbors of X_i
#' @keywords internal
#'
.get_neighbors <- function(i, dists_mat, idxs_mat, n) {

  # Extract the distances of X_i to all other points
  dists <- dists_mat[i, ]

  # Extract the ordered indices of the neighbors of X_i
  idxs <- idxs_mat[i, ]

  # Smallest distance of X_i to any other point.
  # This distance may be zero if X_i is repeated in the data.
  first_dist <- dists[2]

  # Positions of the first group of neighbors that have the same smallest distance from X_i.
  # This might include position 1 (X_i itself) if the smallest distance is zero.
  group1_pos <- which(dists == first_dist)

  # Determine the last index of the first group.
  # If the first distance is not zero, then the first group starts from position 2,
  # and we add one to the group length to adjust for that.
  e1 <- length(group1_pos) + as.numeric(first_dist != 0)

  # Find the remaining positions not in group 1. These represent neighbors
  # with distances greater than the smallest, or the self-index (position 1) when there's no repetition.
  remaining_positions <- setdiff(seq_len(n), group1_pos)

  # Attempt to identify the second group of neighbors, if there are any left.
  # If the first group already includes all possible neighbors (e.g., all X_j are equal to X_i), skip this.
  if ((e1 + 1) <= n) {

    # The second smallest distance is at position e1 + 1
    second_dist <- dists[e1 + 1]

    # Find relative positions (within the remaining set) of neighbors with this second smallest distance
    group2_pos_relative <- which(dists[remaining_positions] == second_dist)

    # Map relative positions back to their positions in the full distance vector
    group2_pos <- remaining_positions[group2_pos_relative]

  } else {

    # If there are no neighbors beyond group 1, define group 2 as empty.
    # Note: using `shuffle()` and `c()` on empty vectors is safe and returns an empty vector as expected.
    group2_pos <- integer(0)
  }

  # Extract actual neighbor indices from group 1 and remove i if it's included (i.e., if distance = 0)
  group1_indices <- setdiff(idxs[group1_pos], i)

  # Extract actual neighbor indices from group 2
  group2_indices <- idxs[group2_pos]

  # Shuffle the neighbors within group 1
  group1_shuffled <- .shuffle(group1_indices)

  # Shuffle the neighbors within group 2
  group2_shuffled <- .shuffle(group2_indices)

  # Combine the shuffled first and second neighbor groups
  all_neighbors <- c(group1_shuffled, group2_shuffled)

  # Extract the first two neighbors (needed for downstream processing)
  first_two <- head(all_neighbors, 2)

  # Return both the full shuffled vector of first two groups of neighbors and the first two neighbors
  return(list(all = all_neighbors, first_two = first_two))
}


# randomized_nn ----
#' The Wrapper Function for All Observations That Performs Randomized Neighbor Extraction in the Presence of Repetition (which occurs with high probability for discrete data)
#'
#' This function finds the indices of the first two nearest neighbors for all observations in matrix X.
#' To handle duplicate distances (e.g., from repeated points), it groups neighbors based on their distance:
#'   - First group: all neighbors with the smallest non-zero distance (excluding the point itself if the distance is zero)
#'   - Second group: all neighbors with the next smallest distance
#' Both groups are randomly shuffled before selecting the first two overall neighbors.
#'
#' @param X A vector, matrix, or data frame of observations
#'
#' @return A list including:
#'   - a list of the first two groups of neighbors (shuffled) for all observations,
#'   - and a matrix containing the first two neighbors of each observation.
#'
#' @export
#'
#' @examples
#' # Create a small example matrix with some repeated rows
#' set.seed(42)
#' X <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5)
#'
#' # Run the randomized neighbor function
#' result <- randomized_nn(X)
#'
#' # View the shuffled full neighbor groups for each observation
#' result$two_groups_of_neighbors
#'
#' # View the matrix of the first two neighbors for each observation
#' result$two_neighbors
randomized_nn <- function(X) {


  # Convert X to a matrix, which is required for using RANN::nn2
  if(!is.matrix(X)) {
    X_mat = as.matrix(X)
  }else{
    X_mat = X
  }

  # Number of observations (sample size)
  n <- nrow(X_mat)

  # Compute the full neighborhood structure using Euclidean distances
  nn_results <- RANN::nn2(X_mat, k = n)

  # Apply get_neighbors(i) to all row indices from 1 to n
  neighbor_results <- lapply(
    1:n,
    function(i) .get_neighbors(i, nn_results$nn.dists, nn_results$nn.idx, n)
  )

  # Extract the list of all shuffled neighbors for each observation
  two_groups_of_neighbors <- lapply(neighbor_results, `[[`, "all")

  # Extract the first two neighbors for each observation into a matrix
  two_neighbors <- do.call(rbind, lapply(neighbor_results, `[[`, "first_two"))

  # Return both the full shuffled vector of first and second neighbor groups,
  # and the matrix of first two neighbors for all observations
  return(list(two_groups_of_neighbors = two_groups_of_neighbors,
              two_neighbors = two_neighbors))
}


