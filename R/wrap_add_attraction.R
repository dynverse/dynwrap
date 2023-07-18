#' Add attraction of cells using RNA velocity
#'
#' @inheritParams common_param
#' 
#' @return A dynwrap object with the attraction added.
add_attraction <- function(
  dataset
) {
  current <- get_expression(dataset, "expression")
  projected <- get_expression(dataset, "expression_future")

  calculate_attraction(current, projected)
}




#' Calculate the attraction of cells to other cells using velocity
#'
#' @param current Current expression
#' @param projected Projected expression based on RNA velocity
#' @param cells Which cells to use
#' @param n_waypoints Number of waypoints to use
#' @param k K knns
#'
#' @return Matrix containing the attraction (\[-1, 1\]) of each cell to the waypoint cells
calculate_attraction <- function(
  current,
  projected,
  cells = colnames(projected),
  n_waypoints = 50,
  k = 50
) {
  assertthat::assert_that(nrow(current) == nrow(projected))
  assertthat::assert_that(ncol(current) == ncol(projected))

  # select waypoint cells
  n_waypoints <- min(n_waypoints, length(cells))
  k <- min(n_waypoints, k)
  waypoint_cells <- sample(cells, n_waypoints)

  em <- as.matrix(current)
  ccells <- cells
  em <- em[, ccells]
  nd <- as.matrix(projected[, ccells] - current[, ccells])
  cgenes <- intersect(rownames(em), rownames(nd))
  nd <- nd[cgenes, ]
  em <- em[cgenes, ]

  # calculate correlation
  # this is an adapted version of colDeltaCorLog10 with waypoints
  transfo <- function(x) (log10(abs(x) + 1) * sign(x))

  nd2 <- transfo(nd)

  waypoint <- waypoint_cells[[1]]
  cell <- cells[[1]]

  emw <- em[, waypoint_cells]

  cors <- map(waypoint_cells, function(waypoint) {
    print(waypoint)
    diff <- transfo(emw[, waypoint] - em)
    cors <- pcor(diff, nd2)
    rownames(cors) <- waypoint
    cors[is.na(cors)] <- 0
    cors
  })
  attraction <- do.call(rbind, cors)
  colnames(attraction) <- colnames(em)
  attraction
}



pcor <- function(x, y = x, method = "pearson", use = "everything") {
  assertthat::assert_that(ncol(x) == ncol(y));
  matrix(purrr::map_dbl(seq_len(ncol(x)), ~ cor(x[,.], y[,.], method = method, use = use)), nrow = 1)
}
