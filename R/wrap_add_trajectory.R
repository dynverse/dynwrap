#' Add a trajectory to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param milestone_ids The ids of the milestones in the trajectory. Type: Character vector.
#' @param milestone_network The network of the milestones.
#'   Type: Data frame(from = character, to = character, length = numeric, directed = logical).
#' @param divergence_regions A data frame specifying the divergence
#'   regions between milestones (e.g. a bifurcation).
#'   Type: Data frame(divergence_id = character, milestone_id = character, is_start = logical).
#' @param milestone_percentages A data frame specifying what percentage milestone each cell
#'   consists of.
#'   Type: Data frame(cell_id = character, milestone_id = character, percentage = numeric).
#' @param progressions Specifies the progression of a cell along a transition in the milestone_network.
#'   Type: Data frame(cell_id = character, from = character, to = character, percentage = numeric).
#' @param ... extra information to be stored in the wrapper.
#'
#' @return The trajectory model
#'
#' @export
#'
#' @importFrom testthat expect_is expect_equal expect_true expect_false
add_trajectory <- function(
  data_wrapper,
  milestone_ids = NULL,
  milestone_network,
  divergence_regions = NULL,
  milestone_percentages = NULL,
  progressions = NULL,
  ...
) {
  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))
  cell_ids <- data_wrapper$cell_ids

  # infer milestone_ids if not given
  if(is.null(milestone_ids)) {
    milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  }

  # check milestone ids and milestone network
  testthat::expect_is(milestone_ids, "character")
  testthat::expect_false(any(duplicated(c(milestone_ids, cell_ids))))
  milestone_network <- check_milestone_network(milestone_ids, milestone_network)

  # check divergence regions
  if (is.null(divergence_regions) || (is.data.frame(divergence_regions) && nrow(divergence_regions) == 0)) {
    divergence_regions <- data_frame(divergence_id = character(0), milestone_id = character(0), is_start = logical(0))
  }
  divergence_regions <- check_divergence_regions(milestone_ids, divergence_regions)

  # check and process milestone percentages and progressions
  if (is.null(milestone_percentages) == is.null(progressions)) {
    if(!is.null(milestone_percentages)) {
      warning("Both milestone_percentages and progressions are given, will only use progressions")
      milestone_percentages <-  NULL
    } else {
      stop("Exactly one of ", sQuote("milestone_percentages"), " or ", sQuote("progressions"), " must be defined, the other must be NULL.")
    }

  }

  if (is.null(progressions)) {
    milestone_percentages <- check_milestone_percentages(cell_ids, milestone_ids, milestone_percentages)

    progressions <- convert_milestone_percentages_to_progressions(
      cell_ids,
      milestone_ids,
      milestone_network,
      milestone_percentages
    )

    check_progressions(cell_ids, milestone_ids, milestone_network, progressions)
  } else if (is.null(milestone_percentages)) {
    progressions <- check_progressions(cell_ids, milestone_ids, milestone_network, progressions)

    milestone_percentages <- convert_progressions_to_milestone_percentages(
      cell_ids,
      milestone_ids,
      milestone_network,
      progressions
    )

    check_milestone_percentages(cell_ids, milestone_ids, milestone_percentages)
  }

  # check whether cells in tents are explicitly mentioned in divergence_regions
  tents <- progressions %>%
    group_by(cell_id) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    group_by(from, to) %>%
    summarise() %>%
    ungroup()

  for (fr in unique(tents$from)) {
    te <- tents %>% filter(from == fr)
    divreg <- divergence_regions %>% filter(is_start, milestone_id == fr)
    if (nrow(divreg) >= 1) {
      divreg2 <- divergence_regions %>% filter(divergence_id == divreg$divergence_id)
      testthat::expect_true(all(te$to %in% divreg2$milestone_id), info = "All divergence regions need to be explicitly defined")
    } else {
      stop("Not all divergence regions are specified; check progressions or divergence regions")
    }
  }

  ## Find out trajectory type from milestone network (before adding FILTERED_CELLS)
  trajectory_type <- classify_milestone_network(milestone_network)$network_type

  ## Create a separate state "FILTERED_CELLS" if some cells have been filtered out
  na_ids <- setdiff(cell_ids, unique(milestone_percentages$cell_id))
  if (length(na_ids) != 0) {
    directed <- any(milestone_network$directed)
    na_mid <- "FILTERED_CELLS"

    milestone_percentages <- milestone_percentages %>%
      add_row(
        cell_id = na_ids,
        milestone_id = na_mid,
        percentage = 1
      )
    progressions <- progressions %>%
      add_row(
        cell_id = na_ids,
        from = na_mid,
        to = na_mid,
        percentage = 1
      )
    milestone_network <- milestone_network %>%
      add_row(
        from = milestone_ids,
        to = na_mid,
        length = sum(milestone_network$length) * 2,
        directed = directed
      )
    milestone_ids <- c(milestone_ids, na_mid)
  }

  # create output structure
  data_wrapper %>% extend_with(
    "dynwrap::with_trajectory",
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    milestone_percentages = milestone_percentages,
    progressions = progressions,
    trajectory_type = trajectory_type,
    ...
  )
}

#' Test whether an object is a data_wrapper and has a trajectory
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_trajectory <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_trajectory" %in% class(object)
}


# Check given trajectory input ----------------------------------------
#' @importFrom testthat expect_is expect_equal expect_true
check_milestone_network <- function(milestone_ids, milestone_network) {
  testthat::expect_is(milestone_network, "data.frame")
  testthat::expect_equal(ncol(milestone_network), 4)
  testthat::expect_setequal(colnames(milestone_network), c("from", "to", "length", "directed"))
  milestone_network <- milestone_network %>% select(from, to, length, directed)
  testthat::expect_equal(sapply(milestone_network, class), c(from = "character", to = "character", length = "numeric", directed = "logical"))
  testthat::expect_true(all(milestone_network$from %in% milestone_ids))
  testthat::expect_true(all(milestone_network$to %in% milestone_ids))
  testthat::expect_false(any(duplicated(milestone_network %>% select(from, to))))

  milestone_network
}

#' @importFrom testthat expect_is expect_equal expect_true
check_divergence_regions <- function(milestone_ids, divergence_regions) {
  testthat::expect_is(divergence_regions, "data.frame")
  testthat::expect_equal(ncol(divergence_regions), 3)
  testthat::expect_setequal(colnames(divergence_regions), c("divergence_id", "milestone_id", "is_start"))
  divergence_regions <- divergence_regions %>% select(divergence_id, milestone_id, is_start)
  testthat::expect_equal(sapply(divergence_regions, class), c(divergence_id = "character", milestone_id = "character", is_start = "logical"))
  testthat::expect_true(all(divergence_regions$milestone_id %in% milestone_ids))

  dr_check <- divergence_regions %>% group_by(divergence_id) %>% summarise(num_starts = sum(is_start))
  testthat::expect_true(all(dr_check$num_starts == 1))

  divergence_regions
}

#' @importFrom testthat expect_is expect_equal expect_true
check_milestone_percentages <- function(cell_ids, milestone_ids, milestone_percentages) {
  testthat::expect_is(milestone_percentages, "data.frame")
  testthat::expect_equal(ncol(milestone_percentages), 3)
  testthat::expect_setequal(colnames(milestone_percentages), c("cell_id", "milestone_id", "percentage"))
  milestone_percentages <- milestone_percentages %>% select(cell_id, milestone_id, percentage)
  testthat::expect_equal(sapply(milestone_percentages, class), c(cell_id = "character", milestone_id = "character", percentage = "numeric"))
  testthat::expect_true(all(milestone_percentages$cell_id %in% cell_ids))
  testthat::expect_true(all(milestone_percentages$milestone_id %in% milestone_ids))

  mp_check <- milestone_percentages %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
  testthat::expect_true(all(abs(mp_check$sum - 1) < 1e-8), info = "Sum of milestone percentages per cell_id should be exactly one")

  milestone_percentages
}

#' @importFrom testthat expect_is expect_equal expect_true
check_progressions <- function(cell_ids, milestone_ids, milestone_network, progressions) {
  testthat::expect_is(progressions, "data.frame")
  testthat::expect_equal(ncol(progressions), 4)
  testthat::expect_setequal(colnames(progressions), c("cell_id", "from", "to", "percentage"))
  progressions <- progressions %>% select(cell_id, from, to, percentage)
  testthat::expect_equal(sapply(progressions, class), c(cell_id = "character", from = "character", to = "character", percentage = "numeric"))
  testthat::expect_true(all(progressions$cell_id %in% cell_ids))
  testthat::expect_true(all(progressions$from %in% milestone_ids))
  testthat::expect_true(all(progressions$to %in% milestone_ids))

  pg_check <- progressions %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
  testthat::expect_true(all(pg_check$sum >= 0 & pg_check$sum < (1 + 1e-8)), info = "Sum of progressions per cell_id should be exactly one")

  pg_check <- progressions %>% left_join(milestone_network, by = c("from", "to"))
  testthat::expect_true(all(!is.na(pg_check$directed)), info = "All progressions (from, to) edges need to be part of the milestone network")

  progressions
}
