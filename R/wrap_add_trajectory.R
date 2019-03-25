#' Define a trajectory dataset given its milestone network and milestone percentages or progressions
#'
#' @inheritParams dynwrap
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
#' @param allow_self_loops Whether to allow self loops
#'   Type: Logical
#' @param ... extra information to be stored in the dataset
#'
#' @keywords create_trajectory
#'
#' @return The trajectory
#'
#' @export
add_trajectory <- function(
  dataset,
  milestone_ids = NULL,
  milestone_network,
  divergence_regions = NULL,
  milestone_percentages = NULL,
  progressions = NULL,
  allow_self_loops = FALSE,
  ...
) {
  # check whether object is a data wrapper
  assert_that(is_data_wrapper(dataset))
  cell_ids <- dataset$cell_ids

  # infer milestone_ids if not given
  if(is.null(milestone_ids)) {
    milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  }

  # check milestone ids and milestone network
  assert_that(is.character(milestone_ids))
  milestone_network <- check_milestone_network(milestone_ids, milestone_network, allow_self_loops = allow_self_loops)

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

    progressions <- check_progressions(cell_ids, milestone_ids, milestone_network, progressions)
  } else if (is.null(milestone_percentages)) {
    progressions <- check_progressions(cell_ids, milestone_ids, milestone_network, progressions)

    milestone_percentages <- convert_progressions_to_milestone_percentages(
      cell_ids,
      milestone_ids,
      milestone_network,
      progressions
    )

    milestone_percentages <- check_milestone_percentages(cell_ids, milestone_ids, milestone_percentages)
  }

  # check whether cells in tents are explicitly mentioned in divergence_regions
  tents <- progressions %>%
    filter(cell_id %in% cell_id[duplicated(cell_id)]) %>% # cell_id must occur multiple times
    group_by(from, to) %>%
    summarise(n = n()) %>%
    ungroup()

  for (fr in unique(tents$from)) {
    te <- tents %>% filter(from == fr)
    divreg <- divergence_regions %>% filter(is_start, milestone_id == fr)
    if (nrow(divreg) >= 1) {
      divreg2 <- divergence_regions %>% filter(divergence_id == divreg$divergence_id)
      assert_that(te$to %all_in% divreg2$milestone_id, msg = "All divergence regions need to be explicitly defined")
    } else {
      stop("Not all divergence regions are specified; check progressions or divergence regions")
    }
  }

  ## Find out trajectory type from milestone network
  classification <- classify_milestone_network(milestone_network)
  trajectory_type <- classification$network_type
  directed <- classification$directed

  # create output structure
  dataset %>% extend_with(
    "dynwrap::with_trajectory",
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    milestone_percentages = milestone_percentages,
    progressions = progressions,
    trajectory_type = trajectory_type,
    directed = directed,
    ...
  )
}

#' @inheritParams add_trajectory
#' @rdname add_trajectory
#'
#' @export
is_wrapper_with_trajectory <- function(trajectory) {
  is_data_wrapper(trajectory) && "dynwrap::with_trajectory" %in% class(trajectory)
}


# Check given trajectory input ----------------------------------------
check_milestone_network <- function(milestone_ids, milestone_network, allow_self_loops = FALSE) {
  assert_that(is.data.frame(milestone_network))
  assert_that(ncol(milestone_network) == 4)
  assert_that(setequal(colnames(milestone_network), c("from", "to", "length", "directed")))

  milestone_network <- milestone_network %>% select(from, to, length, directed)

  assert_that(is.character(milestone_network$from))
  assert_that(is.character(milestone_network$to))
  assert_that(is.numeric(milestone_network$length))
  assert_that(is.logical(milestone_network$directed))

  assert_that(milestone_network$from %all_in% milestone_ids)
  assert_that(milestone_network$to %all_in% milestone_ids)
  assert_that(!any(duplicated(milestone_network %>% select(from, to))))

  if (!allow_self_loops) {
    assert_that(!any((milestone_network$from == milestone_network$to) & milestone_network$length > 0))
  }

  check1 <- milestone_network
  if (allow_self_loops) check1 <- check1 %>% filter(from != to)
  check <-
    inner_join(
      check1 %>% transmute(from, to, left = "left"),
      check1 %>% transmute(from = to, to = from, right = "right"),
      by = c("from", "to")
    )
  assert_that(nrow(check) == 0, msg = "Milestone network should not contain A->B B->A edges")

  milestone_network
}

check_divergence_regions <- function(milestone_ids, divergence_regions) {
  assert_that(is.data.frame(divergence_regions))
  assert_that(ncol(divergence_regions) == 3)
  assert_that(setequal(colnames(divergence_regions), c("divergence_id", "milestone_id", "is_start")))

  divergence_regions <- divergence_regions %>% select(divergence_id, milestone_id, is_start)

  assert_that(is.character(divergence_regions$divergence_id))
  assert_that(is.character(divergence_regions$milestone_id))
  assert_that(is.logical(divergence_regions$is_start))
  assert_that(divergence_regions$milestone_id %all_in% milestone_ids)

  dr_check <- divergence_regions %>% group_by(divergence_id) %>% summarise(num_starts = sum(is_start))
  assert_that(all(dr_check$num_starts == 1))

  divergence_regions
}

check_milestone_percentages <- function(cell_ids, milestone_ids, milestone_percentages) {
  assert_that(is.data.frame(milestone_percentages))
  assert_that(ncol(milestone_percentages) == 3)
  assert_that(setequal(colnames(milestone_percentages), c("cell_id", "milestone_id", "percentage")))

  milestone_percentages <- milestone_percentages %>% select(cell_id, milestone_id, percentage)

  assert_that(is.character(milestone_percentages$cell_id))
  assert_that(is.character(milestone_percentages$milestone_id))
  assert_that(is.numeric(milestone_percentages$percentage))

  assert_that(milestone_percentages$cell_id %all_in% cell_ids)
  assert_that(milestone_percentages$milestone_id %all_in% milestone_ids)

  # fix precision errors
  milestone_percentages$percentage[milestone_percentages$percentage < 0 & milestone_percentages$percentage > -1e-6] <- 0
  milestone_percentages$percentage[milestone_percentages$percentage > 1 & milestone_percentages$percentage < 1+1e-6] <- 1

  mp_check <- tapply(milestone_percentages$percentage, milestone_percentages$cell_id, sum)
  assert_that(all(abs(mp_check - 1) < 1e-6), msg = "Sum of milestone percentages per cell_id should be exactly one")

  milestone_percentages
}

check_progressions <- function(cell_ids, milestone_ids, milestone_network, progressions) {
  assert_that(is.data.frame(progressions))
  assert_that(ncol(progressions) == 4)
  assert_that(setequal(colnames(progressions), c("cell_id", "from", "to", "percentage")))

  progressions <- progressions %>% select(cell_id, from, to, percentage)

  assert_that(is.character(progressions$cell_id))
  assert_that(is.character(progressions$from))
  assert_that(is.character(progressions$to))
  assert_that(is.numeric(progressions$percentage))

  assert_that(progressions$cell_id %all_in% cell_ids)
  assert_that(progressions$from %all_in% milestone_ids)
  assert_that(progressions$to %all_in% milestone_ids)

  # fix precision errors
  progressions$percentage[progressions$percentage < 0 & progressions$percentage > -1e-6] <- 0
  progressions$percentage[progressions$percentage > 1 & progressions$percentage < 1+1e-6] <- 1

  # check percentage sum
  pg_check <- tapply(progressions$percentage, progressions$cell_id, sum)
  assert_that(all(pg_check >= 0 & pg_check < (1 + 1e-6)), msg = "Sum of progressions per cell_id should be exactly one")

  # check edges
  pg_check <- progressions %>% left_join(milestone_network, by = c("from", "to"))
  assert_that(all(!is.na(pg_check$directed)), msg = "All progressions (from, to) edges need to be part of the milestone network")

  progressions
}
