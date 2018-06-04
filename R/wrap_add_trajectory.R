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
#' @export
#'
#' @importFrom testthat expect_is expect_equal expect_true expect_false
add_trajectory <- function(
  data_wrapper,
  milestone_ids,
  milestone_network,
  divergence_regions,
  milestone_percentages = NULL,
  progressions = NULL,
  ...
) {
  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))
  cell_ids <- data_wrapper$cell_ids

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
    stop("Exactly one of ", sQuote("milestone_percentages"), " or ", sQuote("progressions"), " must be defined, the other must be NULL.")
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
    group_by(cell_id, from) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    group_by(from, to) %>%
    summarise() %>%
    ungroup()

  for (fr in unique(tents$from)) {
    te <- tents %>% filter(from == fr)
    divreg <- divergence_regions %>% filter(is_start, milestone_id == fr)
    divreg2 <- divergence_regions %>% filter(divergence_id == divreg$divergence_id)
    testthat::expect_true(all(te$to %in% divreg2$milestone_id), info = "All divergence regions need to be explicitly defined")
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
  testthat::expect_true(all(pg_check$sum >= 0 & pg_check$sum < (1 + 1e-8)), info = "Sum of milestone percentages per cell_id should be exactly one")

  pg_check <- progressions %>% left_join(milestone_network, by = c("from", "to"))
  testthat::expect_true(all(!is.na(pg_check$directed)), info = "All progressions (from, to) edges need to be part of the milestone network")

  progressions
}


# Process trajectory from file ----------------------------------------
read_milestone_network <- function(dir_output) {
  read_csv(
    file.path(dir_output, "milestone_network.csv"),
    col_types = cols(
      from = col_character(),
      to = col_character(),
      directed = col_logical(),
      length = col_number()
    )
  )
}

#' @rdname add_trajectory
#' @param dir_output The output directory
#'
#' @export
write_milestone_network <- function(milestone_network, dir_output) {
  write_csv(milestone_network, file.path(dir_output, "milestone_network.csv"))
}

read_milestone_ids <- function(dir_output, milestone_network) {
  read_vector(file.path(dir_output, "milestone_ids.json"), unique(c(milestone_network$from, milestone_network$to))) %>%
    as.character()
}

#' @rdname add_trajectory
#' @export
write_milestone_ids <- function(milestone_ids, dir_output) {
  jsonlite::write_json(milestone_ids, file.path(dir_output, "milestone_ids.json"))
}

read_milestone_percentages <- function(dir_output) {
  read_csv(
    file.path(dir_output, "milestone_percentages.csv"),
    col_types = cols(
      cell_id = col_character(),
      milestone_id = col_character(),
      percentage = col_number()
    )
  )
}

#' @rdname add_trajectory
#' @export
write_milestone_percentages <- function(milestone_percentages, dir_output) {
  write_csv(milestone_percentages, file.path(dir_output, "milestone_percentages.csv"))
}

read_progressions <- function(dir_output) {
  read_csv(
    file.path(dir_output, "progressions.csv"),
    col_types = cols(
      cell_id = col_character(),
      from = col_character(),
      to = col_character(),
      percentage = col_number()
    )
  )
}

#' @rdname add_trajectory
#' @export
write_progressions <- function(progressions, dir_output) {
  write_csv(progressions, file.path(dir_output, "progressions.csv"))
}

read_divergence_regions <- function(dir_output, milestone_network) {
  path <- file.path(dir_output, "divergence_regions.csv")
  if(file.exists(path)) {
    read_csv(
      path,
      col_types = cols(
        divergence_id = col_character(),
        milestone_id = col_character(),
        is_start = col_logical()
      )
    )
  } else {
    tibble(
      divergence_id = character(0),
      milestone_id = character(0),
      is_start = logical(0)
    )
  }
}

#' @rdname add_trajectory
#' @export
write_divergence_regions <- function(divergence_regions, dir_output) {
  write_csv(divergence_regions, file.path(dir_output, "divergence_regions.csv"))
}


# process trajectory
process_trajectory <- function(model, dir_output) {
  milestone_network <- read_milestone_network(dir_output)
  divergence_regions <- read_divergence_regions(dir_output, milestone_network)
  milestone_ids <- read_milestone_ids(dir_output, milestone_network)

  if(file.exists(file.path(dir_output, "progressions.csv"))) {
    progressions <- read_progressions(dir_output)
    model %>%
      add_trajectory(
        milestone_ids,
        milestone_network,
        divergence_regions,
        progressions = progressions
      )
  } else if(file.exists(file.path(dir_output, "milestone_percentages.csv"))) {
    milestone_percentages <- read_milestone_percentages(dir_output)
    model %>%
      add_trajectory(
        milestone_ids,
        milestone_network,
        divergence_regions,
        milestone_percentages = milestone_percentages
      )
  }
}

output_processors <- output_processors %>% add_row(
  id="trajectory",
  processor=list(process_trajectory),
  required_files=list(c("milestone_network.csv", "progressions.csv", "milestone_percentages.csv")),
  optional_files=list(c("divergence_regions.csv", "milestone_ids.json")),
  required_output=list(c()),
  description="Creates a trajectory using a milestone network and progressions OR milestone percentages (one of either is required)",
  creates_trajectory = TRUE
)

