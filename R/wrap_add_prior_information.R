#' Add or compute prior information for a trajectory
#'
#' If you specify
#'
#' If the dataset contains a trajectory (see [add_trajectory()]) and expression data, this function will compute and add prior information using [generate_prior_information()]
#'
#' @inheritParams common_param
#' @param start_id The start cells
#' @param end_id The end cells
#' @param groups_id The grouping of cells, a dataframe with cell_id and group_id
#' @param groups_network The network between groups, a dataframe with from and to
#' @param features_id The features (genes) important for the trajectory
#' @param groups_n Number of branches
#' @param start_n Number of start states
#' @param end_n Number of end states
#' @param leaves_n Number of leaves
#' @param timecourse_continuous The time for every cell
#' @param timecourse_discrete The time for every cell in groups
#' @param dimred A dimensionality reduction of the cells (see [add_dimred()])
#' @param verbose Whether or not to print informative messages
#'
#' @keywords infer_trajectory
#' 
#' @return A dynwrap object with the prior information added.
#'
#' @examples
#' # add some prior information manually
#' dataset <- example_dataset
#' dataset <- add_prior_information(dataset, start_id = "Cell1")
#' dataset$prior_information$start_id
#'
#' # compute prior information from a trajectory
#' trajectory <- example_trajectory
#' trajectory <- add_prior_information(trajectory)
#' trajectory$prior_information$end_id
#'
#' @export
#'
#' @importFrom purrr discard list_modify
add_prior_information <- function(
  dataset,
  start_id = NULL,
  end_id = NULL,
  groups_id = NULL,
  groups_network = NULL,
  features_id = NULL,
  groups_n = NULL,
  start_n = NULL,
  end_n = NULL,
  leaves_n = NULL,
  timecourse_continuous = NULL,
  timecourse_discrete = NULL,
  dimred = NULL,
  verbose = TRUE
) {
  prior_information <- lst(
    start_id,
    end_id,
    groups_id,
    groups_network,
    features_id,
    groups_n,
    timecourse_continuous,
    timecourse_discrete,
    start_n,
    end_n,
    dimred
  ) %>% discard(is.null)

  if (!is.null(start_id)) {
    assert_that(all(start_id %in% dataset$cell_ids))
  }
  if (!is.null(end_id)) {
    assert_that(all(start_id %in% dataset$cell_ids))
  }
  if (!is.null(groups_id)) {
    if(is.vector(groups_id)) {
      assert_that(!is.null(names(groups_id)))
      groups_id <- enframe(groups_id, "cell_id", "group_id")
      prior_information$groups_id <- groups_id
    }
    assert_that(is.data.frame(groups_id))
    assert_that(setequal(colnames(groups_id), c("cell_id", "group_id")))
    assert_that(setequal(groups_id$cell_id, dataset$cell_ids))
  }
  if (!is.null(groups_network)) {
    assert_that(!is.null(groups_id))
    assert_that(setequal(colnames(groups_network), c("from", "to")))
    assert_that(all(groups_id$group_id %in% c(groups_network$to, groups_network$from)))
  }
  if (!is.null(features_id)) {
    assert_that(is_wrapper_with_expression(dataset))
    assert_that(all(features_id %in% colnames(dataset$counts)))
  }
  if (!is.null(timecourse_continuous)) {
    assert_that(all(is.numeric(timecourse_continuous)))
    assert_that(setequal(dataset$cell_ids, names(timecourse_continuous)))

    prior_information$timecourse_continuous <- timecourse_continuous[dataset$cell_ids]
  }
  if (!is.null(timecourse_discrete)) {
    assert_that(is.numeric(timecourse_discrete))
    assert_that(setequal(dataset$cell_ids, names(timecourse_discrete)))

    prior_information$timecourse_discrete <- timecourse_discrete[dataset$cell_ids]
  }
  if (!is.null(dimred)) {
    assert_that(is.matrix(dimred))
    assert_that(setequal(dataset$cell_ids, rownames(dimred)))

    prior_information$dimred <- dimred[dataset$cell_ids, ]
  }

  if (is_wrapper_with_trajectory(dataset) && is_wrapper_with_expression(dataset)) {
    if (verbose) cat("Calculating prior information using trajectory\n")

    # compute prior information and add it to the wrapper
    calculated_prior_information <-
      with(dataset, generate_prior_information(
        cell_ids = cell_ids,
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        progressions = progressions,
        divergence_regions = divergence_regions,
        expression = get_expression(dataset),
        feature_info = feature_info,
        cell_info = cell_info,
        given = prior_information,
        verbose = verbose
      ))

    prior_information <- purrr::list_modify(calculated_prior_information, !!!prior_information)
  }

  dataset %>% extend_with(
    "dynwrap::with_prior",
    prior_information = prior_information
  )
}


#' @inheritParams add_prior_information
#' @rdname add_prior_information
#'
#' @export
is_wrapper_with_prior_information <- function(dataset) {
  is_data_wrapper(dataset) && "dynwrap::with_prior" %in% class(dataset)
}

#' Extract the prior information from the trajectory
#'
#' For example, what are the start cells, the end cells, to which milestone does each cell belong to, ...
#'
#' The dataset has to contain a trajectory for this to work
#'
#' @inheritParams wrap_data
#' @inheritParams add_trajectory
#' @inheritParams add_expression
#' @param marker_fdr Maximal FDR value for a gene to be considered a marker
#' @param given Prior information already calculated
#'
#' @rdname add_prior_information
#'
#' @importFrom utils installed.packages head
#'
#' @export
generate_prior_information <- function(
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions,
  expression,
  feature_info = NULL,
  cell_info = NULL,
  marker_fdr = 0.005,
  given = NULL,
  verbose = FALSE
) {
  if (is.null(given)) {
    given <- list()
  }

  ## START AND END CELLS ##
  # convert milestone network to an igraph object
  is_directed <- any(milestone_network$directed)
  gr <- igraph::graph_from_data_frame(
    milestone_network,
    directed = is_directed,
    vertices = milestone_ids
  )

  # determine starting milestones
  if (verbose) cat("Computing start milestones\n")
  start_milestones <-
    if (is_directed) {
      deg_in <- igraph::degree(gr, mode = "in")
      deg_out <- igraph::degree(gr, mode = "out")
      names(which(deg_in == 0))
    } else {
      deg <- igraph::degree(gr)
      names(which(deg <= 1))
    }

  # if no milestones can be determined as start, pick a random one
  if (length(start_milestones) == 0) {
    start_milestones <- sample(milestone_ids, 1)
  }

  # if all milestones are start (ie. cyclic), pick a randoom one
  if (setequal(start_milestones, milestone_ids)) {
    start_milestones <- sample(start_milestones, 1)
  }

  # determine ending milestones
  if (verbose) cat("Computing end milestones\n")
  end_milestones <-
    if (is_directed) {
      names(which(igraph::degree(gr, mode = "out") == 0))
    } else {
      start_milestones
    }

  # define helper function for determining the closest cells
  determine_closest_cells <- function(mids) {
    pseudocell <- paste0("MILESTONECELL_", mids)
    tmp <-
      wrap_data(
        id = "tmp",
        cell_ids = c(cell_ids, pseudocell)
      ) %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        divergence_regions = divergence_regions,
        milestone_percentages = bind_rows(
          milestone_percentages,
          tibble(cell_id = pseudocell, milestone_id = mids, percentage = 1)
        )
      )
    geo <- calculate_geodesic_distances(tmp, waypoint_cells = pseudocell)[,cell_ids,drop = FALSE]
    unique(unlist(apply(geo, 1, function(x) {
      sample(names(which(x == min(x))), 1)
    })))
  }

  # determine start cells
  if ("start_id" %in% names(given)) {
    start_id <- given$start_id
  } else {
    if (verbose) cat("Computing start cells\n")
    if (length(start_milestones) > 0) {
      start_id <- determine_closest_cells(start_milestones)
    } else {
      start_id <- unique(progressions$cell_id)
    }
  }

  # determine end cells
  if ("end_id" %in% names(given)) {
    end_id <- given$end_id
  } else {
    if (verbose) cat("Computing end cells\n")
    if (length(end_milestones) > 0) {
      end_id <- determine_closest_cells(end_milestones)
    } else {
      end_id <- c()
    }
  }

  ## CELL GROUPING ##
  if ("groups_id" %in% names(given)) {
    groups_id <- given$groups_id
  } else {
    if (verbose) cat("Computing groups id\n")
    groups_id <-
      milestone_percentages %>%
      group_by(cell_id) %>%
      summarise(group_id = milestone_id[which.max(percentage)])
  }

  if ("groups_network" %in% names(given)) {
    groups_network <- given$groups_network
  } else {
    if (verbose) cat("Computing groups network\n")
    groups_network <- milestone_network %>% select(from, to)
  }

  ## MARKER GENES ##
  if ("features_id" %in% names(given)) {
    features_id <- given$features_id
  } else {
    if (verbose) cat("Computing features id\n")

    if (!is.null(feature_info) && "housekeeping" %in% colnames(feature_info)) {
      features_id <- feature_info %>%
        filter(!housekeeping) %>%
        pull(feature_id)
    } else {
      requireNamespace("ranger")
      data <- data.frame(
        PREDICT = groups_id %>% slice(match(rownames(expression), cell_id)) %>% pull(group_id) %>% as.factor,
        as.matrix(expression), ## TODO: can this work with a sparse matrix, somehow?
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      rf <- ranger::ranger(
        data = data,
        num.trees = 2000,
        mtry = min(50, ncol(expression)),
        sample.fraction = min(250 / nrow(expression), 1),
        min.node.size = 20,
        importance = "impurity",
        write.forest = FALSE,
        dependent.variable.name = "PREDICT",
        verbose = FALSE
      )
      rfsh <- ranger::ranger(
        data = data %>% mutate(PREDICT = sample(PREDICT)),
        num.trees = 2000,
        mtry = min(50, ncol(expression)),
        sample.fraction = min(250 / nrow(expression), 1),
        min.node.size = 20,
        importance = "impurity",
        write.forest = FALSE,
        dependent.variable.name = "PREDICT",
        verbose = FALSE
      )
      features_id <-
        rf$variable.importance %>%
        sort(decreasing = TRUE) %>%
        keep(~ . > quantile(rfsh$variable.importance, .75)) %>%
        names()
    }
  }

  ## NUMBER OF BRANCHES ##
  if ("groups_n" %in% names(given)) {
    groups_n <- given$groups_n
  } else {
    if (verbose) cat("Computing groups n\n")
    groups_n <- nrow(milestone_network)
  }

  ## NUMBER OF START STATES ##
  if ("start_n" %in% names(given)) {
    start_n <- given$start_n
  } else {
    if (verbose) cat("Computing start n\n")
    start_n <- length(start_milestones)
  }

  ## NUMBER OF END STATES ##
  if ("end_n" %in% names(given)) {
    end_n <- given$end_n
  } else {
    if (verbose) cat("Computing end n\n")
    end_n <- length(end_milestones)
  }

  ## NUMBER OF LEAVES STATES ##
  if ("leaves_n" %in% names(given)) {
    leaves_n <- given$leaves_n
  } else {
    if (verbose) cat("Computing end n\n")
    leaves_n <- length(unique(c(start_milestones, end_milestones)))
  }

  ## TIME AND TIME COURSE ##
  if ("timecourse_continuous" %in% names(given)) {
    timecourse_continuous <- given$timecourse_continuous
  } else {
    if (verbose) cat("Computing timecourse continuous\n")
    timecourse_continuous <-
      if (!is.null(cell_info) && "simulationtime" %in% colnames(cell_info)) {
        set_names(cell_info$simulationtime, cell_info$cell_id)
      } else {
        geo <- calculate_geodesic_distances_(
          cell_ids = cell_ids,
          milestone_ids = milestone_ids,
          milestone_network = milestone_network,
          milestone_percentages = milestone_percentages,
          divergence_regions = divergence_regions,
          waypoint_cells = start_id
        )
        apply(geo, 2, function(x) {
          min(x)
        })
      }

    timecourse_continuous[is.infinite(timecourse_continuous)] <- max(timecourse_continuous[!is.infinite(timecourse_continuous)])
  }

  if ("timecourse_discrete" %in% names(given)) {
    timecourse_discrete <- given$timecourse_discrete
  } else {
    if (verbose) cat("Computing timecourse discrete\n")
    timecourse_discrete <-
      if (!is.null(cell_info) && "timepoint" %in% colnames(cell_info)) {
        set_names(cell_info$timepoint, cell_info$cell_id)
      } else {
        cut(timecourse_continuous, breaks = min(10, length(unique(timecourse_continuous))), labels = FALSE)
      }
  }

  # return output
  lst(
    start_milestones,
    start_id,
    end_milestones,
    end_id,
    groups_id,
    groups_network,
    features_id,
    groups_n,
    timecourse_continuous,
    timecourse_discrete,
    start_n,
    end_n,
    leaves_n
  )
}
