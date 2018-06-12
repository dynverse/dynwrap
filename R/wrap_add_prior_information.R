#' Add prior information to a data wrapper
#'
#' Note that the given data wrapper requires a trajectory and expression values
#' to have been added already.
#'
#' @param task A data wrapper to extend upon.
#' @param start_id The start cells
#' @param end_id The end cells
#' @param groups_id The grouping of cells, a dataframe with cell_id and group_id
#' @param grouping_network The network between groups, a dataframe with from and to
#' @param marker_feature_ids The features (genes) important for the trajectory
#' @param n_branches Number of branches
#' @param start_n Number of start states
#' @param end_n Number of end states
#' @param time The time for every cell
#' @param verbose Whether or not to print informative messages or not
#'
#' @export
#'
#' @importFrom testthat expect_true
#' @importFrom purrr discard list_modify
add_prior_information <- function(
  task,
  start_id = NULL,
  end_id = NULL,
  groups_id = NULL,
  grouping_network = NULL,
  marker_feature_ids = NULL,
  n_branches = NULL,
  start_n = NULL,
  end_n = NULL,
  time = NULL,
  verbose = TRUE
) {
  prior_information <- lst(
    start_id,
    end_id,
    groups_id,
    grouping_network,
    marker_feature_ids,
    n_branches,
    time,
    start_n,
    end_n
  ) %>% discard(is.null)

  if (!is.null(start_id)) {
    testthat::expect_true(all(start_id %in% task$cell_ids))
  }
  if (!is.null(end_id)) {
    testthat::expect_true(all(start_id %in% task$cell_ids))
  }
  if (!is.null(groups_id)) {
    testthat::expect_true(is.data.frame(groups_id))
    testthat::expect_setequal(colnames(groups_id), c("cell_id", "group_id"))
    testthat::expect_setequal(groups_id$cell_id, task$cell_id)
  }
  if (!is.null(grouping_network)) {
    testthat::expect_true(!is.null(groups_id))
    testthat::expect_setequal(colnames(grouping_network), c("from", "to"))
    testthat::expect_true(all(groups_id$group_id %in% c(grouping_network$to, grouping_network$from)))
  }
  if (!is.null(marker_feature_ids)) {
    testthat::expect_true(is_wrapper_with_expression(task))
    testthat::expect_true(all(marker_feature_ids %in% colnames(task$counts)))
  }

  if (is_wrapper_with_trajectory(task) && is_wrapper_with_expression(task)) {
    if (verbose) message("Calculating prior information using trajectory")

    # compute prior information and add it to the wrapper
    calculated_prior_information <-
      with(task, generate_prior_information(
        cell_ids = cell_ids,
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        progressions = progressions,
        divergence_regions = divergence_regions,
        expression = expression,
        feature_info = feature_info,
        cell_info = cell_info
      ))

    # update calculated prior information with given prior information (giving precendence to the latter)
    prior_information <- list_modify(calculated_prior_information, !!!prior_information)
  }

  task %>% extend_with(
    "dynwrap::with_prior",
    prior_information = prior_information
  )
}


#' Test whether an object is a task and contains prior information
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_prior_information <- function(object) {
  is_wrapper_with_trajectory(object) && "dynwrap::with_prior" %in% class(object)
}

#' Extract the prior information from the milestone network
#'
#' For example, what are the start cells, the end cells, to which milestone does each cell belong to.
#'
#' @inheritParams wrap_data
#' @inheritParams add_trajectory
#' @inheritParams add_expression
#' @param marker_fdr Maximal FDR value for a gene to be considered a marker
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
  marker_fdr = 0.005
) {
  ## START AND END CELLS ##
  # convert milestone network to an igraph object
  is_directed <- any(milestone_network$directed)
  gr <- igraph::graph_from_data_frame(
    milestone_network,
    directed = is_directed,
    vertices = milestone_ids
  )

  # determine starting and ending milestones
  start_milestones <-
    if (is_directed) {
      names(which(igraph::degree(gr, mode = "in") == 0))
    } else {
      names(which(igraph::degree(gr) <= 1))
    }

  # determine starting and ending milestones
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
          data_frame(cell_id = pseudocell, milestone_id = mids, percentage = 1)
        )
      )

    geo <- compute_tented_geodesic_distances(tmp, waypoint_cells = pseudocell)[,cell_ids,drop = FALSE]

    unique(unlist(apply(geo, 1, function(x) {
      sample(names(which(x == min(x))), 1)
    })))
  }

  # determine start cells
  if (length(start_milestones) > 0) {
    start_id <- determine_closest_cells(start_milestones)
  } else {
    start_id <- unique(progressions$cell_id)
  }

  # determine end cells
  if (length(end_milestones) > 0) {
    end_id <- determine_closest_cells(end_milestones)
  } else {
    end_id <- c()
  }

  ## CELL GROUPING ##
  groups_id <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    summarise(group_id = milestone_id[which.max(percentage)])
  grouping_network <- milestone_network %>% select(from, to)

  ## MARKER GENES ##
  if (!is.null(feature_info) && "housekeeping" %in% colnames(feature_info)) {
    marker_feature_ids <- feature_info %>%
      filter(!housekeeping) %>%
      pull(feature_id)
  } else {
    if ("scran" %in% rownames(utils::installed.packages())) {
      findMarkers <- get("findMarkers", "package:scran")
      markers <- findMarkers(t(expression), groups_id %>% slice(match(rownames(expression), cell_id)) %>% pull(group_id))

      marker_feature_ids <- map(markers, as, "data.frame") %>%
        map(rownames_to_column, "gene") %>%
        bind_rows() %>%
        filter(FDR < marker_fdr) %>%
        pull("gene")
    } else {
      warning("scran should be installed to determine marker features, will simply order by standard deviation")

      marker_feature_ids <- apply(expression, 2, sd) %>% sort() %>% rownames() %>% {utils::head(., round(length(.)*0.1))}
    }
  }

  ## NUMBER OF BRANCHES ##
  n_branches <- nrow(milestone_network)

  ## NUMBER OF START STATES ##
  start_n <- length(start_milestones)

  ## NUMBER OF END STATES ##
  end_n <- length(end_milestones)

  ## TIME AND TIME COURSE ##
  time <-
    if (!is.null(cell_info) && "simulationtime" %in% colnames(cell_info)) {
      set_names(cell_info$simulationtime, cell_info$cell_id)
    } else {
      NULL
    }

  timecourse <-
    if (!is.null(cell_info) && "timepoint" %in% colnames(cell_info)) {
      set_names(cell_info$timepoint, cell_info$cell_id)
    } else {
      NULL
    }

  # return output
  lst(
    start_milestones,
    start_id,
    end_milestones,
    end_id,
    groups_id,
    grouping_network,
    marker_feature_ids,
    n_branches,
    time,
    timecourse,
    start_n,
    end_n
  )
}
