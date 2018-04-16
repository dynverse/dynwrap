#' Add prior information to a data wrapper
#'
#' Note that the given data wrapper requires a trajectory and expression values
#' to have been added already.
#'
#' @param data_wrapper A data wrapper to extend upon.
#'
#' @export
#'
#' @importFrom testthat expect_true
add_prior_information_to_wrapper <- function(
  data_wrapper
) {
  # check data wrapper
  testthat::expect_true(is_wrapper_with_trajectory(data_wrapper))
  testthat::expect_true(is_wrapper_with_expression(data_wrapper))

  # compute prior information and add it to the wrapper
  data_wrapper$prior_information <-
    with(data_wrapper, generate_prior_information(
      cell_ids = cell_ids,
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      progressions = progressions,
      divergence_regions = divergence_regions,
      counts = counts,
      feature_info = feature_info,
      cell_info = cell_info
    ))

  class(data_wrapper) <- c("dynwrap::with_prior", class(data_wrapper))

  data_wrapper
}

#' Test whether an object is a data_wrapper and contains prior information
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
#' @inheritParams add_trajectory_to_wrapper
#' @inheritParams add_expression_to_wrapper
#'
#' @export
generate_prior_information <- function(
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions,
  counts,
  feature_info = NULL,
  cell_info = NULL
) {
  requireNamespace("Seurat")

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
    traj <-
      wrap_data(
        id = "tmp",
        cell_ids = c(cell_ids, pseudocell)
      ) %>%
      add_trajectory_to_wrapper(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        divergence_regions = divergence_regions,
        milestone_percentages = bind_rows(
          milestone_percentages,
          data_frame(cell_id = pseudocell, milestone_id = mids, percentage = 1)
        )
      )

    geo <- compute_tented_geodesic_distances(traj, waypoint_cells = pseudocell)[,cell_ids,drop = FALSE]

    unique(unlist(apply(geo, 1, function(x) {
      sample(names(which(x == min(x))), 1)
    })))
  }

  # determine start cells
  if (length(start_milestones) > 0) {
    start_cells <- determine_closest_cells(start_milestones)
  } else {
    start_cells <- unique(progressions$cell_id)
  }

  # determine end cells
  if (length(end_milestones) > 0) {
    end_cells <- determine_closest_cells(end_milestones)
  } else {
    end_cells <- c()
  }

  ## CELL GROUPING ##
  grouping_assignment <-
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
    ident <- grouping_assignment %>%
      slice(match(rownames(counts), cell_id)) %>%
      pull(group_id) %>%
      factor() %>%
      setNames(rownames(counts))

    seurat <- Seurat::CreateSeuratObject(t(counts[names(ident), ]))

    seurat@ident <- ident

    old_warn <- getOption("warn")
    options(warn = 2)
    changing <- Seurat::FindAllMarkers(seurat, logfc.treshold = 1, min.pct = 0.4)
    options(warn = old_warn)

    marker_feature_ids <- changing %>%
      filter(abs(avg_logFC) >= 1) %>%
      .$gene %>%
      unique()
  }

  ## NUMBER OF BRANCHES ##
  n_branches <- nrow(milestone_network)

  ## NUMBER OF NUMBER OF END STATES ##
  n_end_states <- length(end_milestones)

  ## TIME AND TIME COURSE ##
  time <-
    if (!is.null(cell_info) && "simulationtime" %in% colnames(cell_info)) {
      setNames(cell_info$simulationtime, cell_info$cell_id)
    } else {
      NULL
    }

  timecourse <-
    if (!is.null(cell_info) && "timepoint" %in% colnames(cell_info)) {
      setNames(cell_info$timepoint, cell_info$cell_id)
    } else {
      NULL
    }

  # return output
  lst(
    start_milestones,
    start_cells,
    end_milestones,
    end_cells,
    grouping_assignment,
    grouping_network,
    marker_feature_ids,
    n_branches,
    time,
    timecourse,
    n_end_states
  )
}
