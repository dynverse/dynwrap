#' Add prior information to a data wrapper
#'
#' Note that the given data wrapper requires a trajectory and expression values
#' to have been added already.
#'
#' @param dataset A data wrapper to extend upon.
#' @param start_id The start cells
#' @param end_id The end cells
#' @param groups_id The grouping of cells, a dataframe with cell_id and group_id
#' @param groups_network The network between groups, a dataframe with from and to
#' @param features_id The features (genes) important for the trajectory
#' @param groups_n Number of branches
#' @param start_n Number of start states
#' @param end_n Number of end states
#' @param timecourse_continuous The time for every cell
#' @param timecourse_discrete The time for every cell in groups
#' @param verbose Whether or not to print informative messages or not
#'
#' @export
#'
#' @importFrom testthat expect_true
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
  timecourse_continuous = NULL,
  timecourse_discrete = NULL,
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
    end_n
  ) %>% discard(is.null)

  if (!is.null(start_id)) {
    testthat::expect_true(all(start_id %in% dataset$cell_ids))
  }
  if (!is.null(end_id)) {
    testthat::expect_true(all(start_id %in% dataset$cell_ids))
  }
  if (!is.null(groups_id)) {
    testthat::expect_true(is.data.frame(groups_id))
    testthat::expect_setequal(colnames(groups_id), c("cell_id", "group_id"))
    testthat::expect_setequal(groups_id$cell_id, dataset$cell_ids)
  }
  if (!is.null(groups_network)) {
    testthat::expect_true(!is.null(groups_id))
    testthat::expect_setequal(colnames(groups_network), c("from", "to"))
    testthat::expect_true(all(groups_id$group_id %in% c(groups_network$to, groups_network$from)))
  }
  if (!is.null(features_id)) {
    testthat::expect_true(is_wrapper_with_expression(dataset))
    testthat::expect_true(all(features_id %in% colnames(dataset$counts)))
  }
  if (!is.null(timecourse_continuous)) {
    testthat::expect_true(all(is.numeric(timecourse_continuous)))
    testthat::expect_setequal(dataset$cell_ids, names(timecourse_continuous))

    prior_information$timecourse_continuous <- timecourse_continuous[dataset$cell_ids]
  }
  if (!is.null(timecourse_discrete)) {
    testthat::expect_true(is.numeric(timecourse_discrete))
    testthat::expect_setequal(dataset$cell_ids, names(timecourse_discrete))

    prior_information$timecourse_discrete <- timecourse_discrete[dataset$cell_ids]
  }

  if (is_wrapper_with_trajectory(dataset) && is_wrapper_with_expression(dataset)) {
    if (verbose) message("Calculating prior information using trajectory")

    # compute prior information and add it to the wrapper
    calculated_prior_information <-
      with(dataset, generate_prior_information(
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

  dataset %>% extend_with(
    "dynwrap::with_prior",
    prior_information = prior_information
  )
}


#' Test whether an object is a dataset and contains prior information
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_prior_information <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_prior" %in% class(object)
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

  # determine starting milestones
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
  groups_network <- milestone_network %>% select(from, to)

  ## MARKER GENES ##
  if (!is.null(feature_info) && "housekeeping" %in% colnames(feature_info)) {
    features_id <- feature_info %>%
      filter(!housekeeping) %>%
      pull(feature_id)
  } else {
    if ("scran" %in% rownames(utils::installed.packages())) {
      findMarkers <- get("findMarkers", asNamespace("scran"))
      markers <- findMarkers(t(expression), groups_id %>% slice(match(rownames(expression), cell_id)) %>% pull(group_id))

      features_id <- map(markers, as, "data.frame") %>%
        map(rownames_to_column, "gene") %>%
        bind_rows() %>%
        filter(FDR < marker_fdr) %>%
        pull("gene")
    } else {
      warning("scran should be installed to determine marker features, will simply order by standard deviation")

      features_id <- apply(expression, 2, sd) %>% sort() %>% rownames() %>% {utils::head(., round(length(.)*0.1))}
    }
  }

  ## NUMBER OF BRANCHES ##
  groups_n <- nrow(milestone_network)

  ## NUMBER OF START STATES ##
  start_n <- length(start_milestones)

  ## NUMBER OF END STATES ##
  end_n <- length(end_milestones)

  ## TIME AND TIME COURSE ##
  timecourse_continuous <-
    if (!is.null(cell_info) && "simulationtime" %in% colnames(cell_info)) {
      set_names(cell_info$simulationtime, cell_info$cell_id)
    } else {
      geo <- compute_tented_geodesic_distances_(
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

  timecourse_discrete <-
    if (!is.null(cell_info) && "timepoint" %in% colnames(cell_info)) {
      set_names(cell_info$timepoint, cell_info$cell_id)
    } else {
      cut(timecourse_continuous, breaks = min(10, length(unique(timecourse_continuous))), labels = FALSE)
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
    end_n
  )
}
