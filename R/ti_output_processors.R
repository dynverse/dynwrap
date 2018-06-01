# some generic read functions
read_assignment <- function(
  path,
  col_types = readr::cols(readr::col_character(), readr::col_character())
) {
  output <- readr::read_csv(
    path,
    col_types =col_types
  )
  set_names(
    output %>% pull(2),
    output %>% pull(1)
  )
}

read_vector <- function(
  path,
  default = NULL
) {
  if(!is.null(default) && !file.exists(path)) {
    default
  } else {
    jsonlite::read_json(path) %>% unlist()
  }
}


# actual read functions
read_pseudotime <- function(dir_output) {
  read_assignment(
    file.path(dir_output, "pseudotime.csv"),
    readr::cols(cell_id=readr::col_character(), pseudotime=readr::col_double())
  )
}

read_milestone_network <- function(dir_output) {
  readr::read_csv(
    file.path(dir_output, "milestone_network.csv"),
    col_types = readr::cols(
      from = readr::col_character(),
      to = readr::col_character(),
      directed = readr::col_logical(),
      length = readr::col_number()
    )
  )
}

read_milestone_ids <- function(dir_output, milestone_network) {
  read_vector(file.path(dir_output, "milestone_ids.json"), unique(c(milestone_network$from, milestone_network$to))) %>%
    as.character()
}

read_milestone_percentages <- function(dir_output) {
  readr::read_csv(
    file.path(dir_output, "milestone_percentages.csv"),
    col_types = readr::cols(
      cell_id = readr::col_character(),
      milestone_id = readr::col_character(),
      percentage = readr::col_number()
    )
  )
}

read_progressions <- function(dir_output) {
  readr::read_csv(
    file.path(dir_output, "progressions.csv"),
    col_types = readr::cols(
      cell_id = readr::col_character(),
      from = readr::col_character(),
      to = readr::col_character(),
      percentage = readr::col_number()
    )
  )
}

read_divergence_regions <- function(dir_output, milestone_network) {
  path <- file.path(dir_output, "divergence.regions.csv")
  if(file.exists(path)) {
    readr::read_csv(
      file.path(dir_output, "progressions.csv"),
      col_types = readr::cols(
        cell_id = readr::col_character(),
        from = readr::col_character(),
        to = readr::col_character(),
        percentage = readr::col_number()
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

read_grouping <- function(dir_output) read_assignment(file.path(dir_output, "grouping.csv"))

read_group_ids <- function(dir_output, grouping) {
  read_vector(file.path(dir_output, "group_ids.json"), unique(grouping)) %>% as.character()
}

output_processors <- tribble(
  ~id, ~processor, ~required_files, ~optional_files, ~required_output, ~description, ~creates_trajectory,

  id="trajectory",
  processor=function(model, dir_output) {
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
  },
  required_files=c("milestone_network.csv", "progressions.csv", "milestone_percentages.csv"),
  optional_files=c("divergence_regions.csv", "milestone_ids.json"),
  required_output=c(),
  description="Creates a trajectory using a milestone network and progressions OR milestone percentages (one of either is required)",
  creates_trajectory = TRUE,

  id="linear",
  processor=function(model, dir_output) {
    pseudotime <- read_pseudotime(dir_output)
    model %>% add_linear_trajectory(pseudotime)
  },
  required_files=c("pseudotime.csv"),
  optional_files=c(),
  required_output=c(),
  description="Creates a linear trajectory from pseudotime",
  creates_trajectory=TRUE,

  id="pseudotime",
  processor=function(model, dir_output) {
    pseudotime <- read_pseudotime(dir_output)
    model %>% add_pseudotime(pseudotime)
  },
  required_files=c("pseudotime.csv"),
  optional_files=c(),
  required_output=c(),
  description="Add pseudotime, a single value for every cell which denotes its progression from start to finish",
  creates_trajectory=FALSE,

  id="grouping",
  processor=function(model, dir_output) {
    grouping <- read_grouping(dir_output)
    group_ids <- read_group_ids(dir_output, grouping)

    model %>%
      add_grouping(
        group_ids=group_ids,
        grouping=grouping
      )
  },
  required_files=c("grouping.csv"),
  optional_files=c("group_ids.json"),
  required_output=c(),
  description="Add a cell grouping, a single value for every cell which assigns it to one group",
  creates_trajectory=FALSE,

  id="cluster_graph",
  processor=function(model, dir_output) {
    milestone_network <- read_milestone_network(dir_output)
    model %>% add_cluster_graph(milestone_network)
  },
  required_files=c("milestone_network.csv"),
  optional_files=c(),
  required_output=c("grouping"),
  description="Creates a trajectory by using a cell grouping and a given network between these cell groups",
  creates_trajectory=TRUE
)
#' Wrap the output of a TI method
#'
#' @param model The model to start from, as generated by `wrap_data`
#' @param output_ids The ids of the outputs generated by the methods
#' @param dir_output The directory containing the output files
#'
#' @export
wrap_output <- function(model, output_ids, dir_output) {
  for (output_id in output_ids) {
    model <- output_processors$processor[[which(output_processors$id == output_id)]](model, dir_output)
  }
  model
}
