% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_add_cell_graph.R
\name{add_cell_graph}
\alias{add_cell_graph}
\title{Constructs a trajectory using a graph between cells, by mapping cells onto a set of backbone cells.}
\usage{
add_cell_graph(
  dataset,
  cell_graph,
  to_keep,
  milestone_prefix = "milestone_",
  ...
)
}
\arguments{
\item{dataset}{A dataset created by \code{\link[=wrap_data]{wrap_data()}} or \code{\link[=wrap_expression]{wrap_expression()}}}

\item{cell_graph}{The edges between cells, a dataframe containing the \emph{from} and \emph{to} cells, the *length, and whether this edge is \emph{directed}}

\item{to_keep}{Whether a cells is part of the backbone. May be a character vector with the identifiers of the backbone cells, or a named boolean vector whether a cell is from the backbone}

\item{milestone_prefix}{A prefix to add to the id of the cell ids when they are used as milestones, in order to avoid any naming conflicts,}

\item{...}{extra information to be stored in the wrapper.}
}
\value{
The dataset object with trajectory information, including:
\itemize{
\item \emph{milestone_ids}: The names of the milestones, a character vector.
\item \emph{milestone_network}: The network between the milestones, a dataframe with the \emph{from} milestone, \emph{to} milestone, \emph{length} of the edge, and whether it is \emph{directed}.
\item \emph{divergence_regions}: The regions between three or more milestones where cells are diverging, a dataframe with the divergence id (\emph{divergence_id}), the milestone id (\emph{milestone_id}) and whether this milestone is the start of the divergence (\emph{is_start})
\item \emph{milestone_percentages}: For each cell its closeness to a particular milestone, a dataframe with the cell id (\emph{cell_id}), the milestone id (\emph{milestone_id}), and its \emph{percentage} (a number between 0 and 1 where higher values indicate that a cell is close to the milestone).
\item \emph{progressions}: For each cell its progression along a particular edge of the \emph{milestone_network}. Contains the same information as \emph{milestone_percentages}. A dataframe with cell id (\emph{cell_id}), \emph{from} milestone, \emph{to} milestone, and its \emph{percentage} (a number between 0 and 1 where higher values indicate that a cell is close to the \emph{to} milestone and far from the \emph{from} milestone).
}
}
\description{
The cells that are part of the backbone will form the trajectory. All other cells are moved towards the nearest cell that is part of the backbone.
}
\examples{
library(dplyr)
dataset <- wrap_data(cell_ids = letters)

backbone_cell_graph <- tibble::tibble(
  from = letters[1:10],
  to = letters[2:11],
  length = 1,
  directed = TRUE
)
leaves_cell_graph <- tibble::tibble(
  from = letters[12:26],
  to = sample(letters[1:11], 15, replace = TRUE),
  length = 1,
  directed = TRUE
)
cell_graph <- bind_rows(backbone_cell_graph, leaves_cell_graph)
cell_graph
to_keep <- letters[1:11]
to_keep

trajectory <- add_cell_graph(dataset, cell_graph, to_keep)

# for plotting the result, install dynplot
#- dynplot::plot_graph(trajectory)
}
\keyword{create_trajectory}
