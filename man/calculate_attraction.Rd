% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_add_attraction.R
\name{calculate_attraction}
\alias{calculate_attraction}
\title{Calculate the attraction of cells to other cells using velocity}
\usage{
calculate_attraction(
  current,
  projected,
  cells = colnames(projected),
  n_waypoints = 50,
  k = 50
)
}
\arguments{
\item{current}{Current expression}

\item{projected}{Projected expression based on RNA velocity}

\item{cells}{Which cells to use}

\item{n_waypoints}{Number of waypoints to use}

\item{k}{K knns}
}
\value{
Matrix containing the attraction ([-1, 1]) of each cell to the waypoint cells
}
\description{
Calculate the attraction of cells to other cells using velocity
}
