% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_process_definition.R
\name{def_container}
\alias{def_container}
\title{Meta information on the container in which the wrapper resides}
\usage{
def_container(docker, url = NULL)
}
\arguments{
\item{docker}{The handle of the docker container}

\item{url}{An url of where the docker codebase resides (containing definition.yml, Dockerfile, ...)}
}
\description{
Meta information on the container in which the wrapper resides
}
\examples{
def_container(
  docker = "bobdylan/ti_some_method",
  url = "https://github.com/bobdylan/ti_some_method"
)
}
\keyword{create_ti_method}
