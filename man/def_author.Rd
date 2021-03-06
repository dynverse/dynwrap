% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_process_definition.R
\name{def_author}
\alias{def_author}
\title{Meta information on an author}
\usage{
def_author(given, family, email = NULL, github = NULL, orcid = NULL)
}
\arguments{
\item{given}{The given name}

\item{family}{The family name}

\item{email}{The email address}

\item{github}{The github handle}

\item{orcid}{The orcid id}
}
\description{
Meta information on an author
}
\examples{
def_author(
  given = "Bob",
  family = "Dylan",
  email = "bob@dylan.com",
  github = "bobdylan",
  orcid = "0000-0003-1234-5678"
)

}
\keyword{create_ti_method}
