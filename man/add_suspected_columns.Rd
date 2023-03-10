% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_suspected_data.R
\name{add_suspected_columns}
\alias{add_suspected_columns}
\title{Identify column(s) to be marked as suspicious}
\usage{
add_suspected_columns(x, reason, suspect_data = NULL)
}
\arguments{
\item{x}{a character string containin the names of suspect columns from the tibble of interest}

\item{reason}{a character string describing why this particular subset is
suspect}

\item{suspect_data}{a specifically structure tibble

If suspect_data is NULL, a new suspect_data tibble will be constructed and
the identified data added to that structure.}
}
\value{
a tibble with the suspect data structure
}
\description{
Given a common tibble of interest, a subset of the data can be identified as
suspicious and added to the suspect_data structure for analysis and
visualization.
}
\examples{
df <- create_suspicious_data()
suspect_data <-
df \%>\%
  select(clarity, y) \%>\%
  add_suspected_columns(reason = "These columns are hinky")

suspect_data <-
  df \%>\%
  select(depth) \%>\%
  add_suspected_columns(reason = "This column is not deep enough", suspect_data)
}
