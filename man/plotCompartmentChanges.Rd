% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotCompartmentChanges.R
\name{plotCompartmentChanges}
\alias{plotCompartmentChanges}
\title{Plot compartment changes.}
\usage{
plotCompartmentChanges(
  object,
  chromosome,
  threshold = 0.05,
  xlim = NULL,
  points = FALSE,
  checks = TRUE,
  colour = "gray90"
)
}
\arguments{
\item{object}{A \code{\link{HiCDOCDataSet}}.}

\item{chromosome}{A chromosome name or index in \code{chromosomes(object)}.}

\item{threshold}{Significance threshold for the compartment changes. Defaults to 0.05.}

\item{xlim}{A vector of the minimum and maximum positions to display. If NULL, displays
all positions. Defaults to NULL.}

\item{points}{Whether or not to add points to the concordances. Defaults to FALSE.}

\item{checks}{Whether or not to add sanity checks messages. Default to TRUE.}

\item{colour}{Border color for the compartments. Default to `gray90`. `NA` means no border.}
}
\value{
A \code{ggplot}.
}
\description{
Plots the predicted compartments, along with their concordance in each
replicate, and significant changes between experiment conditions.
}
\examples{
data(exampleHiCDOCDataSetProcessed)
plotCompartmentChanges(exampleHiCDOCDataSetProcessed, chromosome = 1)

}
