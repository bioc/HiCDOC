% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R, R/AllGenerics.R, R/methods.R
\docType{data}
\name{HiCDOCDataSet-parameters}
\alias{HiCDOCDataSet-parameters}
\alias{defaultHiCDOCParameters}
\alias{parameters}
\alias{parameters<-}
\alias{parameters,HiCDOCDataSet-method}
\alias{parameters<-,HiCDOCDataSet-method}
\title{Access the parameters of a \code{\link{HiCDOCDataSet}}.}
\format{
An object of class \code{list} of length 9.
}
\usage{
defaultHiCDOCParameters

\S4method{parameters}{HiCDOCDataSet}(object)

\S4method{parameters}{HiCDOCDataSet}(object) <- value
}
\arguments{
\item{object}{A \code{\link{HiCDOCDataSet}}.}

\item{value}{a named list containing the names and valued of the
parameters to change (see Details).}
}
\description{
Retrieves or sets parameters used for filtering, normalization, and
prediciton of compartments.
}
\details{
A \code{\link{HiCDOCDataSet}}'s parameters are automatically set to default
values retrieved from \code{\link{defaultHiCDOCParameters}}. They are
accessed by filtering, normalization, and compartment detection functions.
If those functions are called with custom arguments, the object's
parameters are updated to record the actual parameters used. If the
object's parameters are customized before calling the functions, the
custom parameters will be used.

See
\code{\link{filterSmallChromosomes}},
\code{\link{filterSparseReplicates}},
\code{\link{filterWeakPositions}},
\code{\link{normalizeDistanceEffect}}, and
\code{\link{detectCompartments}},
for details on how these parameters are used.
\subsection{All parameters are listed here:}{
    \describe{
        \item{\code{smallChromosomeThreshold}}{
            The minimum length (number of positions) for a chromosome to be
            kept when filtering with \code{\link{filterSmallChromosomes}}.
            Defaults to
            \code{defaultHiCDOCParameters$smallChromosomeThreshold} = 100.
        }
        \item{\code{sparseReplicateThreshold}}{
            The minimum percentage of non-zero interactions for a chromosome
            replicate to be kept when filtering with
            \code{\link{filterSparseReplicates}}. If a chromosome replicate's
            percentage of non-zero interactions is lower than this value, it
            is removed. Defaults to
            \code{defaultHiCDOCParameters$smallChromosomeThreshold} = 30%.
        }
        \item{\code{weakPositionThreshold}}{
            The minimum average interaction for a position to be kept when
            filtering with \code{\link{filterWeakPositions}}. If a position's
            average interaction with the entire chromosome is lower than this
            value in any of the replicates, it is removed from all replicates
            and conditions. Defaults to
            \code{defaultHiCDOCParameters$smallChromosomeThreshold} = 1.
        }
        \item{\code{cyclicLoessSpan}}{
            The span for cyclic loess normalization used in 
            \code{\link{normalizeTechnicalBiases}}. This value is passed to 
            \code{multiHiCcompare::cyclic_loess}. 
            Defaults to NA indicating that span will be automatically 
            calculated using generalized cross validation.
            For large dataset, it is highly recommended to set this value
            to reduce computing time and necessary memory.
        }
        \item{\code{loessSampleSize}}{
            The number of positions used as a sample to estimate the effect
            of distance on proportion of interactions when normalizing with
            \code{\link{normalizeDistanceEffect}}. Defaults to
            \code{defaultHiCDOCParameters$loessSampleSize} = 20000.
        }
        \item{\code{kMeansDelta}}{
            The convergence stop criterion for the clustering when detecting
            compartments with \code{\link{detectCompartments}}. When the
            centroids' distances between two iterations is lower than this
            value, the clustering stops. Defaults to
            \code{defaultHiCDOCParameters$kMeansDelta} = 0.0001.
        }
        \item{\code{kMeansIterations}}{
            The maximum number of iterations during clustering when detecting
            compartments with \code{\link{detectCompartments}}. Defaults to
            \code{defaultHiCDOCParameters$kMeansIterations} = 50.
        }
        \item{\code{kMeansRestarts}}{
            The amount of times the clustering is restarted when detecting
            compartments with \code{\link{detectCompartments}}. For each
            restart, the clustering iterates until convergence or reaching
            the maximum number of iterations. The clustering that minimizes
            inner-cluster variance is selected. Defaults to
            \code{defaultHiCDOCParameters$kMeansRestarts} = 20.
        }
        \item{\code{PC1CheckThreshold}}{
            The minimum percentage of variance that should be explained by
            the first principal component of centroids to pass sanity check.
            Defaults to 
            \code{defaultHiCDOCParameters$PC1CheckThreshold} = 0.75
        }
    }
}
}
\examples{
data(exampleHiCDOCDataSet)

# Retrieve parameters
parameters(exampleHiCDOCDataSet)

# Set parameters
parameters(exampleHiCDOCDataSet) <- list("smallChromosomeThreshold" = 50)
parameters(exampleHiCDOCDataSet) <- list(
    "weakPositionThreshold" = 10,
    "kMeansRestarts" = 30
)
}
\keyword{datasets}
