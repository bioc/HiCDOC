% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectCompartments.R
\name{detectCompartments}
\alias{detectCompartments}
\title{A and B compartments detection and differences across conditions.}
\usage{
detectCompartments(
    object,
    parallel = FALSE,
    kMeansDelta = NULL,
    kMeansIterations = NULL,
    kMeansRestarts = NULL,
    PC1CheckThreshold = NULL
)
}
\arguments{
\item{object}{A \code{\link{HiCDOCDataSet}}.}

\item{parallel}{Whether or not to parallelize the processing. Defaults to FALSE
See 'Details'.}

\item{kMeansDelta}{The convergence stop criterion for the clustering. When the centroids'
distances between two iterations is lower than this value, the clustering
stops. Defaults to \code{object$kMeansDelta} which is originally set to
\code{defaultHiCDOCParameters$kMeansDelta} = 0.0001.}

\item{kMeansIterations}{The maximum number of iterations during clustering. Defaults to
\code{object$kMeansIterations} which is originally set to
\code{defaultHiCDOCParameters$kMeansIterations} = 50.}

\item{kMeansRestarts}{The amount of times the clustering is restarted. For each restart, the
clustering iterates until convergence or reaching the maximum number of
iterations. The clustering that minimizes inner-cluster variance is selected.
Defaults to \code{object$kMeansRestarts} which is originally set to
\code{defaultHiCDOCParameters$kMeansRestarts} = 20.}

\item{PC1CheckThreshold}{The minimum percentage of variance that should be explained by
the first principal component of centroids to pass sanity check.
Defaults to \code{object$PC1CheckThreshold} which is originally set to
\code{defaultHiCDOCParameters$PC1CheckThreshold} = 0.75}
}
\value{
A \code{\link{HiCDOCDataSet}}, with compartments, concordances, distances,
centroids, and differences.
}
\description{
Detects compartments for each genomic position in each condition, and
computes p-values for compartment differences between conditions.
}
\details{
\subsection{Genomic positions clustering}{
To clusterize genomic positions, the algorithm follows these steps:
    \enumerate{
        \item{
            For each chromosome and condition, get the interaction vectors of
            each genomic position. Each genomic position can have multiple
            interaction vectors, corresponding to the multiple replicates in
            that condition.
        }
        \item{
            For each chromosome and condition, use constrained K-means to
            clusterize the interaction vectors, forcing replicate interaction
            vectors into the same cluster. The euclidean distance between
            interaction vectors determines their similarity.
        }
        \item{
            For each interaction vector, compute its concordance, which is
            the confidence in its assigned cluster. Mathematically, it is the
            log ratio of its distance to each centroid, normalized by the
            distance between both centroids, and min-maxed to a [-1,1]
            interval.
        }
        \item{
            For each chromosome, compute the distance between all centroids
            and the centroids of the first condition. The cross-condition
            clusters whose centroids are closest are given the same cluster
            label. This results in two clusters per chromosome, spanning all
            conditions.
        }
    }
}
\subsection{A/B compartments prediction}{
To match each cluster with an A or B compartment, the algorithm follows these
steps:
    \enumerate{
        \item{
            For each genomic position, compute its self interaction ratio,
            which is the difference between its self interaction and the
            median of its other interactions.
        }
        \item{
            For each chromosome, for each cluster, get the median self
            interaction ratio of the genomic positions in that cluster.
        }
        \item{
            For each chromosome, the cluster with the smallest median self
            interaction ratio is matched with compartment A, and the cluster
            with the greatest median self interaction ratio is matched with
            compartment B. Compartment A being open, there are more overall
            interactions between distant genomic positions, so it is assumed
            that the difference between self interactions and other
            interactions is lower than in compartment B.
        }
    }
}
\subsection{Significant differences detection}{
To find significant compartment differences across conditions, and compute
their p-values, the algorithm follows three steps:
    \enumerate{
        \item{
            For each pair of replicates in different conditions, for each
            genomic position, compute the absolute difference between its
            concordances.
        }
        \item{
            For each pair of conditions, for each genomic position, compute
            the median of its concordance differences.
        }
        \item{
            For each pair of conditions, for each genomic position whose
            assigned compartment switches, rank its median against the
            empirical cumulative distribution of medians of all non-switching
            positions in that condition pair. Adjust the resulting p-value
            with the Benjamini–Hochberg procedure.
        }
    }
}
\subsection{Parallel processing}{
The parallel version of detectCompartments uses the
\code{\link[BiocParallel]{bpmapply}} function. Before to call the
function in parallel you should specify the parallel parameters such as:
    \itemize{
        \item{On Linux:

             \code{multiParam <- BiocParallel::MulticoreParam(workers = 10)}
         }
         \item{On Windows:

             \code{multiParam <- BiocParallel::SnowParam(workers = 10)}
        }
    }
    And then you can register the parameters to be used by BiocParallel:

    \code{BiocParallel::register(multiParam, default = TRUE)}

    You should be aware that using MulticoreParam, reproducibility of the
    detectCompartments function using a RNGseed may not work. See this
    \href{https://github.com/Bioconductor/BiocParallel/issues/122}{issue}
    for more details.
}
}
\examples{
data(exampleHiCDOCDataSet)
## Run all filtering and normalization steps (not run for timing reasons)
# object <- filterSmallChromosomes(exampleHiCDOCDataSet)
# object <- filterSparseReplicates(object)
# object <- filterWeakPositions(object)
# object <- normalizeTechnicalBiases(object)
# object <- normalizeBiologicalBiases(object)
# object <- normalizeDistanceEffect(object)

# Detect compartments and differences across conditions
object <- detectCompartments(exampleHiCDOCDataSet)

}
