#' @title
#' Normalize technical biases.
#'
#' @description
#' Normalizes technical biases such as sequencing depth by using a cyclic loess
#' to recursively normalize each pair of interaction matrices. Depends on
#' \code{multiHiCcompare}.
#'
#' @details
#' \subsection{Parallel processing}{
#' If \code{parallel = TRUE}, the function
#' \code{\link[multiHiCcompare]{cyclic_loess}}
#' is launched in parallel mode, using \code{\link[BiocParallel]{bplapply}}
#' function. Before to call the function in parallel you should specify
#' the parallel parameters such as:
#'     \itemize{
#'         \item{On Linux:
#'
#'              \code{multiParam <- BiocParallel::MulticoreParam(workers = 10)}
#'          }
#'          \item{On Windows:
#'
#'              \code{multiParam <- BiocParallel::SnowParam(workers = 10)}
#'         }
#'     }
#'     And then you can register the parameters to be used by BiocParallel:
#'
#'     \code{BiocParallel::register(multiParam, default = TRUE)}
#' }
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param parallel 
#' Logical. Whether or not to parallelize the processing. Defaults to FALSE
#' @param cyclicLoessSpan 
#' A numeric value in between 0 and 1. The span for cyclic loess normalization. 
#' This value is passed to \code{multiHiCcompare::cyclic_loess}. 
#' Defaults to NULL, NULL indicates that the value of 
#' parameters(object)$cyclicLoessSpan will be used. 
#' If this value is NA, the span will be automatically calculated using 
#' generalized cross validation. **For large dataset, it is highly recommended 
#' to set this value to reduce computing time and necessary memory.**
#'
#' @return
#' A \code{\link{HiCDOCDataSet}} with normalized interactions.
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' object <- filterSmallChromosomes(exampleHiCDOCDataSet)
#' object <- filterSparseReplicates(object)
#' object <- filterWeakPositions(object)
#' # Not printing loess warnings for example purpose. 
#' # Results should be inspected if there is any.
#' suppressWarnings(
#'     object <- normalizeTechnicalBiases(object)
#' )
#' 
#'
#' @seealso
#' \code{\link{filterSparseReplicates}},
#' \code{\link{filterWeakPositions}},
#' \code{\link{normalizeBiologicalBiases}},
#' \code{\link{normalizeDistanceEffect}},
#' \code{\link{HiCDOC}}
#'
#' @export
normalizeTechnicalBiases <- 
    function(object, parallel = FALSE, cyclicLoessSpan = NULL) {
    message("Normalizing technical biases.")

    if (!is.null(cyclicLoessSpan)) {
        object@parameters$cyclicLoessSpan <- cyclicLoessSpan
    }
        
    hic_table <- as.data.table(InteractionSet::interactions(object))
    hic_table <- hic_table[, .(
        chromosome = seqnames1,
        region1 = start1,
        region2 = start2
    )]
    if (!is.factor(hic_table$chromosome)) {
        hic_table[, chromosome := as.factor(chromosome)]
    }
    hic_table[, chromosome := as.numeric(chromosome)]

    currentAssay <- SummarizedExperiment::assay(object)
    currentAssay[is.na(currentAssay)] <- 0
    # Reordering columns in condition order
    refOrder <- paste(object$condition, object$replicate, sep = ".")
    currentAssay <- currentAssay[, order(refOrder), drop=FALSE]
    
    table_list <- lapply(
        seq_len(ncol(currentAssay)),
        function(x) cbind(hic_table, currentAssay[, x])
    )
    
    experiment <- multiHiCcompare::make_hicexp(
        data_list = table_list,
        groups = sort(object$condition),
        remove_zeros = FALSE,
        filter = TRUE,
        zero.p = 1,
        A.min = 0,
        remove.regions = NULL
    )
    
    normalized <- 
        multiHiCcompare::cyclic_loess(
            experiment, 
            parallel = parallel,
            span = object@parameters$cyclicLoessSpan
        )
    normalized <- multiHiCcompare::hic_table(normalized)
    data.table::setnames(normalized, "chr", "chromosome")

    # Re-sorting the rows in the same order as original
    data.table::setindexv(normalized, c("chromosome", "region1", "region2"))
    data.table::setindexv(hic_table, c("chromosome", "region1", "region2"))
    hic_table <- data.table::merge.data.table(
        hic_table,
        normalized,
        sort = FALSE
    )

    currentAssay <- as.matrix(hic_table[, 5:ncol(hic_table)])
    # Reordering columns in original order
    currentAssay <- currentAssay[, match(refOrder, sort(refOrder))]
    colnames(currentAssay) <- NULL
    currentAssay[currentAssay == 0] <- NA
    SummarizedExperiment::assay(object, withDimnames = FALSE) <- currentAssay
    return(object)
}
