data(exampleHiCDOCDataSet)
test_that("filterWeakPositions behaves as expected", {
    # Filter the 0 values in interactions
    expect_message(
        object <- filterWeakPositions(exampleHiCDOCDataSet),
        "Removed 6 positions in total."
    )
    expect_equal(nrow(object), 42646)
    expect_identical(object@weakBins,
                     list("W" = integer(0),
                          "X" = c(112L, 171L, 200L),
                          "Y" = c(201L, 322L, 346L),
                          "Z" = integer(0)))
    assay <- SummarizedExperiment::assay(object)
    expect_equal(sum(!is.na(assay)), 181180)
    expect_equal(colSums(assay, na.rm=TRUE), 
                 c(8990463, 22387622, 7564195, 29556499, 
                   17445466, 23544963, 22932825))
})

test_that("filterWeakPositions behaves as expected, custom param", {
    # Filter values with 50 threshold
    expect_message(
        object <- filterWeakPositions(exampleHiCDOCDataSet, threshold = 50),
        "Removed 123 positions in total."
    )
    expect_equal(nrow(object), 35743)
    expect_identical(sapply(object@weakBins, length),
                     c("W" = 0L, "Y" = 3L, "Z" = 0L))
    expect_identical(object@parameters$weakPositionThreshold, 50)
    assay <- SummarizedExperiment::assay(object)
    expect_equal(sum(!is.na(assay)), 146461)
    expect_equal(colSums(assay, na.rm=TRUE), 
                 c(5161309, 17717881, 7564195, 29432906, 
                   10521998, 16470630, 14974644))
})
