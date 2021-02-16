test_that("detectCompartments behaves as expected", {
    object <- HiCDOCExample()
    # Detect Compartments
    set.seed(3215) # Test with 123 : no significant differences
    expect_message(object <- detectCompartments(object), "Done.")
    # Keep object format
    expect_is(object@interactions$chromosome, "factor")
    expect_is(object@interactions$bin.1, "integer")
    expect_is(object@interactions$bin.2, "integer")
    expect_is(object@interactions$condition, "factor")
    expect_is(object@interactions$replicate, "factor")
    expect_is(object@interactions$value, "integer")
    # Create new objects in correct format
    expect_is(object@distances, "tbl_df")
    expect_is(object@diagonalRatios, "tbl_df")
    expect_is(object@compartments, "tbl_df")
    expect_is(object@concordances, "tbl_df")
    expect_is(object@differences, "tbl_df")
    expect_is(object@centroids, "tbl_df")
    
    # Differences
    expect_equal(nrow(object@differences), 12)
    expect_equal(nrow(object@differences %>% filter(padj<=0.05)), 6)
    expect_is(object@differences$chromosome, "factor")
    expect_is(object@differences$condition.1, "factor")
    expect_is(object@differences$condition.2, "factor")
    
    # Centroids
    expect_equal(nrow(object@centroids), 8)
    expect_equal(lapply(object@centroids$centroid, length), 
                 list(127, 127, 127, 127, 112, 112, 112, 112))
    expect_equal(lapply(object@centroids$centroid, mean), 
                 list(316.5268, 282.0699, 182.8555, 232.813, 
                      361.9592, 448.6829, 252.8538, 358.9238), 
                 tolerance = 1e-04)
    expect_is(object@centroids$chromosome, "factor")
    expect_is(object@centroids$condition, "factor")
    expect_is(object@centroids$compartment, "factor")
    expect_is(object@centroids$centroid, "list")
    
    # Compartments
    expect_equal(nrow(object@compartments), 478)
    expect_equal(nrow(object@compartments %>% filter(compartment=="B")), 162)
    expect_is(object@compartments$chromosome, "factor")
    expect_is(object@compartments$condition, "factor")
    expect_is(object@compartments$compartment, "factor")
    expect_is(object@compartments$bin, "integer")
    
    # Concordance
    expect_equal(nrow(object@concordances), 1434)
    expect_equal(nrow(object@concordances %>% filter(compartment==1)), 798)
    expect_equal(mean(object@concordances$concordance), 
                 -0.004183894, tolerance = 1e-05)
    expect_is(object@concordances$chromosome, "factor")
    expect_is(object@concordances$bin, "integer")
    expect_is(object@concordances$condition, "factor")
    expect_is(object@concordances$replicate, "factor")
    expect_is(object@concordances$compartment, "numeric")
    expect_is(object@concordances$concordance, "numeric")
    
    # Distances
    expect_equal(nrow(object@distances), 2868)
    expect_equal(mean(object@distances$distance), 7274.914, tolerance = 1e-04)
    expect_is(object@distances$chromosome, "factor")
    expect_is(object@distances$bin, "integer")
    expect_is(object@distances$condition, "factor")
    expect_is(object@distances$replicate, "factor")
    expect_is(object@distances$compartment, "factor")
    expect_is(object@distances$distance, "numeric")

    # DiagonalRatios
    expect_equal(nrow(object@diagonalRatios), 1434)
    expect_equal(mean(object@diagonalRatios$value), 4323.244, tolerance = 1e-04)
    expect_is(object@diagonalRatios$chromosome, "factor")
    expect_is(object@diagonalRatios$bin, "integer")
    expect_is(object@diagonalRatios$condition, "factor")
    expect_is(object@diagonalRatios$replicate, "factor")
    expect_is(object@diagonalRatios$value, "numeric")
})