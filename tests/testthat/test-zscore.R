context("z_scores")

test_that("zscore returns errors",{

    expect_error(zscore(c("a", "B", 2, 4)))
    expect_error(r_zscore(c("a", "B", 2, 4)))
})

test_that("zscore returns same values as scale",{
    data <- iris[,1]
    zscore_out <- zscore(data)
    scale_out <- scale(data)
    attributes(scale_out) <- NULL
    expect_equal(as.vector(zscore_out),
		 as.vector(scale_out))
})
