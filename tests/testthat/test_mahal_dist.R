context("mahalanobis distance works as expected")

test_that("mahal_dist stops when inputs are wrong",{
	expect_error(mahal_dist("string"))
	expect_error(mahal_dist(iris[,1:4], center = 'other'))
})

test_that("mahal_dist returns correct answer",{
	iris_vals <- iris[, 1:4]
	mahal_out_med <- mahal_dist(iris_vals, center = 'median')
	mahal_out_mean <- mahal_dist(iris_vals, center = 'mean')
	expect_is(mahal_out_med, 'numeric')
	expect_is(mahal_out_mean, 'numeric')

	mahal_na <- mahal_dist(1)
	expect_true(is.na(mahal_na))
})