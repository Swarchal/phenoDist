context("Test bhatt_dist")


test_that("returns errors when expected",{
	expect_error(bhatt_dist(c("string", "string")))
	expect_error(bhatt_dist(iris[1:10, ], iris[1:10, ]))
})

test_that("returns expected values",{
	expect_equal(bhatt_dist(iris[, 1:4], iris[, 1:4]), 0, tolerance = 1e-5)
	ans <- bhatt_dist(iris[1:10, 1:4], iris[100:110, 1:4])
	expect_true(ans > 0)
})