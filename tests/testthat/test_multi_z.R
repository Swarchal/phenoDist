context("Test multi_z")

# create data for test
# use iris but with two species, can then test separation of 4 variables
# between the two species and see how well they are separated

iris_2 <- iris[iris$Species != "virginica", ]

out <- multi_z(
	df = iris_2,
	feature_cols = 1:4,
	cmpd_col = "Species",
	pos = "setosa",
	neg = "versicolor")

test_that("error when cmpd col includes a non-numeric",{
	# include column that's a string
	expect_error(multi_z(
		df = iris_2,
		feature_cols = 1:5,
		cmpd_col = "Species",
		pos = "setosa",
		neg = "versicolor"))
})

test_that("error when control isn't present",{
	# include control that's not there
	expect_error(multi_z(
	df = iris_2,
	feature_cols = 1:4,
	cmpd_col = "Species",
	pos = "not_in_here",
	neg = "versicolor"))
})

test_that("returns a numeric answer",{
	expect_true(is.numeric(out))
})