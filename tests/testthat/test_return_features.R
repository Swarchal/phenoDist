context("return_features is working normally")

example_pca <- prcomp(mtcars)

test_that("return_features stops when given incorrect input",{
	expect_error(return_features(example_pca))
	expect_error(return_features(iris, c(0,1)))
	expect_error(return_features(example_pca, c(1)))
	expect_error(return_features(example_pca, c("str", "str")))
})

test_that("return_features gives expected answers",{
	ans <- c("disp", "hp", "mpg", "cyl")
	out <- return_features(example_pca, c(1, 0))
	expect_equal(ans, out)
})