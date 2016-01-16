context("fold_180 folds numbers directly")

test_that("fold_180 stops when given incorrect input",{
	expect_error(fold_180(c("string1", "string2")))
	expect_error(fold_180(iris))
	expect_warning(fold_180(seq(1, 500, 1)))
})

test_that("fold 180 returns expected values",{
	numbers <- c(170, 180, 190)
	outcome <- c(170, 180, 170)
	expect_equal(fold_180(numbers), outcome)
})