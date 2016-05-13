context("winsor")

# create example data
set.seed(12321)
x <- rnorm(1000)

out <- winsorise(x)

test_that("returns a vector of the same length",{
    expect_equal(length(x), length(out))
})

test_that("returns errors when expected",{
    tmp <- c(1, 3, 5, "s", 4, 8)
    expect_error(winsorise(tmp))
})

test_that("returns error if NA values present with na.rm = FALSE",{
    tmp <- c(1, 3, 6, 2, 5, 2, 5, 2, 5, 2, NA, 5, 2)
    expect_error(winsorise(tmp, na.rm = FALSE))
    expect_error(winsorise_mean(tmp, na.rm = FALSE))
})

test_that("winsorise converts tails to quantile values",{
    tmp <- rnorm(10000)
    x <- sort(winsorise(x))
    expect_equal(x[1], x[2])
    expect_equal(x[length(x)], x[length(x)-1])
})
