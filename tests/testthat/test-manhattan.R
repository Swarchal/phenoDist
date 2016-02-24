context("Testing Manhattan distances")

test_that("Manhattan distances return errors when expected",{
    a <- c(1,2,3,4)
    b <- c(1,2,3)
    expect_error(manhattan(a, b))
    expect_error(manhattan_norm(a, b))
})

test_that("Manhattan distances return expected values",{
    a <- c(1,2,3,4,5)
    b <- c(2,3,4,5,6)
    expect_equal(manhattan(a, a), 0)
    expect_equal(manhattan(a, b), 5)
    expect_equal(manhattan_norm(a, b), 1)
})
