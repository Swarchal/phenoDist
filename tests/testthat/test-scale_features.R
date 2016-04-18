context("scale_features")

# example data
v1 <- rnorm(96, 10, 10)
v2 <- rnorm(96, 100, 100)
meta <- sample(letters, 96, replace = TRUE)
df <- data.frame(Metadata_x = meta,
		 v1, v2)
out <- scale_features(df)

test_that("scale_features returns a dataframe",{
    expect_is(out, 'data.frame')
})

test_that("scale features returns correct dimensions",{
    expect_equal(nrow(out), 96L)
    expect_equal(ncol(out), ncol(df))
})

test_that("scales values",{
   expect_equal(mean(out[,2]), 0L, tolerance = 1e-3)
   expect_equal(mean(out[,3]), 0L, tolerance = 1e-3)
})

test_that("has SD of 1",{
    sd_out <- apply(out[,2:3], 2, sd)
    print(sd_out)
    expect_equal(sd_out, c(1,1))
})
