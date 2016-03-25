context("z_factor_scan")
set.seed(123)

# create example data
v1 <- rnorm(96)
v2 <- rnorm(96)
v3 <- rnorm(96)
v4 <- c(rnorm(80, mean = 1),
	rnorm(8, mean = 50),
	rnorm(8, mean = 3))

drugs <- c(rep("cmpd", 80),
	   rep("pos", 8),
	   rep("neg", 8))

df <- data.frame(drugs, v1, v2, v3, v4)
out <- z_factor_scan(data = df,
		     treatment_col = "drugs",
		     treatments = c("pos", "neg"),
		     n = 4)

out_cutoff <- z_factor_scan(data = df,
			    treatment_col = "drugs",
			    treatments = c("pos", "neg"))

test_that("returns a dataframe",{
    expect_is(out, "data.frame")
    expect_equal(ncol(out), 2L)
})

test_that("returns expected values",{
    expect_equal(as.character(out[1,1]), "v4")
    expect_equal(ncol(out_cutoff), 2L)
})
