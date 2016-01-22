context("conversion between angular similarity and cosine similarity")

test_that("both functions return warnings when inputs are outside the expected range",{
	expect_warning(cossim_to_angsim(seq(-2, 2, 0.25)))
	expect_warning(angsim_to_cossim(seq(0, 2, 1)))
})

test_that("return expected answers",{
	expect_true(angsim_to_cossim(c(0, 1, 0.5, NA)))
	expect_true(cossom_to_angsim(c(-1, 1, 0, NA)))
}

test_that("functions accurately convert between the two metrics",{

	cossim_vals <- runif(100, -1, 1)
	cossim_out <- cossim_to_angsim(cossim_vals)
	angsim_return <- angsim_to_cossim(cossim_out)
	expect_equal(angsim_return, cossim_vals, tolerance = 1e-3)

	angsim_vals <- runif(100)
	angsim_out <- angsim_to_cossim(angsim_vals)
	cossim_back <- cossim_to_angsim(angsim_out)
	expect_equal(angsim_vals, cossim_back, tolerance = 1e-3)
})