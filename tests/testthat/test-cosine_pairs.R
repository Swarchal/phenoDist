context("check cosine pairs")

cmpds <- c(rep('a', 100), rep('b', 100), rep('c', 100))
replicate <- rep(1:100, 3)
PC1 <- rnorm(300)
PC2 <- rnorm(300)

df <- data.frame(cmpds, replicate, PC1, PC2)

df_split <- split(df, df$cmpds)
out <- cosine_pairs(df_split, 'PC1', 'PC2')

# works with unequal replicate sizes
df_split$a <- df_split$a[-c(1:10), ]
out_unequal <- cosine_pairs(df_split, 'PC1', 'PC2')

test_that("cosine_pairs returns errors when expected",{
	expect_error(cosine_pairs(df, 'PC1', 'PC2'))
	expect_error(cosine_pairs(df_split, 'x', 'y'))
})

test_that("cosine_pairs returns expected output",{
	expect_true(is.data.frame(out_unequal))
	expect_equal(nrow(out), 10000)
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out_unequal), 9000)
})


# known answers:
A <- c('a', 'a', 'b', 'b')
PC1 <- c(1, 1, -1, -1)
PC2 <- c(2, 2, -2, -2)
df <- data.frame(A, PC1, PC2)
df
split_df <- split(df, df$A)
test <- cosine_pairs(split_df, 'PC1', 'PC2')

test_that("cosine_pairs returns correct results",{
	expect_equal(test$val, c(-1, -1, -1, -1), tolerance = 1e-5)
})