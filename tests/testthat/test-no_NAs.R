
X <- as.data.frame(matrix(rnorm(20*4),20,4))

res <- ClustImpute(X,3)

test_that("No changes if there are no NAs", {
  expect_identical(res$complete_data,X)
})
