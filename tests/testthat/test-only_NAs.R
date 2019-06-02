X <- as.data.frame(matrix(NA,20,4))

test_that("No changes if there are no NAs", {
  expect_error(ClustImpute(X,3),"All values of X are NA")
})
