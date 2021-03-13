
X <- as.data.frame(matrix(rnorm(20*4),20,4))

nr_cluster <- 3
nr_iter <- 10

res <- ClustImpute(X,nr_cluster,nr_iter)
Xres <- res$complete_data

test_that("Dimension complete data", {
  expect_identical(dim(Xres)[1], dim(X)[1])
  expect_identical(dim(Xres)[2], dim(X)[2])
})

test_that("Dimension centroits", {
  expect_equal(dim(res$centroids)[1], nr_cluster)
  expect_equal(dim(res$centroids)[2], dim(X)[2]+1)
})

test_that("Length cluster assignment", {
  expect_length(res$clusters, dim(X)[1])
})


test_that("Row dimension imp stats", {
  expect_equal(dim(res$imp_values_mean)[1], nr_iter+1)
  expect_equal(dim(res$imp_values_sd)[1], nr_iter+1)
})
