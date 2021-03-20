
X <- as.data.frame(matrix(rnorm(20*4),20,4))

res <- ClustImpute(X,3)

test_that("No changes if there are no NAs", {
  expect_identical(res$complete_data,X)
})


# Test that the centroids are similar if data is nearly scaled

set.seed(101)
mean_value <- 1e-10
nr_rows <- 1e2
X <- as.data.frame(matrix(rnorm(nr_rows*4,mean=mean_value,sd=1),nr_rows,4))
empirical_mean_by_feature <- colMeans(X)

X_scaled <- as.data.frame(scale(X,center=TRUE,scale=FALSE))

res_unscaled <- ClustImpute(X,3,shrink_towards_global_mean=TRUE,nr_iter=1)
res_scaled <- ClustImpute(X_scaled,3,shrink_towards_global_mean=FALSE,nr_iter=1)
res_scaled_2 <- ClustImpute(X_scaled,3,shrink_towards_global_mean=TRUE,nr_iter=1)

centroids_unscaled <- res_unscaled$centroids_matrix
centroids_scaled <- res_scaled$centroids_matrix
centroids_scaled_adj <- res_scaled$centroids_matrix+matrix(rep(empirical_mean_by_feature,3),nrow=3)
centroids_scaled_2 <- res_scaled_2$centroids_matrix

test_that("Centroids only have a small distance since k-means works differently on un-centered data", {
  expect_lt(sum((centroids_unscaled-centroids_scaled_adj)^2),1e-2)
})

test_that("shrink_towards_global_mean has no impact on scaled data", {
  expect_equal(sum((centroids_scaled-centroids_scaled_2)^2),0)
})

warn_not_centered <- capture_warnings(ClustImpute(X+10,3,shrink_towards_global_mean=FALSE,nr_iter=1))

test_that("Warning message if shrink_towards_global_mean=FALSE and data is not centered", {
  expect_equal(warn_not_centered,
                 "For non-centered data we recommend the option shrink_towards_global_mean=TRUE")
})





