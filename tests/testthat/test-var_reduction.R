# define parameters
n <- 1e2
frac_missing <- .1
nr_cluster <- 1

# Random data
X <- as.data.frame(matrix(rnorm(2*n),n,2))
Xmiss <- miss_sim(X,p=frac_missing,type="MCAR")

res <- ClustImpute(Xmiss,nr_cluster)

test_that("No variance reduction for a single cluster", {
  expect_equal(var_reduction(res)$Variance_reduction, 0)
})

res <- ClustImpute(Xmiss,nr_cluster=n)

test_that("There can be as many centroids as points", {
  expect_equal(dim(res$centroids)[1], n)
})
