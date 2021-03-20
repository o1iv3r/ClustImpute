# Test that distribution is preserved in MCAR setting

# define parameters
eps <- 5*1e-2 # error tolerance
n <- 1e5
frac_missing <- .5

# Random data
X <- as.data.frame(matrix(rnorm(2*n),n,2))

#### Test missing simulation ####

Xmiss <- miss_sim(X,p=frac_missing,type="MCAR")

test_that("Error for invalid missing simulation", {
  expect_error(miss_sim(X,p=frac_missing,type="invalid string"),"Provide valid type")
})

mean_diff <- sapply(X,mean,na.rm=TRUE)-sapply(Xmiss,mean,na.rm=TRUE)

test_that("MCAR: mean diff", {
  expect_lt(abs(mean_diff)[1], eps)
  expect_lt(abs(mean_diff)[2], eps)
})

sd_diff <- sapply(X,sd,na.rm=TRUE)-sapply(Xmiss,sd,na.rm=TRUE)

test_that("MCAR: sd diff", {
  expect_lt(abs(sd_diff)[1], eps)
  expect_lt(abs(sd_diff)[2], eps)
})

cor_diff <- cor(Xmiss,use="pairwise.complete.obs")[1,2] # -cor(X,use="pairwise.complete.obs")[1,2]

test_that("MCAR: correlation diff", {
  expect_lt(abs(cor_diff), eps)
})


#### Test ClustImpute ####

nr_cluster <- 1 # there are no clusters

# res_true <- ClustImpute(X,nr_cluster)
res <- ClustImpute(Xmiss,nr_cluster)
res_shrink_towards_global_mean_FALSE <- ClustImpute(Xmiss,nr_cluster,shrink_towards_global_mean=FALSE)

test_that("Clustimpute: mean close to zero", {
  expect_lt(abs(res$imp_values_mean[11,1]), eps)
  expect_lt(abs(res$imp_values_mean[11,2]), eps)
})

test_that("Clustimpute: sd close to 1", {
  expect_lt(abs(res$imp_values_sd[11,1]), 1 + eps) # -res_true$imp_values_sd[11,1]
  expect_lt(abs(res$imp_values_sd[11,2]), 1 + eps) # -res_true$imp_values_sd[11,2]
})

cor_diff <- cor(X)[1,2] # -cor(res$complete_data)[1,2]

test_that("Clustimpute: correlation diff", {
  expect_lt(abs(cor_diff), eps)
})

test_that("shrink_towards_global_mean has no impact on scaled data", {
  expect_equal(sum((res$centroids-res_shrink_towards_global_mean_FALSE$centroids)^2),0)
})
