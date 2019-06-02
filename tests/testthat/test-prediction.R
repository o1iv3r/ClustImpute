# define parameters
eps <- 1e-2 # error tolerance
n <- 1e4
frac_missing <- .1
nr_cluster <- 5

# Random data
X <- as.data.frame(matrix(rnorm(2*n),n,2))
Xmiss <- miss_sim(X,p=frac_missing,type="MCAR")

res <- ClustImpute(Xmiss,nr_cluster)

test_that("Prediction function on complete_data return the final cluster assignment", {
  expect_equal(predict(res,newdata = res$complete_data), res$clusters)
})

# quick and dirty using dplyr and nndist
library(dplyr)
# compute the means grouped by cluster assignment
tmp <- res$complete_data %>% mutate(pred=res$clusters)
tmp <- tmp %>% group_by(pred) %>% summarise(V1=mean(V1),V2=mean(V2)) %>% select(-pred)
# take mean from result object in tmp2
tmp2 <- res$centroids
class(tmp2) <- "matrix" # not necessary in newer versions
tmp2 <- as.data.frame(tmp2)

# vector of distance between the centroids
centroid_dist <- (apply((tmp-tmp2)^2,mean,MARGIN=1))^.5

# check that distance is small enough
check1 <- !all(centroid_dist<1e-3)

test_that("Mean provided by the final cluster assignment is indeed the mean by cluster", {
  expect_true(check1)
})
