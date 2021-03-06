# define parameters
n <- 1e2
frac_missing <- .1
nr_cluster <- 1

# Random data
set.seed(101)
X <- as.data.frame(matrix(rnorm(2*n),n,2))
Xmiss <- miss_sim(X,p=frac_missing,type="MCAR")

res <- ClustImpute(Xmiss,nr_cluster)
hist_plot <- plot(res)
hist_plot_mean <- plot(res,vline="mean")
box_plot <- plot(res,type="box")

output_console <- capture.output(print(res))

test_that("Plot returns a ggplot2 object", {
  expect_equal(class(hist_plot)[2], "ggplot")
  expect_equal(class(hist_plot_mean)[2], "ggplot")
  expect_equal(class(box_plot)[2], "ggplot")
})

test_that("Plot throws error for wrong imputs", {
  expect_error(plot(res,type="centoids2"), "type must bei either 'hist' or 'box'")
  expect_error(plot(res,vline="mean2"), "vline must be either 'centroids' or 'mean'")
})

test_that("Print returns an output to the console", {
  expect_equal(output_console[5],"| -0.0196439| -0.0667993|       1|")
})
