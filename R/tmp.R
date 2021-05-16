# One can use such kind of plots to compare to different clustering results


library(ClustImpute)
library(dplyr)
library(ggalluvial)

# Random Dataset
set.seed(739)
n <- 750 # number of points
nr_other_vars <- 2
mat <- matrix(rnorm(nr_other_vars*n),n,nr_other_vars)
me<-4 # mean
x <- c(rnorm(n/3,me/2,1),rnorm(2*n/3,-me/2,1))
y <- c(rnorm(n/3,0,1),rnorm(n/3,me,1),rnorm(n/3,-me,1))
dat <- cbind(mat,x,y)
dat<- as.data.frame(scale(dat)) # scaling

# Create NAs
dat_with_miss <- miss_sim(dat,p=.1,seed_nr=120)

# Run ClustImpute
res2 <- ClustImpute(dat_with_miss,nr_cluster=2)
res3 <- ClustImpute(dat_with_miss,nr_cluster=3)
res4 <- ClustImpute(dat_with_miss,nr_cluster=4)

# all_res <- data.frame(res2=res2$clusters,res3=res3$clusters,res4=res4$clusters)
all_res <- data.frame(res2=factor(res2$clusters),res3=factor(res3$clusters),res4=factor(res4$clusters))
value_count <- count(all_res,res2,res3,res4)

ggplot(value_count,aes(y = n, axis1 = res4, axis2 = res3, axis3 = res2)) +
  geom_alluvium(aes(fill = res2),width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("k=4", "k=3", "k=2")) +
  ylab("Number of observations") +
  coord_flip() +
  ggtitle("Frequency by cluster for varying number of clusters")
