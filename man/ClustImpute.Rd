% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustImpute.R
\name{ClustImpute}
\alias{ClustImpute}
\title{K-means clustering with build-in missing data imputation}
\usage{
ClustImpute(
  X,
  nr_cluster,
  nr_iter = 10,
  c_steps = 1,
  wf = default_wf,
  n_end = 10,
  seed_nr = 150519,
  assign_with_wf = TRUE,
  shrink_towards_global_mean = TRUE
)
}
\arguments{
\item{X}{Data frame with only numeric values or NAs}

\item{nr_cluster}{Number of clusters}

\item{nr_iter}{Iterations of procedure}

\item{c_steps}{Number of clustering steps per iteration}

\item{wf}{Weight function. Linear up to n_end by default. Used to shrink X towards zero or the global mean (default). See shrink_towards_global_mean}

\item{n_end}{Steps until convergence of weight function to 1}

\item{seed_nr}{Number for set.seed()}

\item{assign_with_wf}{Default is TRUE. If set to False, then the weight function is only applied in the centroid computation, but ignored in the cluster assignment.}

\item{shrink_towards_global_mean}{By default TRUE. The weight matrix w is applied on the difference of X from the global mean m, i.e, (x-m)*w+m}
}
\value{
\describe{
  \item{complete_data}{Completed data without NAs}
  \item{clusters}{For each row of complete_data, the associated cluster}
  \item{centroids}{For each cluster, the coordinates of the centroids in tidy format}
  \item{centroids_matrix}{For each cluster, the coordinates of the centroids in matrix format}
  \item{imp_values_mean}{Mean of the imputed variables per draw}
  \item{imp_values_sd}{Standard deviation of the imputed variables per draw}
}
}
\description{
Clustering algorithm that produces a missing value imputation using  on the go. The (local) imputation distribution is defined by the currently assigned cluster. The first draw is by random imputation.
}
\examples{
# Random Dataset
set.seed(739)
n <- 750 # numer of points
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
res <- ClustImpute(dat_with_miss,nr_cluster=3)

# Plot complete data set and cluster assignment
ggplot2::ggplot(res$complete_data,ggplot2::aes(x,y,color=factor(res$clusters))) +
ggplot2::geom_point()

# View centroids
res$centroids

}
