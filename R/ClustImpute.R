#' K-means clustering with build-in missing data imputation
#'
#' Clustering algorithm that produces a missing value imputation using  on the go. The (local) imputation distribution is defined by the currently assigned cluster. The first draw is by random imputation.
#'
#' @param X Data frame with only numeric values or NAs
#' @param nr_cluster Number of clusters
#' @param nr_iter Iterations of procedure
#' @param c_steps Number of clustering steps per iteration
#' @param wf Weight function. linear up to n_end by default
#' @param n_end Steps until convergence of weight function to 1
#' @param seed_nr Number for set.seed()
#'
#' @return
#' \describe{
#'   \item{complete_data}{Completed data without NAs}
#'   \item{clusters}{For each row of complete_data, the associated cluster}
#'   \item{centroids}{For each cluster, the coordinates of the centroids}
#'   \item{imp_values_mean}{Mean of the imputed variables per draw}
#'   \item{imp_values_sd}{Standard deviation of the imputed variables per draw}
#' }
#'
#' @examples
#'# Random Dataset
#'set.seed(739)
#'n <- 750 # numer of points
#'nr_other_vars <- 2
#'mat <- matrix(rnorm(nr_other_vars*n),n,nr_other_vars)
#'me<-4 # mean
#'x <- c(rnorm(n/3,me/2,1),rnorm(2*n/3,-me/2,1))
#'y <- c(rnorm(n/3,0,1),rnorm(n/3,me,1),rnorm(n/3,-me,1))
#'dat <- cbind(mat,x,y)
#'dat<- as.data.frame(scale(dat)) # scaling
#'
#'# Create NAs
#'dat_with_miss <- miss_sim(dat,p=.1,seed_nr=120)
#'
#'# Run ClustImpute
#'res <- ClustImpute(dat_with_miss,nr_cluster=3)
#'
#'# Plot complete data set and cluster assignment
#'ggplot2::ggplot(res$complete_data,ggplot2::aes(x,y,color=factor(res$clusters))) +
#'ggplot2::geom_point()
#'
#'# View centroids
#'res$centroids
#'
#' @export
ClustImpute <- function(X,nr_cluster, nr_iter=10, c_steps=1, wf=default_wf, n_end=10, seed_nr=150519) {

  mis_ind <- is.na(X) # missing indicator

  if (all(is.na.data.frame(X))) stop("All values of X are NA")

  # random imputation
  for (j in 1:dim(X)[2]) { # variable j
    idx_sample_from <- which(mis_ind[,j]==FALSE) # sample from not missing
    sample_len <- dim(X[mis_ind[,j]==TRUE, ])[1] # nr missings = nr of required draws
    set.seed(seed_nr)
    sampled_idx <- sample(idx_sample_from,size=sample_len,replace=TRUE)
    X[mis_ind[,j]==TRUE,j] <- X[sampled_idx,j] # overwrite old with new draws
  }

  # save statistics for imputed values
  org_is_false <- mis_ind
  org_is_false[mis_ind==FALSE] <- NA # imputed is TRUE, original is NA
  mean_imp <- sapply(X*org_is_false,mean,na.rm=TRUE)
  sd_imp <- sapply(X*org_is_false,stats::sd,na.rm=TRUE)

  for (n in 1:nr_iter) {
    # Use weights to adjust the scale of a variable specifically for each observation
    args_wf <- list(n,n_end)
    weight_matrix <- 1 - mis_ind * do.call(wf, args_wf)
    X_down_weighted <- weight_matrix*X

    # perform clustering
    if (n==1) {
      cl_new <- ClusterR::KMeans_arma(data=X_down_weighted,clusters=nr_cluster,n_iter=c_steps,seed=seed_nr+n)
    } else {
      # starting with existing clusters
      cl_old <- cl_new
      class(cl_old) <- "matrix"
      cl_new <- ClusterR::KMeans_arma(data=X_down_weighted,clusters=nr_cluster,n_iter=c_steps,seed=seed_nr+n,
                                      CENTROIDS=cl_old, seed_mode="keep_existing")
    }

    # predict cluster
    pred <- ClusterR::predict_KMeans(X,cl_new) # use X without weight for assignment
    class(pred) <- "integer"

    # draw missing values from current cluster
    for (i in 1:max(pred)) { # cluster i
      for (j in 1:dim(X)[2]) { # variable j
        idx_sample_from <- which(pred==i & mis_ind[,j]==FALSE) # sample from not missing
        if (length(idx_sample_from)==0) { # take donors from all clusters if there are none in the current cluster
          idx_sample_from <- which(mis_ind[,j]==FALSE) # sample from not missing
        }
        sample_len <- dim(X[pred==i & mis_ind[,j]==TRUE, ])[1] # nr missings = nr of required draws
        if (sample_len>0) {
          set.seed(2*seed_nr+n)
          sampled_idx <- sample(idx_sample_from,size=sample_len,replace=TRUE)
          X[pred==i & mis_ind[,j]==TRUE,j] <- X[sampled_idx,j] # overwrite old with new draws
        }
      }
    }

    # compute mean and variance of imputed values for trace plot
    # store in matrix
    mean_imp <- rbind(mean_imp,sapply(X*org_is_false,mean,na.rm=TRUE))
    sd_imp <- rbind(sd_imp,sapply(X*org_is_false,stats::sd,na.rm=TRUE))
  } # loop end: nr_iter

  # add index for imputation iteration
  mean_imp <- cbind(mean_imp,iter=0:nr_iter)
  sd_imp <- cbind(sd_imp,iter=0:nr_iter)

  # perform clustering one last time
  cl_old <- cl_new
  class(cl_old) <- "matrix"
  cl_new <- ClusterR::KMeans_arma(data=X,clusters=nr_cluster,n_iter=c_steps,seed=seed_nr+n,
                                  CENTROIDS=cl_old, seed_mode="keep_existing")

  # predict cluster one last time on final draws
  pred <- ClusterR::predict_KMeans(X,cl_new)
  class(pred) <- "integer"
  class(cl_new) <- "matrix"

  # return all relevant results
  # parameters are provided as attributes
  res <- structure(list(complete_data=X,clusters=pred,centroids=cl_new,imp_values_mean=mean_imp,imp_values_sd=sd_imp),
                   class="kmeans_ClustImpute",nr_iter=nr_iter, c_steps=c_steps, wf=wf, n_end=n_end, seed_nr=seed_nr)

  return (res)
}


#' K-means clustering with build-in missing data imputation
#'
#' Default weight function. One minus the return value is multiplied with missing(=imputed) values.
#'  It starts with 1 and goes to 0 at n_end.
#'
#' @param n current step
#' @param n_end steps until convergence of weight function to 0
#' @return value between 0 and 1
#' @examples
#' x <- 0:20
#' plot(x,1-default_wf(x))
#'
#' @export
default_wf <- function(n,n_end=10) {
  y <- 1-pmin(n/n_end,1)
  return(y)
}


#' Prediction method
#'
#' @param object Object of class kmeans_ClustImpute
#' @param newdata Data frame
#' @param ... additional arguments affecting the predictions produced - not currently used
#' @return integer value (cluster assignment)
#' @examples
#'# Random Dataset
#'set.seed(739)
#'n <- 750 # numer of points
#'nr_other_vars <- 2
#'mat <- matrix(rnorm(nr_other_vars*n),n,nr_other_vars)
#'me<-4 # mean
#'x <- c(rnorm(n/3,me/2,1),rnorm(2*n/3,-me/2,1))
#'y <- c(rnorm(n/3,0,1),rnorm(n/3,me,1),rnorm(n/3,-me,1))
#'dat <- cbind(mat,x,y)
#'dat<- as.data.frame(scale(dat)) # scaling
#'
#'# Create NAs
#'dat_with_miss <- miss_sim(dat,p=.1,seed_nr=120)
#'
#' res <- ClustImpute(dat_with_miss,nr_cluster=3)
#' predict(res,newdata=dat[1,])
#'
#'
#' @rdname predict
#' @export
predict.kmeans_ClustImpute <- function(object,newdata,...) {
  pred <- ClusterR::predict_KMeans(newdata,object$centroids)
  class(pred) <- "integer"
  return(pred)
}


#' Reduction of variance
#'
#' Computes one minus the ratio of the sum of all within cluster variances by the overall variance
#'
#' @param clusterObj Object of class kmeans_ClustImpute
#' @return integer value typically between 0 and 1
#' @examples
#'
#'# Random Dataset
#'set.seed(739)
#'n <- 750 # numer of points
#'nr_other_vars <- 2
#'mat <- matrix(rnorm(nr_other_vars*n),n,nr_other_vars)
#'me<-4 # mean
#'x <- c(rnorm(n/3,me/2,1),rnorm(2*n/3,-me/2,1))
#'y <- c(rnorm(n/3,0,1),rnorm(n/3,me,1),rnorm(n/3,-me,1))
#'dat <- cbind(mat,x,y)
#'dat<- as.data.frame(scale(dat)) # scaling
#'
#'# Create NAs
#'dat_with_miss <- miss_sim(dat,p=.1,seed_nr=120)
#'
#'res <- ClustImpute(dat_with_miss,nr_cluster=3)
#'var_reduction(res)
#'
#' @export
var_reduction <- function(clusterObj) {
  # within cluster variances
  within_var <- clusterObj$complete_data %>% dplyr::mutate(pred=clusterObj$clusters) # add prediciton column
  cluster_sizes <- within_var %>% dplyr::group_by(.data$pred) %>% dplyr::count() %>%
    dplyr::ungroup(.data$pred) %>% dplyr::select(.data$n) %>% dplyr::pull()
  within_var <- within_var %>% dplyr::group_by(.data$pred) %>% dplyr::summarise_all(stats::var) %>%
    dplyr::select(-.data$pred) %>% dplyr::summarise_all(stats::weighted.mean,w=cluster_sizes,na.rm=TRUE)
  within_var_sum <- sum(within_var) # sum of all within cluster variances

  # overall variance
  overall_var <- sum(clusterObj$complete_data %>% dplyr::summarise_all(stats::var))

  return(list(Variance_reduction=1-within_var_sum/overall_var,Variance_by_cluster=within_var))
}
