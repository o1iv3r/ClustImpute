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
#' @param assign_with_wf Default is True. If set to False, then the weight function is only applied in the centroid computation, but ignored in the cluster assignment.
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
ClustImpute <- function(X,nr_cluster, nr_iter=10, c_steps=1, wf=default_wf, n_end=10, seed_nr=150519, assign_with_wf = TRUE) {

  mis_ind <- is.na(X) # missing indicator

  if (all(is.na.data.frame(X))) stop("All values of X are NA")

  # random imputation
  for (j in 1:dim(X)[2]) { # variable j
    idx_sample_from <- which(mis_ind[,j]==FALSE) # sample from not missing
    sample_len <- dim(X[mis_ind[,j]==TRUE, ])[1] # nr missings = nr of required draws
    set.seed(seed_nr+j)
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

    # check new centroids for duplicate rows and replace with random draws if necessary
    cl_new <- check_replace_dups(centroids = cl_new, X = X_down_weighted, seed = seed_nr)

    # predict cluster
    if (assign_with_wf==TRUE) {
      pred <- ClusterR::predict_KMeans(X_down_weighted,cl_new) # use X without weight for assignment
      class(pred) <- "integer"
    } else { # option to assign clusters ignoring the weight
      pred <- ClusterR::predict_KMeans(X,cl_new) # use X without weight for assignment
      class(pred) <- "integer"
    }

    # draw missing values from current cluster: random imputation
    for (i in 1:max(pred)) { # cluster i
      for (j in 1:dim(X)[2]) { # variable j
        idx_sample_from <- which(pred==i & mis_ind[,j]==FALSE) # sample from not missing
        if (length(idx_sample_from)==0) { # take donors from all clusters if there are none in the current cluster
          idx_sample_from <- which(mis_ind[,j]==FALSE) # sample from not missing
        }
        sample_len <- dim(X[pred==i & mis_ind[,j]==TRUE, ])[1] # nr missings = nr of required draws
        if (sample_len>0) {
          set.seed(seed_nr+n+i+j)
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

  # check new centroids for duplicate rows and replace with random draws if necessary
  cl_new <- check_replace_dups(centroids = cl_new, X = X_down_weighted, seed = seed_nr)

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


#' Check and replace duplicate (centroid) rows
#'
#' Internal function of ClustImpute: check new centroids for duplicate rows and replace with random draws in this case.
#'
#' @param centroids Matrix of centroids
#' @param X Underlying data matrix (without missings)
#' @param seed Seed used for random sampling
#'
check_replace_dups <- function(centroids, X, seed) {
  # check if new centroids contain duplicate rows (min checks that indeed all values in a row are duplicates)
  dupcliate_rows <- apply(matrix(duplicated(centroids),nrow=dim(centroids)[1],),1,min)
  # If so, replace them by randomly drawn centroids
  nr_dups <- sum(dupcliate_rows)
  while (nr_dups > 0)
  {
    # keep unique centroids
    centroids <- centroids[!dupcliate_rows,]
    # get support for each column and draw uniformly from there
    sup_min <- apply(X,1,min)
    sup_max <- apply(X,1,max)
    set.seed(seed+1)
    new_centroids <-  matrix(stats::runif(nr_dups,sup_min,sup_max),nr_dups,1)
    for (col in 2:dim(X)[2]) {
      sup_min <- apply(X,col,min)
      sup_max <- apply(X,col,max)
      set.seed(seed+col)
      new_centroids <- cbind(new_centroids, matrix(stats::runif(nr_dups,sup_min,sup_max),nr_dups,1))
    }
    centroids <- rbind(centroids,new_centroids) # add new to existing centroids
    dupcliate_rows <- apply(matrix(duplicated(centroids),nrow=dim(centroids)[1],),1,min)
    nr_dups <- sum(dupcliate_rows)
  }
  return (centroids)
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


#' Plot showing marginal distribution by cluster assignment
#'
#' Returns a plot with the marginal distributions by cluster and feature. The plot shows histograms or boxplots and , as a ggplot object, can be modified further.
#'
#' @param x an object returned from ClustImpute
#' @param type either "hist" to plot a histogram or "box" for a boxplot
#' @param vline for "hist" a vertical line is plotted showing either the centroid value or the mean of all data points grouped by cluster and feature
#' @param hist_bins number of bins for histogram
#' @param color_bins color for the histogram bins
#' @param color_vline color for the vertical line
#' @param size_vline size of the vertical line
#' @param ... currently unused
#' @return Returns a ggplot object
#' @rdname plot
#' @export
plot.kmeans_ClustImpute <- function(x,type="hist",vline="centroids",hist_bins=30,color_bins="#56B4E9",color_vline="#E69F00",size_vline=2,...) {
  complete_data <- x$complete_data
  complete_data$Cluster <- x$cluster
  # reshape data for ggplot
  data4plot <-  complete_data %>% tidyr::pivot_longer(!.data$Cluster, names_to = "Feature", values_to = "value")

  if (type=="hist") {
    # get value for vertical line
    if (vline=="centroids") {
      dataLine <- centroids_tidy_format(x)
    } else if (vline=="mean") {
      dataLine <- data4plot %>%
        dplyr::group_by(.data$Cluster,.data$Feature) %>%
        dplyr::summarize(value = mean(.data$value))
    } else stop("vline must be either 'centroids' or 'mean'")
    p <- ggplot2::ggplot(data4plot) + ggplot2::geom_histogram(ggplot2::aes(x = .data$value), bins=hist_bins, fill=color_bins) +
      ggplot2::facet_grid(.data$Feature~.data$Cluster,labeller = ggplot2::label_both) +
      ggplot2::geom_vline(data=dataLine,ggplot2::aes(xintercept=.data$value),size=size_vline,color=color_vline) + ggplot2::theme_light()
  } else if (type=="box") {
    p <- ggplot2::ggplot(data4plot, ggplot2::aes(x = .data$value, y = .data$Feature)) + ggplot2::geom_boxplot() +
      ggplot2::facet_grid(~.data$Cluster,labeller = ggplot2::label_both) +  ggplot2::theme_classic()
  } else stop("type must bei either 'hist' or 'box'")

  return(p)
}


#' Convert centroids matrix into tidy format
#'
#' A helper function for plot() but may be useful for further post-processing of clustering results.
#'
#' @param x an object returned by ClustImpute
#' @return data frame with three columns ( cluster | Features | value ) that uniquely defines the centroids#'
#' @export
centroids_tidy_format <- function(x) {
  cen <- as.data.frame(x$centroids)
  names(cen) <- names(x$complete_data)
  cen$Cluster <- 1:dim(cen)[1]
  cen <- cen %>% tidyr::pivot_longer(!.data$Cluster, names_to = "Feature", values_to = "value")
  return(cen)
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
#'@importFrom rlang .data
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
