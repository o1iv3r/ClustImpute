#' Simulation of missings
#'
#' Simulates missing at random using a normal copula to create correlations between the missing (type="MAR").
#' Missings appear in each column of the provided data frame with the same ratio.
#'
#' @param dat Data frame with only numeric values
#' @param p Fraction of missings (for entire data frame)
#' @param type Type of missingness. Either MCAR (=missing completely at random) or MAR (=missing at random)
#' @param seed_nr Number for set.seed()
#'
#' @return data frame with only numeric values and NAs
#' @examples
#' data(cars)
#' cars_with_missings <- miss_sim(cars,p = .2,seed_nr = 4)
#' summary(cars_with_missings)
#'
#' @export
miss_sim <- function(dat,p=.2,type="MAR",seed_nr=123) {
  set.seed(seed_nr)
  dim <- dim(dat)[2]

  if (type=="MAR") {
    # create random correlation matrix for the copula
    cor_param <- 2*stats::runif(dim * (dim - 1) / 2)-1 # numbers between -1 and 1
    cor_matrix <- stats::cov2cor(copula::p2P(cor_param) %*% t(copula::p2P(cor_param))) # make sure the matrix is positive definite
    cor_matrix <- copula::P2p(cor_matrix) # convert matrix to vector for copula::normalCopula
  } else if (type=="MCAR") {
    cor_matrix <- diag(dim)
    cor_matrix <- copula::P2p(cor_matrix)
  } else stop("Provide valid type")


  # specify dependence structure of normal copula
  myCop <- copula::normalCopula(param=cor_matrix, dim , dispstr = "un")

  # specify binomial marginals
  myMvd <- copula::mvdc(copula=myCop, margins=c("binom"), marginsIdentical = T, paramMargins=list(list(size=1,prob=p)) )

  # simulate from distribution to obtain missing indicator
  mis_ind <- copula::rMvdc(dim(dat)[1], myMvd)

  # add NAs to data set
  mis_ind_NA <- sapply(mis_ind,function(x) ifelse(x==1,NA,x)) # change 1's to NA

  return (dat+mis_ind_NA)
}



