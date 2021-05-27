# ClustImpute 0.2.4

* print()/cat() replaced by message()/warning(), except for print function
* Improved documentation of return values

# ClustImpute 0.2.3

* Title field now in title case to comply with CRAN policies

# ClustImpute 0.2.2

* Updated description to fix CRAN NOTE

# ClustImpute 0.2.1

* Packages in suggests used conditionally also in the vignette
* Vignettes shows loaded packages

# ClustImpute 0.2.0

* It used to be the (strong) recommendation to center the data if a weight function is used (n_end >1). Now, by default, the scaling with the weight function is towards the global overall mean of each feature. Thus, for centered data there is almost no change (due the random imputation mechanism data with a true unknown mean of zero might have an empirical mean unequal to zero).
* There is a check if the data is centered, and potentially a warning.
* Added a plot function for ClustImpute results showing marginal distributions by cluster and feature. Type histogram and boxplot.
* Added custom print function
* Updated vignette showing new functionality
* All changes above increase dependencies

# ClustImpute 0.1.7

* Removed dependency from psych package.
* Added marginal plots to the vignette via ggExtra.

# ClustImpute 0.1.6

* Added vignette that describes the algorithm in more detail.

# ClustImpute 0.1.5

* Only relevant functions are exported
* Seed for random imputation changes with iteration variables. For this reason previous results may change slightly.
* Full test coverage
* Added citation

# ClustImpute 0.1.4

* There is now a new option assign_with_wf. If set to False, then the weight function is only applied in the centroid computation, but ignored in the cluster assignment
* New check for duplicate centroids. Duplicate rows are replaced with random draws.
* Compatibility with new version 1.2.2 of ClusterR

# ClustImpute 0.1.3

* Full test coverage

# ClustImpute 0.1.2

# ClustImpute 0.1.1

# ClustImpute 0.1.0

* First release
