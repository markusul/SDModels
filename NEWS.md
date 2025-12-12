# SDModels 2.0.2

* Switch all the parallelization to futures. See `vignette("Runtime")`
* Switch all the progress updates to progressr. Progress updates are now also available for parallel processing and are customizable.
* Process are much more RAM efficient now.

# SDModels 2.0.1

* Fix bug in SDTree and SDForest where an error occurred, if X had columns with only one unique value.

# SDModels 2.0.0

* Removal of data.tree dependence. Trees are now saved as a matrix.
* This results in a substantial reduction of RAM usage and space needed to save an SDTree or SDForest. It also results in increased computational speed.
* Removal of copy, fromList, and toList. Remove the copy arguments from all pruning involving functions.
* New plotting of SDTree objects.
* Improved/Robust parallelization.
* Remove gpu support and dependence on GPUmatrix due to it being scheduled for archiving.

# SDModels 1.0.13

* In case of parallel processing use random number generator "L'Ecuyer-CMRG" for reproducibility

# SDModels 1.0.12

* Fix extended SDAM example

# SDModels 1.0.11

* Add option to plot SDForests. The plot shows the out-of-bag performance against the number
of trees. This helps to evaluate whether enough trees were used.

# SDModels 1.0.10

* Added feature to select some predictors not to be regularized closes option to use some covariates not regularized in SDAM #4
* Fix the length of the coefficient list to the number of predictors and name the elements
* change predict_individual_j to expect a numeric new data vector instead of a whole data.frame

# SDModels 1.0.9

* Add the option to select some variables as predictors in SDTree and SDForest.

# SDModels 1.0.8

* Fix various bugs on edge cases with just one variable or just one tree
* SDForest, regPath.SDTree, regPath.SDForest, predict.SDForest, prune.SDForest, varImp.SDTree

# SDModels 1.0.7

* Fix bug in estimation of SDTree when using only one covariate (did stop splitting after one split)
* Add support to predict with an SDForest using multiple cores in parallel

# SDModels 1.0.6

* Fix bug in SDTree.predict(), when predicting using only one covariate

# SDModels 1.0.5

* Fix bug in plot of paths using plotly (remove expression Pi in case of plotly)

# SDModels 1.0.4

* Initial CRAN submission.
