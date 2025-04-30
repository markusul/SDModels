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
