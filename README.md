
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDModels <a href="https://www.markus-ulmer.ch/SDModels/"><img src="man/figures/logo.png" align="right" height="120" alt="SDModels website" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SDModels)](https://CRAN.R-project.org/package=SDModels)
[![R-CMD-check](https://github.com/markusul/SDModels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markusul/SDModels/actions/workflows/R-CMD-check.yaml)
![CRAN Downloads
overall](https://cranlogs.r-pkg.org/badges/grand-total/SDModels?color=brightgreen)
<!-- badges: end -->

Spectrally Deconfounded Models (SDModels) is a package with methods to
screen for and analyze non-linear sparse direct effects in the presence
of unobserved confounding using the spectral deconfounding techniques
(Ćevid, Bühlmann, and Meinshausen (2020), Guo, Ćevid, and Bühlmann
(2022)). These methods have been shown to be a good estimate for the
true direct effect if we observe many covariates, e.g., high-dimensional
settings, and we have fairly dense confounding. Even if the assumptions
are violated, it seems like there is not much to lose, and the SDModels
will, in general, estimate a function closer to the true one than
classical least squares optimization. SDModels provides software for
Spectrally Deconfounded Additive Models (SDAMs) (Scheidegger, Guo, and
Bühlmann (2025)) and Spectrally Deconfounded Random Forests
(SDForest)(Ulmer, Scheidegger, and Bühlmann (2025)).

![](man/figures/confModel.png)

## Installation

To install the SDModels R package from CRAN, just run

``` r
install.packages(SDModels)
```

You can install the development version of SDModels from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("markusul/SDModels")
```

or

``` r
# install.packages('pak')
pak::pkg_install('markusul/SDModels')
```

## Usage

This is a basic example on how to estimate the direct effect of $X$ on
$Y$ using SDForest. You can learn more about analyzing sparse direct
effects estimated by SDForest in the article
[SDForest](https://www.markus-ulmer.ch/SDModels/articles/SDForest.html).

``` r
library(SDModels)

set.seed(42)
# simulation of confounded data
sim_data <- simulate_data_nonlinear(q = 2, p = 50, n = 100, m = 2)
X <- sim_data$X
Y <- sim_data$Y
train_data <- data.frame(X, Y)
# parents
sim_data$j
#> [1] 25 24

fit <- SDForest(Y ~ ., train_data)
fit
#> SDForest result
#> 
#> Number of trees:  100 
#> Number of covariates:  50 
#> OOB loss:  0.1617913 
#> OOB spectral loss:  0.05095329
```

You can also estimate just one Spectrally Deconfounded Regression Tree
using the `SDTree` function. See also the article
[SDTree](https://www.markus-ulmer.ch/SDModels/articles/SDTree.html).

``` r
Tree <- SDTree(Y ~ ., train_data, cp = 0.01)
#plot(Tree)
```

Or you can estimate a Spectrally Deconfounded Additive Model, with
theoretical guarantees, using the `SDAM` function. See also the article
[SDAM](https://www.markus-ulmer.ch/SDModels/articles/SDAM.html).

``` r
model <- SDAM(Y ~ ., train_data)
model
#> SDAM result
#> 
#> Number of covariates:  50 
#> Number of active covariates:  3
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Cevid2020SpectralModels" class="csl-entry">

Ćevid, Domagoj, Peter Bühlmann, and Nicolai Meinshausen. 2020.
“<span class="nocase">Spectral Deconfounding via Perturbed Sparse Linear
Models</span>.” *J. Mach. Learn. Res.* 21 (1).
<http://jmlr.org/papers/v21/19-545.html>.

</div>

<div id="ref-Guo2022DoublyConfounding" class="csl-entry">

Guo, Zijian, Domagoj Ćevid, and Peter Bühlmann. 2022.
“<span class="nocase">Doubly debiased lasso: High-dimensional inference
under hidden confounding</span>.” *The Annals of Statistics* 50 (3).
<https://doi.org/10.1214/21-AOS2152>.

</div>

<div id="ref-scheidegger2023spectral" class="csl-entry">

Scheidegger, Cyrill, Zijian Guo, and Peter Bühlmann. 2025. “Spectral
Deconfounding for High-Dimensional Sparse Additive Models.” *ACM / IMS
J. Data Sci.* <https://doi.org/10.1145/3711116>.

</div>

<div id="ref-ulmer2025spectrallydeconfoundedrandomforests"
class="csl-entry">

Ulmer, Markus, Cyrill Scheidegger, and Peter Bühlmann. 2025. “Spectrally
Deconfounded Random Forests.” *Journal of Computational and Graphical
Statistics*. ASA Website.
<https://doi.org/10.1080/10618600.2025.2569602>.

</div>

</div>
