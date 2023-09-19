
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geocausal <a href="https://github.com/mmukaigawara/geocausal"><img src="inst/figure/logo.png" align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geocausal?color=green)](https://cran.r-project.org/package=geocausal)
[![](http://cranlogs.r-pkg.org/badges/grand-total/geocausal?color=green)](https://cran.r-project.org/package=geocausal)
[![](http://cranlogs.r-pkg.org/badges/geocausal?color=green)](https://cran.r-project.org/package=geocausal)
[![](http://cranlogs.r-pkg.org/badges/last-week/geocausal?color=green)](https://cran.r-project.org/package=geocausal)
<!-- badges: end -->

The goal of the package
[geocausal](https://github.com/mmukaigawara/geocausal) is to implement
causal inference analytic methods based on spatio-temporal data. Users
provide the raw data of locations and timings of treatment and outcome
events, specify counterfactual scenarios, and the package estimates
causal effects over specified spatial and temporal windows.

For methodological details, please refer to the following article.

> Papadogeorgou G, Imai K, Lyall J, and Li F (2022). Causal inference
> with spatio-temporal data: Estimating the effects of airstrikes on
> insurgent violence in Iraq. *J R Stat Soc Series B.*
> <https://doi.org/10.1111/rssb.12548>.

## Citation

Please cite this package as follows:

> Mukaigawara M, Papadogeorgou G, Lyall J, Imai K (2023). geocausal:
> Causal Inference with Spatio-Temporal Data. R package version 0.1.0,
> <https://github.com/mmukaigawara/geocausal>

## Installation

You can install the package
[geocausal](https://github.com/mmukaigawara/geocausal) from
[GitHub](https://github.com/mmukaigawara/geocausal) with:

``` r
# install.packages("devtools")
devtools::install_github("mmukaigawara/geocausal")
```

and [CRAN](https://cran.r-project.org/package=geocausal)
with:

``` r
install.packages("geocausal")
```
