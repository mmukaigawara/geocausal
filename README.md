
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geocausal

<!-- badges: start -->
<!-- badges: end -->

The goal of `geocausal` is to implement causal inference analytic
methods based on spatio-temporal data. Users provide the raw data of
locations and timings of treatment and outcome events, specify
counterfactual scenarios, and the package estimates causal effects over
specified spatial and temporal windows.

For methodological details, please refer to the following article.

> Papadogeorgou G, Imai K, Lyall J, and Li F (2022). Causal inference
> with spatio-temporal data: Estimating the effects of airstrikes on
> insurgent violence in Iraq. *J R Stat Soc Series B.*
> <https://doi.org/10.1111/rssb.12548>.

## Installation

You can install the development version of geocausal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mmukaigawara/geocausal")
```
