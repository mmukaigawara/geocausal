
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geocausal <a href="https://github.com/mmukaigawara/geocausal"><img src="inst/figure/logo.png" align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/geocausal)](https://CRAN.R-project.org/package=geocausal)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/geocausal)](https://cran.r-project.org/package=geocausal)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/geocausal)](https://cran.r-project.org/package=geocausal)
<!-- badges: end -->

The goal of the package
[geocausal](https://github.com/mmukaigawara/geocausal) is to implement
causal inference analytic methods based on spatio-temporal data. Users
provide the raw data of locations and timings of treatment and outcome
events, specify counterfactual scenarios, and the package estimates
causal effects over specified spatial and temporal windows.

## Installation

You can install the package
[geocausal](https://github.com/mmukaigawara/geocausal) from
[GitHub](https://github.com/mmukaigawara/geocausal) with:

``` r
# install.packages("devtools")
devtools::install_github("mmukaigawara/geocausal")
```

and [CRAN](https://cran.r-project.org/package=geocausal) with:

``` r
install.packages("geocausal")
```

## Key publications

<div style="text-align: center;">

<a href="https://github.com/mmukaigawara/geocausal">
<img src="inst/figure/poster.png" align="center" height="auto" /> </a>

</div>

<br>

**General methodological framework** (ATE, heterogeneity, and
**mediation**):

> Mukaigawara M, Imai K, Lyall J, Papadogeorgou G (2025). Spatiotemporal
> causal inference with arbitrary spillover and carryover effects. arXiv
> Preprints. April 4. <https://arxiv.org/abs/2504.03464>

**ATE**:

> Papadogeorgou G, Imai K, Lyall J, and Li F (2022). Causal inference
> with spatio-temporal data: Estimating the effects of airstrikes on
> insurgent violence in Iraq. *J R Stat Soc Series B.*
> <https://doi.org/10.1111/rssb.12548>.

**Heterogeneity**:

> Zhou L, Imai K, Lyall J, Papadogeorgou G (2024). Estimating
> Heterogeneous Treatment Effects for Spatio-Temporal Causal Inference:
> How Economic Assistance Moderates the Effects of Airstrikes on
> Insurgent Violence. arXiv Preprints. Dec 19.
> <https://arxiv.org/abs/2412.15128>

Please refer to the following preprint for the **user guide**.

> Mukaigawara M, Zhou L, Papadogeorgou G, Lyall J, and Imai K (2024).
> Geocausal: An R Package for Spatio-temporal Causal Inference. OSF
> Preprints. December 16. <https://doi.org/10.31219/osf.io/5kc6f>.

## Citation

Please cite this package as follows:

> Mukaigawara M, Imai K, Lyall J, Papadogeorgou G (2025). Spatiotemporal
> causal inference with arbitrary spillover and carryover effects. arXiv
> Preprints. April 4. <https://arxiv.org/abs/2504.03464>

> Mukaigawara M, Zhou L, Papadogeorgou G, Lyall J, and Imai K (2024).
> Geocausal: An R Package for Spatio-temporal Causal Inference. OSF
> Preprints. December 16. <https://doi.org/10.31219/osf.io/5kc6f>.
