
<!-- README.md is generated from README.Rmd. Please edit that file -->

# celavi

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbkunst/celavi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbkunst/celavi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal (to me) of celavi is to join the main feature of two functions
two that I use *really* often `vip::vi_permute` and
`DALEX::model_parts`. Both functions do the *same* task but they have
differents features and approachs.

In the case of `vip::vi_permute` is more direct to use, have an
implementation for parallel processing, can be used with a `sample_frac`
parameter. Otherwise, in the case of `DALEX::model_parts` I like the
user can give custom `metric`s as a loss functions, the *base line* and
*full model* references values, and the plots.

To that features I added some features to my *personal* taste.

-   Add progress bars to the sequential and parallel process using
    `progress::progress_bar` and `progressr::progress`
-   Give the possibility of to the user to acces to the *raw* data.
-   Give verbose information using `cli::cli_alert_info`.

## References

The `vip` package from [koalaverse](https://github.com/koalaverse), and
the `DALEX` package from [MI²](https://www.mi2.ai/). In particular these
links are awesome: <https://koalaverse.github.io/vip/articles/vip.html>
and <https://ema.drwhy.ai/featureImportance.html#featureImportanceR>.

Please, visit the links and used that awesome tools!

## Installation

You can install the development version of celavi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jbkunst/celavi")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(celavi)

lm_model <- lm(mpg ~ ., data = mtcars)

vi <- celavi::variable_importance(lm_model, data = mtcars, iterations = 10)
#> ℹ Using all variables in data.
#> ℹ Trying extract response name using `formula`.
#> ℹ Using `mpg` as response.
#> ℹ Using root mean square error as metric.
#> ℹ Using `base::identity` as sampler.
#> ℹ Using `predict.lm` as predict_function.

dim(vi)
#> [1] 120   3

plot(vi)
```

<img src="man/figures/README-example-1.png" width="100%" />
