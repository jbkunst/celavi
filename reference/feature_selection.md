# Feature selection via iterative rounds of permuted based feature importance

Feature selection via iterative rounds of permuted based feature
importance

## Usage

``` r
feature_selection(
  fit_function = NULL,
  data = NULL,
  test = data,
  response = NULL,
  loss_function = NULL,
  stat = stats::median,
  iterations = 1,
  sample_size = NULL,
  sample_frac = NULL,
  predict_function = NULL,
  parallel = FALSE,
  ...
)
```

## Arguments

- fit_function:

  A function with `formula` and `data` arguments to fit the desired
  model.

- data:

  A data to calculate the loss_function.

- test:

  A testing data frame to evaluate the loss function. By default is the
  data argument.

- response:

  Name of the variable response.

- loss_function:

  The loss function to evaluate, Must be a function with 2 arguments:
  actual and predicted values. Loss function gives a smaller value if
  the model have better performance of the model.

- stat:

  Default `median`. A summary function to compare the values of the loss
  of a variable vs full model. If the `stat` value of the one variable
  is smaller than the value of the loss function full model, then the
  variable is removed in that round.

- iterations:

  Number of iterations.

- sample_size:

  Sample size.

- sample_frac:

  Proportion to sample in each iteration.

- predict_function:

  Predict function, usually is a function(model, newdata) which returns
  a vector (no data frame).

- parallel:

  A logical value indicating if the process should be using
  [`furrr::future_pmap_dbl`](https://furrr.futureverse.org/reference/future_map2.html)
  or
  [`purrr::pmap_dbl`](https://purrr.tidyverse.org/reference/pmap.html).

- ...:

  Specific arguments for `fit_function`.
