# data --------------------------------------------------------------------
# devtools::load_all()
library(celavi)

fit_function <-  randomForest:::randomForest.formula
fit_function <-  stats::lm
data <- tibble::as_tibble(vip::gen_friedman(1000, seed = 101))
test <- tibble::as_tibble(vip::gen_friedman(1000, seed = 123))

response <- "y"

metric = NULL
stat = median
stat = function(x) quantile(x, .25)
iterations = 100
sample_size = NULL
sample_frac = NULL
predict_function = NULL
parallel = FALSE

... <- NULL


# nnet --------------------------------------------------------------------
x <- feature_selection(
  nnet::nnet.formula,
  data = data,
  test = test,
  response = "y",
  stat = function(x) quantile(x, 0.25),
  iterations = 10,
  sample_frac = .1,
  size = 7, decay = 0.1, linout = TRUE
)

x

plot(x)

do.call(plot, attr(x, "variable_importance"))


# lm ----------------------------------------------------------------------
x <- feature_selection(
  lm,
  data = data,
  test = test,
  response = "y",
  # stat = function(x) quantile(x, 0.5),
  iterations = 10
  # size = 7, decay = 0.1, linout = TRUE
)

x

do.call(plot, attr(x, "variable_importance"))


# ctree -------------------------------------------------------------------
x <- feature_selection(
  partykit::ctree,
  data = data,
  test = test,
  response = "y",
  # stat = function(x) quantile(x, 0.5),
  iterations = 10
  # size = 7, decay = 0.1, linout = TRUE
)

x

do.call(plot, attr(x, "variable_importance"))


# randomForest ------------------------------------------------------------
x <- feature_selection(
  randomForest:::randomForest.formula,
  data = data,
  test = test,
  response = "y",
  # stat = function(x) quantile(x, 0.5),
  iterations = 10,
  ntree = 10, do.trace = TRUE
)

x

do.call(plot, attr(x, "variable_importance"))



# MNIST -------------------------------------------------------------------
data <- klassets::mnist_train
data <- dplyr::mutate(data, label = factor(label))
data <- dplyr::sample_n(data, 20000)
# data <- dplyr::select(data, c(1, sample(2:785, 300)))

test <- klassets::mnist_test
test <- dplyr::mutate(test, label = factor(label))
test <- dplyr::sample_n(test, 2000)

ranger::ranger

x <- feature_selection(
  ranger::ranger,
  data = data,
  test = test,
  response = "label",
  stat = median,
  # stat = function(x) quantile(x, .25),
  iterations = 25,
  sample_frac = .25,
  # predict_function = function(object, newdata){ranger:::predict.ranger(object, data = newdata)$predictions},
  predict_function = DALEX::yhat,
  # parallel = TRUE,
  max.depth = 10
)

plot(x)

plot(x) +
  ggplot2::scale_y_reverse()

x |>
  dplyr::mutate(auc = 1 - value)

do.call(plot, attr(x, "variable_importance")) +
  ggplot2::scale_y_reverse()


#
# predict_function <- function(object, newdata){ranger:::predict.ranger(object, data = newdata)$predictions}
# response <- "label"
# metric <- NULL
# sample_size <- NULL
#
# iterations  <-  20
# sample_frac <- .1
# stat        <-  median
# stat        <-  function(x) quantile(x, .75)
# ...         <- NULL
