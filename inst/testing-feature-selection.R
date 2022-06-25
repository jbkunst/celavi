# data --------------------------------------------------------------------
devtools::load_all()

data <- tibble::as_tibble(vip::gen_friedman(10000, seed = 101))
test <- tibble::as_tibble(vip::gen_friedman(10000, seed = 123))


# nnet --------------------------------------------------------------------
x <- feature_selection(
  nnet::nnet.formula,
  data = data,
  test = test,
  response = "y",
  # stat = function(x) quantile(x, 0.5),
  iterations = 10,
  size = 7, decay = 0.1, linout = TRUE
)

x

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
# data <- dplyr::sample_n(data, 10000)
# data <- dplyr::select(data, c(1, sample(2:785, 300)))

test <- klassets::mnist_test
test <- dplyr::mutate(test, label = factor(label))
# test <- dplyr::sample_n(test, 1000)


ranger::ranger

x <- feature_selection(
  ranger::ranger,
  data = data,
  test = test,
  response = "label",
  stat = median,
  iterations = 10,
  # sample_frac = .2,
  # predict_function = function(object, newdata){ranger:::predict.ranger(object, data = newdata)$predictions},
  predict_function = DALEX::yhat,
  # parallel = TRUE,
  max.depth = 15
)

x |>
  dplyr::mutate(auc = 1 - value)

do.call(plot, attr(x, "variable_importance")) +
  ggplot2::scale_y_reverse()


x

plot2 <- function(x){

  x2 <- x |>
    dplyr::select(1:3) |>
    tidyr::pivot_longer(cols = c(2:3))

  x2

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(x2, name == "value"),
      ggplot2::aes(round, value)
      ) +
    ggplot2::geom_col(
      data = dplyr::filter(x2, name == "n_variables"),
      ggplot2::aes(round, value)
    ) +

    ggplot2::ylim(0, NA) +

    ggplot2::facet_wrap(ggplot2::vars(.data$name), scales = "free")


}

plot2(x)

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
