# basic metric
auc <- function (actual, predicted) {
  if (inherits(actual, "factor")) {
    actual <- as.integer(actual) - 1L
  }
  else if (inherits(actual, "character")) {
    actual <- as.integer(as.factor(actual)) - 1L
  }
  r <- rank(predicted)
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- length(actual) - n_pos
  (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

one_minus_auc <- function(actual, predicted){

  1 - auc(actual, predicted)

}

accuracy <- function(actual, predicted, na.rm = FALSE) {
  mean(actual == predicted, na.rm = FALSE)
}

one_minus_accuracy <- function(actual, predicted, na.rm = FALSE) {

  1 - accuracy(actual, predicted)

}

rmse <- function(actual, predicted, na.rm = FALSE) {
  sqrt(mean((predicted - actual)^2, na.rm = na.rm))
}
