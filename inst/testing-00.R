# vip:
# - easy to implement
# - parallel
# - sample_frac
#
# DALEX:
# - yhat, good idea
# - custom metric
# - plots
# - loss: full, baseline, features
# - model_info
#
# missing imho:
# - progress bars! xd
# - accesing raw data

DALEX:::model_info.cv.glmnet
DALEX::explain
ingredients:::feature_importance.default
vip:::vi_permute.default


vip:::permute_columns
modelr:::residuals
modelr:::response
modelr:::response_var.default


devtools::load_all()

data <- tibble::as_tibble(vip::gen_friedman(500, seed = 101))


# nnet --------------------------------------------------------------------
object <- nnet::nnet(y ~ ., data = data, size = 7, decay = 0.1, linout = TRUE, maxit = 500)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
variable_importance(object, data, iterations = 100, sample_frac = 1/10)

str(.Last.value)

# randomForest ------------------------------------------------------------
object <- randomForest::randomForest(y ~ ., data = data, ntree = 50)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
variable_importance(object, data, iterations = 100, sample_frac = 1/10)

# lm ----------------------------------------------------------------------
object <- stats::lm(y ~ ., data = data)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
variable_importance(object, data, iterations = 100, sample_frac = 1/10)


# MARS --------------------------------------------------------------------
object <- earth::earth(y ~ ., data = data)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
variable_importance(object, data, iterations = 100, sample_frac = 1/10)


# xgboost (GMB) -----------------------------------------------------------
object <- xgboost::xgboost(
  data = data.matrix(subset(data, select = -y)),
  label = data$y,
  objective = "reg:linear",
  nrounds = 100,
  max_depth = 5,
  eta = 0.3,
  verbose = 0  # suppress printing
)

variable_importance(
  object,
  data,
  response = "y",
  iterations = 100,
  sample_frac = 1/10,
  predict_function = function(object, newdata){ xgboost:::predict.xgb.Booster(object, data.matrix(subset(data, select = -y))) }
  )


# ranger ------------------------------------------------------------------
object <- ranger::ranger(y ~ ., data = data)

variable_importance(
  object, data, response = "y", iterations = 100, sample_frac = 1/10,
  predict_function = function(object, newdata){ ranger:::predict.ranger(object, data = newdata)$predictions }
  )

variable_importance(
  object, data, response = "y", iterations = 100, sample_frac = 1/10,
  predict_function = DALEX::yhat
)

# for `parallel = TRUE`
variable_importance(
  object, data, response = "y", iterations = 100, sample_frac = 1/10, parallel = TRUE,
  # predict_function = getS3method("yhat", class = class(object))
  predict_function = function(object, newdata){ ranger:::predict.ranger(object, data = newdata)$predictions }
)

