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
vi_nnet <- variable_importance(object, data, iterations = 100, sample_frac = 1/10)



# ctree -------------------------------------------------------------------
object <- partykit::ctree(y ~ ., data = data)
object

vi_party <- variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)



# randomForest ------------------------------------------------------------
object <- randomForest::randomForest(y ~ ., data = data, ntree = 50)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
vi_randomForest <- variable_importance(object, data, iterations = 100, sample_frac = 1/10)



# lm ----------------------------------------------------------------------
object <- stats::lm(y ~ ., data = data)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
vi_lm <- variable_importance(object, data, iterations = 100, sample_frac = 1/10)



# MARS --------------------------------------------------------------------
object <- earth::earth(y ~ ., data = data)
object

variable_importance(object, data, response = "y", iterations = 100, sample_frac = 1/10)
vi_earth <- variable_importance(object, data, iterations = 100, sample_frac = 1/10)

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

vi_xgboost <- variable_importance(
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

# DALEX is nice!
variable_importance(
  object, data, response = "y", iterations = 100, sample_frac = 1/10,
  predict_function = DALEX::yhat
)

# for `parallel = TRUE`
vi_ranger <- variable_importance(
  object, data, response = "y", iterations = 100, sample_frac = 1/10, parallel = TRUE,
  # predict_function = getS3method("yhat", class = class(object))
  predict_function = function(object, newdata){ ranger:::predict.ranger(object, data = newdata)$predictions }
)


# plot --------------------------------------------------------------------
vis <- ls() |>
  stringr::str_subset("vi_") |>
  purrr::map(get)

ls() |>
  stringr::str_subset("vi_") |>
  purrr::map(get) |>
  purrr::map(class)

do.call(plot, args = vis)

celavi:::plot.celavi_metric_permutations_raw(
  vi_earth, vi_lm, vi_randomForest, vi_ranger, vi_xgboost
  )

plot(vi_earth)
plot(vi_earth, vi_lm, )

vi_randomForest
get("vi_randomForest")
