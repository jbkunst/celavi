devtools::load_all()

# data(package = "DALEX")

# data <- risk3r::credit |>
#   dplyr::mutate_if(is.character, as.factor) |>
#   dplyr::select(where(~ 1 != length(unique(.x))))
#
# data <- data[complete.cases(data), ]

data(apartments, package = "DALEX")

data <- tibble::as_tibble(apartments)

# glm ---------------------------------------------------------------------
m_lm <- lm(m2.price ~ ., data = data)

vi_lm <- variable_importance(m_lm, data = data, iteration = 50)

class(vi_lm)

plot(vi_lm)

# ctree -------------------------------------------------------------------
m_tre <- partykit::ctree(m2.price ~ ., data = data)

predict(m_tre, head(data))

vi_tre <- variable_importance(
  m_tre,
  data = data,
  iteration = 50,
  predict_function = function(o, d){ predict(o, d)  }
  )

class(m_tre)

plot(vi_tre)

# random foresrt ----------------------------------------------------------
m_rfr <- ranger::ranger(m2.price ~ ., data = data)

vi_rfr <- variable_importance(
  m_rfr,
  data = data,
  response = "m2.price",
  iteration = 50,
  predict_function = function(object, newdata){ ranger:::predict.ranger(object, data = newdata)$predictions }
  )

plot(vi_rfr)

# plot --------------------------------------------------------------------
plot(vi_lm, vi_rfr, vi_tre)
