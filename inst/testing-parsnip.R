library(parsnip)
library(ggplot2)

devtools::load_all()

set.seed(192)

mod <- rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("randomForest") %>%
  set_mode("regression") %>%
  fit(mpg ~ ., data = mtcars)

predict(mod, head(mtcars))
predict(mod, head(mtcars))[[1]]

as.vector(predict(mod, head(mtcars)))

parsnip:::predict.model_fit

fp <- function(o, d){ parsnip:::predict.model_fit(o, d)[[1]] }

vi <- variable_importance(mod, data = mtcars, response = "mpg", predict_function = fp, iterations = 100)

ggplot(vi) + geom_boxplot(aes(variable, value))

sd(sample(mtcars$mpg))

