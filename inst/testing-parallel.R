devtools::load_all()

data   <- tibble::as_tibble(vip::gen_friedman(1000, seed = 101))

object <- nnet::nnet(y ~ ., data = data, size = 7, decay = 0.1, linout = TRUE, maxit = 500)

# as.formula(object)


set.seed(123)
t1 <- system.time({r1 <- variable_importance(object, data, response = "y", iterations = 10000)})

set.seed(1234)
t2 <- system.time({r2 <- variable_importance(object, data, response = "y", iterations = 10000, parallel = TRUE)})

t1
t2

library(ggplot2)
library(patchwork)

g1 <- ggplot(r1) + geom_boxplot(aes(variable, value))
g2 <- ggplot(r2) + geom_boxplot(aes(variable, value))

g1 + g2
