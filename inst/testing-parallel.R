library(celavi)

data   <- tibble::as_tibble(vip::gen_friedman(1000, seed = 101))

# object <- nnet::nnet(y ~ ., data = data, size = 7, decay = 0.1, linout = TRUE, maxit = 500)
object <- randomForest::randomForest(y ~ ., data = data, ntrees = 1000)
# object <- lm(y ~ ., data = data)

# as.formula(object)


set.seed(123)
t1 <- system.time({r1 <- variable_importance(object, data, response = "y", iterations = 500)})

set.seed(1234)
t2 <- system.time({r2 <- variable_importance(object, data, response = "y", iterations = 500, parallel = TRUE)})

t1

t2

plot(r1, r2)

r1 |> dplyr::group_by(variable) |> dplyr::summarise_all(mean)
r2 |> dplyr::group_by(variable) |> dplyr::summarise_all(mean)


# library(ggplot2)
# library(patchwork)
#
# plot(t1)
#
# g1 <- ggplot(r1) + geom_boxplot(aes(variable, value))
# g2 <- ggplot(r2) + geom_boxplot(aes(variable, value))
#
# g1 + g2
