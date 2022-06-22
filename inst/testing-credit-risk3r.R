library(risk3r)
library(ggplot2)

data("credit_woe")

devtools::load_all()

object <- glm(bad ~ ., family = binomial, data = credit_woe)

vi <- variable_importance(object, iterations = 10)

ggplot(vi) +
  geom_boxplot(aes(variable, value)) +
  scale_y_continuous(
    sec.axis = sec_axis(~ 1 - ., labels = scales::percent, name = "AUC")
    ) +
  coord_flip()

