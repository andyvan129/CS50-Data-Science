library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

rsme <- replicate(n, {
  test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  model <- train %>%
    lm(y ~ x, data = .)
  predicted <- predict(model, newdata = test, type = "response")
  sqrt(mean((predicted - test$y)^2))
})
mean(rsme)
sd(rsme)
