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

n <- 100

rsme_sum <- function(n) {
  Sigma <- 9*matrix(c(1, 0.5, 0.5, 1), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    model <- lm(y ~ x, data = train)
    predicted <- predict(model, test, type = "response")
    sqrt(mean((predicted - test$y)^2))
  }) %>%
    data.frame() %>%
    setNames("rsme") %>%
    summarise(mean = mean(rsme), sd = sd(rsme))
}

rsme_sum(100)

set.seed(1)

n <- c(100, 500, 1000, 5000, 10000)
map(n, rsme_sum)


# Q4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat, pch = 20)
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


# Q5
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

model <- lm(y ~ x_1, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))

model <- lm(y ~ x_2, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))

model <- lm(y ~ x_1 + x_2, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))


# Q8
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 
                  0.75, 1.0, 0.95, 
                  0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

model <- lm(y ~ x_1, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))

model <- lm(y ~ x_2, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))

model <- lm(y ~ x_1 + x_2, data = train)
predicted <- predict(model, newdata = test, type = "response")
sqrt(mean((predicted - test$y)^2))
