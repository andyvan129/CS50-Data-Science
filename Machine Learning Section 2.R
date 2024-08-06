mnist <- read_mnist()
head(mnist)
ncol(mnist$train$images)


packages <- c("dplyr", "lubridate", "dslabs", "caret", "purrr")
for (package in packages) {
  if (!require(package, character.only = TRUE)) install.packages(package)
  library(package, character.only = TRUE)
}

data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>%
  group_by(type) %>%
  summarise(female = mean(sex == 'Female'), male = mean(sex == 'Male'))

y_hat <- ifelse(x == 'inclass', 'Female', 'Male') %>% factor(levels = levels(y))
mean(y == y_hat)

table(y_hat, y)
sensitivity(y_hat, y)
specificity(y_hat, y)
table(predict = y_hat, actual = y)
(26+13) / (26+42+13+69)


# 2.1 Q3
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


r <- range(train$Petal.Width)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, 'virginica', 'versicolor') %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()
wid <- cutoff[which.max(accuracy)]


y_hat <- ifelse(test$Petal.Width > 1.5, 'virginica', 'versicolor') %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)


plot(iris, pch=21, bg=iris$Species)


r <- range(train$Petal.Length)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, 'virginica', 'versicolor') %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()
len <- cutoff[which.max(accuracy)]


y_hat <- ifelse(test$Petal.Width > wid & test$Petal.Length > len, 'virginica', 'versicolor') %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)
