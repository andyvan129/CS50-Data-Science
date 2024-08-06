mnist <- read_mnist()
head(mnist)
ncol(mnist$train$images)

<<<<<<< HEAD

# 2.1 Q2
if (!require("dplyr", "lubridate")) install.packages("dplyr", "lubridate")
library(dslabs)
library(dplyr)
library(lubridate)
=======
packages <- c("dplyr", "lubridate", "dslabs", "caret")
for (package in packages) {
  if (!require(package, character.only = TRUE)) install.packages(package)
  library(package, character.only = TRUE)
}

>>>>>>> 97c0a97454b3c0b884565723974279a7cd9513eb
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
<<<<<<< HEAD
x <- dat$type
=======
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
createDataPartition(y, times = 1, p = 0.5)
>>>>>>> 97c0a97454b3c0b884565723974279a7cd9513eb
