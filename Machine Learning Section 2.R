mnist <- read_mnist()
head(mnist)
ncol(mnist$train$images)

packages <- c("dplyr", "lubridate", "dslabs")
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

y_hat <- ifelse(x == 'inclass', 'Female', 'Male')
mean(y == y_hat)
