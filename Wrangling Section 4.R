library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
sum(month(brexit_polls$startdate) == 4)
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

brexit_polls %>% mutate(weekday = weekdays(enddate)) %>%
  count(weekday)


data(movielens)
head(movielens)
movielens %>%
  mutate(date = as_datetime(timestamp),
         year = year(date),
         hour = hour(date)) %>%
  count(hour)


gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))
gutenberg_works()
