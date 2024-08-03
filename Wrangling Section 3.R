library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE) %>%
  `colnames<-`(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>%
  filter(str_ends(remain, "\\%")) %>%
  mutate(undecided = str_replace(undecided, "N/A", "0"))

temp <- str_extract_all(polls$dates, "\\d{1,2}\\s\\D{3,5}")
end_date <- sapply(temp, function(x) x[length(x)])
