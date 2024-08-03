co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>%
  pivot_longer(-year, names_to = "month", values_to = "co2")

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp
pivot_wider(tmp, names_from = gender_key, values_from = value)
unite(tmp, column_name, c(gender, key))

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
People %>% as_tibble()
top_names <- top %>% left_join(People, by = "playerID") %>%
  select(playerID)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names, by = "playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary)

AwardsPlayers %>% as_tibble() %>%
  filter(yearID == 2016) %>%
  select(playerID) %>%
  setdiff(top_names)


# 2.3 Question 1-3
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[1]])
tab_2 <- html_table(nodes[[19]]) %>%
  `colnames<-`(c("Team", "Payroll", "Average")) %>%
  slice(-1)
tab_1 <- html_table(nodes[[10]]) %>%
  select(-1) %>%
  `colnames<-`(c("Team", "Payroll", "Average")) %>%
  slice(-1)
full_team <- full_join(tab_1, tab_2, by = "Team")


# 2.3 Question 4-5
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
html_table(tab[[6]], fill = TRUE)
