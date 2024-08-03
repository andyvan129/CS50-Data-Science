# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

Teams %>% filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(BB_per = BB/G, R_per = R/G, HR_per = HR/G) %>%
  lm(R_per ~ BB_per + HR_per, data = .)


# Question 7-8
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- lm(mother ~ daughter, data = female_heights)
predict(model)


# Question 9-12
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% c(1999, 2000, 2001)) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

comp <- inner_join(bat_02, bat_01)
comp %>% ggplot(aes(mean_bb, bb)) +
  geom_point()

lm(singles ~ mean_singles, data = comp)
lm(bb ~ mean_bb, data = comp)


# Part 2.3
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

dat <- galton %>%
  group_by(pair) %>%
  summarise(fit = tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE))

dat %>% mutate(diff = fit$conf.high - fit$conf.low)


# Part 2.4
head(Teams)
dat <- Teams %>%
  select(yearID, BB, HR, R, G) %>%
  filter(yearID == 1971) %>%
  mutate(BB_rate = BB/G, HR_rate = HR/G, R_rate = R/G)

fit <- lm(R_rate ~ BB_rate + HR_rate, data = dat)
tidy(fit)       
