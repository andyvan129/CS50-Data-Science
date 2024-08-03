library(Lahman)
library(tidyverse)
library(dslabs)

?Teams
head(Teams)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = X2B/G, R_per_game = X3B/G) %>%
  summarize(cor(AB_per_game, R_per_game))


set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

head(GaltonFamilies)

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

height_sum <- female_heights %>%
  summarise(avg_mom = mean(mother), sd_mom = sd(mother), avg_dt = mean(daughter), sd_dt = sd(daughter), cor = cor(mother, daughter))

slope <- height_sum$cor * height_sum$sd_dt / height_sum$sd_mom
b <- height_sum$avg_dt - slope * height_sum$avg_mom
height_sum^2
slope * 60 + b
