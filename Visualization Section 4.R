data(murders)

murders %>%
  mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region, rate)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()

