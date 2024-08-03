options(digits = 3)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>%
  ggplot(aes(Age, fill=Sex, alpha=0.2)) +
  #geom_point() +
  #geom_jitter()
  geom_density()

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarise(mean = mean(Age))

titanic %>%
  ggplot() +
  geom_qq(aes(sample = Age), dparams = params) +
  geom_abline()

titanic %>%
  ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(position = position_dodge())
  
titanic %>%
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(aes(y = after_stat(count)), alpha = 0.2)

titanic %>%
  filter(Fare != 0) %>%
  ggplot(aes(x = Survived, y = Fare)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  coord_trans(y = "log2")

titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  #geom_bar()
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(x = Age, fill = Survived, alpha = 0.2)) +
  geom_density(aes(y = after_stat(count))) +
  facet_grid(Sex ~ Pclass)
