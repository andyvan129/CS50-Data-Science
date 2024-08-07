# bayes theorem
p <- 0.85 * 0.02 / (0.85*0.02 + 0.1*0.98)

set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test == 1)
p <- 0.15 * 0.02 / (0.15 * 0.02 + 0.9 * 0.98)
p_dis <- 0.85 * 0.02 / (0.85 * 0.02 + 0.1 * 0.98)
p_dis / 0.02


# Q2
library(dslabs)
data("heights")
# MISSING CODE
heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarise(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
