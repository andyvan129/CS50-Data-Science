# Assessment 1-2
set.seed(21, sample.kind = "Rounding")

n <- 44
B <- 10000
X <- replicate(B, sum(sample(c(1, -0.25), n, prob = c(0.2, 0.8), replace = TRUE)))
mean(X >= 8)

avg <- n * (0.2*1 + 0.8*-0.25)
se <- sqrt(n) * 1.25 * sqrt(0.2*0.8)
1 - pnorm(8, avg, se)

0.25 * 44

p <- seq(0.25, 0.95, 0.05)
avg_new <- n * (p*1)
se_new <- sqrt(n) * 1 * sqrt(p*(1-p))
Y <- 1-pnorm(35, avg_new, se_new)

data.frame(p, avg_new, se_new, Y)

# Assessment 3
win <- 5/38
lose <- 1-win
value <- 6 * win + -1 * lose
se <- 7 * sqrt(win * lose)

sqrt(500) * se
pnorm(0, -39.5, 52.9)
