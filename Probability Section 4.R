data("death_prob")
death_prob

p <- filter(death_prob, sex == "Female", age == 50)$prob
avg <- (-150000 * p + 1150 * (1-p)) * 1000
se <- abs(-150000 - 1150) * sqrt(p * (1-p)) * sqrt(1000)

pnorm(0, avg, se)

p <- filter(death_prob, sex == "Male", age == 50)$prob
n <- 1000
a <- -150000
b <- (700000 / n - a*p) / (1-p)

avg <- 700000
se <- sqrt(1000) * abs(b - a) * sqrt(p * (1-p))
pnorm(0, avg, se)

p <- 0.015
avg <- 1000 * (-150000 * p + 1150 * (1-p))
se <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p * (1-p))
pnorm(0, avg, se)
pnorm(-1000000, avg, se)

p <- seq(.01, .03, .0025)
avg <- 1000 * (-150000 * p + 1150 * (1-p))
se <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p * (1-p))
prob <- pnorm(-1000000, avg, se)
data.frame(p, prob)

# Question 4
set.seed(27, sample.kind = "Round")

p_loss <- .015
X <- replicate(10000, sum(sample(c(-150000, 1150), 1000, replace = TRUE, prob = c(p_loss, 1-p_loss))) / 10^6)
mean(X <= -1)

# Question 5-6
p <- 0.015
l <- -150000
n <- 1000

f_e <- function(x){
  avg <- n*(p*l + (1-p)*x)
  se <- sqrt(n) * abs(l-x) * sqrt(p*(1-p))
  pnorm(0, avg, se) - 0.05
}
x <- 2000:5000
plot(x, f_e(x), type = "l")
abline(h = 0, col = "blue")

x <- uniroot.all(f_e, c(2000, 5000))
# points(x = roots, y = rep(0, length(roots)), col = "red", pch = 16)

x * (1-p) + l * p

# Question 5-6
set.seed(29, sample.kind = "Round")
B <- 10000
p <- 0.015
l <- -150000
rand_p <- function(p){
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
}

X <- replicate(B, sum(sample(c(l, x), n, prob = c(rand_p(p), 1-rand_p(p)), replace = TRUE)))

avg <- mean(X)
sd <- sd(X)
pnorm(-1000000, avg, sd)
