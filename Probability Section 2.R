set.seed(16, sample.kind = "Rounding")

act_scores <- rnorm(10000, 20.9, 5.7)
mean <- mean(act_scores)
sd <- sd(act_scores)

x <- seq(1, 36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

Z <- (act_scores - mean) / sd
score <- function(x){
  x * sd + mean
}
score(2)

dnorm(32, mean, sd)

act_cdf <- function(a){
  mean(act_scores <= a)
}

prob <- data.frame(x, p <- sapply(x, function(e) act_cdf(e)))
qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(theoretical_quantiles, sample_quantiles)
