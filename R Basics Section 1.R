library(dslabs)
data(movielens)

length(movielens$movieId)
nlevels(movielens$genres)
?mean

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time <- time/60

df <- data.frame(name = name, dist = distance, time = time)

df$time[df$name == "Olivia"]
df$speed <- df$dist / df$time
df

x <- c(4, "seven", 9)

data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data = olive)
