library(dslabs)
data(heights)

sum(ifelse(heights$sex == "Female", 1, 2))
ifelse(heights$height > 72, heights$height, 0) |> mean()

inches_to_ft <- function(x) {
  x / 12
}

inches_to_ft(144)

sum(inches_to_ft(heights$height) < 5)
