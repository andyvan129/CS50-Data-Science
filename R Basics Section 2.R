library(dslabs)
library(dplyr)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

head(heights)
class(heights$height)
ind <- heights |> filter(heights$height > mean(heights$height) & sex == "Female")
nrow(ind)

female <- heights |> filter(sex == "Female")
nrow(female) / nrow(heights)

min(heights$height)
short <- match(min(heights$height), heights$height)

heights$sex[short]

x <- min(heights$height):max(heights$height)
x
ind <- !x %in% heights$height
sum(ind)

heights2 <- heights |> mutate(ht_cm = height * 2.54)
heights2[18,]
mean(heights2$ht_cm)

filter(heights2, sex == "Female") |> pull(ht_cm) |> mean()

data(murders)
match(c("Massachusetts"), murders$state)
which(murders$state == "Massachusetts")
c("Massachusetts") %in% murders$state
which(murders$state = "Massachusetts")

murders %>% filter(region == "Northeast")
select(murders, region == "Northeast")
