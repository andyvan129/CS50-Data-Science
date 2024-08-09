library(caret)
library(dslabs)
data("heights")

test_index <- createDataPartition(heights$sex, list = FALSE)
test <- heights[test_index,]
train <- heights[-test_index,]
k <- seq(1, 101, 3)

knn_fit <- function(n) {
  fit <- knn3(sex ~ height, data = train, k = n)
  predict <- predict(fit, test, type = "class")
  conf <- confusionMatrix(predict, test$sex)
  conf$byClass["F1"]
}

sapply(k, knn_fit) %>%
  data.frame() %>%
  plot()
F_meas(F1)
