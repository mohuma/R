options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

head (brca)

# Q1
nrow(brca$x)
ncol(brca$x)
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

# Q2
x <- brca$x
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

sd(x_standardized[,1])
median(x_standardized[,1])

# Q3
d <- dist(x_standardized)
d_BtoB <- as.matrix(d)[1, brca$y == "B"]
mean(d_BtoB)

d_BtoM <- as.matrix(d)[1, brca$y == "M"]
mean(d_BtoM)

# Q4
d_features <- dist(t(x_standardized))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Q5
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

# Q6
pca <- prcomp(x_standardized)
summary(pca)

# Q7
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           tumor = brca$y) %>%
  ggplot(aes(pc_1, pc_2, color = tumor)) +
  geom_point()

# Q8
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

set.seed(1, sample.kind = "Rounding")    
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_standardized[test_index,]
test_y <- brca$y[test_index]
train_x <- x_standardized[-test_index,]
train_y <- brca$y[-test_index]

# Q9
mean(train_y == "B")
mean(test_y == "B")

# Q10
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding")

k <- kmeans(train_x, centers = 2)

kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")

# Q11
set.seed(1, sample.kind = "Rounding")
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
confusionMatrix(glm_preds, test_y)$overall[["Accuracy"]]

# Q12
set.seed(1, sample.kind = "Rounding")
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
confusionMatrix(lda_preds, test_y)$overall[["Accuracy"]]

set.seed(1, sample.kind = "Rounding")
train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
confusionMatrix(qda_preds, test_y)$overall[["Accuracy"]]

# Q13
set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
confusionMatrix(loess_preds, test_y)$overall[["Accuracy"]]

# Q14
set.seed(7, sample.kind = "Rounding")
train_knn <- train(train_x, train_y, method = "knn",
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn$bestTune

knn_preds <- predict(train_knn, test_x, k = 21)
confusionMatrix(knn_preds, test_y)$overall[["Accuracy"]]

# Q15
set.seed(9, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method = "rf", importance = TRUE,
                   tuneGrid = data.frame(mtry = c(3, 5, 7, 9)))
train_rf$bestTune

rf_preds <- predict(train_rf, test_x, mtry = 3)
confusionMatrix(rf_preds, test_y)$overall[["Accuracy"]]

varImp(train_rf)

# Q16
preds <- data.frame(kmeans = kmeans_preds, logistic = glm_preds, lda = lda_preds, qda = qda_preds, loess = loess_preds, knn = knn_preds, rf = rf_preds)
ensemble_preds <- ifelse(rowCounts(as.matrix(preds), value = "M") > 3, "M", "B") %>%
  factor(levels = levels(test_y))
mean(ensemble_preds == test_y)

ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

# Q17
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)


