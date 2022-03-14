library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
head(titanic_clean)

# Q1
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index) 
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

# Q2
set.seed(3, sample.kind = "Rounding")
y_hat <- sample(c(0,1), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# Q3
train_set %>%
  filter(Sex == "female") %>%
  summarize(mean = mean(Survived == 1))
train_set %>%
  filter(Sex == "male") %>%
  summarize(mean = mean(Survived == 1))

test_set %>%
  filter(Sex == "female") %>%
  summarize(mean = mean(Survived == 1))
test_set %>%
  filter(Sex == "male") %>%
  summarize(mean = mean(Survived == 1))

y_hat_sex <- ifelse(test_set$Sex == "male", 0, 1) %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat_sex == test_set$Survived)

# Q4
train_set %>%
  group_by(Pclass) %>%
  summarize(mean = mean(Survived == 1))

test_set %>%
  group_by(Pclass) %>%
  summarize(mean = mean(Survived == 1))

y_hat_pclass <- ifelse(test_set$Pclass == 1, 1, 0) %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat_pclass == test_set$Survived)

train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(mean = mean(Survived == 1))

y_hat_sex_pclass <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2), 1, 0) %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat_sex_pclass == test_set$Survived)

# Q5
confusionMatrix(data = factor(y_hat_sex), reference = factor(test_set$Survived))
confusionMatrix(data = factor(y_hat_pclass), reference = factor(test_set$Survived))
confusionMatrix(data = factor(y_hat_sex_pclass), reference = factor(test_set$Survived))

# Q6
F_meas(data = factor(y_hat_sex), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_pclass), reference = factor(test_set$Survived))
F_meas(data = factor(y_hat_sex_pclass), reference = factor(test_set$Survived))

# Q7
set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
y_hat_lda <- predict(train_lda, test_set)
confusionMatrix(data = y_hat_lda, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set)
confusionMatrix(data = y_hat_qda, reference = test_set$Survived)$overall["Accuracy"]

# Q8
set.seed(1, sample.kind = "Rounding") 
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

set.seed(1, sample.kind = "Rounding") 
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding") 
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)

# Q9
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~ ., method = "knn", 
                   data = train_set, 
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

ggplot(train_knn, highlight = TRUE)

y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]

# Q10
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .1)
train_knn_cv <- train(Survived ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)

train_knn_cv$bestTune

y_hat_knn_cv <- predict(train_knn_cv, test_set, type = "raw")
confusionMatrix(y_hat_knn_cv, test_set$Survived)$overall[["Accuracy"]]

# Q11
set.seed(10, sample.kind = "Rounding")
train_tree <- train(Survived ~ ., method = "rpart", 
                    data = train_set, 
                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

train_tree$bestTune

y_hat_tree <- predict(train_tree, test_set, type = "raw")
confusionMatrix(y_hat_tree, test_set$Survived)$overall[["Accuracy"]]

plot(train_tree$finalModel, margin = 0.1)
text(train_tree$finalModel, cex = 0.75)

# Q12
set.seed(14, sample.kind="Rounding")
train_rf <- train(Survived ~ ., method = "rf", 
                  data = train_set, 
                  ntree = 100, 
                  tuneGrid = data.frame(mtry = seq(1:7)))

train_rf$bestTune

y_hat_rf <- predict(train_rf, test_set, type = "raw")
confusionMatrix(y_hat_rf, test_set$Survived)$overall[["Accuracy"]]

varImp(train_rf)
