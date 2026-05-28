install.packages(c("tidyverse", "caret", "randomForest", "gbm", "nnet", "pROC"))
library(tidyverse)
library(caret)
library(randomForest)
library(gbm)
library(nnet)
library(pROC)

column_names <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week", "native_country",
  "income"
)

adult_train <- read.table("adult.data",
                          header = FALSE,
                          sep = ",",
                          strip.white = TRUE,
                          na.strings = "?",
                          col.names = column_names)

adult_test <- read.table("adult.test",
                         header = FALSE,
                         sep = ",",
                         strip.white = TRUE,
                         na.strings = "?",
                         col.names = column_names,
                         skip = 1)

adult_test$income <- gsub("\\.", "", adult_test$income)
head(adult_train)
head(adult_test)
str(adult_train)

# Combine the two datasets into one
adult_all <- rbind(adult_train, adult_test)
adult_all$income <- factor(adult_all$income,
                           levels = c("<=50K", ">50K"))
adult_all <- adult_all %>%
  mutate(across(where(is.character), as.factor))
adult_all <- na.omit(adult_all)
sum(is.na(adult_all))   # should be 0
nrow(adult_all)         # number of remaining rows
set.seed(123)  # for reproducibility

train_index <- createDataPartition(adult_all$income,
                                   p = 0.8,
                                   list = FALSE)

train_data <- adult_all[train_index, ]
test_data  <- adult_all[-train_index, ]
dim(train_data)
dim(test_data)

prop.table(table(train_data$income))
prop.table(table(test_data$income))
x_train <- train_data %>% select(-income)
y_train <- train_data$income

x_test  <- test_data %>% select(-income)
y_test  <- test_data$income

# Logistic Regression mode
model_glm <- glm(income ~ ., 
                 data = train_data, 
                 family = binomial)

summary(model_glm)  
# Predictions on test data 
# Get predicted probabilities of >50K
pred_glm_prob <- predict(model_glm, 
                         newdata = test_data, 
                         type = "response")

# Convert probabilities to class labels using 0.5 threshold
pred_glm <- ifelse(pred_glm_prob > 0.5, ">50K", "<=50K")
pred_glm <- factor(pred_glm, levels = levels(y_test))

# Confusion matrix + basic metrics 
cm_glm <- confusionMatrix(pred_glm, y_test, positive = ">50K")
cm_glm

# ROC and AUC
roc_glm <- roc(y_test, pred_glm_prob, levels = c("<=50K", ">50K"))
auc(roc_glm)

# Random Forest
set.seed(123)

model_rf <- randomForest(
  income ~ ., 
  data = train_data,
  ntree = 300,        # number of trees
  mtry = 5,           # number of variables sampled at each split
  importance = TRUE
)
model_rf
pred_rf <- predict(model_rf, newdata = test_data, type = "class")
cm_rf <- confusionMatrix(pred_rf, y_test, positive = ">50K")
cm_rf
importance(model_rf)
varImpPlot(model_rf)

#GBM Model
str(train_data$income_num)
table(train_data$income_num)



train_data$income_num <- as.numeric(train_data$income == ">50K")
test_data$income_num  <- as.numeric(test_data$income == ">50K")
str(train_data$income_num)
table(train_data$income, train_data$income_num)

set.seed(123)

model_gbm <- gbm(
  formula = income_num ~ . - income,
  distribution = "bernoulli",
  data = train_data,
  n.trees = 300,
  interaction.depth = 3,
  shrinkage = 0.05,
  n.minobsinnode = 20,
  verbose = FALSE
)
str(train_data$income_num)

train_data$income_num <- as.numeric(train_data$income == ">50K")
test_data$income_num  <- as.numeric(test_data$income == ">50K")
library(gbm)

set.seed(123)

model_gbm <- gbm(
  income_num ~ age + workclass + fnlwgt + education + education_num +
    marital_status + occupation + relationship + race + sex +
    capital_gain + capital_loss + hours_per_week + native_country,
  distribution = "bernoulli",
  data = train_data,
  n.trees = 300,
  interaction.depth = 3,
  shrinkage = 0.05,
  n.minobsinnode = 20,
  verbose = FALSE
)
# Probabilities for class 1 (= >50K)
pred_gbm_prob <- predict(
  model_gbm,
  newdata = test_data,
  n.trees = 300,
  type = "response"
)

# Convert to factor labels
pred_gbm <- ifelse(pred_gbm_prob > 0.5, ">50K", "<=50K")
pred_gbm <- factor(pred_gbm, levels = levels(test_data$income))

# Confusion matrix
cm_gbm <- confusionMatrix(pred_gbm, test_data$income, positive = ">50K")
cm_gbm

# ROC & AUC
roc_gbm <- roc(test_data$income, pred_gbm_prob)
auc(roc_gbm)

#MLP

# Rename class levels to valid R names
train_data$income <- factor(train_data$income,
                            levels = c("<=50K", ">50K"),
                            labels = c("le50K", "gt50K"))

test_data$income <- factor(test_data$income,
                           levels = c("<=50K", ">50K"),
                           labels = c("le50K", "gt50K"))
levels(train_data$income)
ctrl_mlp <- trainControl(
  method = "none",
  classProbs = TRUE
)

mlp_grid <- expand.grid(
  size  = 10,
  decay = 0.001
)

set.seed(123)
model_mlp <- train(
  income ~ .,
  data = train_data,
  method = "nnet",
  trControl = ctrl_mlp,
  tuneGrid = mlp_grid,
  preProcess = c("center", "scale"),
  trace = FALSE,
  maxit = 200
)

library(caret)
library(nnet)

# control
ctrl_mlp <- trainControl(
  method = "none",
  classProbs = TRUE
)

# smaller hidden layer
mlp_grid <- expand.grid(
  size  = 5,        # fewer hidden units
  decay = 0.001
)

set.seed(123)
model_mlp <- train(
  income ~ .,
  data = train_data,
  method = "nnet",
  trControl = ctrl_mlp,
  tuneGrid = mlp_grid,
  preProcess = c("center", "scale"),
  trace = FALSE,
  maxit = 200,
  MaxNWts = 5000     # allow up to 5000 weights
)
# Class predictions
pred_mlp <- predict(model_mlp, newdata = test_data)

cm_mlp <- confusionMatrix(pred_mlp, test_data$income, positive = "gt50K")
cm_mlp

# Probabilities for ROC–AUC
pred_mlp_prob_all <- predict(model_mlp, newdata = test_data, type = "prob")
pred_mlp_prob <- pred_mlp_prob_all[, "gt50K"]

roc_mlp <- roc(test_data$income, pred_mlp_prob)
auc(roc_mlp)
# Make sure y_test matches current test_data$income
y_test <- test_data$income
# Logistic Regression
pred_glm_prob <- predict(model_glm, newdata = test_data, type = "response")
pred_glm <- ifelse(pred_glm_prob > 0.5, "gt50K", "le50K")
pred_glm <- factor(pred_glm, levels = levels(y_test))

cm_glm <- confusionMatrix(pred_glm, y_test, positive = "gt50K")
roc_glm <- roc(y_test, pred_glm_prob)

# Random Forest
pred_rf_prob <- predict(model_rf, newdata = test_data, type = "prob")[, "gt50K"]
pred_rf <- ifelse(pred_rf_prob > 0.5, "gt50K", "le50K")
pred_rf <- factor(pred_rf, levels = levels(y_test))

cm_rf <- confusionMatrix(pred_rf, y_test, positive = "gt50K")
roc_rf <- roc(y_test, pred_rf_prob)

# GBM
# pred_gbm_prob <- predict(model_gbm, newdata = test_data, n.trees = 300, type = "response")
pred_gbm <- ifelse(pred_gbm_prob > 0.5, "gt50K", "le50K")
pred_gbm <- factor(pred_gbm, levels = levels(y_test))

cm_gbm <- confusionMatrix(pred_gbm, y_test, positive = "gt50K")
roc_gbm <- roc(y_test, pred_gbm_prob)

set.seed(123)
model_rf <- randomForest(
  income ~ .,
  data = train_data,
  ntree = 300,
  mtry = 5,
  importance = TRUE
)
pred_rf_prob <- predict(model_rf, newdata = test_data, type = "prob")[, "gt50K"]
pred_rf <- ifelse(pred_rf_prob > 0.5, "gt50K", "le50K")
pred_rf <- factor(pred_rf, levels = levels(y_test))
cm_rf <- confusionMatrix(pred_rf, y_test, positive = "gt50K")
cm_rf

roc_rf <- roc(y_test, pred_rf_prob)
auc(roc_rf)

train_data_rf <- train_data %>% select(-income_num)
test_data_rf  <- test_data %>% select(-income_num)
set.seed(123)
model_rf <- randomForest(
  income ~ .,
  data = train_data_rf,
  ntree = 300,
  mtry = 5,
  importance = TRUE
)
pred_rf_prob <- predict(model_rf, newdata = test_data_rf, type = "prob")[, "gt50K"]

pred_rf <- ifelse(pred_rf_prob > 0.5, "gt50K", "le50K")
pred_rf <- factor(pred_rf, levels = levels(y_test))

cm_rf <- confusionMatrix(pred_rf, y_test, positive = "gt50K")
cm_rf

roc_rf <- roc(y_test, pred_rf_prob)
auc(roc_rf)

#MLP results are off 
train_data_mlp <- train_data %>% select(-income_num)
test_data_mlp  <- test_data %>% select(-income_num)
names(train_data_mlp)
ctrl_mlp <- trainControl(
  method = "none",
  classProbs = TRUE
)

mlp_grid <- expand.grid(
  size = 5,
  decay = 0.001
)

set.seed(123)
model_mlp <- train(
  income ~ .,
  data = train_data_mlp,
  method = "nnet",
  trControl = ctrl_mlp,
  tuneGrid = mlp_grid,
  preProcess = c("center", "scale"),
  trace = FALSE,
  maxit = 200,
  MaxNWts = 5000
)
pred_mlp <- predict(model_mlp, newdata = test_data_mlp)

cm_mlp <- confusionMatrix(pred_mlp, test_data_mlp$income, positive = "gt50K")
cm_mlp
pred_mlp_prob_all <- predict(model_mlp, newdata = test_data_mlp, type = "prob")
pred_mlp_prob <- pred_mlp_prob_all[, "gt50K"]

roc_mlp <- roc(test_data_mlp$income, pred_mlp_prob)
auc(roc_mlp)

#Results Table
results <- tibble::tibble(
  Model    = c("Logistic Regression", "Random Forest", "GBM", "MLP"),
  Accuracy = c(
    cm_glm$overall["Accuracy"],
    cm_rf$overall["Accuracy"],
    cm_gbm$overall["Accuracy"],
    cm_mlp$overall["Accuracy"]
  ),
  AUC = c(
    pROC::auc(roc_glm),
    pROC::auc(roc_rf),
    pROC::auc(roc_gbm),
    pROC::auc(roc_mlp)
  )
)

results

