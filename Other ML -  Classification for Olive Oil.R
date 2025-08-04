# Install and load necessary packages
install.packages(c("caTools", "e1071", "MASS", "randomForest", "class", "rpart", "ggplot2", "caret", "pls"))
install.packages("magrittr")
library(caTools)
library(e1071)
library(MASS)
library(randomForest)
library(class)
library(rpart)
library(ggplot2)
library(caret)
library(pls)

# Load data
load("oliveoils.RData")

# Convert to data frame
oliveoils_df <- as.data.frame(oliveoils)
oliveoillabels_df <- as.factor(oliveoillabels)

# Select spectras from 788 to 988
selected_spectras <- oliveoils_df[, 195:295]

# Combine labels and selected spectras
data <- cbind(label = oliveoillabels_df, selected_spectras)

# Split data into training, validation, and test sets
set.seed(123)
split <- sample.split(data$label, SplitRatio = 0.5)
trainData <- subset(data, split == TRUE)
tempData <- subset(data, split == FALSE)

split_temp <- sample.split(tempData$label, SplitRatio = 0.5)
validationData <- subset(tempData, split_temp == TRUE)
testData <- subset(tempData, split_temp == FALSE)

# Exploratory data analysis on training data
summary(trainData)

# Feature Scaling (Standardization)
scaler <- preProcess(trainData[, -1], method = c("center", "scale"))
train_scaled <- predict(scaler, trainData[, -1])
validation_scaled <- predict(scaler, validationData[, -1])
test_scaled <- predict(scaler, testData[, -1])

# Combine scaled features with labels
train_scaled <- data.frame(label = trainData$label, train_scaled)
validation_scaled <- data.frame(label = validationData$label, validation_scaled)
test_scaled <- data.frame(label = testData$label, test_scaled)

# k-Nearest Neighbors
set.seed(123)
k_values <- 1:20
accuracy <- sapply(k_values, function(k) {
  knn_pred <- knn(train = train_scaled[, -1], test = validation_scaled[, -1], cl = train_scaled$label, k = k)
  mean(knn_pred == validationData$label)
})

best_k <- k_values[which.max(accuracy)]
knn_predictions <- knn(train = train_scaled[, -1], test = validation_scaled[, -1], cl = train_scaled$label, k = best_k)

# Making the Confusion Matrix for Validation Set
cm_knn <- table(validationData$label, knn_predictions)
print(cm_knn)

# Apply k-NN on the test set
knn_test_predictions <- knn(train = train_scaled[, -1], test = test_scaled[, -1], cl = train_scaled$label, k = best_k)
cm_knn_test <- table(test_scaled$label, knn_test_predictions)
print(cm_knn_test)

# Decision Tree
tree_model <- rpart(label ~ ., data = train_scaled)
tree_predictions <- predict(tree_model, validation_scaled, type = "class")

# Making the Confusion Matrix for Validation Set
cm_tree <- table(validation_scaled$label, tree_predictions)
print(cm_tree)

# Apply Decision Tree on the test set
tree_test_predictions <- predict(tree_model, test_scaled, type = "class")
cm_tree_test <- table(test_scaled$label, tree_test_predictions)
print(cm_tree_test)

# Random Forest
rf_model <- randomForest(label ~ ., data = train_scaled, ntree = 10)
rf_predictions <- predict(rf_model, validation_scaled)

# Making the Confusion Matrix for Validation Set
cm_rf <- table(validation_scaled$label, rf_predictions)
print(cm_rf)

# Apply Random Forest on the test set
rf_test_predictions <- predict(rf_model, test_scaled)
cm_rf_test <- table(test_scaled$label, rf_test_predictions)
print(cm_rf_test)

# Kernel SVM
svm_model <- svm(label ~ ., data = train_scaled, type = "C-classification", kernel = "radial")
svm_predictions <- predict(svm_model, validation_scaled)

# Making the Confusion Matrix for Validation Set
cm_svm <- table(validation_scaled$label, svm_predictions)
print(cm_svm)

# Apply SVM on the test set
svm_test_predictions <- predict(svm_model, test_scaled)
cm_svm_test <- table(test_scaled$label, svm_test_predictions)
print(cm_svm_test)

# PLS-DA
plsda_model <- plsda(train_scaled[, -1], train_scaled$label, ncomp = 10, scale = TRUE)
plsda_predictions <- predict(plsda_model, validation_scaled[, -1])

# Making the Confusion Matrix for Validation Set
cm_plsda <- table(validation_scaled$label, plsda_predictions)
print(cm_plsda)

# Apply PLS-DA on the test set
plsda_test_predictions <- predict(plsda_model, newdata = test_scaled[, -1], ncomp = 10)
cm_plsda_test <- table(test_scaled$label, plsda_test_predictions)
print(cm_plsda_test)

# Summarize results
results_list_validation <- list(
  knn = cm_knn, 
  tree = cm_tree, 
  rf = cm_rf, 
  svm = cm_svm,
  plsda = cm_plsda
)

results_list_test <- list(
  knn = cm_knn_test, 
  tree = cm_tree_test, 
  rf = cm_rf_test, 
  svm = cm_svm_test,
  plsda = cm_plsda_test
)

# Determine the best model based on validation set accuracy
best_model <- NULL
best_accuracy <- 0
for (method in names(results_list_validation)) {
  accuracy <- sum(diag(results_list_validation[[method]])) / sum(results_list_validation[[method]])
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- method
  }
}
print(paste("Best model based on validation set:", best_model))
print(paste("Best model accuracy on validation set:", best_accuracy))

best_test_accuracy <- sum(diag(results_list_test[[best_model]])) / sum(results_list_test[[best_model]])
print(paste("Best model accuracy on test set:", best_test_accuracy))

# Plot the confusion matrix for the best model on the test set
plot_confusion_matrix <- function(cm, title) {
  cm_df <- as.data.frame(as.table(cm))
  colnames(cm_df) <- c("True", "Predicted", "Freq")
  ggplot(cm_df, aes(True, Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = title, x = "True Label", y = "Predicted Label") +
    theme_minimal()
}

plot_confusion_matrix(results_list_test[[best_model]], paste(best_model, "Confusion Matrix (Test Set)"))