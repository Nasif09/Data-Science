library(caret)
library(ggplot2)   
library(lattice)
library(readr)
library(class)
library(Metrics)

data<- read.csv("E:/mushrooms.csv",header = TRUE,sep=',')
str(data)

missing_values <- c("", "NA", "N/A", "NULL", "?")
missing_counts <- sapply(data, function(col) sum(col %in% missing_values))
print(missing_counts)

# Convert categorical attributes to factors
categorical_columns <- colnames(data)
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Filter out columns with only one level
filtered_data <- data[, sapply(data, function(x) length(unique(x)) > 1)]

# Convert factors to numeric using one-hot encoding
encoded_data <- as.data.frame(model.matrix(~.-1, data = filtered_data))

# Perform z-score normalization (standardization)
zscore_normalized_data <- scale(encoded_data)

# Print the first few rows of the z-score normalized data
head(zscore_normalized_data)
# correlation for all variables
round(cor(encoded_data), digits = 2)

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(encoded_data), 0.7 * nrow(encoded_data))
training_data <- encoded_data[train_indices, ]
test_data <- encoded_data[-train_indices, ]

# Define target column index
target_column_index <- 1

# Calculate Pearson's correlation coefficient and select important attributes
correlation_matrix <- cor(training_data[,-target_column_index], training_data[, target_column_index])
important_attributes <- colnames(training_data)[abs(correlation_matrix) > 0.1]
correlation_matrix <- cor(training_data[,-target_column_index], training_data[, target_column_index])
print(correlation_matrix)

# Ensure column names are correctly matching
matching_columns <- intersect(important_attributes, colnames(training_data))

# Include the target column index in the list of matching columns
columns_to_select <- c(matching_columns, colnames(training_data)[target_column_index])

# Create training and test datasets with important attributes
training_dataset <- training_data[, columns_to_select]
test_dataset <- test_data[, columns_to_select]

# Train KNN model
k <- 5
knn_model <- knn(train = training_dataset[, -target_column_index], 
                 test = test_dataset[, -target_column_index], 
                 cl = training_dataset[, target_column_index], 
                 k = k)
knn_model1<-knn_model
test_datasetcm<-test_dataset[, target_column_index]

# Calculate accuracy (training and test set)
accuracy_test <- sum(knn_model == test_dataset[, target_column_index]) / nrow(test_dataset)
cat("Accuracy (Test Set):", accuracy_test, "\n")

# Convert the target column to a factor (if not already)
training_dataset[, target_column_index] <- factor(training_dataset[, target_column_index])


# Specify the number of folds for cross-validation
num_folds <- 10

cv_folds <- createFolds(training_dataset[, target_column_index], k = num_folds)

# Initialize an empty vector to store accuracy results
cv_accuracy <- numeric(num_folds)

# Initialize a variable to store the best k value
best_k <- 5  # This should be the optimal k value determined earlier

# Train k-NN models using cross-validation
for (fold in 1:num_folds) {
  train_indices <- cv_folds[[fold]]
  train_data <- training_dataset[train_indices, ]
  test_data <- training_dataset[-train_indices, ]
  
  knn_model <- knn(
    train = train_data[, -target_column_index],
    test = test_data[, -target_column_index],
    cl = train_data[, target_column_index],
    k = best_k
  )
  
  accuracy <- sum(knn_model == test_data[, target_column_index]) / nrow(test_data)
  cv_accuracy[fold] <- accuracy
}

# Print accuracy results for each fold
cat("Accuracy (", num_folds, "-fold CV):\n")
print(cv_accuracy)

# Average accuracy across folds
average_accuracy <- mean(cv_accuracy)
cat("Average Accuracy:", average_accuracy, "\n")
##

# Confusion matrix
xtab <- table(knn_model1, test_datasetcm)
cm <- caret::confusionMatrix(xtab)
print(cm)

Metrics::recall(knn_model1, test_datasetcm)

# Convert factors to numeric or logical

predicted_labels <- as.numeric(as.character(knn_model1))
true_labels <- as.numeric(as.character(test_datasetcm))
# Calculate precision

precision_value <- Metrics::precision(predicted_labels, true_labels)
print(precision_value)
