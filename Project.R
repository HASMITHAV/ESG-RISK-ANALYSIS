

setwd("/Users/hasmitha/Downloads")

install.packages("psych")
install.packages("Amelia")
install.packages("VIM")
install.packages("caret")
install.packages("ROSE")
install.packages("nnet")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("DMwR2")
install.packages("smotefamily")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("neuralnet")
install.packages("MLmetrics")
install.packages("randomForest")
install.packages(c("e1071", "pROC", "Metrics", "ROCR", "ggplot2"))
install.packages("GGally")
install.packages("reshape2")
install.packages("RColorBrewer")


# Load necessary libraries
library(tidyverse)
library(cluster)
library(caret)
library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)
library(Amelia)
library(VIM)
library(nnet)
library(rpart)
library(rpart.plot)
library(DMwR2)
library(ROSE)
library(smotefamily)
library(neuralnet)
library(MLmetrics)
library(randomForest)
library(pROC)
library(e1071)
library(Metrics)
library(ROCR)
library(class)
library(GGally)
library(reshape2)
library(RColorBrewer)
library(melt)



# Load the dataset
esg_data <- read.csv("esg.csv")

# Quick overview of the data
head(esg_data)
head(esg_data[, c(1:6, 8:15)])
summary(esg_data)
str(esg_data)
View(esg_data)
colnames(esg_data)


###PARTI-1:

##Descriptive Statistics:
# Summary statistics
summary(esg_data)

# Check for missing values
sapply(esg_data, function(x) sum(is.na(x)))

##Distribution of Key Variables:
# Histogram for Total ESG Risk Score
ggplot(esg_data, aes(x = Total.ESG.Risk.score)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Total ESG Risk Score", x = "ESG Risk Score", y = "Count")

## Correlation Analysis:
# Select numeric columns for correlation analysis
numeric_cols <- esg_data %>% select_if(is.numeric)

# Correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")

#Visualizing Relationships (Scatter Plot):
# Scatter plot of Environmental Risk vs Governance Risk
ggplot(esg_data, aes(x = Environment.Risk.Score, y = Governance.Risk.Score)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Environmental vs Governance Risk", x = "Environmental Risk Score", y = "Governance Risk Score")

#Outlier Detection:
# Boxplot to detect outliers in Total ESG Risk Score
ggplot(esg_data, aes(y = Total.ESG.Risk.score)) +
  geom_boxplot(fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Total ESG Risk Score", y = "Total ESG Risk Score")

#Missing Data Visualization:
# Visualize missing data
aggr(esg_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(esg_data), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

# Generate Additional Interaction Terms
esg_data <- esg_data %>%
  mutate(
    Env_Gov_Interaction = Environment.Risk.Score * Governance.Risk.Score,
    Env_Social_Gov_Interaction = Environment.Risk.Score * Social.Risk.Score * Governance.Risk.Score,
    Env_Log_Social = Environment.Risk.Score * log1p(Social.Risk.Score),
    Social_Gov_Ratio = Social.Risk.Score / (Governance.Risk.Score + 1e-5),
    Env_Social_Squared = Environment.Risk.Score^2 + Social.Risk.Score^2
  )

# Validate new columns
head(esg_data %>% select(Env_Gov_Interaction, Env_Social_Gov_Interaction, Env_Log_Social, Social_Gov_Ratio, Env_Social_Squared))

# Add the log-transformed column to your dataset
esg_data$log_ESG_Risk <- log1p(esg_data$Total.ESG.Risk.score)  # log1p ensures no errors with zero or negative values


# Create the pair plot
ggpairs(
  esg_data %>% select(Environment.Risk.Score, Social.Risk.Score, Governance.Risk.Score, Total.ESG.Risk.score, log_ESG_Risk),
  title = "Pair Plot of Selected ESG Features"
)


# Compute correlation matrix for numeric columns
numeric_data <- select_if(esg_data, is.numeric)  # Select numeric columns
cor_matrix <- cor(numeric_data, use = "complete.obs")  # Correlation matrix

# Melt the correlation matrix for heatmap input
cor_melt <- melt(cor_matrix)

# Plot the heatmap
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###PART-2

##Handling Missing Values:
# Check missing values
colSums(is.na(esg_data))

# Calculate percentage of missing values
missing_percent <- colSums(is.na(esg_data)) / nrow(esg_data) * 100
missing_percent

# Impute missing values for Governance and Social Risk Scores using median
esg_data$Governance.Risk.Score[is.na(esg_data$Governance.Risk.Score)] <- median(esg_data$Governance.Risk.Score, na.rm = TRUE)
esg_data$Social.Risk.Score[is.na(esg_data$Social.Risk.Score)] <- median(esg_data$Social.Risk.Score, na.rm = TRUE)
esg_data$Total.ESG.Risk.score[is.na(esg_data$Total.ESG.Risk.score)] <- median(esg_data$Total.ESG.Risk.score, na.rm = TRUE)
esg_data$Environment.Risk.Score[is.na(esg_data$Environment.Risk.Score)] <- median(esg_data$Environment.Risk.Score, na.rm = TRUE)


# Ensure no missing values remain
colSums(is.na(esg_data))

# Visualize the updated distributions
# Visualize distributions for imputed Governance Risk Score
hist(esg_data$Governance.Risk.Score, 
     main = "Imputed Governance Risk Score", 
     col = "blue", 
     xlab = "Governance Risk Score", 
     border = "black")

# Visualize distributions for imputed Social Risk Score
hist(esg_data$Social.Risk.Score, 
     main = "Imputed Social Risk Score", 
     col = "green", 
     xlab = "Social Risk Score", 
     border = "black")

# Visualize distributions for Total ESG Risk Score
hist(esg_data$Total.ESG.Risk.score, 
     main = "Total ESG Risk Score", 
     col = "orange", 
     xlab = "Total ESG Risk Score", 
     border = "black")

# Visualize distributions for Environmental Risk Score
hist(esg_data$Environment.Risk.Score, 
     main = "Environmental Risk Score", 
     col = "purple", 
     xlab = "Environmental Risk Score", 
     border = "black")


##Outlier Detection and Handling:
# Identify outliers using IQR for Total ESG Risk Score
iqr <- IQR(esg_data$Total.ESG.Risk.score, na.rm = TRUE)
upper_bound <- quantile(esg_data$Total.ESG.Risk.score, 0.75, na.rm = TRUE) + 1.5 * iqr
lower_bound <- quantile(esg_data$Total.ESG.Risk.score, 0.25, na.rm = TRUE) - 1.5 * iqr

# Cap outliers to upper and lower bounds
esg_data$Total.ESG.Risk.score <- pmin(pmax(esg_data$Total.ESG.Risk.score, lower_bound), upper_bound)


##Feature Transformation:
# Log transformation for skewed data (example on ESG Risk Score)
esg_data$log_ESG_Risk <- log1p(esg_data$Total.ESG.Risk.score)

##Feature Binning:
# Bin Total ESG Risk Score into categories
esg_data$Risk_Level <- cut(esg_data$Total.ESG.Risk.score,
                           breaks = c(-Inf, 20, 30, Inf),
                           labels = c("Low", "Medium", "High"))

##Creating Interaction Terms:
# Interaction between Environmental and Social Risk Scores
esg_data$Env_Social_Interaction <- esg_data$Environment.Risk.Score * esg_data$Social.Risk.Score

##Feature Selection:
# Correlation matrix to assess multicollinearity
cor_matrix <- cor(esg_data %>% select_if(is.numeric), use = "complete.obs")
# Visualize correlation to identify highly correlated features
corrplot::corrplot(cor_matrix, method = "circle")


# Select relevant features including the target variable for regression
# Add ESG.Risk.Level back to selected_features if missing
selected_features <- esg_data %>%
  select(Total.ESG.Risk.score, Environment.Risk.Score, Governance.Risk.Score, 
         Social.Risk.Score, Env_Social_Interaction, ESG.Risk.Level)


# Check unique levels
unique(selected_features$ESG.Risk.Level)
# Count occurrences of each level
table(selected_features$ESG.Risk.Level, useNA = "ifany")
# Filter out rows with missing or empty ESG.Risk.Level
selected_features <- selected_features %>%
  filter(ESG.Risk.Level != "" & !is.na(ESG.Risk.Level))

print(table(esg_data$ESG.Risk.Level))

colnames(esg_data)

# Log Transformations
esg_data$log_Env_Risk <- log1p(esg_data$Environment.Risk.Score)
esg_data$log_Social_Risk <- log1p(esg_data$Social.Risk.Score)

# Interaction Terms
esg_data$Env_Social_Interaction <- esg_data$Environment.Risk.Score * esg_data$Social.Risk.Score
esg_data$Env_Gov_Interaction <- esg_data$Environment.Risk.Score * esg_data$Governance.Risk.Score

# Feature Binning
esg_data$binned_ESG_Score <- cut(esg_data$Total.ESG.Risk.score, 
                                 breaks = c(-Inf, 20, 30, Inf), 
                                 labels = c("Low", "Medium", "High"))


# Step 1: Define the minimum target sample size per class
target_sample_size <- 50  # You can adjust this to any desired threshold

# Step 2: Oversample classes that have fewer than the target_sample_size
oversampled_data <- selected_features %>%
  group_by(ESG.Risk.Level) %>%
  filter(n() < target_sample_size) %>%  # Filter classes with less than target size
  slice_sample(n = target_sample_size, replace = TRUE) %>%  # Oversample to target size
  ungroup()

# Step 3: Combine oversampled data with the original dataset
balanced_data <- bind_rows(selected_features, oversampled_data)

# Step 4: Perform a stratified split
set.seed(123)
trainIndex <- createDataPartition(balanced_data$ESG.Risk.Level, p = 0.8, list = FALSE)

# Create training and test datasets
train_data <- balanced_data[trainIndex, ]
test_data <- balanced_data[-trainIndex, ]

# Step 5: Verify class distributions
print(table(train_data$ESG.Risk.Level))
print(table(test_data$ESG.Risk.Level))


# Perform PCA on training data excluding the target variable
pca_train <- prcomp(train_data %>% select(-Total.ESG.Risk.score, -ESG.Risk.Level), scale. = TRUE)

# Transform the training data using PCA
train_data_pca <- as.data.frame(predict(pca_train, train_data %>% select(-Total.ESG.Risk.score, -ESG.Risk.Level)))
train_data_pca$Total.ESG.Risk.score <- train_data$Total.ESG.Risk.score
train_data_pca$ESG.Risk.Level <- train_data$ESG.Risk.Level

# Apply the same PCA transformation to test data
test_data_pca <- as.data.frame(predict(pca_train, test_data %>% select(-Total.ESG.Risk.score, -ESG.Risk.Level)))
test_data_pca$Total.ESG.Risk.score <- test_data$Total.ESG.Risk.score
test_data_pca$ESG.Risk.Level <- test_data$ESG.Risk.Level
# Convert ESG.Risk.Level to a factor
test_data_pca$ESG.Risk.Level <- factor(test_data_pca$ESG.Risk.Level, 
                                       levels = c("High", "Low", "Medium", "Negligible", "Severe"))

#Now we have:\
#train_data and test_data: For models requiring original features.
#train_data_pca and test_data_pca: For models requiring PCA-transformed features.

# Check for any missing or non-numeric values in train_data_pca
sum(is.na(train_data_pca))  # Should be 0
sum(is.na(test_data_pca))   # Should be 0

# Ensure all columns except ESG.Risk.Level are numeric
sapply(train_data_pca, class)
sapply(test_data_pca, class)

# Replace any NA with the column mean (if any)
train_data_pca[is.na(train_data_pca)] <- apply(train_data_pca, 2, mean, na.rm = TRUE)
test_data_pca[is.na(test_data_pca)] <- apply(test_data_pca, 2, mean, na.rm = TRUE)



###PART-3

# Linear Regression on original data
linear_model <- lm(Total.ESG.Risk.score ~ ., data = train_data)
summary(linear_model)

# Predict and evaluate
linear_preds <- predict(linear_model, test_data)
postResample(pred = linear_preds, obs = test_data$Total.ESG.Risk.score)


### Logistic Regression on PCA-transformed data
# Fit multinomial logistic regression
multi_log_model <- multinom(ESG.Risk.Level ~ ., data = train_data_pca)

# Summary of the model
summary(multi_log_model)

# Predict on test data
multi_log_preds <- predict(multi_log_model, test_data_pca)

# Check levels of predicted values
levels(multi_log_preds)
# Check levels of actual values
levels(test_data_pca$ESG.Risk.Level)

# Align levels of predictions to match actual values
multi_log_preds <- factor(multi_log_preds, levels = levels(test_data_pca$ESG.Risk.Level))

# Compute confusion matrix
confusionMatrix(multi_log_preds, test_data_pca$ESG.Risk.Level)





##Decision Tress: Supervised Learning for Classification
tree_model <- rpart(ESG.Risk.Level ~ ., data = train_data_pca, method = "class")

rpart.plot(tree_model, type = 4, extra = 104, fallen.leaves = TRUE,
           box.palette = "RdBu", shadow.col = "gray", main = "Decision Tree for ESG Risk Levels")

# Predict on the test data
tree_preds <- predict(tree_model, test_data_pca, type = "class")

# Confusion matrix to evaluate performance
confusionMatrix(tree_preds, test_data_pca$ESG.Risk.Level)




###Random Forest:For Non-Linear Relationships
### Random Forest Training with Selected Features ###
###Random Forest:For Non-Linear Relationships
# Ensure ESG.Risk.Level is a factor
train_data$ESG.Risk.Level <- as.factor(train_data$ESG.Risk.Level)
test_data$ESG.Risk.Level <- as.factor(test_data$ESG.Risk.Level)

# Fit Random Forest model
rf_model <- randomForest(ESG.Risk.Level ~ ., data = train_data, ntree = 100)

# Print model summary
print(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)

# Predict on test data
rf_preds <- predict(rf_model, test_data)

# Confusion matrix to evaluate the model
confusionMatrix(rf_preds, test_data$ESG.Risk.Level)

# Plotting Feature Importance for Random Forest
varImpPlot(rf_model, main="Feature Importance in Random Forest")


###Neural Networks: Supervised Learning for Complex, Non-Linear Problems
# Fit a neural network model
nn_model <- neuralnet(ESG.Risk.Level ~ PC1 + PC2 + PC3, 
                      data = train_data_pca, hidden = c(5, 3), linear.output = FALSE)

# Visualize the Neural Network
plot(nn_model)

# Predict on the test data
nn_preds <- compute(nn_model, test_data_pca[, c("PC1", "PC2", "PC3")])
nn_class <- apply(nn_preds$net.result, 1, which.max)  # Assign class based on max probability

# Check if nn_class contains only NAs or empty values
print(table(nn_class, useNA = "ifany"))

# Define the class labels in the correct order
class_labels <- c("High", "Low", "Medium", "Negligible", "Severe")

# Convert numeric predictions to factor with correct labels
nn_class <- factor(nn_class, levels = 1:5, labels = class_labels)

# Ensure the actual labels are also factors with the same levels
test_data_pca$ESG.Risk.Level <- factor(test_data_pca$ESG.Risk.Level, levels = class_labels)

# Check the levels
levels(nn_class)  # Should show "High", "Low", "Medium", "Negligible", "Severe"
levels(test_data_pca$ESG.Risk.Level)  # Should show the same


confusionMatrix(nn_class, test_data_pca$ESG.Risk.Level)


# Cross-validation setup
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE)

# Train using cross-validation
nn_cv_model <- train(
  ESG.Risk.Level ~ ., 
  data = train_data_pca, 
  method = "nnet", 
  trControl = train_control, 
  tuneLength = 5
)

# Print cross-validation results
print(nn_cv_model)

nn_tuned_model <- train(
  ESG.Risk.Level ~ ., 
  data = train_data_pca, 
  method = "nnet", 
  trControl = train_control,
  tuneGrid = expand.grid(size = c(5, 10), decay = c(0.1, 0.5)), 
  maxit = 200
)

# Use the best-tuned model for predictions
nn_tuned_preds <- predict(nn_tuned_model, test_data_pca)

# Confusion matrix to evaluate
confusionMatrix(nn_tuned_preds, test_data_pca$ESG.Risk.Level)


###Clustering:Unsupervised Learning
# K-Means Clustering
# Fit K-Means with 3 clusters on PCA-transformed training data
kmeans_model <- kmeans(train_data_pca[, c("PC1", "PC2", "PC3")], centers = 3, nstart = 20)

# Visualize clusters on the first two principal components
plot(
  train_data_pca$PC1, train_data_pca$PC2, 
  col = kmeans_model$cluster, 
  main = "K-Means Clusters (PCA)", 
  xlab = "PC1", ylab = "PC2"
)

# Add cluster centers
points(kmeans_model$centers[, 1:2], col = 1:3, pch = 8, cex = 2)


set.seed(123)  # For reproducibility
# Exclude Total.ESG.Risk.score and ESG.Risk.Level as these are non-numeric or target columns.
knn_preds <- knn(train = train_data_pca %>% select(-Total.ESG.Risk.score, -ESG.Risk.Level),
                 test = test_data_pca %>% select(-Total.ESG.Risk.score, -ESG.Risk.Level),
                 cl = train_data_pca$ESG.Risk.Level,  # Target labels
                 k = 5,                              # Number of neighbors
                 prob = TRUE)                        # Return probabilities


knn_probs <- attr(knn_preds, "prob")
# Ensure probabilities match the correct "High" class
knn_probs_adjusted <- ifelse(knn_preds == "High", knn_probs, 1 - knn_probs)

# Ensure the predicted and actual labels are factors with the same levels
knn_preds <- factor(knn_preds, levels = levels(test_data_pca$ESG.Risk.Level))
test_labels <- factor(test_data_pca$ESG.Risk.Level)

# Generate the confusion matrix
library(caret)  # Load caret for confusionMatrix
confusion_matrix_knn <- confusionMatrix(knn_preds, test_labels)

# Print the confusion matrix
print(confusion_matrix_knn)

confusion_matrix_knn <- confusionMatrix(knn_preds, test_labels)

# Print the confusion matrix
print(confusion_matrix_knn)

#####PART-4:MODEL EVALUATION

##Model Evaluation for Regression Models:Linear Regression

# Post-resample metrics
linear_metrics <- postResample(pred = linear_preds, obs = test_data$Total.ESG.Risk.score)
cat("FOR LINEAR REGRESSION:\n",
    "RMSE = ", linear_metrics['RMSE'], "\n",
    "MAE = ", linear_metrics['MAE'], "\n",
    "R-Squared = ", linear_metrics['Rsquared'], "\n\n")

# Residual Analysis
# Calculate residuals
residuals <- test_data$Total.ESG.Risk.score - linear_preds

# Residual plot
ggplot(data = data.frame(Predicted = linear_preds, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Linear Regression", x = "Predicted Values", y = "Residuals") +
  theme_minimal()
# Plot histogram of residuals
ggplot(data = data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals for Linear Regression", x = "Residuals", y = "Frequency") +
  theme_minimal()
# QQ plot for residuals
ggplot(data = data.frame(Residuals = residuals), aes(sample = Residuals)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals for Linear Regression", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()



##classification models
compute_classification_metrics <- function(predictions, actual, model_name) {
  # Ensure both predictions and actuals have the same levels
  predictions <- factor(predictions, levels = levels(actual))
  
  # Calculate confusion matrix
  metrics <- confusionMatrix(predictions, actual)
  
  # Overall metrics
  accuracy <- metrics$overall['Accuracy']
  kappa <- metrics$overall['Kappa']
  
  cat("FOR", model_name, ":\n")
  cat("Accuracy = ", accuracy, "\n")
  cat("Kappa = ", kappa, "\n\n")
  
  # Class-wise Precision, Recall, and F1-Score
  class_levels <- levels(actual)
  
  for (class in class_levels) {
    # Filter actual and predicted for binary evaluation of the current class
    binary_actual <- factor(ifelse(actual == class, class, "Other"), levels = c(class, "Other"))
    binary_predictions <- factor(ifelse(predictions == class, class, "Other"), levels = c(class, "Other"))
    
    precision <- posPredValue(binary_predictions, binary_actual, positive = class)
    recall <- sensitivity(binary_predictions, binary_actual, positive = class)
    f1 <- (2 * precision * recall) / (precision + recall)
    
    cat("Class: ", class, "\n")
    cat(" Precision = ", round(precision, 4), "\n")
    cat(" Recall = ", round(recall, 4), "\n")
    cat(" F1-Score = ", round(f1, 4), "\n\n")
  }
}

# For Logistic Regression
compute_classification_metrics(multi_log_preds, test_data_pca$ESG.Risk.Level, "LOGISTIC REGRESSION")
# For Decision Tree
compute_classification_metrics(tree_preds, test_data_pca$ESG.Risk.Level, "DECISION TREE")
# For Random Forest
compute_classification_metrics(rf_preds, test_data$ESG.Risk.Level, "RANDOM FOREST")
# For Neural Network
compute_classification_metrics(nn_class, test_data_pca$ESG.Risk.Level, "NEURAL NETWORK")

# Compute classification metrics for KNN
compute_knn_metrics <- function(predictions, actual, model_name) {
  # Ensure both predictions and actuals have the same levels
  predictions <- factor(predictions, levels = levels(actual))
  
  # Calculate confusion matrix
  metrics <- confusionMatrix(predictions, actual)
  
  # Overall metrics
  accuracy <- metrics$overall['Accuracy']
  kappa <- metrics$overall['Kappa']
  
  cat("FOR", model_name, ":\n")
  cat("Accuracy = ", accuracy, "\n")
  cat("Kappa = ", kappa, "\n\n")
  
  # Class-wise Precision, Recall, and F1-Score
  class_levels <- levels(actual)
  
  for (class in class_levels) {
    # Filter actual and predicted for binary evaluation of the current class
    binary_actual <- factor(ifelse(actual == class, class, "Other"), levels = c(class, "Other"))
    binary_predictions <- factor(ifelse(predictions == class, class, "Other"), levels = c(class, "Other"))
    
    precision <- posPredValue(binary_predictions, binary_actual, positive = class)
    recall <- sensitivity(binary_predictions, binary_actual, positive = class)
    f1 <- (2 * precision * recall) / (precision + recall)
    
    cat("Class: ", class, "\n")
    cat(" Precision = ", round(precision, 4), "\n")
    cat(" Recall = ", round(recall, 4), "\n")
    cat(" F1-Score = ", round(f1, 4), "\n\n")
  }
}

# Evaluate KNN model
compute_knn_metrics(knn_preds, test_data_pca$ESG.Risk.Level, "KNN")


###VISUALIZATION CURVES

##LOGISTIC REGRESSION:

# Step 1: Predict probabilities for the "High" class
# Ensure `multi_log_model` is trained and `test_data_pca` contains valid predictors
log_probs_high <- predict(multi_log_model, newdata = test_data_pca, type = "prob")[, "High"]

# Step 2: Create a data frame with actual and predicted values
results <- data.frame(
  Predicted_Prob = log_probs_high,
  Actual = ifelse(test_data_pca$ESG.Risk.Level == "High", 1, 0)  # Binary target for "High"
)

# Ensure no missing or NA values
results <- results %>% na.omit()

# Step 3: Sort results and calculate metrics
results <- results %>%
  arrange(desc(Predicted_Prob)) %>%
  mutate(
    Rank = row_number(),
    Percentile = Rank / n() * 100,
    Cumulative_Positive = cumsum(Actual),
    Total_Positive = sum(Actual),
    Cumulative_Rate = Cumulative_Positive / Total_Positive,
    Lift = Cumulative_Rate / (Rank / n())
  )

# Step 4: Gain Chart
ggplot(results, aes(x = Percentile, y = Cumulative_Rate)) +
  geom_line(color = "green") +
  geom_point(color = "brown") +
  labs(title = "Smoothed Gain Chart - Logistic Regression", 
       x = "Percentile", 
       y = "Cumulative % of Positives") +
  theme_minimal()

# Step 5: Lift Chart
ggplot(results, aes(x = Percentile, y = Lift)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Smoothed Lift Chart - Logistic Regression", 
       x = "Percentile", 
       y = "Lift") +
  theme_minimal()

# Step 6: ROC Curve and AUC Calculation
roc_curve <- roc(response = results$Actual, 
                 predictor = results$Predicted_Prob, 
                 levels = c(0, 1), 
                 direction = "<")


# Plot ROC Curve
plot(roc_curve, col = "blue", 
     main = "ROC Curve for Logistic Regression", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)

# Optional: Smoothing the ROC curve
# Note: `smooth.spline` requires specificities and sensitivities from `roc_curve`
smoothed_roc <- smooth.spline(1 - roc_curve$specificities, roc_curve$sensitivities, spar = 0.8)
lines(smoothed_roc, col = "darkgreen", lwd = 2)

##The charts seem appropriately constructed. Here's what they typically indicate:

#Lift Chart:
  
#  Measures the performance of your model compared to random guessing.
#A lift of 10 at the top percentiles suggests your model is highly effective at predicting positives in that percentile.

#Gain Chart:
  
#  Indicates how well your model captures positive instances as you include more data.
# An early plateau reaching 100% positive rate indicates the model captures all positives quickly.


##DECISION TREE
# Predict probabilities for the "High" class
tree_probs <- predict(tree_model, newdata = test_data_pca, type = "prob")[, "High"]

# Create ROC object and calculate AUC
tree_roc_pred <- ROCR::prediction(tree_probs, as.numeric(test_data_pca$ESG.Risk.Level == "High"))
tree_roc_perf <- ROCR::performance(tree_roc_pred, "tpr", "fpr")
tree_auc <- ROCR::performance(tree_roc_pred, "auc")@y.values[[1]]

# Plot ROC Curve
plot(tree_roc_perf, col = "green", main = "ROC Curve - Decision Tree")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("Decision Tree AUC: ", tree_auc, "\n")


# Ensure the data is sorted by predicted probability
results_tree <- data.frame(
  Predicted_Prob = tree_probs,
  Actual = as.numeric(test_data_pca$ESG.Risk.Level == "High")
) %>% arrange(desc(Predicted_Prob))

results_tree <- results_tree %>%
  arrange(desc(Predicted_Prob)) %>%
  mutate(Percentile = (row_number() / n()) * 100,
         Cumulative_Positive = cumsum(Actual),
         Total_Positive = sum(Actual),
         Cumulative_Rate = Cumulative_Positive / Total_Positive,
         Lift = Cumulative_Positive / row_number() / (Total_Positive / n()))

# Smoothed Lift Chart for Decision Tree
ggplot(results_tree, aes(x = Percentile, y = Lift)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Smoothed Lift Chart - Decision Tree", x = "Percentile", y = "Lift")

# Smoothed Gain Chart for Decision Tree
ggplot(results_tree, aes(x = Percentile, y = Cumulative_Rate)) +
  geom_line(color = "green") +
  geom_point(color = "brown") +
  labs(title = "Smoothed Gain Chart - Decision Tree", x = "Percentile", y = "Cumulative % of Positives")



#The Lift and Gain charts for the Decision Tree model have been successfully generated, showing strong cumulative gains and lift compared to random expectations.
#These metrics reinforce the model's effectiveness in identifying positives.


###RANDOM FOREST
# Predict probabilities for the "High" class
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "High"]

# Create ROC object and calculate AUC
rf_roc_pred <- ROCR::prediction(rf_probs, as.numeric(test_data$ESG.Risk.Level == "High"))
rf_roc_perf <- ROCR::performance(rf_roc_pred, "tpr", "fpr")
rf_auc <- ROCR::performance(rf_roc_pred, "auc")@y.values[[1]]

# Plot ROC Curve
plot(rf_roc_perf, col = "purple", main = "ROC Curve - Random Forest")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("Random Forest AUC: ", rf_auc, "\n")

# Lift and Gain Charts
results_rf <- data.frame(
  Predicted_Prob = rf_probs,
  Actual = ifelse(test_data$ESG.Risk.Level == "High", 1, 0)
) %>% arrange(desc(Predicted_Prob))

results_rf <- results_rf %>%
  mutate(Cumulative_Positive = cumsum(Actual),
         Total_Positive = sum(Actual),
         Cumulative_Rate = Cumulative_Positive / Total_Positive,
         Lift = Cumulative_Positive / row_number() / (Total_Positive / n()),
         Percentile = row_number() / n())

ggplot(results_rf, aes(x = Percentile, y = Lift)) +
  geom_line(color = "blue") +
  labs(title = "Lift Chart - Random Forest", x = "Percentile", y = "Lift")

ggplot(results_rf, aes(x = Percentile, y = Cumulative_Rate)) +
  geom_line(color = "orange") +
  labs(title = "Gain Chart - Random Forest", x = "Percentile", y = "Cumulative Gains")


###NEURAL NETWORK
# Predict probabilities for the "High" class
nn_probs <- predict(nn_tuned_model, newdata = test_data_pca, type = "prob")[, "High"]

# Create ROC object and calculate AUC
nn_roc_pred <- ROCR::prediction(nn_probs, as.numeric(test_data_pca$ESG.Risk.Level == "High"))
nn_roc_perf <- ROCR::performance(nn_roc_pred, "tpr", "fpr")
nn_auc <- ROCR::performance(nn_roc_pred, "auc")@y.values[[1]]

# Plot ROC Curve
plot(nn_roc_perf, col = "orange", main = "ROC Curve - Neural Network")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("Neural Network AUC: ", nn_auc, "\n")

# Ensure the actual values are numeric (1 for "High", 0 for others)
actual_binary_nn <- as.numeric(test_data_pca$ESG.Risk.Level == "High")

# Create a data frame with actual and predicted values
results_nn <- data.frame(
  Actual = actual_binary_nn,
  Predicted_Prob = nn_probs
)

# Sort the data frame by predicted probabilities in descending order
results_nn <- results_nn %>%
  arrange(desc(Predicted_Prob)) %>%
  mutate(
    Percentile = (row_number() / n()) * 100,
    Cumulative_Positive = cumsum(Actual),
    Total_Positive = sum(Actual),
    Cumulative_Rate = Cumulative_Positive / Total_Positive,
    Lift = Cumulative_Positive / row_number() / (Total_Positive / n())
  )
#lift chart
ggplot(results_nn, aes(x = Percentile, y = Lift)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  labs(title = "Smoothed Lift Chart - Neural Network", x = "Percentile", y = "Lift")
#gain chart
ggplot(results_nn, aes(x = Percentile, y = Cumulative_Rate)) +
  geom_line(color = "green") +
  geom_point(color = "brown") +
  labs(title = "Smoothed Gain Chart - Neural Network", x = "Percentile", y = "Cumulative % of Positives")


###kNN
knn_roc_pred <- prediction(knn_probs_adjusted, as.numeric(test_data_pca$ESG.Risk.Level == "High"))
knn_roc_perf <- performance(knn_roc_pred, "tpr", "fpr")
knn_auc <- performance(knn_roc_pred, "auc")@y.values[[1]]

# Plot ROC Curve
plot(knn_roc_perf, col = "blue", main = "ROC Curve - KNN")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("KNN AUC: ", knn_auc, "\n")

results_knn <- data.frame(
  Predicted_Prob = knn_probs_adjusted,
  Actual = as.numeric(test_data_pca$ESG.Risk.Level == "High")
)

results_knn <- results_knn %>%
  arrange(desc(Predicted_Prob)) %>%
  mutate(Cumulative_Positive = cumsum(Actual),
         Total_Positive = sum(Actual),
         Cumulative_Rate = Cumulative_Positive / Total_Positive,
         Lift = Cumulative_Positive / row_number() / (Total_Positive / n()),
         Percentile = 100 * row_number() / n())

# Lift Chart
ggplot(results_knn, aes(x = Percentile, y = Lift)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Smoothed Lift Chart - KNN", x = "Percentile", y = "Lift")

#Gain Chart
ggplot(results_knn, aes(x = Percentile, y = Cumulative_Rate)) +
  geom_line(color = "green") +
  geom_point(color = "brown") +
  labs(title = "Smoothed Gain Chart - KNN", x = "Percentile", y = "Cumulative % of Positives")







colnames(test_data)


# Combine Predictions with Test Data for Visualization
test_data_with_predictions <- test_data %>%
  mutate(Predicted_Risk = rf_preds) %>%
  select(Predicted_Risk, Environment.Risk.Score, Social.Risk.Score)

# Check the dataset to verify
head(test_data_with_predictions)


ggplot(test_data_with_predictions, aes(x = Environment.Risk.Score, y = Social.Risk.Score, color = Predicted_Risk)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Predicted ESG Risk Levels by Random Forest",x = "Environmental Risk Score",
    y = "Social Risk Score",
    color = "Predicted Risk Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(
    values = c(
      "High" = "red",
      "Low" = "green",
      "Medium" = "blue",
      "Negligible" = "purple",
      "Severe" = "orange"
    )
  )


