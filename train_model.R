# Load required libraries
library(dplyr)        # Data manipulation
library(ggplot2)      # Visualization
library(caTools)      # Splitting dataset

# Set file path (change this if needed)
file_path <- "C:/Users/koras/Downloads/archive (2)/indiancrop_dataset.csv"

# Load the dataset
crop_data <- read.csv(file_path, stringsAsFactors = TRUE)

# View the first few rows
head(crop_data)
# Check for missing values
sum(is.na(crop_data))

# Fill missing values (if any) - Replace NA with column mean for numeric columns
crop_data <- crop_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Convert categorical columns to factors (if not already)
crop_data$STATE <- as.factor(crop_data$STATE)
crop_data$CROP <- as.factor(crop_data$CROP)

# Check structure of the dataset
str(crop_data)

# Split data into training (80%) and testing (20%)
set.seed(123)  # For reproducibility
split <- sample.split(crop_data$CROP, SplitRatio = 0.8)
train_data <- subset(crop_data, split == TRUE)
test_data <- subset(crop_data, split == FALSE)

# View dataset dimensions
cat("Training Data:", dim(train_data), "\n")
cat("Testing Data:", dim(test_data), "\n")


# Load required libraries
library(randomForest)  # For Random Forest model
library(caret)         # For evaluation (Confusion Matrix)


# ✅ Step 6: Train Random Forest Model (KEEPING `CROP_PRICE`)
rf_model <- randomForest(CROP ~ ., data = train_data, ntree = 100, mtry = 3, importance = TRUE)
print(rf_model)  # Check model summary

# ✅ Step 7: Model Evaluation (Check Accuracy)
predictions <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$CROP)
print(conf_matrix)

# ✅ Step 8: Predict Crop for New Soil Sample
new_sample <- data.frame(
  N_SOIL = 80, P_SOIL = 40, K_SOIL = 45, TEMPERATURE = 25, 
  HUMIDITY = 60, ph = 6.5, RAINFALL = 200, STATE = "Andaman and Nicobar",
  CROP_PRICE = 7000  # KEEPING `CROP_PRICE`
)

# Convert `STATE` to factor with same levels as training data
new_sample$STATE <- factor(new_sample$STATE, levels = levels(train_data$STATE))


# Make Prediction
predicted_crop <- predict(rf_model, new_sample)

# Convert factor level to actual crop name
predicted_crop_name <- levels(train_data$CROP)[predicted_crop]

# Display Correct Crop Name
cat("🌾 Recommended Crop:", predicted_crop_name, "\n")

saveRDS(rf_model, file = "crop_recommendation_model.rds")




