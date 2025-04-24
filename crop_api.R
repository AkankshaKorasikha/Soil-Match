# Load required libraries
library(plumber)
library(randomForest)

# Load trained model
rf_model <- readRDS("crop_recommendation_model.rds")

# Create a Plumber router
pr <- plumber$new()

#* @apiTitle Crop Recommendation API
#* @apiDescription Predicts the best crop based on soil and climate conditions.

#* Predict crop recommendation
#* @param N_SOIL:int Nitrogen content in soil
#* @param P_SOIL:int Phosphorus content in soil
#* @param K_SOIL:int Potassium content in soil
#* @param TEMPERATURE:double Temperature in Celsius
#* @param HUMIDITY:double Humidity percentage
#* @param ph:double Soil pH value
#* @param RAINFALL:double Annual rainfall in mm
#* @get /predict
pr$handle("GET", "/predict", function(N_SOIL, P_SOIL, K_SOIL, TEMPERATURE, HUMIDITY, ph, RAINFALL) {
  input_data <- data.frame(
    N_SOIL = as.integer(N_SOIL),
    P_SOIL = as.integer(P_SOIL),
    K_SOIL = as.integer(K_SOIL),
    TEMPERATURE = as.numeric(TEMPERATURE),
    HUMIDITY = as.numeric(HUMIDITY),
    ph = as.numeric(ph),
    RAINFALL = as.numeric(RAINFALL)
  )
  
  predicted_crop <- predict(rf_model, input_data)
  
  list(predicted_crop = as.character(predicted_crop))
})

# Run the API
pr$run(port = 8000)
