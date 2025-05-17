# 🌾 Crop Recommendation System

This is a Machine Learning-based Crop Recommendation System developed to suggest the most suitable crop to cultivate based on given soil and climate conditions.

## 📖 Project Overview
The system takes input parameters like:
- Nitrogen (N)
- Phosphorus (P)
- Potassium (K)
- Temperature
- Humidity
- pH
- Rainfall

Using these inputs, it predicts the best crop for cultivation.

## 🛠️ How It Works
- A Machine Learning model (Random Forest / Decision Tree) is trained on a crop dataset.
- The model is served as an **API using R Plumber**.
- Users send data to the API, and it responds with the recommended crop.

## ✅ Current Status
- ✅ Model trained and tested successfully.
- ✅ API created using R (Plumber).
- 🚧 Frontend development (React) is planned but not yet done.

## 🗒️ Example Workflow
1. User sends soil & weather data to the API.
2. The API processes the input and returns a crop recommendation.
3. Example response:
```json
{
  "recommended_crop": "Rice"
}
Purpose
This project aims to help farmers and agricultural advisors make informed decisions about crop selection based on scientific data.
