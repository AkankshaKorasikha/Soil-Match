# Soil-Match 🌾

Soil-Match is an R-based crop recommendation system that predicts the most suitable crop for a farm or region using soil and climate parameters such as nitrogen, phosphorus, potassium, temperature, humidity, pH, rainfall, and state. The project trains a machine learning model and exposes it through a REST API using the `plumber` package.

## Project Overview

Farming decisions depend heavily on soil quality and environmental conditions. Soil-Match helps users choose a crop that has the best chance of thriving in a given location by combining agricultural data with a trained predictive model.

This system is useful for:
- Farmers making crop selection decisions
- Agricultural advisors and extension officers
- Researchers analyzing crop suitability
- Decision-support applications for precision agriculture

## Features

- Predicts the best crop based on soil and weather conditions
- Uses a trained Random Forest model
- Accepts real-world agronomic inputs
- Exposes predictions through a simple HTTP API
- Built entirely in R for ease of deployment and experimentation

## Tech Stack

- R
- Random Forest (`randomForest`)
- Plumber API framework
- `dplyr` and `ggplot2` for preprocessing and analysis
- `caTools` for train/test splitting

## Repository Structure

```text
Soil-Match/
├── README.md
├── train_model.R
├── crop_api.R
├── crop_recommendation_model.rds
├── 1️⃣ Crop Recommendation System 🌱.txt
└── ...
```

### Files

- `train_model.R` – prepares the dataset, trains the model, and saves it as an `.rds` file
- `crop_api.R` – creates the Plumber API and serves predictions
- `crop_recommendation_model.rds` – the trained machine learning model
- `README.md` – project documentation

## Dataset

The project uses an Indian crop dataset containing agronomic and environmental features such as:

- `N_SOIL`
- `P_SOIL`
- `K_SOIL`
- `TEMPERATURE`
- `HUMIDITY`
- `ph`
- `RAINFALL`
- `STATE`
- `CROP`

The model is trained to classify the most suitable crop for a given input profile.

## Model Training

The training script performs the following steps:

1. Loads the crop dataset
2. Checks for missing values
3. Fills missing numeric values using column means
4. Converts categorical variables to factors
5. Splits the data into training and testing sets
6. Trains a Random Forest model
7. Evaluates model accuracy
8. Saves the trained model to `crop_recommendation_model.rds`

## API Usage

The API is implemented using the `plumber` package and runs locally on port `8000`.

### Start the API

```r
source("crop_api.R")
```

This will launch the API server on:

```text
http://localhost:8000
```

### Prediction Endpoint

```http
GET /predict
```

### Parameters

- `N_SOIL` : Nitrogen content in soil
- `P_SOIL` : Phosphorus content in soil
- `K_SOIL` : Potassium content in soil
- `TEMPERATURE` : Temperature in Celsius
- `HUMIDITY` : Humidity percentage
- `ph` : Soil pH value
- `RAINFALL` : Annual rainfall in mm

### Example Request

```bash
curl "http://localhost:8000/predict?N_SOIL=80&P_SOIL=40&K_SOIL=45&TEMPERATURE=25&HUMIDITY=60&ph=6.5&RAINFALL=200"
```

### Example Response

```json
{
  "predicted_crop": "Rice"
}
```

## How It Works

1. The user sends soil and climate values to the API.
2. The API converts the inputs into a data frame.
3. The Random Forest model predicts the crop.
4. The API returns the predicted crop name as JSON.

## Example Use Case

A farmer enters the following conditions:

- Nitrogen: 80
- Phosphorus: 40
- Potassium: 45
- Temperature: 25°C
- Humidity: 60%
- pH: 6.5
- Rainfall: 200 mm

The model may predict:

```json
{"predicted_crop":"Rice"}
```

## Results and Capabilities

The project demonstrates a working agriculture decision-support pipeline using machine learning and an API-based deployment model. It is a strong foundation for further enhancements such as:

- Crop price prediction
- Soil suitability classification
- State-wise crop suitability analysis
- Frontend integration
- Deployment to cloud platforms

## Installation

1. Install R on your machine.
2. Install the required packages:

```r
install.packages(c("plumber", "randomForest", "dplyr", "ggplot2", "caTools", "caret"))
```

3. Open the project folder in R Studio.
4. Run the training script to generate the model file:

```r
source("train_model.R")
```

5. Start the API:

```r
source("crop_api.R")
```

## Future Improvements

- Add a web frontend for easy farm input collection
- Add more crop classes and richer agricultural data
- Include regional and seasonal recommendations
- Support deployment via Docker or a web server
- Add model evaluation plots and performance reports

## License

This project does not currently include a license file. If you plan to distribute or share the project publicly, consider adding an open-source license.

## Author

Akanksha Korasikha

## Acknowledgements

This project was developed to support intelligent agricultural decision-making using machine learning and accessible data-driven recommendations.
