# Load Required Libraries
library(shiny)
library(caret)
library(randomForest)
library(ggplot2)
library(mailR)

# Load Dataset
data <- read.csv("hr_dataset.csv")
View(data)
# Convert Attrition to Factor
data$Attrition <- as.factor(data$Attrition)

# Ensure Important Categorical Variables are Factors
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$Department <- as.factor(data$Department)
data$JobRole <- as.factor(data$JobRole)
data$OverTime <- as.factor(data$OverTime)
data$EducationField <- as.factor(data$EducationField)
data$Gender <- as.factor(data$Gender)
data$MaritalStatus <- as.factor(data$MaritalStatus)

# Remove Unnecessary Columns
data <- data[, !names(data) %in% c("EmployeeCount", "Over18", "StandardHours", "EmployeeNumber")]

# Handle Missing Values (if any)
data <- na.omit(data)

# Train Random Forest Model
set.seed(123)
model <- randomForest(Attrition ~ ., data = data, ntree = 100, importance = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Employee Attrition Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", "Age", value = 30, min = 18, max = 65),
      selectInput("Department", "Department", choices = unique(data$Department)),
      selectInput("BusinessTravel", "Business Travel", choices = unique(data$BusinessTravel)),
      selectInput("MonthlyIncome", "Monthly Income Range",
                  choices = c("Below 3000", "3000-6000", "6000-10000", "10000-15000", "Above 15000")),
      selectInput("JobRole", "Job Role", choices = unique(data$JobRole)),
      selectInput("Education", "Education Level", choices = 1:5),
      numericInput("YearsAtCompany", "Years at Company", value = 3, min = 0, max = 40),
      numericInput("DistanceFromHome", "Distance from Home", value = 5, min = 0, max = 50),
      selectInput("JobSatisfaction", "Job Satisfaction", choices = 1:4),
      selectInput("WorkLifeBalance", "Work-Life Balance", choices = 1:4),
      selectInput("JobInvolvement", "Job Involvement", choices = 1:4),
      selectInput("OverTime", "OverTime", choices = unique(data$OverTime)),
      selectInput("EducationField", "Education Field", choices = unique(data$EducationField)),
      selectInput("Gender", "Gender", choices = unique(data$Gender)),
      selectInput("MaritalStatus", "Marital Status", choices = unique(data$MaritalStatus)),
      textInput("email", "Enter Email for Notification (Optional)", ""),
      helpText("Select the appropriate values and click Predict to get results."),
      actionButton("predict", "Predict"),
      downloadButton("downloadReport", "Download Report")
    ),
    mainPanel(
      textOutput("prediction"),
      tableOutput("topReasons"),  # Show only personalized top 5 reasons
      plotOutput("attritionPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    salary_mean <- switch(input$MonthlyIncome,
                          "Below 3000" = 2500,
                          "3000-6000" = 4500,
                          "6000-10000" = 8000,
                          "10000-15000" = 12500,
                          "Above 15000" = 16000)
    
    new_data <- data.frame(
      Age = as.numeric(input$Age),
      BusinessTravel = as.factor(input$BusinessTravel),
      DailyRate = median(data$DailyRate, na.rm = TRUE),
      Department = as.factor(input$Department),
      DistanceFromHome = as.numeric(input$DistanceFromHome),
      Education = as.numeric(input$Education),
      EducationField = as.factor(input$EducationField),
      EnvironmentSatisfaction = median(data$EnvironmentSatisfaction, na.rm = TRUE),
      Gender = as.factor(input$Gender),
      HourlyRate = median(data$HourlyRate, na.rm = TRUE),
      JobInvolvement = as.numeric(input$JobInvolvement),
      JobLevel = median(data$JobLevel, na.rm = TRUE),
      JobRole = as.factor(input$JobRole),
      JobSatisfaction = as.numeric(input$JobSatisfaction),
      MaritalStatus = as.factor(input$MaritalStatus),
      MonthlyIncome = salary_mean,
      MonthlyRate = median(data$MonthlyRate, na.rm = TRUE),
      NumCompaniesWorked = median(data$NumCompaniesWorked, na.rm = TRUE),
      OverTime = as.factor(input$OverTime),
      PercentSalaryHike = median(data$PercentSalaryHike, na.rm = TRUE),
      PerformanceRating = median(data$PerformanceRating, na.rm = TRUE),
      RelationshipSatisfaction = median(data$RelationshipSatisfaction, na.rm = TRUE),
      StockOptionLevel = median(data$StockOptionLevel, na.rm = TRUE),
      TotalWorkingYears = median(data$TotalWorkingYears, na.rm = TRUE),
      TrainingTimesLastYear = median(data$TrainingTimesLastYear, na.rm = TRUE),
      WorkLifeBalance = as.numeric(input$WorkLifeBalance),
      YearsAtCompany = as.numeric(input$YearsAtCompany),
      YearsInCurrentRole = median(data$YearsInCurrentRole, na.rm = TRUE),
      YearsSinceLastPromotion = median(data$YearsSinceLastPromotion, na.rm = TRUE),
      YearsWithCurrManager = median(data$YearsWithCurrManager, na.rm = TRUE)
    )
    
    # Predict Probability
    pred_prob <- predict(model, new_data, type = "prob")
    
    # Extract Individual Contribution to Prediction
    impact_values <- predict(model, new_data, type = "response", predict.all = TRUE)$individual
    impact_means <- colMeans(impact_values)
    
    # Sort and Select Top 5 Factors
    top_factors <- sort(impact_means, decreasing = TRUE)[1:5]
    
    list(prob = pred_prob, reasons = data.frame(Factor = names(top_factors), Impact = round(top_factors, 2)))
  })
  
  # Show Prediction Result
  output$prediction <- renderText({
    req(input$predict)
    prob <- prediction()$prob
    paste("Likelihood of Employee Leaving:", round(prob[1, "Yes"] * 100, 2), "%")
  })
  
  # Show Top 5 Personalized Reasons
  output$topReasons <- renderTable({
    req(input$predict)
    prediction()$reasons
  })
  
  # Plot Attrition Trends
  output$attritionPlot <- renderPlot({
    ggplot(data, aes(x = Age, fill = Attrition)) + 
      geom_histogram(binwidth = 5, position = "dodge") + 
      theme_minimal()
  })
  
  # Download Report
  output$downloadReport <- downloadHandler(
    filename = function() { "attrition_report.csv" },
    content = function(file) {
      report_data <- data.frame(
        Prediction = round(prediction()$prob[1, "Yes"] * 100, 2),
        TopReasons = prediction()$reasons
      )
      write.csv(report_data, file, row.names = FALSE)
    }
  )
  
  # Email Notification
  observeEvent(input$predict, {
    if (input$email != "") {
      tryCatch({
        send.mail(
          from = "your-email@example.com",
          to = input$email,
          subject = "Attrition Prediction",
          body = paste("Likelihood of Employee Leaving:", round(prediction()$prob[1, "Yes"] * 100, 2), "%"),
          smtp = list(host.name = "smtp.example.com", port = 465, user.name = "your-email@example.com", passwd = "password", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE
        )
      }, error = function(e) {
        showNotification("Failed to send email. Please check your email configuration.", type = "error")
      })
    }
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)
