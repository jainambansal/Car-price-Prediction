# Required Libraries
#install.packages("RSQLite")
library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(randomForest)
library(caret)
library(DBI)
library(RSQLite)

# Data Import
# Replace with your actual file path
data <- read.csv("C:/Users/hp/OneDrive/Documents/car price prediction.csv")

# Data Preprocessing
data$Brand <- as.factor(data$Brand)
data$Fuel.Type <- as.factor(data$Fuel.Type)
data$Transmission <- as.factor(data$Transmission)
data$Condition <- as.factor(data$Condition)
data$Model <- as.factor(data$Model)

# Train Random Forest Model
train_price_model <- function(data) {
  set.seed(123)
  features <- c("Brand", "Year", "Engine.Size", "Fuel.Type", 
                "Transmission", "Mileage", "Condition", "Model")
  processed_data <- data[, c(features, "Price")]
  
  train_idx <- createDataPartition(processed_data$Price, p = 0.8, list = FALSE)
  train_data <- processed_data[train_idx, ]
  
  rf_model <- randomForest(Price ~ ., data = train_data, ntree = 100, importance = TRUE)
  saveRDS(rf_model, "car_price_model.rds")
  return(rf_model)
}

rf_model <- train_price_model(data)

# Prediction Function
predict_car_price <- function(new_data) {
  model <- readRDS("car_price_model.rds")
  predict(model, newdata = new_data)
}

# UI
ui <- navbarPage(
  title = "Car Price Predictor",
  theme = shinytheme("flatly"),
  
  # Login/Signup
  tabPanel("Login",
           fluidPage(
             fluidRow(
               column(6, offset = 3,
                      tags$div(class = "panel panel-primary",
                               tags$div(class = "panel-heading", tags$h3("Login")),
                               tags$div(class = "panel-body",
                                        textInput("username", "Username"),
                                        passwordInput("password", "Password"),
                                        actionButton("login", "Login", class = "btn-primary")
                               )
                      )
               )
             ),
             fluidRow(
               column(6, offset = 3,
                      tags$div(class = "panel panel-success",
                               tags$div(class = "panel-heading", tags$h3("Sign Up")),
                               tags$div(class = "panel-body",
                                        textInput("new_username", "Username"),
                                        passwordInput("new_password", "Password"),
                                        actionButton("signup", "Sign Up", class = "btn-success")
                               )
                      )
               )
             )
           )
  ),
  
  # About Project
  tabPanel("About Project",
           fluidPage(
             tags$div(
               class = "jumbotron text-center",
               tags$h1("Car Price Prediction Tool"),
               tags$p("An AI-driven application to predict car prices based on features like brand, year, engine size, and condition.")
             ),
             fluidRow(
               column(8, offset = 2,
                      tags$div(
                        class = "well",
                        tags$h3("Project Purpose"),
                        tags$p("This project assists buyers and sellers in estimating fair car prices through a data-driven approach."),
                        tags$h4("Team Information:"),
                        tags$ul(
                          tags$li("Name: Jainam Bansal"),
                          tags$li("Roll No.: 22ESKCX048"),
                          tags$li("Email: bansaljainam249@gmail.com"),
                          tags$li("Role: Sole Developer")
                        )
                      )
               )
             )
           )
  ),
  
  # Dashboard
  tabPanel("Dashboard",
           fluidPage(
             fluidRow(
               column(6, plotlyOutput("bar_plot")),
               column(6, plotlyOutput("doughnut_chart"))
             ),
             fluidRow(
               column(6, plotlyOutput("line_chart")),
               column(6, plotlyOutput("violin_chart"))
             )
           )
  ),
  
  # Price Prediction
  tabPanel("Price Predictor",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("brand", "Select Brand", choices = levels(data$Brand)),
                 selectInput("model", "Select Model", choices = levels(data$Model)),
                 numericInput("year", "Manufacturing Year", value = median(data$Year),
                              min = min(data$Year), max = max(data$Year)),
                 numericInput("engine_size", "Engine Size (L)", value = 2.0, min = 0.5, max = 6.0),
                 selectInput("fuel_type", "Fuel Type", choices = levels(data$Fuel.Type)),
                 selectInput("transmission", "Transmission", choices = levels(data$Transmission)),
                 numericInput("mileage", "Mileage", value = median(data$Mileage), min = 0, max = max(data$Mileage)),
                 selectInput("condition", "Condition", choices = levels(data$Condition)),
                 actionButton("predict", "Predict Price", class = "btn-primary"),
                 tags$div(
                   class = "alert alert-info",
                   tags$h4("Instructions"),
                   tags$p("Ensure inputs fall within valid ranges."),
                   tags$ul(
                     tags$li("Year: ", min(data$Year), "-", max(data$Year)),
                     tags$li("Engine Size: 0.5 - 6.0"),
                     tags$li("Mileage: 0 - ", max(data$Mileage))
                   )
                 )
               ),
               mainPanel(
                 tags$div(
                   class = "well text-center",
                   tags$h3("Predicted Price"),
                   tags$h1(textOutput("predicted_price"), style = "color: #007bff;")
                 )
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  # Database connection for login/signup
  con <- dbConnect(SQLite(), "user_data.sqlite")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS users (username TEXT, password TEXT)")
  
  observeEvent(input$signup, {
    username <- input$new_username
    password <- input$new_password
    if (username != "" && password != "") {
      dbExecute(con, "INSERT INTO users (username, password) VALUES (?, ?)", params = list(username, password))
      showNotification("Signup Successful!", type = "message")
    } else {
      showNotification("Please fill in all fields.", type = "error")
    }
  })
  
  observeEvent(input$login, {
    username <- input$username
    password <- input$password
    user <- dbGetQuery(con, "SELECT * FROM users WHERE username = ? AND password = ?", params = list(username, password))
    if (nrow(user) > 0) {
      showNotification("Login Successful!", type = "message")
    } else {
      showNotification("Invalid Credentials.", type = "error")
    }
  })
  
  # Dashboard Plots
  output$bar_plot <- renderPlotly({
    avg_price <- data %>%
      group_by(Brand) %>%
      summarise(Average_Price = mean(Price))
    plot_ly(avg_price, x = ~Brand, y = ~Average_Price, type = "bar", marker = list(color = "skyblue")) %>%
      layout(title = "Average Price by Brand", xaxis = list(title = "Brand"), yaxis = list(title = "Average Price"))
  })
  
  output$doughnut_chart <- renderPlotly({
    transmission_count <- data %>%
      group_by(Transmission) %>%
      summarise(Count = n())
    plot_ly(transmission_count, labels = ~Transmission, values = ~Count, type = "pie", hole = 0.4) %>%
      layout(title = "Transmission Distribution")
  })
  
  output$line_chart <- renderPlotly({
    yearly_price <- data %>%
      group_by(Year) %>%
      summarise(Average_Price = mean(Price))
    plot_ly(yearly_price, x = ~Year, y = ~Average_Price, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Yearly Average Price Trend", xaxis = list(title = "Year"), yaxis = list(title = "Average Price"))
  })
  
  output$violin_chart <- renderPlotly({
    plot_ly(data, y = ~Price, x = ~Condition, type = "violin", split = ~Condition) %>%
      layout(title = "Price Distribution by Condition", xaxis = list(title = "Condition"), yaxis = list(title = "Price"))
  })
  
  # Prediction
  predicted_price <- eventReactive(input$predict, {
    new_data <- data.frame(
      Brand = input$brand,
      Model = input$model,
      Year = input$year,
      Engine.Size = input$engine_size,
      Fuel.Type = input$fuel_type,
      Transmission = input$transmission,
      Mileage = input$mileage,
      Condition = input$condition
    )
    predict_car_price(new_data)
  })
  
  output$predicted_price <- renderText({
    paste("$", round(predicted_price(), 2))
  })
}

# Run App
shinyApp(ui, server)
