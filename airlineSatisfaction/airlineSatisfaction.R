library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(plotly)
library(caret)
library(randomForest)
library(DT)

# Load data
data <- read_csv("train.csv") %>% 
  rename_with(~ gsub(" ", "_", .)) %>% 
  rename_with(~ gsub("/", "_", .)) %>% 
  rename_with(~ gsub("-", "_", .)) # Rename variables

# Replace zero values in ranking columns with NA
ranking_columns <- c("Inflight_wifi_service", "Ease_of_Online_booking", "Food_and_drink", 
                     "Online_boarding", "Seat_comfort", "Inflight_entertainment", 
                     "On_board_service", "Leg_room_service", "Baggage_handling", 
                     "Checkin_service", "Inflight_service", "Cleanliness")
data <- data %>% mutate(across(all_of(ranking_columns), ~ na_if(., 0)))

# Remove rows with NA in any ranking column
data <- data %>% drop_na(all_of(ranking_columns))

# Select the first 10,000 rows of data
data <- data[1:10000, ]

# Remove columns with non-descriptive names
if ("...1" %in% colnames(data)) {
  data <- data %>% select(-"...1")
}

# Prepare data for modeling
data$satisfaction <- as.factor(data$satisfaction)
model_data <- data %>% select(-c(id, Gender, Customer_Type, Type_of_Travel, Class)) %>% na.omit()

# Train random forest model for feature importance
set.seed(1)
rf_model <- randomForest(satisfaction ~ ., data = model_data, importance = TRUE)
feature_importance <- as.data.frame(importance(rf_model)) %>% 
  mutate(Feature = rownames(.)) %>% 
  arrange(desc(MeanDecreaseGini))

# Define UI
ui <- fluidPage(
  titlePanel("Airline Passenger Satisfaction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("featureX", "Select X-axis:", 
                  choices = setdiff(names(data), "id"), selected = "Age"),
      selectInput("featureY", "Select Y-axis:", 
                  choices = names(data), selected = "Flight_Distance"),
      selectInput("satisfaction", "Satisfaction Filter:",
                  choices = c("All", "satisfied", "neutral or dissatisfied"),
                  selected = "All"),
      sliderInput("ageRange", "Select Age Range:", min = min(data$Age), 
                  max = max(data$Age), value = range(data$Age))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", 
                 plotlyOutput("scatterPlot"), 
                 DTOutput("scatterDataTable")),
        tabPanel("Pie Chart", 
                 uiOutput("dynamicPieCharts")),
        tabPanel("Bar Plot", 
                 plotlyOutput("barPlot"), 
                 DTOutput("barDataTable")),
        tabPanel("Feature Importance", 
                 plotOutput("featureImportance"), 
                 DTOutput("featureImportanceTable"), 
                 verbatimTextOutput("featureImportanceExplanation")),
        tabPanel("Confusion Matrix (Wait for it to Load)", 
                 plotOutput("confMatrix"), 
                 verbatimTextOutput("confMatrixStats"), 
                 verbatimTextOutput("confMatrixExplanation"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  filteredData <- reactive({
    dataFiltered <- data
    if (input$satisfaction != "All") {
      dataFiltered <- dataFiltered %>% filter(satisfaction == input$satisfaction)
    }
    dataFiltered <- dataFiltered %>% filter(Age >= input$ageRange[1] & Age <= input$ageRange[2])
    return(dataFiltered)
  })
  
  valid_features <- c(ranking_columns, "Customer_Type", "Type_of_Travel", "Class", "Gender")
  
  output$scatterPlot <- renderPlotly({
    gg <- ggplot(filteredData(), aes_string(x = input$featureX, y = input$featureY, color = "satisfaction")) +
      scale_color_manual(values = c("satisfied" = "#61BDC4", "neutral or dissatisfied" = "#E37A6D")) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(x = input$featureX, y = input$featureY, title = "Interactive Scatter Plot")
    ggplotly(gg)
  })
  
  output$scatterDataTable <- renderDT({
    filteredData() %>% select(all_of(c(input$featureX, input$featureY, "satisfaction")))
  })
  
  # Bar Plot rendering logic with updated binning and group name formatting
  output$barPlot <- renderPlotly({
    data_for_plot <- filteredData()
    
    if (input$featureX == "Age") {
      # Group ages by every 5 years with readable labels
      data_for_plot <- data_for_plot %>%
        mutate(grouped_featureX = cut(
          Age, 
          breaks = seq(0, max(Age, na.rm = TRUE) + 5, by = 5), 
          include.lowest = TRUE, 
          labels = paste(
            seq(0, max(Age, na.rm = TRUE), by = 5), 
            seq(5, max(Age, na.rm = TRUE) + 5, by = 5) - 1, 
            sep = "-"
          )
        ))
      
      gg <- ggplot(data_for_plot, aes(x = grouped_featureX, fill = satisfaction)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("satisfied" = "#61BDC4", "neutral or dissatisfied" = "#E37A6D")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Age Group", y = "Count", title = "Bar Plot of Age Groups")
      
    } else if (input$featureX %in% c("Flight_Distance", "Departure_Delay_in_Minutes", "Arrival_Delay_in_Minutes")) {
      # Bin delay and distance variables with readable labels
      bin_width <- ifelse(input$featureX == "Flight_Distance", 100, 10) # Adjust bin widths
      data_for_plot <- data_for_plot %>%
        mutate(grouped_featureX = cut(
          !!sym(input$featureX), 
          breaks = seq(0, max(!!sym(input$featureX), na.rm = TRUE) + bin_width, by = bin_width), 
          include.lowest = TRUE, 
          labels = paste(
            seq(0, max(!!sym(input$featureX), na.rm = TRUE), by = bin_width), 
            seq(bin_width, max(!!sym(input$featureX), na.rm = TRUE) + bin_width, by = bin_width) - 1, 
            sep = "-"
          )
        ))
      
      gg <- ggplot(data_for_plot, aes(x = grouped_featureX, fill = satisfaction)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("satisfied" = "#61BDC4", "neutral or dissatisfied" = "#E37A6D")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = paste(input$featureX, "Group"), y = "Count", title = paste("Bar Plot of", input$featureX, "Groups"))
      
    } else {
      # For categorical variables
      gg <- ggplot(data_for_plot, aes(x = !!sym(input$featureX), fill = satisfaction)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("satisfied" = "#61BDC4", "neutral or dissatisfied" = "#E37A6D")) +
        theme_minimal() +
        labs(x = input$featureX, y = "Count", title = paste("Bar Plot of", input$featureX))
    }
    
    ggplotly(gg)
  })
  
  # Update Data Table rendering logic to include grouped feature columns
  output$barDataTable <- renderDT({
    data_for_table <- filteredData()
    
    if (input$featureX == "Age") {
      data_for_table %>%
        mutate(grouped_featureX = cut(
          Age, 
          breaks = seq(0, max(Age, na.rm = TRUE) + 5, by = 5), 
          include.lowest = TRUE, 
          labels = paste(
            seq(0, max(Age, na.rm = TRUE), by = 5), 
            seq(5, max(Age, na.rm = TRUE) + 5, by = 5) - 1, 
            sep = "-"
          )
        )) %>%
        count(grouped_featureX, satisfaction) %>%
        rename(`Age Group` = grouped_featureX, Count = n)
      
    } else if (input$featureX %in% c("Flight_Distance", "Departure_Delay_in_Minutes", "Arrival_Delay_in_Minutes")) {
      bin_width <- ifelse(input$featureX == "Flight_Distance", 100, 10)
      data_for_table %>%
        mutate(grouped_featureX = cut(
          !!sym(input$featureX), 
          breaks = seq(0, max(!!sym(input$featureX), na.rm = TRUE) + bin_width, by = bin_width), 
          include.lowest = TRUE, 
          labels = paste(
            seq(0, max(!!sym(input$featureX), na.rm = TRUE), by = bin_width), 
            seq(bin_width, max(!!sym(input$featureX), na.rm = TRUE) + bin_width, by = bin_width) - 1, 
            sep = "-"
          )
        )) %>%
        count(grouped_featureX, satisfaction) %>%
        rename(!!paste0(input$featureX, " Group") := grouped_featureX, Count = n)
      
    } else {
      data_for_table %>%
        count(!!sym(input$featureX), satisfaction) %>%
        rename(Count = n)
    }
  })
  
  output$featureImportance <- renderPlot({
    ggplot(feature_importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "#61BDC4") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Feature Importance (Random Forest)", x = "Feature", y = "Mean Decrease Gini")
  })
  
  output$featureImportanceTable <- renderDT({
    feature_importance %>% select(Feature, MeanDecreaseGini)
  })
  
  output$featureImportanceExplanation <- renderText({
    "The Feature Importance plot and table show how much each feature contributes to the prediction of satisfaction levels. Features with higher MeanDecreaseGini values are more important for making accurate predictions\n in the Random Forest model. The values represent how much splitting on each feature reduces impurity in the data."
  })
  
  output$confMatrix <- renderPlot({
    set.seed(1)
    trainIndex <- createDataPartition(model_data$satisfaction, p = 0.7, list = FALSE)
    trainData <- model_data[trainIndex, ]
    testData <- model_data[-trainIndex, ]
    rf_model <- randomForest(satisfaction ~ ., data = trainData)
    predictions <- predict(rf_model, newdata = testData)
    cm <- confusionMatrix(predictions, testData$satisfaction)
    cm_table <- as.data.frame(cm$table)
    
    ggplot(cm_table, aes(Prediction, Reference, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 5) +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal() +
      labs(title = "Confusion Matrix", x = "Prediction", y = "Reference")
  })
  
  output$confMatrixStats <- renderText({
    set.seed(1)
    trainIndex <- createDataPartition(model_data$satisfaction, p = 0.7, list = FALSE)
    trainData <- model_data[trainIndex, ]
    testData <- model_data[-trainIndex, ]
    rf_model <- randomForest(satisfaction ~ ., data = trainData)
    predictions <- predict(rf_model, newdata = testData)
    cm <- confusionMatrix(predictions, testData$satisfaction)
    
    total <- sum(cm$table)
    correct <- sum(diag(cm$table))
    incorrect <- total - correct
    
    paste0("Correct Predictions: ", correct, " (", round(correct / total * 100, 2), "%)\n",
           "Incorrect Predictions: ", incorrect, " (", round(incorrect / total * 100, 2), "%)")
  })
  
  output$confMatrixExplanation <- renderText({
    "The confusion matrix evaluates the Random Forest model's performance. The rows represent the actual classes (satisfied or neutral/dissatisfied), while the columns represent predicted classes. Correct predictions are shown\n on the diagonal, with off-diagonal cells indicating misclassifications. The total and percentages are based on the comparison of predictions to actual labels."
  })
  
  output$dynamicPieCharts <- renderUI({
    req(input$featureX)
    if (!(input$featureX %in% valid_features)) {
      return(h4("Pie charts are only available for ranking and categorical variables. Please select a different X-axis variable."))
    }
    unique_values <- sort(unique(filteredData()[[input$featureX]]), decreasing = TRUE)
    pie_outputs <- lapply(unique_values, function(value) {
      plotname <- paste0("pieChart_", value)
      tagList(
        h4(paste("Value:", value)),
        plotlyOutput(plotname)
      )
    })
    do.call(tagList, pie_outputs)
  })
  
  observe({
    req(input$featureX)
    if (!(input$featureX %in% valid_features)) {
      return()
    }
    unique_values <- sort(unique(filteredData()[[input$featureX]]), decreasing = TRUE)
    lapply(unique_values, function(value) {
      local({
        plotname <- paste0("pieChart_", value)
        filtered_subset <- filteredData() %>% filter(!!sym(input$featureX) == value)
        dataStats <- filtered_subset %>% 
          group_by(satisfaction) %>% 
          summarise(Count = n(), .groups = "drop") %>% 
          mutate(Percentage = round(Count / sum(Count) * 100, 2))
        
        output[[plotname]] <- renderPlotly({
          plot_ly(dataStats, labels = ~satisfaction, values = ~Percentage, type = 'pie',
                  textinfo = 'percent', insidetextorientation = 'radial',
                  marker = list(colors = c("#E37A6D", "#61BDC4"))) %>%
            layout(title = paste("Satisfaction Breakdown for", value), showlegend = TRUE)
        })
      })
    })
  })
}

# Run app
shinyApp(ui = ui, server = server)
