# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# define choices for UI
choice_TRT = c("Treated", "Control", "Pooled", "Individual")
choice_plotvariable = c("Visit", "Time")
choice_summary = c("mean", "sd" ,"median", "min", "max", "IQR")

# UI -----------------------------------------------------------------------

ui <- fluidPage(
  # App Title ---------------------------------------------------------------
  titlePanel("Summary statistics and AUC calculation for Longitudinal data"),
  
  # Set up sidebar for user interaction ---------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      helpText("Upload your CSV file with columns named: ID, Time, Visit, Measurement, treatment"),
      fileInput("file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      tags$hr(),
      
      radioButtons("Treatment", "Select visualization type:",
                   choices = choice_TRT, 
                   selected = choice_TRT[1]),
      
      conditionalPanel(condition = "input.Treatment != 'Individual'",
                       {
                         checkboxInput("Summary", "Summary (default: Mean)", value = FALSE)
                       }
      ),
      conditionalPanel(condition = "input.Summary == false && input.Treatment != 'Individual'",
                       {
                         checkboxInput("CI", "Confidence Interval", value = FALSE)
                       }
      ),
      conditionalPanel(condition = "input.CI == true && input.Summary == false && input.Treatment != 'Individual'",
                       sliderInput("CI_level", "Confidence Interval Level",
                                   min = 0.5, max = 1.0, value = 0.95, step = 0.05)),
      radioButtons("plotvariable", "Select variable for X-axis",
                   choices = choice_plotvariable,
                   selected = "Visit"),
      
      # Display this only if the plotvariable is Individual
      conditionalPanel(condition = "input.Treatment == 'Individual'",
                       uiOutput("individualSelect")),
      
      conditionalPanel(condition = "input.Summary == true",
                       selectInput("SummaryType", "Select Summary Type",
                                   choices = choice_summary,
                                   selected = choice_summary[1])),
      
      # AUC Calculation
      checkboxInput("showAUC", "Area Under Curve (AUC)", value = FALSE),
      
      # Display this only if the AUC is shown
      conditionalPanel(condition = "input.showAUC == true && input.Treatment != 'Individual'",
                       selectInput("AUC_SummaryType", "Select AUC Summary Type",
                                   choices = choice_summary,
                                   selected = choice_summary[1])),
      
      checkboxInput("hideplot", "Hide Plot", value = FALSE),
      actionButton("runBtn", "Run")
    ),
    
    # Main panel for showing results
    mainPanel(
      tabsetPanel(
        tabPanel("Interaction Plot", 
                 conditionalPanel(condition = "input.runBtn > 0 && input.hideplot == false",
                                  plotOutput("InteractionPlot1")),
                 conditionalPanel(condition = "input.runBtn > 0 && input.hideplot == false",
                                  plotOutput("InteractionPlot2"))),
        tabPanel("Summary",
                 conditionalPanel(condition = "input.runBtn > 0",
                                  verbatimTextOutput("Summary"))),
        tabPanel("AUC Plot",
                 conditionalPanel(condition = "input.runBtn > 0 && input.showAUC == true && input.hideplot == false",
                                  plotOutput("AUCPlot1")),
                 conditionalPanel(condition = "input.runBtn > 0 && input.showAUC == true && input.hideplot == false",
                                  plotOutput("AUCPlot2"))),
        tabPanel("AUC Summary",
                 conditionalPanel(condition = "input.runBtn > 0 && input.showAUC == true",
                                  verbatimTextOutput("AUCSummary"))),
        tabPanel("Comparison Table",
                 conditionalPanel(condition = "input.runBtn > 0 && input.Treatment == 'Individual' && input.showComparisonTable",
                                  tableOutput("ComparisonTable")))
      )
    )
  )
)


# Function to calculate AUC using the trapezoidal rule relative to specified baseline Visit and Time
trapezoidal_auc_baseline <- function(data, visit_baseline, time_baseline) {
  data <- data %>%
    filter(Visit == visit_baseline) %>%
    arrange(Time)
  
  baseline_value <- data %>%
    filter(Time == time_baseline) %>%
    pull(Measurement)
  
  time <- data$Time
  value <- data$Measurement
  
  # Find indices where baseline is located
  baseline_index <- which.min(abs(time - time_baseline))
  
  # Subsetting data from baseline to end
  time_sub <- time[baseline_index:length(time)]
  value_sub <- value[baseline_index:length(value)]
  
  n <- length(time_sub)
  auc <- 0
  
  for (i in 1:(n-1)) {
    auc <- auc + (time_sub[i+1] - time_sub[i]) * (value_sub[i+1] + value_sub[i]) / 2
  }
  
  return(abs(auc))
}


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)
  })
  
  output$individualSelect <- renderUI({
    req(data())
    selectInput("selectedID", "Select ID:",
                choices = unique(data()$ID),
                selected = unique(data()$ID)[1])
  })
  
  # Interaction Plot --------------------------------------------------------
  output$InteractionPlot1 <- renderPlot({
    req(input$file, input$runBtn, input$hideplot == FALSE)
    
    data_treated <- data() %>% filter(ID == input$selectedID, treatment == 1)
    
    ggplot(data_treated, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
      stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
      stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
      labs(x = "Time",
           y = paste(input$SummaryType),
           color = "Visit") +
      theme_classic() +
      ggtitle(paste("Interaction plot for all Observation in the", "Treatment 1", "group"))

    data_control <- data() %>% filter(ID == input$selectedID, treatment == 0)
    
    ggplot(data_control, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
      stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
      stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
      labs(x = "Time",
           y = paste(input$SummaryType),
           color = "Visit") +
      theme_classic() +
      ggtitle(paste("Interaction plot for all Observation in the", "Treatment 0", "group"))
  })
  
  # Summary --------------------------------------------------------
  output$Summary <- renderPrint({
    req(input$file, input$runBtn)
    
    if (input$Treatment == "Individual") {
      selected_data <- data() %>% filter(ID == input$selectedID)
      summary(selected_data)
    } else {
      data_summary <- data() %>% group_by(Visit) %>% summarise(
        mean = mean(Measurement, na.rm = TRUE),
        median = median(Measurement, na.rm = TRUE),
        sd = sd(Measurement, na.rm = TRUE),
        n = n()
      )
      data_summary
    }
  })
  
  # AUC Plot --------------------------------------------------------
  output$AUCPlot1 <- renderPlot({
    req(input$file, input$runBtn, input$showAUC, input$hideplot == FALSE)
    
    data_treated <- data() %>% filter(ID == input$selectedID, treatment == 1)
    
    ggplot(data_treated, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
      stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
      stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
      labs(x = "Time",
           y = paste(input$SummaryType),
           color = "Visit") +
      theme_classic() +
      ggtitle(paste("AUC plot for all Observation in the", "Treatment 1", "group"))
  })
  
  output$AUCPlot2 <- renderPlot({
    req(input$file, input$runBtn, input$showAUC, input$hideplot == FALSE)
    
    data_control <- data() %>% filter(ID == input$selectedID, treatment == 0)
    
    ggplot(data_control, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
      stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
      stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
      labs(x = "Time",
           y = paste(input$SummaryType),
           color = "Visit") +
      theme_classic() +
      ggtitle(paste("AUC plot for all Observation in the", "Treatment 0", "group"))
  })
  
  # AUC Summary --------------------------------------------------------
  output$AUCSummary <- renderPrint({
    req(input$file, input$runBtn, input$showAUC)
    
    if (input$Treatment == "Individual") {
      selected_data <- data() %>% filter(ID == input$selectedID)
      AUC_value <- AUC(selected_data$Time, selected_data$Measurement)
      return(AUC_value)
    } else {
      data_summary <- data() %>% group_by(Visit) %>% summarise(
        mean = mean(Measurement, na.rm = TRUE),
        median = median(Measurement, na.rm = TRUE),
        sd = sd(Measurement, na.rm = TRUE),
        n = n()
      )
      data_summary
    }
  })
  
  # Comparison Table --------------------------------------------------------
  output$ComparisonTable <- renderTable({
    req(input$file, input$runBtn, input$Treatment == "Individual")
    
    data_treatment1 <- data() %>% filter(ID == input$selectedID, treatment == 1)
    data_treatment0 <- data() %>% filter(ID == input$selectedID, treatment == 0)
   # print records side by side
    cbind(data_treatment1, data_treatment0)
    
  })
}

shinyApp(ui, server)
