# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Define choices for UI
choice_TRT = c("Treated", "Control", "Pooled", "Individual")
choice_plotvariable = c("Visit", "Time")
choice_summary = c("mean", "sd" ,"median", "min", "max", "IQR")

# Function to calculate AUC using the trapezoidal rule
trapezoidal_auc <- function(time, value) {
  n <- length(time)
  auc <- 0
  for (i in 1:(n-1)) {
    auc <- auc + (time[i+1] - time[i]) * (value[i+1] + value[i]) / 2
  }
  return(abs(auc))
}

# User Interface ----------------------------------------------------------------------

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
                       checkboxInput("Summary", "Summary (default: Mean)", value = FALSE)),
      
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
      conditionalPanel(condition = "input.runBtn > 0 && input.hideplot == false",
                       plotOutput("InteractionPlot")),
      conditionalPanel(condition = "input.runBtn > 0",
                       verbatimTextOutput("Summary")),
      conditionalPanel(condition = "input.runBtn > 0 && input.showAUC == true && input.hideplot == false",
                       plotOutput("AUCPlot")),
      conditionalPanel(condition = "input.runBtn > 0 && input.showAUC == true",
                       verbatimTextOutput("AUCSummary"))
    )
  )
)

# Server ------------------------------------------------------------------

# Define server logic
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
  
  output$InteractionPlot <- renderPlot({
    data <- data()
    data_treated <- data %>% filter(treatment == 1)
    data_control <- data %>% filter(treatment == 0)
    switch(input$Treatment,
           "Treated" = {
             switch(input$plotvariable,
                    "Visit" = {
                      ggplot(data_treated , aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                    },
                    "Time" = {
                      ggplot(data_treated, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                    }
             )
           },
           "Control" = {
             switch(input$plotvariable,
                    "Visit" = {
                      ggplot(data_control, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                    },
                    "Time" = {
                      ggplot(data_control, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                    }
             )
           },
           "Pooled" = {
             switch(input$plotvariable,
                    "Visit" = {
                      ggplot(data, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle("Interaction plot for all observations")
                    },
                    "Time" = {
                      ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle("Interaction plot for all observations")
                    }
             )
           },
           "Individual" = {
             switch(input$plotvariable,
                    "Visit" = {
                      ggplot(data %>% filter(ID == input$selectedID), 
                             aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        geom_line(size = 0.5) + geom_point(size = 1.5) +
                        labs(x = "Time",
                             y = "Measurement",
                             color = "Visit") +
                        ggtitle(paste("Subject ID:", input$selectedID, "Assigned arm:", ifelse(as.numeric(unique(data %>% filter(ID == input$selectedID) %>% select(treatment))[1]) == 1, "Treated", "Control"))) +
                        theme_classic()
                    },
                    "Time" = {
                      ggplot(data %>% filter(ID == input$selectedID), 
                             aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        geom_line(size = 0.5) + geom_point(size = 1.5) +
                        labs(x = "Visit",
                             y = "Measurement",
                             color = "Time") +
                        ggtitle(paste("Subject ID:", input$selectedID, "Assigned arm:", ifelse(as.numeric(unique(data %>% filter(ID == input$selectedID) %>% select(treatment))[1]) == 1, "Treated", "Control"))) +
                        theme_classic()
                    }
             )
           }
    )
  })
  
  # Summary statistics ------------------------------------------------------
  
  output$Summary <- renderPrint({
    data <- data()
    data_treated <- data %>% filter(treatment == 1)
    data_control <- data %>% filter(treatment == 0)
    switch(input$Treatment,
           "Treated" = {
             data_treated %>% group_by(Visit) %>% summarise(mean = mean(Measurement),
                                                            sd = sd(Measurement),
                                                            median = median(Measurement),
                                                            min = min(Measurement),
                                                            max = max(Measurement),
                                                            IQR = IQR(Measurement))
           },
           "Control" = {
             data_control %>% group_by(Visit) %>% summarise(mean = mean(Measurement),
                                                            sd = sd(Measurement),
                                                            median = median(Measurement),
                                                            min = min(Measurement),
                                                            max = max(Measurement),
                                                            IQR = IQR(Measurement))
           },
           "Pooled" = {
             data %>% group_by(Visit) %>% summarise(mean = mean(Measurement),
                                                    sd = sd(Measurement),
                                                    median = median(Measurement),
                                                    min = min(Measurement),
                                                    max = max(Measurement),
                                                    IQR = IQR(Measurement))
           },
           "Individual" = {
             data %>% filter(ID == input$selectedID) %>% group_by(Visit) %>% summarise(mean = mean(Measurement),
                                                                                       sd = sd(Measurement),
                                                                                       median = median(Measurement),
                                                                                       min = min(Measurement),
                                                                                       max = max(Measurement),
                                                                                       IQR = IQR(Measurement))
           }
    )
  })
  
  # AUC Calculation and Plot ------------------------------------------------
  
  output$AUCPlot <- renderPlot({
    data <- data()
    data_treated <- data %>% filter(treatment == 1)
    data_control <- data %>% filter(treatment == 0)
    auc_data <- data %>% group_by(ID, treatment) %>% summarise(AUC = trapezoidal_auc(Time, Measurement))
    
    ggplot(auc_data, aes(x = factor(treatment), y = AUC, fill = factor(treatment))) +
      geom_boxplot() +
      labs(x = "Treatment",
           y = "AUC") +
      theme_classic() +
      ggtitle("AUC Boxplot")
  })
  
  output$AUCSummary <- renderPrint({
    data <- data()
    auc_data <- data %>% group_by(ID, treatment) %>% summarise(AUC = trapezoidal_auc(Time, Measurement))
    
    switch(input$AUC_SummaryType,
           "mean" = auc_data %>% group_by(treatment) %>% summarise(mean_AUC = mean(AUC)),
           "sd" = auc_data %>% group_by(treatment) %>% summarise(sd_AUC = sd(AUC)),
           "median" = auc_data %>% group_by(treatment) %>% summarise(median_AUC = median(AUC)),
           "min" = auc_data %>% group_by(treatment) %>% summarise(min_AUC = min(AUC)),
           "max" = auc_data %>% group_by(treatment) %>% summarise(max_AUC = max(AUC)),
           "IQR" = auc_data %>% group_by(treatment) %>% summarise(IQR_AUC = IQR(AUC))
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
