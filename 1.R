library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)

# Define choices for UI
choice_TRT = c("Treated", "Control", "Pooled", "Individual")
choice_plotvariable = c("Visit", "Time")
choice_summary = c("mean", "sd", "median", "min", "max", "IQR")

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
      conditionalPanel(condition = "input.showAUC == true",
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
    req(input$runBtn)
    data <- data()
    data_treated <- data %>% filter(treatment == "Treated")
    data_control <- data %>% filter(treatment == "Control")
    
    switch(input$Treatment,
           "Treated" = {
             switch(input$plotvariable,
                    "Visit" = {ggplot(data_treated, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observations in the", input$Treatment, "group"))
                    },
                    "Time" = {ggplot(data_treated, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observations in the", input$Treatment, "group"))
                    })
           },
           "Control" = {
             switch(input$plotvariable,
                    "Visit" = {ggplot(data_control, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observations in the", input$Treatment, "group"))
                    },
                    "Time" = {ggplot(data_control, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle(paste("Interaction plot for all Observations in the", input$Treatment, "group"))
                    })
           },
           "Pooled" = {
             switch(input$plotvariable,
                    "Visit" = {ggplot(data, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit") +
                        theme_classic() +
                        ggtitle("Interaction plot for all observations")
                    },
                    "Time" = {ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic() +
                        ggtitle("Interaction plot for all observations")
                    })
           },
           "Individual" = {
             individual_data <- data %>% filter(ID == input$selectedID)
             switch(input$plotvariable,
                    "Visit" = {ggplot(individual_data, 
                                      aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                        geom_line(size = 0.5) + geom_point(size = 1.5) +
                        labs(x = "Time",
                             y = "Measurement",
                             color = "Visit") +
                        ggtitle(paste("Subject ID:", input$selectedID, 
                                      " Assigned arm:", 
                                      ifelse(as.numeric(unique(individual_data$treatment)[1]) == 1, "Treated", "Control"))) +
                        theme_classic()
                    },
                    "Time" = {ggplot(individual_data, 
                                     aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                        geom_line(size = 0.5) + geom_point(size = 1.5) + 
                        labs(x = "Visit",
                             y = "Measurement",
                             color = "Time") +
                        ggtitle(paste("Subject ID:", input$selectedID, 
                                      " Assigned arm:", 
                                      ifelse(as.numeric(unique(individual_data$treatment)[1]) == 1, "Treated", "Control"))) +
                        theme_classic()
                    })
           })
  })
  
  # Summary statistics ------------------------------------------------------
  output$Summary <- renderPrint({
    req(input$runBtn)
    data <- data()
    data_treated <- data %>% filter(treatment == "Treated")
    data_control <- data %>% filter(treatment == "ControlControl")
    data_pooled <- data
    
    switch(input$Treatment,
           "Treated" = {
             o <- data_treated %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b <- xtabs(Measurement ~ Visit + Time, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$SummaryType, "for", input$Treatment, "group"))
           },
           "Control" = {
             o <- data_control %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b <- xtabs(Measurement ~ Visit + Time, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$SummaryType, "for", input$Treatment, "group"))
           },
           "Pooled" = {
             o <- data_pooled %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b <- xtabs(Measurement ~ Visit + Time, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$SummaryType, "for", input$Treatment, "group"))
           },
           "Individual" = {
             individual_data <- data %>% filter(ID == input$selectedID)
             b <- xtabs(Measurement ~ Visit + Time, individual_data)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Individual record for subject ID:", input$selectedID, "Group:", ifelse(as.numeric(unique(individual_data$treatment)[1]) == 1, "Treated", "Control")))
           }
    )
  })
  
  # AUC Plot-----------------------------------
  output$AUCPlot <- renderPlot({
    req(input$runBtn)
    data <- data()
    
    # Calculate AUC for each individual ID at each Visit using the trapezoidal rule
    auc_trapezoidal <- data %>%
      group_by(ID, Visit) %>%
      summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
      ungroup()
    auc_trapezoidal$treatment <- data$treatment[match(auc_trapezoidal$ID, data$ID)]
    auc_trapezoidal$treatment <- factor(auc_trapezoidal$treatment, levels = c(0, 1), labels = c("Control", "Treatment"))
    auc_treated <- auc_trapezoidal %>% filter(treatment == "Treatment")
    auc_control <- auc_trapezoidal %>% filter(treatment == "Control")
    
    switch(input$Treatment,
           "Treated" = {
             ggplot(auc_treated, aes(x = Visit, y = auc, color = ID, group = ID)) +
               geom_line(size = 0.5) + geom_point(size = 1.5) +
               labs(x = "Visit",
                    y = "AUC",
                    color = "ID") +
               ggtitle("AUC plot for all observations in the Treated group") +
               theme_classic()
           },
           "Control" = {
             ggplot(auc_control, aes(x = Visit, y = auc, color = ID, group = ID)) +
               geom_line(size = 0.5) + geom_point(size = 1.5) +
               labs(x = "Visit",
                    y = "AUC",
                    color = "ID") +
               ggtitle("AUC plot for all observations in the Control group") +
               theme_classic()
           },
           "Pooled" = {
             ggplot(auc_trapezoidal, aes(x = Visit, y = auc, color = treatment, group = treatment)) +
               stat_summary(fun = input$AUC_SummaryType, geom = "line", size = 0.5) +
               stat_summary(fun = input$AUC_SummaryType, geom = "point", size = 1.5) +
               labs(x = "Time",
                    y = paste(input$AUC_SummaryType),
                    color = "Group") +
               theme_classic() +
               ggtitle(paste("Summary measure", input$AUC_SummaryType, "for AUC in Treatment Groups"))
           },
           "Individual" = {
             ggplot(data %>% filter(ID == input$selectedID) %>%
                      group_by(ID, Visit) %>%
                      summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
                      ungroup(), aes(x = Visit, y = auc, color = ID, group = ID)) +
               geom_line(size = 0.5) + geom_point(size = 1.5) +
               labs(x = "Visit",
                    y = "AUC",
                    color = "ID") +
               ggtitle(paste("AUC plot for subject ID:", input$selectedID, "Group:", ifelse(as.numeric(unique(data %>% filter(ID == input$selectedID) %>% select(treatment))[1]) == 1, "Treated", "Control"))) +
               theme_classic()
           }
    )
  })
  
  # AUC Summary -------------------------------------------------------------
  output$AUCSummary <- renderPrint({
    req(input$runBtn)
    data <- data()
    
    switch(input$Treatment,
           "Treated" = {
             o <- auc_treated %>% group_by(Visit) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
             b <- xtabs(auc ~ Visit, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$AUC_SummaryType, "for AUC in", input$Treatment, "group"))
           },
           "Control" = {
             o <- auc_control %>% group_by(Visit) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
             b <- xtabs(auc ~ Visit, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$AUC_SummaryType, "for AUC in", input$Treatment, "group"))
           },
           "Pooled" = {
             o <- auc_trapezoidal %>% group_by(Visit, treatment) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
             b <- xtabs(auc ~ Visit + treatment, data = o)
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics", input$AUC_SummaryType, "for AUC in", input$Treatment, "group"))
           },
           "Individual" = {
             b <- xtabs(auc ~ Visit, 
                        data %>% filter(ID == input$selectedID) %>%
                          group_by(ID, Visit) %>%
                          summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
                          ungroup()
             )
             if (input$plotvariable == "Visit") b = b else b = t(b)
             knitr::kable(b, caption = paste("AUC for subject ID:", input$selectedID, "Group:", ifelse(as.numeric(unique(data %>% filter(ID == input$selectedID) %>% select(treatment))[1]) == 1, "Treated", "Control")))
           }
    )
  })
}

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

