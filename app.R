#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# Load required libraries
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(knitr)

# define choices for UI
choice_TRT = c("Treated", "Control", "Pooled", "Individual")
choice_plotvariable = c("Visit", "Time")
choice_summary = c("mean", "sd" ,"median", "min", "max", "IQR")
# Function to calculate AUC using the trapezoidal rule
trapezoidal_auc <- function(time, value) {
  n <- length(time)
  time = as.numeric(time)
  auc <- 0
  for (i in 1:(n-1)) {
    auc <- auc + (time[i+1] - time[i]) * (value[i+1] + value[i]) / 2
  }
  return(abs(auc))
}
# function to calculate confidence interval

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
                                   min = 0.5, max = 1.0, value = 0.95, step = 0.01)),
      radioButtons("plotvariable", "Select variable for X-axis",
                   choices = choice_plotvariable,
                   selected = "Visit"),
      
      # Display this only if the plotvariable is Individual further details in server function
      conditionalPanel(condition = "input.Treatment == 'Individual'",
                       uiOutput("individualSelect")),
      
      conditionalPanel(condition = "input.Summary == true",
                       selectInput("SummaryType", "Select Summary Type",
                                   choices = choice_summary,
                                   selected = choice_summary[1]))
,  
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
  
  
output$InteractionPlot =renderPlot({  
  data <- data()
  data_treated <- data %>% filter(treatment == 1)
  data_control <- data %>% filter(treatment == 0)
  fun_CI <-  function(x) {
    data <- mean(x)
    se <- sd(x) / sqrt(length(x))
    ci <- qnorm(1 - (1 - input$CI_level) / 2) * se
    data.frame(y = data, ymin = data - ci, ymax = data + ci)
  }
    switch(input$Treatment,
         "Treated" = {
               if(input$CI == FALSE){switch(input$plotvariable,
                      "Visit" = {ggplot(data_treated , aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                          stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                          stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                          labs(x = "Time",
                               y = paste(input$SummaryType),
                               color = "Visit")+
                          theme_classic()+
                          ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                      },
                      "Time" = {ggplot(data_treated, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                          stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                          stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                          labs(x = "Visit",
                               y = paste(input$SummaryType),
                               color = "Time") +
                          theme_classic()+
                          ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                      }
               )}
              else {switch(input$plotvariable,
                      "Visit" = {ggplot(data_treated , aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                          stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                          stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                          stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                          labs(x = "Time",
                               y = paste(input$SummaryType),
                               color = "Visit")+
                          theme_classic()+
                          ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                      },
                      "Time" = {ggplot(data_treated, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                          stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                          stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                          stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                          labs(x = "Visit",
                               y = paste(input$SummaryType),
                               color = "Time") +
                          theme_classic()+
                          ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                      }
               )
             }
           },
         "Control" = {
           if(input$CI == FALSE){switch(input$plotvariable,
                                        "Visit" = {ggplot( data_control , aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                                            labs(x = "Time",
                                                 y = paste(input$SummaryType),
                                                 color = "Visit")+
                                            theme_classic()+
                                            ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                                        },
                                        "Time" = {ggplot( data_control, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                                            labs(x = "Visit",
                                                 y = paste(input$SummaryType),
                                                 color = "Time") +
                                            theme_classic()+
                                            ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                                        }
           )}
           else {switch(input$plotvariable,
                        "Visit" = {ggplot( data_control , aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                            labs(x = "Time",
                                 y = paste(input$SummaryType),
                                 color = "Visit")+
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                        },
                        "Time" = {ggplot( data_control, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                            labs(x = "Visit",
                                 y = paste(input$SummaryType),
                                 color = "Time") +
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                        }
           )
           }
         },
         "Pooled" =  {
           if(input$CI == FALSE){
             switch(input$plotvariable,
                    "Visit" = {ggplot(data, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Time",
                             y = paste(input$SummaryType),
                             color = "Visit")+
                        theme_classic()+
                        ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                    },
                    "Time" = {ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                        stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                        stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                        labs(x = "Visit",
                             y = paste(input$SummaryType),
                             color = "Time") +
                        theme_classic()+
                        ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                    }
             )}
           else {switch(input$plotvariable,
                        "Visit" = {ggplot(data, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                            labs(x = "Time",
                                 y = paste(input$SummaryType),
                                 color = "Visit")+
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                        },
                        "Time" = {ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            stat_summary(fun.data = fun_CI, geom = "errorbar", width = 0.1) +
                            labs(x = "Visit",
                                 y = paste(input$SummaryType),
                                 color = "Time") +
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the",input$Treatment, "group"))
                        }
           )
           }  
             },
         "Individual" = {
           data<- data %>% filter(ID == input$selectedID)
           switch(input$plotvariable,
                  "Visit" = {
                  ggplot(data, aes(x = Time, y = Measurement, color = as.factor(Visit), group = Visit)) +
                    geom_line(size = 0.5) + geom_point(size = 1.5) +
                    labs(x = "Time",
                         y = paste(input$SummaryType),
                         color = "Visit") +
                    theme_classic() +
                    ggtitle(paste("Interaction plot for Subject", input$selectedID))
                  },
                  "Time" = {
                    ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group = Time)) +
                      geom_line(size = 0.5) + geom_point(size = 1.5) +
                      labs(x = "Visit",
                           y = paste(input$SummaryType),
                           color = "Time") +
                      theme_classic() +
                      ggtitle(paste("Interaction plot for Subject", input$selectedID))
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
             o=data_treated %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b=xtabs(Measurement ~ Visit + Time, data = o)
             if(input$plotvariable=="Visit") b = b
             else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics",input$SummaryType,"for", input$Treatment, "group"))
           },
           "Control" = {
             o=data_control %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b=xtabs(Measurement ~ Visit + Time, data = o)
             if(input$plotvariable=="Visit") b = b
             else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics",input$SummaryType,"for", input$Treatment, "group"))
           },
           "Pooled" = {
             o=data %>% group_by(Visit, Time) %>% summarise_at(vars(Measurement), list(input$SummaryType))
             b=xtabs(Measurement ~ Visit + Time, data = o)
             if(input$plotvariable=="Visit") b = b
             else b = t(b)
             knitr::kable(b, caption = paste("Summary statistics",input$SummaryType,"for", input$Treatment, "group"))
           },
           "Individual" = {
             b = xtabs(Measurement ~ Visit + Time, 
                       data %>% filter(ID == input$selectedID))
             if(input$plotvariable=="Visit") b=b
             else b = t(b)
             knitr::kable(b, caption = paste("Individual record for subject ID:", input$selectedID))
           }
    )
  })


  # AUC Plot-----------------------------------

    # Calculate AUC for each individual ID at each Visit using the trapezoidal rule

output$AUCPlot <- renderPlot({  
                 data <- data()
  auc_trapezoidal<- data %>%
    group_by(ID,Visit) %>%
    summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement))%>%
    ungroup()
  auc_trapezoidal$treatment <- data$treatment[match(auc_trapezoidal$ID, data$ID)]
  auc_trapezoidal$treatment <- factor(auc_trapezoidal$treatment, levels = c(0, 1), labels = c("Control", "Treatment"))
  auc_treated <- auc_trapezoidal %>% filter(treatment == "Treatment")
  auc_control <- auc_trapezoidal %>% filter(treatment == "Control")
  auc_ID <- auc_trapezoidal %>% filter(ID == input$selectedID) %>% ungroup()
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
                 ggplot(auc_trapezoidal, aes(x = Visit, y = auc, color = treatment,group = treatment )) +
                   stat_summary(fun = input$AUC_SummaryType, geom = "line", size = 0.5) +
                   stat_summary(fun = input$AUC_SummaryType, geom = "point", size = 1.5) +
                   labs(x = "Time",
                        y = paste(input$AUC_SummaryType),
                        color = "Group")+
                   theme_classic()+
                   ggtitle(paste("Summary measure",input$AUC_SummaryType,"for AUC in Treatment Groups"))
               },
               "Individual" = {
                 ggplot(auc_ID, aes(x = Visit, y = auc, group = ID)) +
                   geom_line(size = 0.5) + geom_point(size = 1.5) +
                   labs(x = "Visit",
                        y = "AUC",
                        color = "ID") + theme_classic() +
                  ggtitle(paste("AUC plot for subject ID:", input$selectedID))
               }
        )
      })
    
  # AUC Summary -------------------------------------------------------------
output$AUCSummary <- renderPrint({
  data <- data()
  auc_trapezoidal<- data %>%
    group_by(ID,Visit) %>%
    summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement))%>%
    ungroup()
  auc_trapezoidal$treatment <- data$treatment[match(auc_trapezoidal$ID, data$ID)]
  auc_trapezoidal$treatment <- factor(auc_trapezoidal$treatment, levels = c(0, 1), labels = c("Control", "Treatment"))
  auc_treated <- auc_trapezoidal %>% filter(treatment == "Treatment")
  auc_control <- auc_trapezoidal %>% filter(treatment == "Control")
  auc_ID <- auc_trapezoidal %>% filter(ID == input$selectedID)
      switch(input$Treatment,
             "Treated" = {
               o=auc_treated %>% group_by(Visit) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
               b=xtabs(auc ~ Visit, data = o)
               if(input$plotvariable=="Visit") b = b
               else b = t(b)
               knitr::kable(b, caption = paste("Summary statistics",input$AUC_SummaryType,"for AUC in", input$Treatment, "group"))
             },
             "Control" = {
               o=auc_control %>% group_by(Visit) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
               b=xtabs(auc ~ Visit, data = o)
               if(input$plotvariable=="Visit") b = b
               else b = t(b)
               knitr::kable(b, caption = paste("Summary statistics",input$AUC_SummaryType,"for AUC in", input$Treatment, "group"))
             },
             "Pooled" = {
               o=auc_trapezoidal %>% group_by(Visit, treatment) %>% summarise_at(vars(auc), list(input$AUC_SummaryType))
               b=xtabs(auc ~ Visit + treatment, data = o)
               if(input$plotvariable=="Visit") b = b
               else b = t(b)
               knitr::kable(b, caption = paste("Summary statistics",input$AUC_SummaryType,"for AUC in", input$Treatment, "group"))
             },
             "Individual" = {
               b =  xtabs(auc ~ Visit, 
                          auc_trapezoidal %>% filter(ID == input$selectedID))
               if(input$plotvariable=="Visit") b=b
               else b = t(b)
               knitr::kable(b, caption = paste("AUC for subject ID:", input$selectedID, "Group:", input$Treatment))
              
             }
      )
      
})
}
# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)







