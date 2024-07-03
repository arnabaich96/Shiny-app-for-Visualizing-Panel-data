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
library(ggplot2)
library(dplyr)
library(readr)
data <- read.csv("panel_data.csv")
# split dataset into treated and control
data_treated <- data %>% filter(treatment == 1)
data_control <- data %>% filter(treatment == 0)

# define choices for UI
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

# set up sidebar for user interaction ---------------------------------------------------

  
  sidebarLayout(
    sidebarPanel(
      helpText("The dataset must have columns named: ID, Time, Visit, Measurement")
    ,
  
    radioButtons("Treatment", "Select visualization type:",
                 choices = choice_TRT 
                 ,selected = choice_TRT[1])
    
    ,
conditionalPanel(condition = "input.Treatment != 'Individual'",
                 checkboxInput("Summary", "Summary (default: Mean)", value = FALSE)), 
radioButtons("plotvariable", "Select variable for X-axis",
                  choices = choice_plotvariable
                  ,selected = "Visit"),
# Display this only if the plotvariable is Individual
conditionalPanel(condition = "input.Treatment == 'Individual'",
                   selectInput("selectedID", "Select ID:",
                               choices = unique(data$ID),
                               selected = unique(data$ID)[1]
                 )),
conditionalPanel(condition = "input.Summary ==true",
                 selectInput("SummaryType", "Select Summary Type",
                             choices = choice_summary,
                             selected = choice_summary[1])
                 ),
# Auc Calculation---------------------------------------------------------
checkboxInput("showAUC", "AUC", value = FALSE),
# Display this only if the AUC is shown
conditionalPanel(condition = "input.showAUC == true",
                 checkboxInput ("sum_AUC","AUC_Summary", value = FALSE)),
actionButton("runBtn", "Run")
    ),
    
# main panel for showing results
    mainPanel(
                       uiOutput("conditionalUI"),
                       plotOutput("InteractionPlot",)
      
              )
  )
)



# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output) {
  
  # data <- reactive({
  #   req(input$file)
  #   df <- read.csv(input$file$datapath)
  #   return(df)
  # })
  
  output$conditionalUI<-renderUI({
    
    req(input$runBtn) # wait for the button to be clicked
    
    if(input$Summary == TRUE) verbatimTextOutput("Summary") 
    if(input$showAUC == TRUE) 
    {
      plotOutput("AUCPlot")
      if(input$ShowAUC == TRUE) verbatimTextOutput("AUCSummary")
    }
# Interaction Plot --------------------------------------------------------

    
switch(input$Treatment,
           "Treated" = {
             output$InteractionPlot =
               renderPlot({ 
                 switch(input$plotvariable,
                        "Visit" = {ggplot(data_treated , aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
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
                 )
               })
           },
           "Control" = {
             output$InteractionPlot =
               renderPlot({ 
                 switch(input$plotvariable,
                        "Visit" = {ggplot(data_control, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            labs(x = "Time",
                                 y = paste(input$SummaryType),
                                 color = "Visit")+
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                        },
                        "Time" = {ggplot(data_control, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            labs(x = "Visit",
                                 y = paste(input$SummaryType),
                                 color = "Time") +
                            theme_classic()+
                            ggtitle(paste("Interaction plot for all Observation in the", input$Treatment, "group"))
                        }
                 )
               })
           },
           "Pooled" = {
             output$InteractionPlot =
               renderPlot({ 
                 switch(input$plotvariable,
                        "Visit" = {ggplot(data, aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            labs(x = "Time",
                                 y = paste(input$SummaryType),
                                 color = "Visit")+
                            theme_classic()+
                            ggtitle("Interaction plot for all observations")
                        },
                        "Time" = {ggplot(data, aes(x = Visit, y = Measurement, color = as.factor(Time), group =Time )) +
                            stat_summary(fun = input$SummaryType, geom = "line", size = 0.5) +
                            stat_summary(fun = input$SummaryType, geom = "point", size = 1.5) +
                            labs(x = "Visit",
                                 y = paste(input$SummaryType),
                                 color = "Time") +
                            theme_classic()+
                            ggtitle("Interaction plot for all observations")
                        }
                 )
               })
           },
           "Individual" = {
             output$InteractionPlot =
               renderPlot({ggplot(data %>% filter(ID == input$selectedID) , 
                              aes(x = Time, y = Measurement, color = Visit, group = Visit)) +
           geom_line( size = 0.5) + geom_point(size = 1.5) +
           labs(x = "Time",
                y = "Measurement",
                color = "Visit")+
           ggtitle(paste("Subject ID:", input$selectedID,"     Assigned arm:",ifelse(as.numeric(unique(data %>% filter(ID ==input$selectedID)
                                                                                                       %>%select(treatment))[1])==1, "Treated","Control")))+
           theme_classic()}
           )}
    )
    
# Summary statistics ------------------------------------------------------

    output$Summary <- renderPrint({
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
                         data %>% filter(ID == input$selectedID)
                         )
               if(input$plotvariable=="Visit") b=b
               else b = t(b)
               knitr::kable(b, caption = paste("Summary statistics",input$SummaryType,"for", input$Treatment, "with subject ID:", input$selectedID))
  
             }
      )
  })

# AUC calculation using trapezoidal Rule-----------------------------------

    
})
}

# Run App -----------------------------------------------------------------

# run the application
shinyApp(ui = ui, server = server)







