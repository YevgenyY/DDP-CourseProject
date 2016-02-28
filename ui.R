library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  # Application title
  headerPanel("Email/Spam predictors analysis"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    radioButtons("radio", label = h4("Choose SPAM TAGS/predictors:"),
                 choices = list("All TAGS" = 6, "1 spamTAGS/predictor" = 1,
                                "2 spamTAGS/predictors" = 2, "3 spamTAGS/predictors" = 3,
                                "4 spamTAGS/predictors" = 4, "5 spamTAGS/predictors" = 5),
                 selected = 6),
    sliderInput("threshold", "Spam detection threshold:",  
                min = 0.1, max = 1, value = 0.5)
    ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    
    tabsetPanel(
      tabPanel("Plot", plotOutput("plotResiduals")),
      tabPanel("Accuracy", plotOutput("plotPredictions"),
               h4("Model accuracy:"), verbatimTextOutput("accuracy")), 
      tabPanel("Summary", h4("Model summary:"), verbatimTextOutput("summary")),
      tabPanel("Spam TAGS", h4("Most significant predictors/TAGS:"), verbatimTextOutput("spamTags"))
    )
  )
))
