library(shiny)
library(caret)
library(e1071)

# Load data
load("data/msgTags.Rda")

# Prepare data for fitting
df0 <- df[, colSums(df!=0) != 0]
df0 <- df0[, -c(2,3,4)] # remove 'rules' field
df0[df0 > 1] <- 1 

df0$isSpam <- as.factor(df0$isSpam)

inTrain <- createDataPartition(y=df0$isSpam, p=0.7, list=FALSE)

train <- df0[inTrain, ]
test <- df0[-inTrain,]

selectTags <- function(x) {
  if (x == 1) {
    glm(isSpam ~ BAYES_HAM, family = binomial(link="logit"), data=train)
  } else if (x == 2) {
    glm(isSpam ~ MISSING_SUBJECT + FORGED_SENDER, family = binomial(link="logit"), data=train)
  } else if (x == 3) {
    glm(isSpam ~ MISSING_SUBJECT + FORGED_SENDER + MIME_HTML_ONLY, family = binomial(link="logit"), data=train)
  } else if (x == 4) {
    glm(isSpam ~ MISSING_SUBJECT + FORGED_SENDER + MIME_HTML_ONLY + HTML_SHORT_LINK_IMG_1, family = binomial(link="logit"), data=train)
  } else if (x == 5) {
    glm(isSpam ~ MISSING_SUBJECT + FORGED_SENDER + MIME_HTML_ONLY + HTML_SHORT_LINK_IMG_1 + BAYES_SPAM, family = binomial(link="logit"), data=train)
  } else {
    glm(isSpam ~ ., family = binomial(link="logit"), data=train)
  }
}

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested radio button
  modFitTags <- reactive({
    switch(input$radio,
           "1" = selectTags(1),
           "2" = selectTags(2),
           "3" = selectTags(3),
           "4" = selectTags(4),
           "5" = selectTags(5),
           "6" = selectTags(6)
           )   
  })
  
  sliderVal <- reactive({
    as.numeric(input$threshold)
  })

  # Generate a summary of the dataset
  output$summary <- renderPrint({
    modFit <- modFitTags()
    summary(modFit)
  })
  
  # Find accuracy
  output$accuracy <- renderPrint({
    modFit <- modFitTags()
    threshold <- sliderVal()
    predictions <- predict(modFit, newdata=test, type="response")
    predictions[predictions <= threshold ] <- 0
    predictions[predictions > threshold ] <- 1
    confusionMatrix(predictions, test$isSpam)
  })
  
  # Plot predictions
  output$plotPredictions <- renderPlot({
    modFit <- modFitTags()
    threshold <- sliderVal()
    predictions <- predict(modFit, newdata=test, type="response")
    plot(predictions, col=test$isSpam, pch=21, main="Spam predictions (red - spam / black - ham)")
    abline(h=threshold, lwd=3, col="blue")
  })
  
  # Show the first "n" observations
  output$plotResiduals <- renderPlot({
    modFit <- modFitTags()
    par(mfrow=c(2,2))
    plot(modFit)
  })
  
  # Show Spam TAGS
  output$spamTags <- renderPrint({
    ### Check the significant predictors
    summary <- summary(modFit)
    summary(modFit)$coefficients[summary$coefficients[,4] <= 0.05,]
  })
  
  
})
