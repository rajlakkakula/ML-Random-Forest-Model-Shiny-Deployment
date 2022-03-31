# Import required libraries

library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)
library(AER)
library(flexdashboard)

# Read the random forest built and saved from model.R

model <- readRDS("model.rds")


######### User Interface #############
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  #Page header
  headerPanel('Prediction: Probability of Credit Card Approval'),
  
  #Input values
  sidebarPanel(
    HTML("<h5>Select Input Features</h5>"),
    
    selectInput("owner",
                label = "Owner",
                choices = list("yes" = "yes", "no" = "no"),
                selected = "yes"),
    selectInput("selfemp",
                label = "Selfemployed",
                choices = list("yes" = "yes", "no" = "no"),
                selected = "no"),
    
    #sliderInput("reports", 
    #            label = "Derogatory_Reports",
    #            min = 0,
    #            max = 14,
    #            value = 3),
    sliderInput("age",
                label = "Age",
                min = 1,
                max = 84,
                value = 33),
    sliderInput("income",
                label = "Income",
                min = 0.2,
                max = 14,
                value = 3.5),
    #sliderInput("share",
    #            label = "Share_of_CCard_Exp_to_Yrly_Inc",
    #            min = 0,
    #            max = 1,
    #            value = 0.10),
    #sliderInput("expenditure",
    #            label = "Avg_Mthly_CCard_Expenditure",
    #            min = 0,
    #            max = 3100,
    #            value = 185),
    #sliderInput("dependents",
    #            label = "No_of_Dependents",
    #            min = 0,
    #            max = 6,
    #            value = 2),
    #sliderInput("months",
    #            label = "Months_Living_at_Current_Address",
    #            min = 0,
    #            max = 540,
    #            value = 56),
    #sliderInput("majorcards",
    #            label = "No_of_Major_CCards_held",
    #            min = 0,
    #            max = 1,
    #            value = 0.8),
    #sliderInput("active",
    #            label = "No_of_Active_Credit_Accounts",
    #            min = 0,
    #            max = 46,
    #            value = 7),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
    #submitButton("Submit")
  ),
  
  mainPanel(
    tags$label(h3('Status of Credit Card Application')),
    verbatimTextOutput('contents'),
    tableOutput('tabledata')#, # Prediction results table
    # Output: Histogram ---
    #gaugeOutput('Scale')
  )
)

######### Server #####################

server <- function(input, output, session) {
  
  #Input data
  datasetInput <- reactive({
    
    df <- data.frame(
    Name = c("Owner",
             "Selfemployed",
             #"Derogatory_Reports",
             "Age",
             "Income"
             #"Share_of_CCard_Exp_to_Yrly_Inc",
             #"Avg_Mthly_CCard_Expenditure",
             #"No_of_Dependents",
             #"Months_Living_at_Current_Address",
             #"No_of_Major_CCards_held",
             #"No_of_Active_Credit_Accounts"
             ),
    Value = as.character(c(input$owner,
                           input$selfemp,
                           #input$reports,
                           input$age,
                           input$income
                           #input$share,
                           #input$expenditure,
                           #input$dependents,
                           #input$months,
                           #input$majorcards,
                           #input$active
                           )),
    stringsAsFactors = FALSE)
  
  card <- "card"
  
  
  df <- rbind(df, card)
  input <- transpose(df)
  write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  #write.table(input, "input4.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
  
  #test$Owner <- factor(test$Owner, levels = c("yes", "no"))
  #test$Selfemployed <- factor(test$Selfemployed, levels = c("yes", "no"))
  #test$Age <- numeric(test$Age)
  #test$Income <- numeric(test$Income)
  
  Output <- data.frame(Prediction = predict(model,test), round(predict(model, test, type = "prob"), 3))
  
  print(Output)
  })
  
  #my_plot <- reactive({
  #  gauge(datasetInput()$Yes, min = 0, max = 100, symbol = '%', label = "Approval", gaugeSectors(
  #    success = c(80,100), warning = c(40, 79), danger = c(0, 39)
  #  ))
  #})
  
  #datasetInput()
  #output$Scale <- renderGauge({
  #  my_plot()
  #})

# Probability Text Box
output$contents <- renderPrint({
  if (input$submitbutton>0) {
    isolate("Decision Made.")
  } else {
    return("Click SUBMIT After Selecting the Values of Features")
  }
})

# Prediction results table
output$tabledata <- renderTable({
  if (input$submitbutton>0) {
    isolate(datasetInput())
  }
})
}


######## Create the shiny app #########
shinyApp(ui = ui, server = server)