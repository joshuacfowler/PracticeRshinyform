#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

source("helper.R")
mockendodata <- read.csv(file = "~/Documents/R_Projects/PracticeRshinyform/practice/mockendodata.csv")


# Define the fields we want to save from the form
fields <- c("Plot", "ID", "size_t1")


# Define UI for plant demography data collection app ----
ui <- fluidPage(
    
    # App title ----
    headerPanel("LTREB Plants"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        # Input: Selector for variable to plot against mpg ----
        selectInput("variable", "Variable:", 
                    c("Size" = "size_t",
                      "Flowers" = "flw_t")),
        # Select plot number
        selectInput("Plot", "Plot #:", choices = unique(mockendodata$plot)),
        # Select plant ID
        selectInput("ID", "Plant_ID:",  choices = unique(mockendodata$ID)),
        # Input plant size measurment as numeric
        numericInput("size_t1", "Size", NA, min = 1, max = 100),
        # Submit the form
        actionButton("submit", "Submit")
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        # Output: Formatted text for caption ----
        h3(textOutput("caption")),
        
        # Output: Plot of the requested variable against mpg ----
        plotOutput("endoPlot"),
        
        # Output: previous year size and flowering of selected id
        h3(textOutput("caption2")),
        tableOutput("table"),
        
        # Output: Map of the location within the plot ----
        plotOutput("mapPlot"),
        
        
        # Output: Table of all previous responses
        h3(textOutput("caption3")),
        tableOutput("responses")
    )
    
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "ID", choices = as.character(mockendodata[mockendodata$plot==input$Plot, "ID"]))
  
  })
    # Compute the formula text ----
    # This is in a reactive expression since it is shared by the
    # output$caption and output$mpgPlot functions
    formulaText <- reactive({
        paste("endo ~", input$variable)
    })
    
    
    # Return the formula text for printing as a caption ----
    output$caption <- renderText({
        formulaText()
    })
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
  
    output$endoPlot <- renderPlot({
        boxplot(as.formula(formulaText()),
                data = mockendodata,
                col = "#75AADB", pch = 19)
    })
    
    #table of the previous years data for the given ID
    output$caption2 <- renderText({
      paste("year_t measurements from plant #", input$ID)
    })
    
    output$caption3 <- renderText({
      "ID's recorded"
    })
    output$table <- renderTable({ 
        mockendodata %>% 
            filter(ID == input$ID)
    })
  
    output$mapPlot <- renderPlot({
        plot(y_coord ~ x_coord,
                data = subset(mockendodata, plot == input$Plot),
                col = ifelse(ID == input$ID,"#FF0000","#75AADB" ), pch = 19)
    })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    # When the Submit button is clicked, save the form data   
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- renderTable({
      input$submit
      loadData()
    })  
    
}

shinyApp(ui, server)
