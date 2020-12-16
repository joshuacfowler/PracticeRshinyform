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


mockendodata <- read.csv(file = "/Users/joshuacfowler/Documents/R_projects/PracticeRshinyform/practice/mockendodata.csv")
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
    
    # App title ----
    headerPanel("LTREB Plants"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        # Input: Selector for variable to plot against mpg ----
        selectInput("variable", "Variable:", 
                    c("Size" = "size_t",
                      "Flowers" = "flw_t")),
        selectizeInput("ID", "Plant_ID:", choices = c("choose" = "", levels(as.factor(mockendodata$ID)))
                    ),
        numericInput("size_t1", "Size", NA, min = 1, max = 100),
        verbatimTextOutput("value")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        # Output: Formatted text for caption ----
        h3(textOutput("caption")),
        
        # Output: Plot of the requested variable against mpg ----
        plotOutput("endoPlot"),
        
        # Output: size and flowering of selected id
        tableOutput("table"),
        
        # Output: Map of the location within the plot ----
        plotOutput("mapPlot"),
        
    )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
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
    
    
    output$table <- renderTable({ 
        mockendodata %>% 
            filter(ID == input$ID)
    })
    output$value <- renderText({ input$obs })
    output$mapPlot <- renderPlot({
        plot(y_coord ~ x_coord,
                data = mockendodata,
                col = ifelse(ID == input$ID,"#FF0000","#75AADB" ), pch = 19)
    })
}

shinyApp(ui, server)
