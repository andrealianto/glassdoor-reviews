library(shiny)
library(DT)
library(tidyverse)

source("helpers.R")
reviews <- read.csv("data/reviews_190428.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel(h3("Glassdoor Reviews")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            helpText("Compare Glassdoor reviews of market research companies."),
            
            # Dropdown menu: Choose company
            selectInput("company1",
                        label = "Select company #1",
                        choices = c("Euromonitor", 
                                    "GfK", 
                                    "Kantar Worldpanel",
                                    "Millward Brown",
                                    "Mintel",
                                    "Nielsen",
                                    "TNS Global"),
                        selected = "Euromonitor"),
            
            selectInput("company2",
                        label = "Select company #2",
                        choices = c("Euromonitor", 
                                    "GfK", 
                                    "Kantar Worldpanel",
                                    "Millward Brown",
                                    "Mintel",
                                    "Nielsen",
                                    "TNS Global"),
                        selected = "Euromonitor"),
            
            dateRangeInput("daterange",
                           label = "Select date range",
                           start = "2008-06-11",
                           end = "2019-04-13")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            textOutput("selected_company"),
            # DT::dataTableOutput("review_table"),
            plotOutput("score1"),
            plotOutput("score2")
            
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) ({
    
    data <- reactive({
        filter_reviews(data = reviews, 
                       company1 = input$company1, 
                       company2 = input$company2,
                       start_date = as.Date(input$daterange[1]),
                       end_date = as.Date(input$daterange[2]))
    }) 
    
    # For debugging
    # output$review_table <- DT::renderDataTable({
    #     DT::datatable(data()[,c(2:5)])
    # })
    
    output$score1 <- renderPlot({
        ggplot(subset(data(), !is.na(score_overall)), 
               aes(x = score_overall, fill = company)) +
            geom_bar(width = 0.5,
                     position = "dodge") +
            labs(x = "Score",
                 y = "Count",
                 title = paste("Score Distribution of", input$company1,
                               "VS", input$company2))
    })
    
})

shinyApp(ui = ui, server = server)