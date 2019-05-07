library(shiny)
library(DT)
library(tidyverse)

source("helpers.R")
reviews <- read.csv("data/reviews_190428.csv")
companies <- c("Euromonitor",
               "GfK",
               "Kantar Worldpanel",
               "Millward Brown",
               "Mintel",
               "Nielsen",
               "TNS Global")
selected1 <- companies[1]
selected2 <- companies[2]

# Define UI for app

ui <- navbarPage(
    "Glassdoor Reviews",
    tabPanel("Compare Companies",
        sidebarLayout(
            # Sidebar Panel for selecting inputs
            sidebarPanel(
                helpText("Compare Glassdoor reviews of two companies."),
                
                # Dropdown menus: Choose company
                selectInput("company1",
                            label = "Select company #1",
                            choices = companies,
                            selected = selected1),
                
                selectInput("company2",
                            label = "Select company #2",
                            choices = companies,
                            selected = selected2),
                
                dateRangeInput("daterange",
                               label = "Select date range",
                               start = "2008-06-11",
                               end = "2019-04-13")
            ),
            
            # Main Panel for displaying outputs
            mainPanel(
                textOutput("text"),
                plotOutput("rating_overall")
            )
        )     
    ),
    tabPanel("Jaccard Similarity"),
    tabPanel("Data",
        DT::dataTableOutput("review-table")
    ),
    tabPanel("Credits")
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

    output$rating_overall <- renderPlot({
        ggplot(subset(data(), !is.na(score_overall)),
               aes(x = score_overall, fill = company)) +
            geom_bar(width = 0.5,
                     position = "dodge") +
            labs(x = "Score",
                 y = "Count",
                 title = paste("Rating Distribution of", input$company1,
                               "VS", input$company2))
    })

})

shinyApp(ui = ui, server = server)