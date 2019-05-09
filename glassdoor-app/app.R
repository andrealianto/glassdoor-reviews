library(shiny)
library(DT)
library(tidyverse)
library(scales)

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
countries <- c("All", levels(reviews$country))

# Define UI for app

ui <- navbarPage(
    "Glassdoor Reviews",
    tabPanel("Rating Distribution",
        sidebarLayout(
            # Sidebar Panel for selecting inputs
            sidebarPanel(
                helpText("Compare Glassdoor ratings of two companies."),
                
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
                               end = "2019-04-13"),
                
                selectInput("country",
                            label = "Select country",
                            choices = countries,
                            selected = "All")
                
            ),
            
            # Main Panel for displaying outputs
            mainPanel(
                fixedRow(
                    column(6,
                           h3(textOutput("company_1"), align = "center"),
                           h5(textOutput("num_reviews_1"), 
                              align = "center", style = "color:grey; font-weight:normal"),
                           br(),
                           plotOutput("rating_overall_1"),
                           br(),
                           plotOutput("rating_career_1"),
                           br(),
                           plotOutput("rating_compensation_1"),
                           br(),
                           plotOutput("rating_culture_1"),
                           br(),
                           plotOutput("rating_mgmt_1"),
                           br(),
                           plotOutput("rating_balance_1")
                    ),
                    column(6,
                           h3(textOutput("company_2"), align = "center"),
                           h5(textOutput("num_reviews_2"), 
                              align = "center", style = "color:grey; font-weight:normal"),
                           br(),
                           plotOutput("rating_overall_2"),
                           br(),
                           plotOutput("rating_career_2"),
                           br(),
                           plotOutput("rating_compensation_2"),
                           br(),
                           plotOutput("rating_culture_2"),
                           br(),
                           plotOutput("rating_mgmt_2"),
                           br(),
                           plotOutput("rating_balance_2")
                    )
                )
            )
        )     
    ),
    tabPanel("Rating Trend"),
    tabPanel("Pros and Cons"),
    tabPanel("Data",
        DT::dataTableOutput("review_table")
    ),
    tabPanel("Credits")
)


# Define server logic required to draw a histogram ----
server <- function(input, output) ({
    
    data1 <- reactive({
        filter_reviews(input_data = reviews,
                       selected_company = input$company1,
                       selected_country = input$country,
                       start_date = as.Date(input$daterange[1]),
                       end_date = as.Date(input$daterange[2]))
    })

    data2 <- reactive({
        filter_reviews(input_data = reviews,
                       selected_company = input$company2,
                       selected_country = input$country,
                       start_date = as.Date(input$daterange[1]),
                       end_date = as.Date(input$daterange[2]))
    })

    #####################################################################
    # TAB 1: Compare Distribution
    
    ### Name of selected companies
    output$company_1 <- renderText({input$company1})
    output$company_2 <- renderText({input$company2})
    
    
    ### Number of reviews
    output$num_reviews_1 <- renderText({paste("Number of reviews =", nrow(data1()))})
    output$num_reviews_2 <- renderText({paste("Number of reviews =", nrow(data2()))})


    ### 1. Rating distribution: Overall
    output$rating_overall_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_overall", "#ef8a62")
    })

    output$rating_overall_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_overall", "#67a9cf")
    })


    ### 2. Rating distribution: Career Opportunities
    output$rating_career_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_career_opp", "#ef8a62")
    })

    output$rating_career_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_career_opp", "#67a9cf")
    })


    ### 3. Rating distribution: Compensation and Benefits
    output$rating_compensation_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_comp_and_benefits", "#ef8a62")
    })

    output$rating_compensation_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_comp_and_benefits", "#67a9cf")
    })


    ### 4. Rating distribution: Culture and Values
    output$rating_culture_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_culture_and_values", "#ef8a62")
    })

    output$rating_culture_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_culture_and_values", "#67a9cf")
    })


    ### 5. Rating distribution: Senior Management
    output$rating_mgmt_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_senior_mgmt", "#ef8a62")
    })

    output$rating_mgmt_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_senior_mgmt", "#67a9cf")
    })


    ### 6. Rating distribution: Work Life Balance
    output$rating_balance_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_work_life_balance", "#ef8a62")
    })

    output$rating_balance_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_work_life_balance", "#67a9cf")
    })
    
    #####################################################################
    # TAB 3: Data
    output$review_table <- DT::renderDataTable({
        DT::datatable(data1()[,c(2:5)])
    })

})

shinyApp(ui = ui, server = server)