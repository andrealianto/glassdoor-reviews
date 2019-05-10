library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(lubridate)

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
                selectInput("t1_company1",
                            label = "Select company #1",
                            choices = companies,
                            selected = selected1),
                
                selectInput("t1_company2",
                            label = "Select company #2",
                            choices = companies,
                            selected = selected2),
                
                dateRangeInput("t1_daterange",
                               label = "Select date range",
                               start = "2008-06-11",
                               end = "2019-04-13"),
                
                selectInput("t1_country",
                            label = "Select country",
                            choices = countries,
                            selected = "All")
                
            ),
            
            # Main Panel for displaying outputs
            mainPanel(
                fixedRow(
                    column(6,
                           h3(textOutput("t1_company_1"), align = "center"),
                           h5(textOutput("t1_num_reviews_1"), 
                              align = "center", style = "color:grey; font-weight:normal"),
                           # br(),
                           plotOutput("t1_rating_overall_1", height = "200px"),
                           br(),
                           plotOutput("t1_rating_career_1", height = "200px"),
                           br(),
                           plotOutput("t1_rating_compensation_1", height = "200px"),
                           br(),
                           plotOutput("t1_rating_culture_1", height = "200px"),
                           br(),
                           plotOutput("t1_rating_mgmt_1", height = "200px"),
                           br(),
                           plotOutput("t1_rating_balance_1", height = "200px")
                    ),
                    column(6,
                           h3(textOutput("t1_company_2"), align = "center"),
                           h5(textOutput("t1_num_reviews_2"), 
                              align = "center", style = "color:grey; font-weight:normal"),
                           # br(),
                           plotOutput("t1_rating_overall_2", height = "200px"),
                           br(),
                           plotOutput("t1_rating_career_2", height = "200px"),
                           br(),
                           plotOutput("t1_rating_compensation_2", height = "200px"),
                           br(),
                           plotOutput("t1_rating_culture_2", height = "200px"),
                           br(),
                           plotOutput("t1_rating_mgmt_2", height = "200px"),
                           br(),
                           plotOutput("t1_rating_balance_2", height = "200px")
                    )
                )
            )
        )     
    ),
    tabPanel("Rating Trend",
        sidebarLayout(
            # Sidebar Panel for selecting inputs
            sidebarPanel(
                helpText("Compare Glassdoor rating trends of two companies."),
                
                # Dropdown menus: Choose company
                selectInput("t2_company1",
                            label = "Select company #1",
                            choices = companies,
                            selected = selected1),
                
                selectInput("t2_company2",
                            label = "Select company #2",
                            choices = companies,
                            selected = selected2),
                
                radioButtons("t2_ratingcategory",
                             label = "Select category",
                             choices = c("Overall" = "all",
                                         "Career Opportunities" = "career",
                                         "Compensation and Benefits" = "comp",
                                         "Culture and Values" = "culture",
                                         "Senior Management" = "mgmt",
                                         "Work-Life Balance" = "balance"),
                             selected = "all")
                
            ),
            
            # Main Panel for displaying outputs
            mainPanel(
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'all'", plotOutput("t2_trend_all")),
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'career'", plotOutput("t2_trend_career")),
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'comp'", plotOutput("t2_trend_comp")),
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'culture'", plotOutput("t2_trend_culture")),
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'mgmt'", plotOutput("t2_trend_mgmt")),
                conditionalPanel(
                    condition = "input.t2_ratingcategory == 'balance'", plotOutput("t2_trend_balance"))
            )
        )
    ),
    tabPanel("Pros and Cons"),
    tabPanel("Data",
        DT::dataTableOutput("t3_review_table")
    ),
    tabPanel("Credits")
)


# Define server logic ----
server <- function(input, output) ({
    
    ############################################################################
    # TAB 1: Compare Distribution
    
    data1 <- reactive({
        filter_reviews(input_data = reviews,
                       selected_company = input$t1_company1,
                       selected_country = input$t1_country,
                       start_date = as.Date(input$t1_daterange[1]),
                       end_date = as.Date(input$t1_daterange[2]))
    })

    data2 <- reactive({
        filter_reviews(input_data = reviews,
                       selected_company = input$t1_company2,
                       selected_country = input$t1_country,
                       start_date = as.Date(input$t1_daterange[1]),
                       end_date = as.Date(input$t1_daterange[2]))
    })

    ### Name of selected companies
    output$t1_company_1 <- renderText({input$t1_company1})
    output$t1_company_2 <- renderText({input$t1_company2})
    
    
    ### Number of reviews
    output$t1_num_reviews_1 <- renderText({paste("Number of reviews =", nrow(data1()))})
    output$t1_num_reviews_2 <- renderText({paste("Number of reviews =", nrow(data2()))})


    ### 1. Rating distribution: Overall
    output$t1_rating_overall_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_overall", "#ef8a62")
    }, height = 200)

    output$t1_rating_overall_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_overall", "#67a9cf")
    }, height = 200)


    ### 2. Rating distribution: Career Opportunities
    output$t1_rating_career_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_career_opp", "#ef8a62")
    }, height = 200)

    output$t1_rating_career_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_career_opp", "#67a9cf")
    }, height = 200)


    ### 3. Rating distribution: Compensation and Benefits
    output$t1_rating_compensation_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_comp_and_benefits", "#ef8a62")
    }, height = 200)

    output$t1_rating_compensation_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_comp_and_benefits", "#67a9cf")
    }, height = 200)


    ### 4. Rating distribution: Culture and Values
    output$t1_rating_culture_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_culture_and_values", "#ef8a62")
    }, height = 200)

    output$t1_rating_culture_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_culture_and_values", "#67a9cf")
    }, height = 200)


    ### 5. Rating distribution: Senior Management
    output$t1_rating_mgmt_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_senior_mgmt", "#ef8a62")
    }, height = 200)

    output$t1_rating_mgmt_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_senior_mgmt", "#67a9cf")
    }, height = 200)


    ### 6. Rating distribution: Work Life Balance
    output$t1_rating_balance_1 <- renderPlot({
        plot_rating_distribution(data1(), "score_work_life_balance", "#ef8a62")
    }, height = 200)

    output$t1_rating_balance_2 <- renderPlot({
        plot_rating_distribution(data2(), "score_work_life_balance", "#67a9cf")
    }, height = 200)
    
    
    ############################################################################
    # TAB 2: Rating Trend
    
    datainput <- reactive({
        reviews %>% 
            mutate(date = as.Date(date),
                   year = year(date)) %>% 
            select(year, company, starts_with("score")) %>% 
            dplyr::filter((company == input$t2_company1) | (company == input$t2_company2)) %>% 
            group_by(year, company) %>% 
            summarise(avg_overall = mean(score_overall, na.rm = TRUE),
                      avg_career = mean(score_career_opp, na.rm = TRUE),
                      avg_comp = mean(score_comp_and_benefits, na.rm = TRUE),
                      avg_culture = mean(score_culture_and_values, na.rm = TRUE),
                      avg_mgmt = mean(score_senior_mgmt, na.rm = TRUE),
                      avg_balance = mean(score_work_life_balance, na.rm = TRUE))
    })
    
    output$t2_trend_all <- renderPlot({
        plot_rating_trend(datainput(), "avg_overall")}, height = 600)
    
    output$t2_trend_career <- renderPlot({
        plot_rating_trend(datainput(), "avg_career")}, height = 600)
    
    output$t2_trend_comp <- renderPlot({
        plot_rating_trend(datainput(), "avg_comp")}, height = 600)
    
    output$t2_trend_culture <- renderPlot({
        plot_rating_trend(datainput(), "avg_culture")}, height = 600)
    
    output$t2_trend_mgmt <- renderPlot({
        plot_rating_trend(datainput(), "avg_mgmt")}, height = 600)
    
    output$t2_trend_balance <- renderPlot({
        plot_rating_trend(datainput(), "avg_balance")}, height = 600)
    
    
    ############################################################################
    # TAB 3: Data
    output$t3_review_table <- DT::renderDataTable({
        DT::datatable(data1()[,c(2:5)])
    })

})

shinyApp(ui = ui, server = server)