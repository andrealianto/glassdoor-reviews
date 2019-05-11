library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(tidyr)
library(tidytext)
library(shinythemes)

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

ui <- navbarPage(theme = shinytheme("cosmo"),
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
    tabPanel("Pros and Cons",
         sidebarLayout(
             # Sidebar Panel for selecting inputs
             sidebarPanel(
                 helpText("Compare pros and cons of two companies."),
                 
                 # Dropdown menus: Choose company
                 selectInput("t3_company1",
                             label = "Select company #1",
                             choices = companies,
                             selected = selected1),
                 
                 selectInput("t3_company2",
                             label = "Select company #2",
                             choices = companies,
                             selected = selected2)
                 
             ),
             
             # Main Panel for displaying outputs
             mainPanel(
                 fixedRow(
                     column(6,
                            h3(textOutput("t3_company1"), align = "center"),
                            h5(textOutput("t3_num_reviews_1"), 
                               align = "center", style = "color:grey; font-weight:normal"),
                            plotOutput("t3_pros_1"),
                            br(),
                            plotOutput("t3_cons_1"),
                            br(),
                            plotOutput("t3_advice_1")
                     ),
                     column(6,
                            h3(textOutput("t3_company2"), align = "center"),
                            h5(textOutput("t3_num_reviews_2"), 
                               align = "center", style = "color:grey; font-weight:normal"),
                            plotOutput("t3_pros_2"),
                            br(),
                            plotOutput("t3_cons_2"),
                            br(),
                            plotOutput("t3_advice_2")
                     )
                 )
             )
         )
    ),
    tabPanel("About",
        fluidRow(
            column(7,
                   h1(strong("About")),
                   
                   p("This Shiny app is for comparing Glassdoor reviews of two market research companies"),
                   p("The dataset is scraped from",
                     a("Glassdoor.com", href = "https://www.glassdoor.sg/index.htm"), 
                     ", and contains 6,414 reviews for 7 companies  from June 2008 to March 2019."),
                   
                   p("Access the full project ",
                      a("here", href = "https://github.com/andreamarsha/glassdoor-reviews"), "."),
                   
                   br(),
                   br(),
                   
                   h4(strong("Credits")),
                   p("1. Data source: ", a("Glassdoor", href = "https://www.glassdoor.sg/index.htm")),
                   p("2. Webscraping tool: ", a("Webscraper.io", href = "https://www.webscraper.io/"))
                   
                   
            )
        )
  
    )
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
        plot_rating_trend(datainput(), "avg_overall")})
    
    output$t2_trend_career <- renderPlot({
        plot_rating_trend(datainput(), "avg_career")})
    
    output$t2_trend_comp <- renderPlot({
        plot_rating_trend(datainput(), "avg_comp")})
    
    output$t2_trend_culture <- renderPlot({
        plot_rating_trend(datainput(), "avg_culture")})
    
    output$t2_trend_mgmt <- renderPlot({
        plot_rating_trend(datainput(), "avg_mgmt")})
    
    output$t2_trend_balance <- renderPlot({
        plot_rating_trend(datainput(), "avg_balance")})
    
    
    ############################################################################
    # TAB 3: Pros and Cons Analysis
    
    data_text1 <- reactive({
        reviews %>% 
            dplyr::filter(company == input$t3_company1) %>% 
            select(X, pros, cons, advice_to_management) %>% 
            mutate(pros = as.character(pros),
                   cons = as.character(cons),
                   advice_to_management = as.character(advice_to_management)) 
            
    })
    
    data_text2 <- reactive({
        reviews %>% 
            dplyr::filter(company == input$t3_company2) %>% 
            select(X, pros, cons, advice_to_management) %>% 
            mutate(pros = as.character(pros),
                   cons = as.character(cons),
                   advice_to_management = as.character(advice_to_management))
    })
    
    data_pro1 <- reactive({
        data_text1() %>% 
            select(X, pros) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, pros, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    data_pro2 <- reactive({
        data_text2() %>% 
            select(X, pros) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, pros, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    data_cons1 <- reactive({
        data_text1() %>% 
            select(X, cons) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, cons, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    data_cons2 <- reactive({
        data_text2() %>% 
            select(X, cons) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, cons, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    data_advice1 <- reactive({
        data_text1() %>% 
            select(X, advice_to_management) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, advice_to_management, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    data_advice2 <- reactive({
        data_text2() %>% 
            select(X, advice_to_management) %>% 
            
            # Break text into bigrams
            unnest_tokens(bigram, advice_to_management, token = "ngrams", n = 2, drop = FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% 
            
            # Remove bigrams containing stop words
            filter(!word1 %in% stop_words$word & !is.na(word1)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            filter(!word2 %in% stop_words$word & !is.na(word2)) %>%
            filter(str_detect(word1, "[^\\d]")) %>%
            
            unite(bigram, word1, word2, sep = " ") %>%
            
            count(bigram, sort = TRUE) %>%
            
            head(25) %>%
            mutate(bigram = reorder(bigram, n))
    })
    
    
    ### Name of selected companies
    output$t3_company1 <- renderText({input$t3_company1})
    output$t3_company2 <- renderText({input$t3_company2})
    
    
    ### Number of reviews
    output$t3_num_reviews_1 <- renderText({paste("Number of reviews =", nrow(data_text1()))})
    output$t3_num_reviews_2 <- renderText({paste("Number of reviews =", nrow(data_text2()))})
    
    
    ### 1. Pros
    output$t3_pros_1 <- renderPlot({
        data_pro1() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#67a9cf") +
                coord_flip() +
                labs(title = "Top 25 Likes") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })
    
    output$t3_pros_2 <- renderPlot({
        data_pro2() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#67a9cf") +
                coord_flip() +
                labs(title = "Top 25 Likes") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })
    
    
    ### 2. Cons
    output$t3_cons_1 <- renderPlot({
        data_cons1() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#ef8a62") +
                coord_flip() +
                labs(title = "Top 25 Complaints") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })
    
    output$t3_cons_2 <- renderPlot({
        data_cons2() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#ef8a62") +
                coord_flip() +
                labs(title = "Top 25 Complaints") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })
    
    ### 3. Advice to Management
    output$t3_advice_1 <- renderPlot({
        data_advice1() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#969696") +
                coord_flip() +
                labs(title = "Top 25 Advice") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })
    
    output$t3_advice_2 <- renderPlot({
        data_advice2() %>%
            # Plot horizontal bar graph for word frequency
            ggplot(aes(bigram, n)) +
                geom_col(fill = "#969696") +
                coord_flip() +
                labs(title = "Top 25 Advice") +
                theme_minimal() +
                theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
                      plot.subtitle = element_text(size = 12, color = "dimgrey"),
                      axis.title.x =  element_blank(),
                      axis.title.y =  element_blank(),
                      axis.title = element_text(size = 12, color = "dimgray"),
                      axis.text = element_text(size = 12, color = "dimgray"))
    })

})

shinyApp(ui = ui, server = server)