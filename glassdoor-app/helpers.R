filter_reviews <- function(input_data, selected_company, selected_country, start_date, end_date){
    df <- input_data %>% 
        mutate(date = as.Date(date)) %>% 
        dplyr::filter(company == selected_company,
               date >= start_date, 
               date <= end_date) %>% 
        {if (selected_country != "All") dplyr::filter(., country == selected_country) else filter(.)}
    
    return(df)
}

plot_rating_distribution <- function(input_data, rating, fill_color){

    df <- input_data %>%
        dplyr::filter(!is.na(input_data[[rating]]))

    avg_rating <- round(mean(df[[rating]]), 2)

    df[[rating]] <- factor(floor(df[[rating]]), levels = c(1, 2, 3, 4, 5))

    ggplot(df,
           aes(x = df[[rating]])) +
        geom_bar(width = 0.7, fill = fill_color) +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        coord_flip() +
        labs(x = "Score",
             y = "Count",
             title = case_when(
                 grepl("overall", rating) ~ "Overall",
                 grepl("career", rating) ~ "Career Opportunities",
                 grepl("comp", rating) ~ "Compensation and Benefits",
                 grepl("culture", rating) ~ "Culture and Values",
                 grepl("mgmt", rating) ~ "Senior Management",
                 grepl("balance", rating) ~ "Work-Life Balance"
             ),
             subtitle = paste("Average:", avg_rating)
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", color = "dimgrey"),
              plot.subtitle = element_text(size = 12, color = "dimgrey"),
              axis.title = element_text(size = 12, color = "dimgray"),
              axis.text = element_text(size = 12, color = "dimgray"))
}

plot_rating_trend <- function(input_data, rating){
    ggplot(input_data, aes(x = input_data$year, y = input_data[[rating]], color = company)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = seq(2008, 2019)) +
        scale_y_continuous(limits = c(1, 5)) +
        scale_color_manual(name = "Company",
                           values = c("#ef8a62", "#67a9cf")) +
        labs(x = "Year",
             y = "Average Score",
             title = case_when(
                 grepl("overall", rating) ~ "Overall",
                 grepl("career", rating) ~ "Career Opportunities",
                 grepl("comp", rating) ~ "Compensation and Benefits",
                 grepl("culture", rating) ~ "Culture and Values",
                 grepl("mgmt", rating) ~ "Senior Management",
                 grepl("balance", rating) ~ "Work-Life Balance"
             )
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 20, face = "bold", color = "dimgrey"),
              axis.text = element_text(size = 14, color = "dimgray"),
              axis.title = element_text(size = 16, color = "dimgray"),
              legend.text = element_text(size = 16, color = "dimgray"),
              legend.title = element_text(size = 16, color = "dimgray", face = "bold"),
              legend.background = element_rect(fill = "white", colour = "grey"),
              legend.justification = c(1,1),
              legend.position = c(0.97,0.98))
}