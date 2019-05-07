filter_reviews <- function(data, company1, company2, start_date, end_date){
    data %>% 
        mutate(date = as.Date(date)) %>% 
        filter((company == company1) | (company == company2)) %>% 
        filter(date >= start_date)
}