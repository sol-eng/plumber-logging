# Packages ----
library(shiny)
library(shinydashboard)
library(fs)
library(tidyverse)
# Dependency of config.yml
library(here)

# Utils ----
read_plumber_log <- function(log_file) {
  readr::read_log(file = log_file, 
                  col_names = c("log_level",
                                "timestamp", 
                                "remote_addr", 
                                "user_agent",
                                "host",
                                "method",
                                "endpoint",
                                "status",
                                "execution_time"))
}

# Config ----
config <- config::get()

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "API Usage"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(type="text/css",
                 ".recalculating { opacity: 1.0; }"
      ) 
    ),
    fluidRow(
      valueBoxOutput("total_requests", width = 6),
      valueBoxOutput("requests_per_second", width = 6)
    ),
    fluidRow(
      valueBoxOutput("percent_success", width = 6),
      valueBoxOutput("average_execution", width = 6)
    ),
    fluidRow(
      valueBoxOutput("highest_endpoint", width = 6),
      valueBoxOutput("highest_user", width = 6)
    ),
    fluidRow(
      box(
        plotOutput("requests_plot"), 
        title = "Requests Overview",
        width = 12,
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        plotOutput("status_plot"),
        title = "Response Status Overview",
        width = 6
      ),
      box(
        plotOutput("endpoints_plot"),
        title = "Endpoints Overview",
        width = 6
      )
    ),
    fluidRow(
      box(
        plotOutput("users_plot"),
        title = "Users Overview",
        width = 12,
        collapsible = TRUE
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Create memoised read_plumber_log function to cache results
  mem_read_plumber_log <- memoise::memoise(function(file, timestamp) {
    read_plumber_log(file)
  })
  
  observe({
    # Invalidate memoise cache every 30 minutes to avoid cache explosion
    invalidateLater(30*60*1000)
    memoise::forget(mem_read_plumber_log)
  })
  
  log_data <- reactivePoll(1000, # 1 second
                           checkFunc = function() {
                             files <- dir_ls(config$log_dir)
                             file_info(files)$modification_time
                           },
                           valueFunc = function() {
                             files <- dir_ls(config$log_dir)
                             purrr::map2_df(files, file_info(files)$modification_time, mem_read_plumber_log)
                           }, session = session)
  
  filtered_log <- reactive({
    log_data()
  })
  
  output$total_requests <- renderValueBox({
    valueBox(value = nrow(filtered_log()),
             subtitle = "Total Requests")
  })
  
  output$requests_per_second <- renderValueBox({
    data <- filtered_log()
    n_requests <- nrow(data)
    seconds <- n_distinct(data$timestamp)
    valueBox(value = round(n_requests / seconds, 2),
             subtitle = "Requests per Second")
  })
  
  output$percent_success <- renderValueBox({
    p <- filtered_log() %>% 
      count(status) %>% 
      mutate(p = n/sum(n)) %>% 
      filter(status == 200) %>% 
      pull(p) %>% 
      round(4)
    valueBox(value = glue::glue("{p*100}%"),
             subtitle = "Success",
             color = ifelse(p >= .9, "green", "red"))
  })
  
  output$average_execution <- renderValueBox({
    avg_execution <- round(mean(filtered_log()$execution_time, na.rm = TRUE), digits = 4)
    valueBox(value = avg_execution,
             subtitle = "Average Execution Time (S)")
  })
  
  output$highest_endpoint <- renderValueBox({
    highest_endpoint <- filtered_log() %>% 
      count(host, method, endpoint) %>% 
      top_n(1, wt = n)
    valueBox(value = highest_endpoint$n,
             subtitle = glue::glue("{highest_endpoint$method} {highest_endpoint$host}{highest_endpoint$endpoint}"))
  })
  
  output$highest_user <- renderValueBox({
    highest_user <- filtered_log() %>% 
      count(user_agent, remote_addr) %>% 
      top_n(1, wt = n)
    valueBox(value = highest_user$n,
             subtitle = glue::glue("{highest_user$user_agent}@{highest_user$remote_addr}"))
  })
  
  output$requests_plot <- renderPlot({
    filtered_log() %>% 
      count(timestamp) %>% 
      ggplot(aes(x = timestamp, y = n)) +
      geom_point() +
      labs(x = "Time", 
           y = "Requests") +
      scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
      theme_bw()
  })
  
  output$status_plot <- renderPlot({
    filtered_log() %>% 
      count(status = as.factor(status)) %>% 
      ggplot(aes(x = fct_reorder(status, n), y = n)) +
      geom_col() +
      theme_bw() +
      coord_flip() +
      labs(x = "Response Status",
           y = "Requests")
  })
  
  output$endpoints_plot <- renderPlot({
    filtered_log() %>% 
      count(method, host, endpoint) %>% 
      unite(method, method, host, sep = " ") %>% 
      unite(endpoint, method, endpoint, sep = "") %>% 
      top_n(10, wt = n) %>% 
      ggplot(aes(x = fct_reorder(endpoint, n), y = n)) +
      geom_col() +
      theme_bw() +
      coord_flip() +
      labs(x = "Endpoint",
           y = "Requests")
  })
  
  output$users_plot <- renderPlot({
    filtered_log() %>% 
      count(user_agent, remote_addr) %>% 
      unite(user, user_agent, remote_addr, sep = "@") %>% 
      top_n(10, wt = n) %>% 
      ggplot(aes(x = fct_reorder(user, n),y = n)) +
      geom_col() +
      theme_bw() +
      coord_flip() +
      labs(x = "Users",
           y = "Requests")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
