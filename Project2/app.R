#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(ggplot2)


ui <- fluidPage(

    
    titlePanel("Articles about the Olympics"),
    
    sidebarLayout(
      mainPanel(
        verbatimTextOutput("summary")
      ),
      function (sidebarPanel, position = c("left", "right"), 
                fluid = TRUE) 
      {
        position <- match.arg(position)}
    ),
      sidebarPanel(
        # Sidebar panel with tabs
        tabsetPanel(
          tabPanel("About", tags$img(src= "https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.cleanpng.com%2Fpng-olympic-games-rings-official-png-transparent-logo-50209%2F&psig=AOvVaw27It-zB3EWX6B9BTEVaNFd&ust=1720543581157000&source=images&cd=vfe&opi=89978449&ved=0CBEQjRxqFwoTCLDo2rvyl4cDFQAAAAAdAAAAABAE"),
                   tags$div(
                     h2("Purpose of App:"),
                     p("This app finds articles that discuss the olympics since June 02, 2024 to now. With the app we can visualize the frequency of sports being discussed as well as different countries."),
                     h3("Data and Source:"),
                     p("The data source is from newsapi, variables are source, author, title, description, url, urltoImage, PublishedAt, and content. For chart and summary purposes, I create variables corresponding to if the country or sport are mentioned or not.",
                     a("Data Source", href= "https://newsapi.org/v2/everything")),
                     h4("Purpose of tabs:"),
                     p("The about tab corresponds to decribing this app and its data and source. The data download tab allows for users to make changes to the API query, return data, subset the data set, and save a csv file. The Data Exploration tab allows for the user to make numerical and graphical summaries.")
                   )),
          tabPanel("Download",
                   fluidRow(
                     column(4,
                            textInput("api_url", "API URL:", value= "https://newsapi.org/v2/everything"),
                            textAreaInput("api_param", "API Parameters:", value='{"q":choice(title), "apiKey":key, "from":choice(time)}'),
                            actionButton("submit_api", "Submit API Request"),
                            br(),
                            h3("Subset"),
                            checkboxInput("subset_rows", "Subset Rows"),
                            sliderInput("row slider", "Select Rows:", min = 1, max = 8, value=8),
                            br(),
                            checkboxGroupInput("subset_cols", "Select Columns:", choices = NULL, selected=NULL)
                            ),
                     column(8, DTOutput("data_download"),
                            br(),
                            downloadButton("downloadData", "Download Data")
                            )
                   )),
          tabPanel("Data Exploration", 
                   fluidRow(4,
                   selectInput("x_var", "X-axis Variable", choices = names(data)),
                   selectInput("y_var", "Y-axis Variable", choices = names(data)),
                   selectInput("plot_type", "Plot Type",
                               choices = "bar"),
                   selectInput("summary_type", "Summary Type",
                               choices = "summary"),
                   checkboxInput("facet_plot", "Facet Plot by:",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.facet_plot == true",
                     selectInput("facet_var", "Facet Variable",
                                 choices = names(data))
                   )
                   )
                   )
        )
      ),
      column(8,
             # Display plot
             plotOutput("plot"),
             br(),
             # Display summary
             verbatimTextOutput("summary")
      )
    )
  


server <- function(input, output, session) {
  output$about_text <- renderText({
    "This is the About tab."()
  })
  
  req(api_url)
  req(api_param)
  
  
  response <- GET(api_url, query = list(parse_json(api_param)))
  
  
  if (http_error(response)) {
    stop("HTTP error ", response$status_code)
  }
  
  
  data <- content(response, "parsed", simplifyVector = TRUE)
  
  
  data <- as_tibble(data)
  
  
  data

api_data <- reactive({
  api_call(input$api_url, input$api_param)
})


output$table <- DT::renderDataTable({
  api_data()
})


filtered_data <- reactive({
  data <- api_data()
  
  if (input$subset_rows) {
    data <- data[1:min(nrow(data), input$rows_slider), ]
  }
  
  if (!is.null(input$subset_cols)) {
    data <- data[, input$subset_cols, drop = FALSE]
  }
  
  data
})


observe({
  if (!is.null(api_data())) {
    updateCheckboxGroupInput(session, "subset_cols", 
                             choices = names(api_data()), 
                             selected = names(api_data()))
  }
})


output$downloadData <- downloadHandler(
  filename = function() {
    "data.csv"
  },
  content = function(file) {
    write.csv(filtered_data(), file, row.names = FALSE)
  }
)

api_data_download <- reactive({
  api_call(input$api_url, input$api_param)
})


output$table_download <- DT::renderDataTable({
  api_data_download()
})


filtered_data_download <- reactive({
  data <- api_data_download()
  
  if (input$subset_rows_download) {
    data <- data[1:min(nrow(data), input$rows_slider), ]
  }
  
  if (!is.null(input$subset_cols_download)) {
    data <- data[, input$subset_cols, drop = FALSE]
  }
  
  data
})


observe({
  if (!is.null(api_data_download())) {
    updateCheckboxGroupInput(session, "subset_cols_download", 
                             choices = names(api_data_download()), 
                             selected = names(api_data_download()))
  }
})


output$downloadData_download <- downloadHandler(
  filename = function() {
    "downloaded_data_download.csv"
  },
  content = function(file) {
    write.csv(filtered_data_download(), file, row.names = FALSE)
  }
)

# Generate plot based on user inputs
output$plot <- renderPlot({
  
  plot_data <- data[, c(input$x_var,input$y_var), drop = FALSE]()
  
  
  if (input$plot_type == "bar") {
    ggplot(data=data()|> 
             drop_na(input$x_var), aes(x=input$x_var)) +
      geom_bar() +
      labs(x = input$x_var, y = "Count", title = "Bar Chart")
  }
  if (input$facet_plot) {
    facet_var <- input$facet_var
    plot_data[[facet_var]] <- as.factor(plot_data[[facet_var]])
    facet_wrap(~ .data[[facet_var]])
  }
  
  output$summary <- renderPrint({
   
    summary_data <- data[, c(input$x_var, input$y_var), drop = FALSE]()
  
})
  
})

}
# Run the application 
shinyApp(ui = ui, server = server)
