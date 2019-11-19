#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(parlitools)

# Load data
#elections_general <- read_csv("data/test1.csv")
#elections_local <- read_csv("data/test2.csv")
polls <- read_csv("Scheduled_polls.csv")
polls2 <- polls %>% select(-`Authority Type`,-`County Council Name`)
ui <- fluidPage(
    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "type", label = strong("Type of Election"),
                choices = c("Parliamentary","Local"), #unique(trend_data$type),
                selected = "Parliamentary"),
    
    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "countries", label = strong("Countries of Interest"),
                choices = c("UK","GB","England","Scotland","Wales"), #unique(trend_data$type),
                selected = "UK"),
    
    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "year", label = strong("Year Interest"),
                choices = c("2018","2019","2020"), #unique(trend_data$type),
                selected = "2020"),
    
    # Select date range to be plotted
    dateRangeInput("date", strong("Date range"), start = "2019-10-01", end = "2020-12-31",
                   min = "2019-10-01", max = "2020-12-31"),
    tableOutput("polls"),
    
    # Sidebar with a slider input for how to arrange data 
    selectInput(inputId = "sort", label = strong("Sort By"),
                choices = c("Authority Name","Year","Poll"), #unique(trend_data$type),
                selected = "Year")
)

server <- function(input, output) {
    output$polls <- renderTable(
        polls_small <- polls2 %>% 
            filter(Year==input$year) %>%
            arrange(input$sort)
        )
}

shinyApp(ui = ui, server = server)





# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(theme = shinytheme("lumen"),
#                 titlePanel("Elections Calendar"),     # Application title
#                 sidebarLayout(
#                     sidebarPanel(
#                         
#                         # Sidebar with a slider input for number of bins 
#                         selectInput(inputId = "type", label = strong("Type of Election"),
#                                     choices = c("Parliamentary","Local"), #unique(trend_data$type),
#                                     selected = "Parliamentary"),
# 
#                         # Sidebar with a slider input for number of bins 
#                         selectInput(inputId = "countries", label = strong("Countries of Interest"),
#                                     choices = c("UK","GB","England","Scotland","Wales"), #unique(trend_data$type),
#                                     selected = "UK"),
#                         
#                         # Select date range to be plotted
#                         dateRangeInput("date", strong("Date range"), start = "2019-10-01", end = "2020-12-31",
#                                        min = "2019-10-01", max = "2020-12-31"),
#                         
#                         # Select whether to overlay smooth trend line
#                         checkboxInput(inputId = "smoother", label = strong("This is a future check box choice"), value = FALSE)
#                         #                 
#                         # # Show a plot of the generated distribution
#                     ),
#                 
#                         mainPanel(
#                             renderDataTable(iris, options = list(
#                                 pageLength = 5,
#                                 initComplete = I('function(setting, json) { alert("done"); }')
#                             ))                        )
#                 )
# )
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#     output$dto <- renderDataTable({polls})
#     # output$distPlot <- renderPlot({
#     #     # generate bins based on input$bins from ui.R
#     #     x    <- faithful[, 2]
#     #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     # 
#     #     # draw the histogram with the specified number of bins
#     #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     }
# 
# # Create Shiny object
# shinyApp(ui = ui, server = server)
# 
# 
# # 
# # # Define UI
# # ui <- fluidPage(theme = shinytheme("lumen"),
# #                 titlePanel("Google Trend Index"),
# #                 sidebarLayout(
# #                     sidebarPanel(
# #                         
# #                         # Select type of trend to plot
# #                         selectInput(inputId = "type", label = strong("Trend index"),
# #                                     choices = unique(trend_data$type),
# #                                     selected = "Travel"),
# #                         
# #                         # Select date range to be plotted
# #                         dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
# #                                        min = "2007-01-01", max = "2017-07-31"),
# #                         
# #                         # Select whether to overlay smooth trend line
# #                         checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
# #                         
# #                         # Display only if the smoother is checked
# #                         conditionalPanel(condition = "input.smoother == true",
# #                                          sliderInput(inputId = "f", label = "Smoother span:",
# #                                                      min = 0.01, max = 1, value = 0.67, step = 0.01,
# #                                                      animate = animationOptions(interval = 100)),
# #                                          HTML("Higher values give more smoothness.")
# #                         )
# #                     ),
# #                     
# #                     # Output: Description, lineplot, and reference
# #                     mainPanel(
# #                         plotOutput(outputId = "lineplot", height = "300px"),
# #                         textOutput(outputId = "desc"),
# #                         tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
# #                     )
# #                 )
# # )
# # 
# # # Define server function
# # server <- function(input, output) {
# #     
# #     # Subset data
# #     selected_trends <- reactive({
# #         req(input$date)
# #         validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
# #         validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
# #         trend_data %>%
# #             filter(
# #                 type == input$type,
# #                 date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
# #                 ))
# #     })
# #     
# #     
# #     # Create scatterplot object the plotOutput function is expecting
# #     output$lineplot <- renderPlot({
# #         color = "#434343"
# #         par(mar = c(4, 4, 1, 1))
# #         plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
# #              xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
# #         # Display only if smoother is checked
# #         if(input$smoother){
# #             smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
# #             lines(smooth_curve, col = "#E6553A", lwd = 3)
# #         }
# #     })
# #     
# #     # Pull in description of trend
# #     output$desc <- renderText({
# #         trend_text <- filter(trend_description, type == input$type) %>% pull(text)
# #         paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
# #     })
# # }
# # 
# # # Create Shiny object
# # shinyApp(ui = ui, server = server)