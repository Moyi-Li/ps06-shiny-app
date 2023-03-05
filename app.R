#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(rsconnect)

game <- read.csv('vgsales.csv')
input_col <- c("Year","NA_Sales", "EU_Sales","JP_Sales","Other_Sales", "Global_Sales")


# View(game)
ui <- fluidPage(
  
  titlePanel("Analysis of the Video Game Industry"),
  
  tabsetPanel(
    
    tabPanel("Data Sample",
             
             sidebarPanel(
               h4("General Information"),
               p("This weekly assginment was created by", strong("Moyi Li"), "for ps06 in", em("INFO 201."),
                 "The dataset includes 16598 rows and 11 columns, including the name, releasing platform, year, genre, publisher, 
                  sales in global and each region, and rank of overall sales. The dataset was downloaded from "), 
               a('Kaggle datasets', href='https://www.kaggle.com/datasets/gregorut/videogamesales')
               
             ),
             
             mainPanel(
               h4("Sample Figure"),
               p("This is a sample of the game dataset."),
               tableOutput("data_sample")
             )
             
    ),
    
    
    tabPanel("Plot Figure",
             
             sidebarPanel(
               h4("Plot Options"),
               selectInput("x_var", "X Variable", choices = input_col, selected = "Year"),
               selectInput("y_var", "Y Variable", choices = input_col, selected = "Global_Sales"),
               selectInput("color_var", "Color", choices = list("Blue" = "blue", "Black" = "black", "Green" = "green"), selected = 'Blue')
               
               
             ),
             
             mainPanel(
               h4("Game Data Plot"),
               plotOutput("data_plot"),
               textOutput("plot_summary")
             )
             
    ),
    
    tabPanel("Display Data",
             
             sidebarPanel(
               h4("Columns to Display"),
               checkboxGroupInput("columns", "Columns", choices = names(game), selected = c('Name', 'Rank', 'Platform')),
               sliderInput("row_num", "How many rows do you want to display?", min = 10, max = 200, value = 100)

             ),
             
             mainPanel(
               h4("Game Data Table"),
               textOutput("table_summary"),
               tableOutput("data_table")
             )
             
    )
    
  )
  
)


server <- function(input, output) {
  
  game <- read.csv('vgsales.csv')
  
  
  # First Page: data sample table!
  output$data_sample <- renderTable({
    head(game, 10)
  })
  
  # Second Page: Plot!
  output$data_plot <- renderPlot({
    
    ggplot(game) +
      geom_line(mapping = aes_string(x = input$x_var, y = input$y_var), 
                color = input$color_var) +
      labs(title = "Plot of Sales in different reigon and Year", x = input$x_var, y = input$y_var)
    
    
  })
  
  
  
  #Second Page: Plot summary!
  output$plot_summary <- renderText({
    
    paste("This is a plot contains the x variable of", input$x_var, "and y variable of", input$y_var,
          ". Users can modifies the x variable and y variable by selecting the drop-down menu, 
           or alternate visuals by adjusting the widget.")
    
    
  })
  
  # Third Page: Table!
  output$data_table <- renderTable({
    
    game %>%
      select(input$columns) %>% 
      head(input$row_num)
  })
  
  #Third Page: Table summary!
  output$table_summary <- renderText({
    
    paste("There are ", input$row_num,"rows being selected! 
          Users can freely select the checkbox on the widget to determine which columns they want to display
          on the main panel. Users can also select the numbers of rows to display on the page.
          As it is time-consuming to display all rows of the dataset, I only set the maximum to 200 here.
          From the table, it is clear to see exact rows information.")
  })
  
}


shinyApp(ui = ui, server = server)
