library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Elephant Age & Height"),
  
  dashboardSidebar(
    selectInput("y_var", 
                "Select Variable", 
                choices = c("age", "height"), 
                selected = "age")
  ),
  
  dashboardBody(
    plotOutput("elephant_plot", width = "600px", height = "500px")
  )
)

server <- function(input, output, session) {
  
  output$elephant_plot <- renderPlot({
    
    elephants %>% 
      filter(sex != "NA") %>% 
      ggplot(aes(x = sex, y = .data[[input$y_var]], fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      labs(title = paste("Range of", input$y_var, "by Sex"),
           x = "Sex",
           y = input$y_var) +
      theme_minimal()
  })
}

shinyApp(ui, server)
