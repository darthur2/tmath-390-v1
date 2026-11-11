library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Confidence Interval Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:", 
                  choices = c("Standard Normal" = "norm", "t-Distribution" = "t")),
      conditionalPanel(
        condition = "input.dist == 't'",
        numericInput("df", "Degrees of Freedom:", value = 5, min = 1, step = 1)
      ),
      sliderInput("conf", "Confidence Level:", min = 0, max = 1, value = 0.95, step = 0.01)
    ),
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("quantiles")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    alpha <- 1 - input$conf
    q_left <- q_right <- NULL
    x_vals <- seq(-4, 4, length.out = 1000)
    
    if (input$dist == "norm") {
      q_left <- qnorm(alpha / 2)
      q_right <- qnorm(1 - alpha / 2)
      y_vals <- dnorm(x_vals)
    } else {
      q_left <- qt(alpha / 2, df = input$df)
      q_right <- qt(1 - alpha / 2, df = input$df)
      y_vals <- dt(x_vals, df = input$df)
    }
    
    data <- data.frame(x = x_vals, y = y_vals)
    
    ggplot(data, aes(x, y)) +
      geom_line(size = 1) +
      geom_area(data = subset(data, x <= q_left), aes(y = y), fill = "red", alpha = 0.4) +
      geom_area(data = subset(data, x >= q_right), aes(y = y), fill = "red", alpha = 0.4) +
      geom_vline(xintercept = c(q_left, q_right), linetype = "dashed", color = "blue") +
      labs(title = paste("Density Curve of", ifelse(input$dist == "norm", "Standard Normal", paste("t-Distribution (df=", input$df, ")"))),
           x = "x", y = "Density") +
      theme_minimal()
  })
  
  output$quantiles <- renderText({
    alpha <- 1 - input$conf
    if (input$dist == "norm") {
      q_left <- qnorm(alpha / 2)
      q_right <- qnorm(1 - alpha / 2)
    } else {
      q_left <- qt(alpha / 2, df = input$df)
      q_right <- qt(1 - alpha / 2, df = input$df)
    }
    paste("Quantiles:", round(q_left, 4), "and", round(q_right, 4))
  })
}

shinyApp(ui = ui, server = server)
