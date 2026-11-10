library(shiny)
library(ggplot2)
library(dplyr)

# UI for the app
ui <- fluidPage(
  titlePanel("Critical Values and P-Values Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:", 
                  choices = c("Normal", "t-Distribution")),
      uiOutput("dfUI"),
      selectInput("test_type", "Test Type:", 
                  choices = c("Two-tailed", "Right-tailed", "Left-tailed")),
      numericInput("alpha", "Significance Level (α):", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("test_stat", "Test Statistic (Observed Value):", value = 1.5),
      actionButton("plot_btn", "Plot")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("result")
    )
  )
)

# Server logic for the app
server <- function(input, output, session) {
  
  # UI output for the degrees of freedom input (only for t-distribution)
  output$dfUI <- renderUI({
    if(input$dist == "t-Distribution") {
      numericInput("df", "Degrees of Freedom:", value = 10, min = 1)
    }
  })
  
  observeEvent(input$plot_btn, {
    # Get inputs
    dist <- input$dist
    test_type <- input$test_type
    alpha <- input$alpha
    test_stat <- input$test_stat
    
    if(dist == "Normal") {
      # Normal distribution (Standard Normal)
      df <- NULL
    } else {
      # t-distribution
      df <- input$df
    }
    
    # Calculate critical values and rejection region
    if(dist == "Normal") {
      crit_vals <- qnorm(1 - alpha / 2)  # for two-tailed test
      if(test_type == "Right-tailed") {
        crit_vals <- qnorm(1 - alpha)
        rejection_region <- test_stat > crit_vals
      } else if(test_type == "Left-tailed") {
        crit_vals <- qnorm(alpha)
        rejection_region <- test_stat < crit_vals
      } else {
        crit_vals <- c(-qnorm(1 - alpha / 2), qnorm(1 - alpha / 2))
        rejection_region <- test_stat < crit_vals[1] | test_stat > crit_vals[2]
      }
    } else {
      # t-distribution with df degrees of freedom
      if(test_type == "Right-tailed") {
        crit_vals <- qt(1 - alpha, df)
        rejection_region <- test_stat > crit_vals
      } else if(test_type == "Left-tailed") {
        crit_vals <- qt(alpha, df)
        rejection_region <- test_stat < crit_vals
      } else {
        crit_vals <- c(-qt(1 - alpha / 2, df), qt(1 - alpha / 2, df))
        rejection_region <- test_stat < crit_vals[1] | test_stat > crit_vals[2]
      }
    }
    
    # Create plot
    if(dist == "Normal") {
      plot_data <- data.frame(x = seq(-4, 4, length.out = 1000))
      plot_data$y <- dnorm(plot_data$x)
    } else {
      plot_data <- data.frame(x = seq(-4, 4, length.out = 1000))
      plot_data$y <- dt(plot_data$x, df = df)
    }
    
    gg <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_line() +
      # Adding vertical lines for critical values and test statistic
      geom_vline(xintercept = crit_vals, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = test_stat, color = "green", linetype = "solid") +
      labs(title = "Density Plot with Critical Values and Test Statistic",
           x = "Test Statistic", y = "Density") +
      theme_minimal()
    
    # Shading based on the test type
    if (test_type == "Right-tailed") {
      gg <- gg + geom_area(data = subset(plot_data, x >= crit_vals), fill = "red", alpha = 0.5)
    } else if (test_type == "Left-tailed") {
      gg <- gg + geom_area(data = subset(plot_data, x <= crit_vals), fill = "red", alpha = 0.5)
    } else {  # Two-tailed
      gg <- gg + geom_area(data = subset(plot_data, x <= crit_vals[1]), fill = "red", alpha = 0.5) + 
        geom_area(data = subset(plot_data, x >= crit_vals[2]), fill = "red", alpha = 0.5)
    }
    
    output$distPlot <- renderPlot({ gg })
    
    # Results message
    if(rejection_region) {
      output$result <- renderText({
        paste("Rejection Region: Yes, we reject the null hypothesis.")
      })
    } else {
      output$result <- renderText({
        paste("Rejection Region: No, we fail to reject the null hypothesis.")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
