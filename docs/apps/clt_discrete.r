library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sampling from a Bernoulli Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("p", "Probability of success (p):", min = 0, max = 1, value = 0.5, step = 0.01),
      numericInput("n", "Sample size (n):", value = 10, min = 1, step = 1),
      selectInput("comp", "Comparison:", choices = c("<=", "=", ">=")),
      numericInput("x_value", "Value of x:", value = 5, min = 0, step = 1)
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    p <- input$p
    n <- input$n
    x_value <- input$x_value
    comp <- input$comp
    
    # Generate 2000 samples and compute their sums
    sample_sums <- colSums(matrix(rbinom(2000 * n, size = 1, prob = p), nrow = n))
    
    # Create a histogram with the appropriate breaks
    bin_breaks <- seq(0, n + 1, by = 1) - 0.5  # Center the bins
    
    # Compute normal approximation
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    
    # Define shading logic
    df <- data.frame(x = sample_sums)
    df$shade <- ifelse(comp == "<=" & df$x <= x_value, 1,
                       ifelse(comp == "=" & df$x == x_value, 1,
                              ifelse(comp == ">=" & df$x >= x_value, 1, 0)))
    
    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(count), fill = as.factor(shade)),
                     breaks = bin_breaks, color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("gray", "blue"), guide = "none") +
      stat_function(fun = function(x) dnorm(x, mean = mu, sd = sigma) * 2000, 
                    color = "red", linetype = "dashed", size = 1) +
      scale_x_continuous(breaks = 0:n) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16)) +
      labs(title = "Histogram of Sum of Bernoulli Samples with Normal Approximation",
           x = "Sum of Bernoulli Samples", y = "Frequency")
  })
}

shinyApp(ui = ui, server = server)
