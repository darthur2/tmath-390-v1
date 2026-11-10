library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Sampling Distribution Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a distribution:", 
                  choices = c("Uniform" = "unif", 
                              "Exponential" = "exp", 
                              "Gamma" = "gamma", 
                              "Beta" = "beta",
                              "Bernoulli" = "bern",
                              "Binomial" = "binom",
                              "Poisson" = "pois",
                              "Geometric" = "geom")),
      
      uiOutput("param_inputs"),
      
      sliderInput("sample_size", "Sample Size (n):", 
                  min = 1, max = 100, value = 30, step = 1),
      
      checkboxInput("show_normal", "Show Normal Approximation", value = FALSE),
      
      actionButton("simulate", "Simulate")
    ),
    
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$param_inputs <- renderUI({
    switch(input$dist,
           "unif" = tagList(
             numericInput("param1", "a (min):", value = 0),
             numericInput("param2", "b (max):", value = 1)
           ),
           "exp" = numericInput("param1", "Lambda (rate):", value = 1, min = 0.01),
           "gamma" = tagList(
             numericInput("param1", "Alpha (shape):", value = 2, min = 0.01),
             numericInput("param2", "Lambda (rate):", value = 1, min = 0.01)
           ),
           "beta" = tagList(
             numericInput("param1", "Alpha:", value = 2, min = 0.01),
             numericInput("param2", "Beta:", value = 2, min = 0.01)
           ),
           "bern" = numericInput("param1", "Probability (p):", value = 0.5, min = 0, max = 1),
           "binom" = tagList(
             numericInput("param1", "Number of trials (size):", value = 10, min = 1),
             numericInput("param2", "Probability of success (p):", value = 0.5, min = 0, max = 1)
           ),
           "pois" = numericInput("param1", "Lambda (rate):", value = 3, min = 0.01),
           "geom" = numericInput("param1", "Probability of success (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      n <- input$sample_size
      B <- 1000  # Number of simulations
      means <- numeric(B)
      
      for (i in 1:B) {
        samples <- switch(input$dist,
                          "unif" = runif(n, input$param1, input$param2),
                          "exp" = rexp(n, rate = input$param1),
                          "gamma" = rgamma(n, shape = input$param1, rate = input$param2),
                          "beta" = rbeta(n, shape1 = input$param1, shape2 = input$param2),
                          "bern" = rbinom(n, size = 1, prob = input$param1),
                          "binom" = rbinom(n, size = input$param1, prob = input$param2),
                          "pois" = rpois(n, lambda = input$param1),
                          "geom" = rgeom(n, prob = input$param1))
        
        means[i] <- mean(samples)
      }
      
      mu <- switch(input$dist,
                        "unif" = (input$param1 + input$param2)/2,
                        "exp" = 1/input$param1,
                        "gamma" = input$param1/input$param2,
                        "beta" = input$param1/(input$param1 + input$param2),
                        "bern" = input$param1,
                        "binom" = input$param1*input$param2,
                        "pois" = input$param1,
                        "geom" = (1-input$param1)/input$param1)
      
      sigma2 <- switch(input$dist,
                           "unif" = (input$param2 - input$param1)^2/12,
                           "exp" = 1/input$param1^2,
                           "gamma" = input$param1/input$param2^2,
                           "beta" = (input$param1*input$param2)/((input$param1+input$param2)^2*(input$param1+input$param2+1)),
                           "bern" = input$param1*(1-input$param1),
                           "binom" = input$param1*input$param2*(1-input$param2),
                           "pois" = input$param1,
                           "geom" = (1-input$param1)/input$param1^2)
      
      hist_data <- hist(means, breaks = 20, probability = TRUE, 
                        main = "Histogram of Sample Means", 
                        xlab = "Sample Mean", col = "skyblue", 
                        border = "white", plot = FALSE)
      
      plot(hist_data, col = "skyblue", border = "white", freq = FALSE)
      
      if (input$show_normal) {
        curve(dnorm(x, mean = mu, sd = sqrt(sigma2/n)), 
              col = "red", lwd = 2, add = TRUE)
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

