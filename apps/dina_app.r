library(shiny)
library(bayesplot)
library(dina)

ui <- fluidPage(
  titlePanel("DINA Bayesian Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("resp", "Upload Response Data (CSV)"),
      fileInput("qmat", "Upload Q-Matrix (CSV)"),
      
      numericInput("niter", "Iterations", 2000),
      numericInput("nburn", "Burn-in", 500),
      numericInput("nchains", "Chains", 2),
      
      numericInput("a_slip", "a_slip", 1),
      numericInput("b_slip", "b_slip", 1),
      numericInput("a_guess", "a_guess", 1),
      numericInput("b_guess", "b_guess", 1),
      
      actionButton("run", "Run Gibbs Sampler"),
      
      hr(),
      
      textInput("param", "Parameter name (e.g., slip[1])"),
      selectInput("plotType", "Plot Type",
                  choices = c("Histogram", "Traceplot"))
    ),
    
    mainPanel(
      plotOutput("mcmcPlot"),
      tableOutput("profile")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Load Data ----
  Y <- reactive({
    req(input$resp)
    as.matrix(read.csv(input$resp$datapath, header=FALSE))
  })
  
  Q <- reactive({
    req(input$qmat)
    as.matrix(read.csv(input$qmat$datapath, header=FALSE))
  })
  
  # ---- Run Gibbs ----
  mcmc_results <- eventReactive(input$run, {
    
    req(Y(), Q())
    
    withProgress(message = "Running Gibbs sampler...", value = 0, {
    
      data <- list(Y = Y(),
                   Q = Q())
      
      fit_method <- "fit_catdina_gibbs"
      
      sgdina_stan <- cmdstanr::cmdstan_model(exe_file = "C://Users/dbarthur/OneDrive - UW/Research/Projects/RegDINA v2/Code/sgdina.exe")
      
      incProgress(0.3, detail = "Initializing model...")
      
      fit_method_args <- list(prior_args = list(prior_gen_model_name = "sgdina",
                                                prior_gen_fit_method = "fit_sgdina_stan_optim",
                                                prior_gen_fit_method_args = list(prior_args = list(a_slip = input$a_slip,
                                                                                                   b_slip = input$b_slip,
                                                                                                   a_guess = input$a_guess,
                                                                                                   b_guess = input$b_guess),
                                                                                 stan_model = sgdina_stan)),
                              gibbs_args = list(niter = input$niter,
                                                nburn = input$nburn,
                                                nchains = input$nchains),
                              stan_model = NULL)
      
      incProgress(0.6, detail = "Running sampler (this may take a while)...")
      
      fit <- dina(data, "fit_catdina_gibbs", fit_method_args)
      
      incProgress(1, detail = "Done!")
      
      fit
    })
  })
  
  # ---- Plot ----
  output$mcmcPlot <- renderPlot({
    req(mcmc_results(), input$param)
    
    arr <- mcmc_results()$draws
    
    if (!(input$param %in% dimnames(arr)[[3]])) {
      plot.new()
      text(0.5, 0.5, "Invalid parameter name")
      return()
    }
    
    if (input$plotType == "Histogram") {
      bayesplot::mcmc_hist(arr, pars = input$param)
    } else {
      bayesplot::mcmc_trace(arr, pars = input$param)
    }
  })
  
  output$profile <- renderTable({
    req(mcmc_results(), input$param)
    
    params_hat <- mcmc_results()$params_hat
    z <- params_hat[startsWith(names(params_hat), "z[")]
    
    N <- length(z)
    
    # ---- Extract index from param if it's z[...] ----
    if (grepl("^z\\[\\d+\\]$", input$param)) {
      person_index <- as.numeric(gsub("z\\[(\\d+)\\]", "\\1", input$param))
    } else {
      return(data.frame(Message = "Enter a parameter like z[1] to view profile"))
    }
    
    # Validate
    if (person_index < 1 || person_index > N) {
      return(data.frame(Error = "Invalid individual index"))
    }
    
    # ---- Get attribute patterns ----
    K <- ncol(Q())
    patterns <- cdmUtils::make_patterns(K)
    
    class_index <- z[person_index]
    profile <- patterns[class_index, ]
    
    data.frame(
      Attribute = c("Probability", "Random Variables", "Confidence Intervals", "Hypothesis Testing"),
      Mastery = as.integer(profile)
    )
  })
}

shinyApp(ui, server)
