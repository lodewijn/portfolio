library(shiny)
library(ggplot2)
library(tidyverse)
library(kSamples)

ui <- fluidPage(
  titlePanel("Multiverse Analysis Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Decision",
        "Choose an analytical decision",
        choices = c("Dependent Variable", "Statistical Model", "Data Type")
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("adTest")
    )
  )
)

server <- function(input, output) {
  
  simulated_data <- reactive({
    set.seed(123)
    
    # conditions for normally distributed odds ratios
    n <- 400
    means <- c(0.95, 1.025, 1, 0.975, 1.05)
    sd <- 0.03
    
    # create a data frame with simulated odds ratio per operationalisation of the DV
    dat <- data.frame(
      odds_ratio = unlist(
        lapply(means, function(mu) {
          # let 5 of the outcomes follow normal distributions
          exp(rnorm(n, mean = log(mu), sd = sd)) 
        })
      ),
      DV = rep(
        paste0("Operationalisation ", 1:5),
        each = n
      )
    ) 
    
    # and the other 2 follow different beta distributions
    or6 <- data.frame(odds_ratio = rbeta(n, 3,2), 
                      DV = rep("Operationalisation 6", 400))
    
    or7 <- data.frame(odds_ratio=rbeta(n, 6,2), 
                      DV = rep("Operationalisation 7", 400))
    
    # combine all DV datasets
    dat <- rbind(dat, or6, or7)
    
    # create model type decision
    dat$model <- rep(
      c("Cox Hazard", 
        "Logistic", 
        "Log-binomial",
        "Mixed-effects", 
        "Discrete Time",
        "Generalised Estimating Equation"), 
      length.out = nrow(dat)) # make sure it is repeated as many times as the df is long
    
    # create data type decision
    dat$datatype <- rep(
      c("Strict Longitudinal", 
        "Non-strict Longitudinal", 
        "Cross-sectional"),
      length.out = nrow(dat))
    
    dat
  })
  
  # connect variables in df to option bar
  grouping_var <- reactive({
    switch(
      input$Decision,
      "Dependent Variable" = "DV",
      "Statistical Model" = "model",
      "Data Type" = "datatype"
    ) })
  
  # output plot of OR distributions per decision
  output$distPlot <- renderPlot({
    dat <- simulated_data()
    g <- grouping_var()
    
    ggplot(dat, aes(x = odds_ratio, colour = .data[[g]])) +
      geom_density(size = 1) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      labs(
        x = "Odds Ratio",
        y = "Density",
        colour = g
      ) +
      theme_minimal() +
      theme(text = element_text(size = 16))
  })
  
  # output of the k-samples Anderson Darling (AD) test to identify sensitive decisions 
  output$adTest <- renderPrint({
    dat <- simulated_data()
    g <- grouping_var()
    
    split_or <- split(dat$odds_ratio, dat[[g]]) # group the odds ratios per decision
    
    ad <- do.call(ad.test, split_or) # do a within-decision AD-test
    ad
  })
}

shinyApp(ui = ui, server = server)
