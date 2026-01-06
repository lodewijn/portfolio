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
        choices = c(
          "Dependent Variable",
          "Statistical Model",
          "Data Type",
          "Confounder"
        )
      ),
      hr(),
      strong("Decision sensitivity"),
      textOutput("sensitivityText")
    )
    
    ,
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("adText1"),
      verbatimTextOutput("adText2"),
      verbatimTextOutput("adTest")
    )
  )
)

server <- function(input, output) {
  
  simulated_data <- reactive({
    # Set up decision space, where all combinations of decisions are mapped
    decision_space <- expand.grid(
      
      datatype       = c("Strict Longitudinal", 
                         "Non-strict Longitudinal",
                         "Cross-sectional"),  
      
      
      model      = c("Cox Hazard", 
                     "Logistic", 
                     "Log-binomial",
                     "Mixed-effects", 
                     "Discrete Time",
                     "Generalised Estimating Equation"), 
      
      outcome         = c(paste0("Operationalisation ", 1:7)),
      
      confounder      = c("Age", 
                          "Sex", 
                          "Country",
                          "Smoking",
                          "Education",
                          "Physical Activity", 
                          "None")
    )
    
    # Set seed to ensure reproducibility
    set.seed(123)
    
    n <- nrow(decision_space)
    
    # For good visualisation of decision sensitivity
    # 2/3 of the resulting odds ratios will follow a normal distribution 
    n_norm <- floor(2 * n / 3)
    
    # and 1/3 will follow a beta
    n_beta <- n - n_norm
    
    # Conditions for normal distribution
    mean <- 1
    sd  <- 0.05
    
    # Normally distributed odds ratios
    or_norm <- data.frame(
      odds_ratio = unlist(
        lapply(mean, function(mu) {
          exp(rnorm(n_norm, mean = log(mu), sd = sd)) 
        })
      )
    ) 
    
    # Beta distributed odds ratios
    or_beta <- data.frame(odds_ratio = rbeta(n_beta, 3,2))
    
    # Create one data frame that contains the decisions and outcomes
    dat <- decision_space
    dat$odds_ratio <- c(or_norm$odds_ratio, or_beta$odds_ratio)
    
    # Output is complete dataframe
    dat
  })
  
  # connect variables in df to option bar
  grouping_var <- reactive({
    switch(
      input$Decision,
      "Dependent Variable" = "outcome",
      "Statistical Model" = "model",
      "Data Type" = "datatype", 
      "Confounder" = "confounder"
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
        colour = input$Decision
      ) +
      theme_minimal() +
      theme(text = element_text(size = 16))
  })
  
  # output of the k-samples Anderson Darling (AD) test to identify sensitive decisions 
  ad_result <- reactive({
    dat <- simulated_data()
    g <- grouping_var()
    
    split_or <- split(dat$odds_ratio, dat[[g]])
    ad.test(split_or)
  })
  
  output$adText1 <- renderText({
    paste(
      "Results of k-sample Anderson Darling Test:")
  })
  
  output$adText2 <- renderText({
    ad <- ad_result()
  
    paste("Number of samples:", ad$k,"\nSample sizes:", paste(ad$ns, collapse = ", "))
  })
  
  output$adTest <- renderPrint({
    ad <- ad_result()
    ad$ad
  })
  
  output$sensitivityText <- renderText({
    ad_sens <- ad_result()
    
      if (ad$ad[6] < 0.05) {
      "Since the Anderson–Darling test is significant (p < 0.05), this analytical decision can be classified as sensitive."
    } else {
      "Since the Anderson–Darling test is not significant (p > 0.05), this analytical decision cannot be classified as sensitive."
    }
  })
  
}






shinyApp(ui = ui, server = server)
