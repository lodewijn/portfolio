ui <- fluidPage(
  titlePanel("Multiverse Analysis: Decision Space"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "dimensions",
        "Include decision dimensions",
        choices = c(
          "Data",
          "Outcome",
          "Covariates",
          "Model"
        ),
        selected = c(
          "Data",
          "Outcome",
          "Covariates",
          "Model"
        )
      )
    )
    ,
    mainPanel(
      plotOutput("decisionspacePlot")
    )
  )
)

server <- function(input, output) {
  decision_space <- reactive({
    # Set up decision space, where all combinations of decisions are mapped
    decision_space <- expand.grid(
      
      datatype       = c("SL", 
                         "NSL",
                         "CS"),  
      
      
      model      = c("COX", 
                     "LOG", 
                     "LOG-BIN",
                     "ME", 
                     "TIME-ME",
                     "GEE"), 
      
      outcome         = c(paste0("O", 1:7)),
      
      confounder      = c("AGE", 
                          "SEX", 
                          "COUN",
                          "SMOK",
                          "EDU",
                          "PHYA", 
                          "NONE")
    )
    
    # Reshape to long form for plotting
    decisions <- decision_space %>% 
      mutate(spec_id = row_number()) %>%
      pivot_longer(-spec_id, names_to = "decision", values_to = "option")
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
  output$decisionspacePlot <- renderPlot({
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