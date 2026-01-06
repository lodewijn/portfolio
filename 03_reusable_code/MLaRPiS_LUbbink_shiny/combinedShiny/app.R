library(shiny)
library(ggplot2)
library(ggalluvial)
library(dplyr)

ui <- fluidPage(
  titlePanel("Multiverse Analysis: Decision Space and Sensitivity"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = "Decision Space",
      "This plot shows how a decision space can be created, 
      by combining different analytical decision options in all possible ways. 
      It also shows how rapidly the size of a multiverse can increase with the 
      number of different decisions and options.",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "dimensions",
            "Choose decisions included in multiverse:",
            choices = c(
              "Data Type",
              "Dependent Variable",
              "Confounder",
              "Statistical Model"
            ),
            selected = c(
              "Data Type",
              "Dependent Variable",
              "Confounder",
              "Statistical Model"
            )
          )
        )
        ,
        mainPanel(
          plotOutput("decisionspacePlot"),
          verbatimTextOutput("nPaths")
        )
      )
      
    ),
    tabPanel(
      title = "Decision Sensitivity",
      "This plot shows how the sensitivity of different analytical decisions 
      can be assessed by looking at the distributions of the outcomes of 
      all different analytical paths. By using a k-sample Anderson Darling test, 
      the null-hypothesis that all outcomes come from the same underlying 
      distribution is tested. When this null hypothesis can be rejected, the 
      decision can be classified as sensitive.",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "Decision",
            "Choose an analytical decision",
            choices = c(
              "Data Type",
              "Dependent Variable",
              "Confounder",
              "Statistical Model"
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
  ),
  
)

server <- function(input, output) {
  
  # Set up decision space in a list (not in a reactive element)
  decisions <- list(
    
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
  
  # Create decision space by combining all options of the decisions in the list
  decision_space <- reactive({
    
    df <- expand.grid(
      datatype   = decisions$datatype,
      outcome    = decisions$outcome,
      confounder = decisions$confounder,
      model      = decisions$model,
      KEEP.OUT.ATTRS = FALSE
    )
    
    
    # When the box is not ticked, this means that this decision is fixed 
    # so for example just one type of model (e.g., logistic regression) is used
    if (!"Data Type" %in% input$dimensions) {
      df$datatype <- "FIXED"
    }
    if (!"Dependent Variable" %in% input$dimensions) {
      df$outcome <- "FIXED"
    }
    if (!"Confounder" %in% input$dimensions) {
      df$confounder <- "FIXED"
    }
    if (!"Statistical Model" %in% input$dimensions) {
      df$model <- "FIXED"
    }
    
    df |>
      group_by(datatype, outcome, confounder, model) |>
      summarise(n = n(), .groups = "drop")
  })
  
  
  # output plot of OR distributions per decision
  output$decisionspacePlot <- renderPlot({
    df <- decision_space()
    
    # Plot the different analytical paths
    decisionplot <- ggplot(df,
                           aes(axis1 = datatype, 
                               axis2 = outcome, 
                               axis3 = confounder, 
                               axis4 = model,
                               y = n)) +
      geom_alluvium(aes(fill = datatype), alpha = .55) +
      geom_stratum() +
      geom_text(stat = "stratum", 
                aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c("Data Type", 
                                  "Dependent Variable", 
                                  "Confounder", 
                                  "Statistical Model"))  +
      theme_minimal() + 
      ylab("Number of possible analytical paths") +
      labs(fill = "Data")+
      theme(
        axis.title = element_text(size = 16),   # x and y axis titles
        axis.text  = element_text(size = 14),   # tick labels
        legend.title = element_text(size = 16), # "fill" label
        legend.text  = element_text(size = 14), # legend item labels
        strip.text = element_text(size = 16)    # facet labels (if any)
      )
    
    decisionplot
  })
  
  output$nPaths <- renderText({
    dims <- input$dimensions
    
    sizes <- c(
      "Data Type" = length(decisions$datatype),
      "Dependent Variable" = length(decisions$outcome),
      "Confounder" = length(decisions$confounder),
      "Statistical Model" = length(decisions$model)
    )
    
    paste(
      "Number of possible analytical paths (universes):",
      prod(sizes[dims])
    )
  })
  
}


shinyApp(ui = ui, server = server)