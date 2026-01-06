library(shiny)
library(ggplot2)
library(ggalluvial)
library(dplyr)

ui <- fluidPage(
  titlePanel("Multiverse Analysis: Decision Space"),
  
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
    
    # Create desicion space by combining all elements in the list
    decision_space <- reactive({
      
      df <- expand.grid(
        datatype   = decisions$datatype,
        outcome    = decisions$outcome,
        confounder = decisions$confounder,
        model      = decisions$model,
        KEEP.OUT.ATTRS = FALSE
      )

  
    # Make sure the plot shows an empty decision when the box is not ticked
    if (!"Data" %in% input$dimensions) {
      df$datatype <- "-"
    }
    if (!"Dependent Variable" %in% input$dimensions) {
      df$outcome <- "-"
    }
    if (!"Confounder" %in% input$dimensions) {
      df$confounder <- "-"
    }
    if (!"Model" %in% input$dimensions) {
      df$model <- "-"
    }
      
    df |>
      group_by(datatype, outcome, confounder, model) |>
        summarise(n = n(), .groups = "drop")
    })
    
    
  # output plot of OR distributions per decision
  output$decisionspacePlot <- renderPlot({
    df <- decision_space()
    
    # Plot the different research paths
    decisionplot <- ggplot(df,
                           aes(axis1 = datatype, 
                               axis2 = outcome, 
                               axis3 = confounder, 
                               axis4 = model,
                               y = n)) +
      geom_alluvium(fill = datatype, alpha = .55) +
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
  })
  
  output$nPaths <- renderText({
    df <- decision_space()
    
    paste(c(
      "Number of possible analytical paths (universes):",
      df$n)
    )
  })
}


shinyApp(ui = ui, server = server)