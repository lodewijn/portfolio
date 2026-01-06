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
        "Choose decisions to include in the multiverse:",
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

    # Set up decision space, where all combinations of decisions are mapped
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
    
    decision_space <- reactive({
      
      df <- expand.grid(
        datatype   = decisions$datatype,
        outcome    = decisions$outcome,
        confounder = decisions$confounder,
        model      = decisions$model,
        KEEP.OUT.ATTRS = FALSE
      )
    })
  
  
  
  # output plot of OR distributions per decision
  output$decisionspacePlot <- renderPlot({
    df <- multiverse_df()
    
    # Plot the different research paths
    decisionplot <- ggplot(df,
                           aes(axis1 = datatype, 
                               axis2 = outcome, 
                               axis3 = confounder, 
                               axis4 = model)) +
      geom_alluvium(aes(alluvium = path_id, fill = datatype), 
                    knot.pos = 0.1, alpha = .55) +
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c("Data", 
                                  "Dependent Variable", 
                                  "Confounder", 
                                  "Model"))  +
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
  
  output$nPaths <- renderPrint({
    paste(
      "Number of possible analytical paths (universes):",
      n_multiverse_paths()
    )
  })
  
}


shinyApp(ui = ui, server = server)