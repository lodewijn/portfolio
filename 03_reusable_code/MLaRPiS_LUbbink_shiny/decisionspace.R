library(ggalluvial)

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
    
  })
  
  multiverse_df <- reactive({
    dims <- input$dimensions
    
    cols <- c(
      "Data Type"           = "datatype",
      "Dependent Variable"  = "outcome",
      "Confounder"          = "confounder",
      "Statistical Model"   = "model"
    )
    
    selected_cols <- cols[dims]
    
    df <- do.call(
      expand.grid,
      c(decision_space()[selected_cols], KEEP.OUT.ATTRS = FALSE)
    )
    
    df$path_id <- seq_len(nrow(df))
    df
  })
  
  n_multiverse_paths <- reactive({
    dims <- input$dimensions
    
    cols <- c(
      "Data Type"           = "datatype",
      "Dependent Variable"  = "outcome",
      "Confounder"          = "confounder",
      "Statistical Model"   = "model"
    )
    
    prod(vapply(
      decision_space()[cols[dims]],
      length,
      numeric(1)
    ))
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