ui <- fluidPage(
  useShinyjs(),
  titlePanel("WEB APP LAB PROJECT"),
  sidebarLayout( 
       sidebarPanel(position = "right",
                    radioButtons(inputId = "radio_button",
                              label = "Seleziona tipo di grafico di cui mostrare i risultati",
                              choices = c("Istogrammi", "Boxplot", "QQPlot", 
                                          "Considerazioni generali", "Nessuno"),
                                selected = "Nessuno"),
                    br(),
                    
                    numericInput('indice_target_cos', 
                                 'Inserire indice del video (Cosine)',
                                 min = 1,
                                 max = 24413, 
                                 value = 1),
                    br(),
                   
                    numericInput('indice_target_pea', 
                                 'Inserire indice del video (Pearson)',
                                 min = 1,
                                 max = 24413, 
                                 value = 1)
                    ),
  
  mainPanel( 
    tabsetPanel(
      tabPanel("Grafici", 
    plotOutput(outputId = "hist1"),
    plotOutput(outputId = "hist2"),
    plotOutput(outputId = "hist3"),
    plotOutput(outputId = "hist4"),

    plotOutput(outputId = "boxplot_plot1"),
    plotOutput(outputId = "boxplot_plot2"),
    plotOutput(outputId = "boxplot_plot3"),
    plotOutput(outputId = "boxplot_plot4"),

    plotOutput(outputId = "qq_plot1"),
    plotOutput(outputId = "qq_plot2"),
    plotOutput(outputId = "qq_plot3"),
    plotOutput(outputId = "qq_plot4"),


    #plot per le considerazioni generali
    plotOutput(outputId = "corr1"),
    plotOutput(outputId = "corr2"),
    
    plotOutput(outputId = "vid_tend"),
   

    plotOutput(outputId = "piu_views"),
    plotOutput(outputId = "piu_likes"),
    plotOutput(outputId = "piu_dislikes")),
    
    tabPanel("Reccomendation",
            verbatimTextOutput("recc_cosine1"),
            br(),
            br(),
            verbatimTextOutput("recc_pearson1")
            
    )
    
    
  )
  )
)
)


