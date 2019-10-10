ui <- fluidPage(

  titlePanel("Seabass Catch Options Tool by Age"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the allowances for each gear."),
      
      uiOutput("checkbox"),

      
      uiOutput("sliders")
    ),
    
    mainPanel(
      #textOutput("selected_var")
      tableOutput("values"),
      plotOutput("catch_plot"),
      plotOutput("n_plot"),
      verbatimTextOutput("debug_text")
    )
  )
)