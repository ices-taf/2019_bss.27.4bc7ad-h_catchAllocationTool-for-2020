require(shiny)
require(rhandsontable)

ui <- fluidPage(
  
  
  
  titlePanel("Seabass Catch Options Tool by Age"),
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Select the allowances for each gear."),
      
      
      uiOutput("checkbox"),
      
      
      uiOutput("sliders"),
      
      rHandsontableOutput('table'),
      textOutput('result'),
      br(),
      actionButton("recalc", "Set all to zero")

      
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