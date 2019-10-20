require(shiny)
require(rhandsontable)

ui <- fluidPage(
  
  
  titlePanel("Seabass Catch Options Tool by Age"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      radioButtons("AdviceType", label = h4("Choose Catch Advice"),
                   choices = list("EU MAP Fmsy" = "MSY", "EU MAP Fmsy lower" = 1634), 
                   inline= TRUE, selected = 1946),
      
      
      radioButtons("TimeStep", label = h4("Choose Time Step"),
                   choices = list("Annual" = 1, "Monthly" = 12), 
                   inline= TRUE, selected = 1),
      br(),
      
      h5(helpText("Recreational management measures.")),
      uiOutput("vx"), # vx is coming from renderUI in server.r
      #br(),
      uiOutput("vy"), # vy is coming from renderUI in server.r
      textOutput("RecF"),
      br(),
      h5(helpText("Select allowances for commercial gears.")),
      rHandsontableOutput('table')
      


      ,width=5   
    ),
    
    
    mainPanel(
      
    
      tableOutput("values"),
      
      plotOutput("catch_plot"),
      
      plotOutput("n_plot"),
      
      verbatimTextOutput("debug_text")
      
    )
    
  )
  
)