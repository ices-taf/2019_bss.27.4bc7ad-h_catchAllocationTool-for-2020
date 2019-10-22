require(shiny)
require(rhandsontable)
require(shinythemes)

side_width <- 5

sidebar_panel <-
  sidebarPanel(
    width = side_width,
    radioButtons("AdviceType",
                 label = h4("Choose Catch Advice"),
                 choices = list("EU MAP Fmsy" = "MSY", 
                                "EU MAP Fmsy lower" = "MSYlow"), 
                 inline = TRUE, 
                 selected = "MSY"),
    
    h5(textOutput("ICESadv")),
    
hr(),

    radioButtons("TimeStep", 
                 label = h4("Choose Time Step"),
                 choices = list("Annual" = 1, "Monthly" = 12), 
                 inline = TRUE, 
                 selected = 1),
    
    br(),
    h5(helpText("Recreational management measures.")),

    # SelectOpenSeason is coming from renderUI in server.r
    uiOutput("SelectOpenSeason"), 

    # SelectBagLimit is coming from renderUI in server.r
    uiOutput("SelectBagLimit"), 

    h5(textOutput("RecF")),

    
    h5(textOutput("ICESadvComm")),

    hr(),

    h4(helpText("Select allowances for commercial gears.")),

    actionButton("go", "Go"),
    rHandsontableOutput('table')
  )


main_panel <-    
  mainPanel(
    width = 12 - side_width,
    tableOutput("CatchGearTable"),
    tableOutput("forecastTable"),
    #plotOutput("catch_plot"),
    plotOutput("plot"),
    verbatimTextOutput("debug_text"),
    verbatimTextOutput("debug_text_output")
  )
 
# user interface
ui <- 
  fluidPage(
    navbarPage("Seabass catch allocation Tool",
               theme=shinytheme("united"),
               position ="fixed-top",
               
               #img(src='ICES_logo_white.png',
               #width="111", 
               #height="53",
               #align = "right"),
               
               tabPanel("Instructions",
                        verbatimTextOutput("Instructions")), 
               tabPanel("Allocations",
    titlePanel("Seabass Catch Options Tool by Age"),
    sidebarLayout(
      sidebarPanel = sidebar_panel,
      mainPanel = main_panel,
      position = "left"
    )
    ),
    #title = "Seabass Catch Options Tool" # title in the tab
    tabPanel(
      "summary",
      verbatimTextOutput("summary")) 
    
               )
  )
