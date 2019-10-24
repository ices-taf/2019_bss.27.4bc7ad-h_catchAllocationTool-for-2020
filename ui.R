require(shiny)
require(rhandsontable)
require(shinythemes)

side_width <- 5

sidebar_panel <-
  sidebarPanel(
    width = side_width,
    radioButtons("AdviceType",
                 label = h4("Select catch advice"),
                 choices = list("EU MAP Fmsy" = "MSY", 
                                "EU MAP Fmsy lower" = "MSYlow"), 
                 inline = TRUE, 
                 selected = "MSY"),
    
    h5(textOutput("ICESadv")),
    
hr(),

    radioButtons("TimeStep", 
                 label = h4("Select time step"),
                 choices = list("Annual" = 1, "Monthly" = 12), 
                 inline = TRUE, 
                 selected = 1),
    
    hr(),

    h4(helpText(" Select recreational management measures.")),

    # SelectOpenSeason is coming from renderUI in server.r
    uiOutput("SelectOpenSeason"), 

# tags$div(selectInput("SelectOpenSeason",
#                      "Select duration of open season   ", 
#                      choices = c("0 months"  = 1,
#                                  "3 months"  = 2,
#                                  "6 months"  = 3,
#                                 "7 months"  = 4,
#                                  "9 months"  = 5,
#                                "10 months" = 6,
#                                  "12 months" = 7),
#                      width = "120%",
#                      selected = 5),  style="display:inline-block"),



    # SelectBagLimit is coming from renderUI in server.r
    uiOutput("SelectBagLimit"), 

# tags$div(selectInput("SelectBagLimit",
#                      "   Select Bag Limit size", 
#                      choices = c("1 Fish"  = 1,
#                                  "2 Fish"  = 2,
#                                 "3 Fish"  = 3,
#                                "4 Fish"  = 4,
#                               "5+ Fish" = 5),
#                  width = "150%",
#                 selected = 1),  style="display:inline-block"),




    h5(textOutput("RecF")),

    
    h5(textOutput("ICESadvComm")),

    hr(),

    h4(helpText("Input catch allocations")),

    actionButton("go", "Run simulation"),
    rHandsontableOutput('table'),

    #h5(textOutput("RemQuota"))
     htmlOutput("RemQuota")
  )


main_panel <-    
  mainPanel(
    width = 12 - side_width,
    verbatimTextOutput("FigureCap"),
    plotOutput("plot"),
    verbatimTextOutput("AllocTabCap"),
    tableOutput("CatchGearTable"),
    verbatimTextOutput("ScenTabCap"),
    tableOutput("forecastTable")
    #plotOutput("catch_plot"),
    #verbatimTextOutput("debug_text"),
    #verbatimTextOutput("debug_text_output")
  )
 
# user interface
ui <- 
  fluidPage(
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$head(tags$style(HTML('#go{background-color:#dd4814}'))),
    
    navbarPage("Seabass catch allocation Tool",
               theme=shinytheme("united"),
               position ="fixed-top",
               
               #img(src='ICES_logo_white.png',
               #width="111", 
               #height="53",
               #align = "right"),
               
               tabPanel("Instructions",
                        
                        includeMarkdown("Instructions.Rmd")
                        
                        ),
                        
                        
                        
               tabPanel("Allocations",
                        
    #titlePanel("Seabass Catch Options Tool by Age"),
    sidebarLayout(
      sidebarPanel = sidebar_panel,
      mainPanel = main_panel,
      position = "left"
    )
    ),
    #title = "Seabass Catch Options Tool" # title in the tab
    tabPanel(
      "Useful links",
      includeMarkdown("UsefulLinks.Rmd"),
      verbatimTextOutput("summary")) 
    
               )
  )
