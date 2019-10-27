require(shiny)
require(rhandsontable)
require(shinythemes)
require(markdown)
require(dplyr)
require(ggplot2)

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

    h4(helpText("Input catch allocations per vessel (in tonnes)")),

    actionButton("go", "Run simulation"),
    rHandsontableOutput('table'),

    #h5(textOutput("RemQuota"))
     htmlOutput("RemQuota")
  )


main_panel <-    
  mainPanel(
    width = 12 - side_width,
    
    conditionalPanel("output.hide_panel",
    
  wellPanel(
      plotOutput("plot"),
       #verbatimTextOutput("FigureCap") 
     h5(helpText("Figure 1: Simulated catch at age, by gear. 
                 The dashed line (---) indicates the predicted catch at age in the ICES forecast."))),
  wellPanel(
    h5(helpText("Table 1: Simulated catch allocations. Catch allocations may be less than inputted since total
                catch is limited to the chosen advice level.")),
    br(),
    #verbatimTextOutput("AllocTabCap"),
    tableOutput("vclsGearTable")),
  
  wellPanel(
    h5(helpText("Table 2: Simulated catch and F by gear, including recreational catches.")),
  br(),
    #verbatimTextOutput("CatchTabCap"),
    tableOutput("CatchGearTable")),
  
   wellPanel(
     fluidRow(
       column(width=12),
  
    h5(helpText("Table 3: Forecast scenarios.")),
    br(),
    #verbatimTextOutput("ScenTabCap"),
    
    #tableOutput("forecastTable"))
  
  DT::dataTableOutput("forecastTable"))
  
    #plotOutput("catch_plot"),
    #verbatimTextOutput("debug_text"),
    #verbatimTextOutput("debug_text_output")
  )
    )
  )
# user interface
ui <- shiny::navbarPage(
  
  title = div(img(src='ICES_logo_toolName.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  #windowTitle="Seabass catch allocation Tool", 

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
      verbatimTextOutput("summary")

      ),
    
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$head(tags$style(HTML('#go{background-color:#dd4814}'))),
    theme=shinytheme("united"),
    position ="fixed-top",
    
    tags$script(HTML("var header = $('.navbar > .container-fluid');
 header.append('<div style=\"float:right\"><ahref=\"https://github.com/ices-taf/2019_4017-19_TechnicalService\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"> </a>`</div>');
 console.log(header)"))
    
               )
 
