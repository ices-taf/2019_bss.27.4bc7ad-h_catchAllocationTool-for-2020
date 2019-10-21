require(shiny)
require(rhandsontable)
require(markdown)
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
                 selected = "MSYlow"),

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

    textOutput("RecF"),

    br(),
    h5(helpText("Select allowances for commercial gears.")),

    actionButton("go", "Go"),
    rHandsontableOutput('table')
  )


main_panel <-    
  mainPanel(
    
    width = 12 - side_width,
    verbatimTextOutput("debug_text"),
    #tableOutput("values"),
    #plotOutput("catch_plot"),
    plotOutput("plot"),
    verbatimTextOutput("debug_text_output")
  )

       
 
# user interface
ui <- 
  fluidPage(
    #titlePanel("Seabass Catch Options Tool by Age"),
    #shinythemes::themeSelector(),
    list(tags$head(HTML('<link rel="icon", href="ICES_logo_orange.png", 
                                   type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="My Window Title"
        )
    ),

    navbarPage("Seabass catch allocation Tool",
               theme=shinytheme("united"),
               title=div(img(src="ICES_logo_orange.png"), "My Title in the Navbar"),
                        
             #img(src='ICES_logo_orange.png',
                   #width="111", 
                   #height="53",
                   #align = "right"),
               
      tabPanel("Instructions",
                 verbatimTextOutput("Instructions")), 
     tabPanel("Allocations",
    sidebarLayout(
      sidebarPanel = sidebar_panel,
      mainPanel = main_panel,
      position = "left"
    )
    
    #title = "Seabass Catch Options Tool" # title in the tab

),
tabPanel(
  "summary",
  verbatimTextOutput("summary")) 

)
)