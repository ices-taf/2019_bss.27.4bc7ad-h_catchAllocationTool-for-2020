
# width of input panel in in allocations tab (full page = 12)
side_width <- 5

# ----------------------------
# panels for allocation tab

# allocations input panel
allocations_inputpanel <- 
  sidebarPanel(
    width = side_width,
    radioButtons("AdviceType",
                label = h4("Select 2020 catch advice"),
                choiceNames = list(HTML("EU MAP F<sub>MSY</sub>"), 
                                   HTML("EU MAP F<sub>MSY lower</sub>")),
                choiceValues = list("MSY", "MSYlow"), 
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

    # SelectBagLimit is coming from renderUI in server.r
    uiOutput("SelectBagLimit"), 

    h5(textOutput("RecF")),

    h5(textOutput("ICESadvComm")),

    hr(),
    h4(helpText("Input catch allocations (in tonnes)")),

    actionButton("go", "Run simulation"),
    rHandsontableOutput('table'),

    htmlOutput("RemQuota"),
    position = "left"
  )

# results side panel 
allocations_resultspanel <-            
  mainPanel(
    width = 12 - side_width,
    conditionalPanel(
      "output.hide_panel",              
      wellPanel(
        plotOutput("plot"),
        h5(helpText("Figure 1: Simulated catch-at-age, by gear. 
       The dashed line (---) indicates the predicted catch-at-age in the ICES forecast."))
      ),
      wellPanel(
        h5(helpText("Table 1: Simulated catch allocations. Catch allocations may be less than those entered since total
      catch is limited to the advice level chosen.")),
        br(),
        tableOutput("vclsGearTable")
      ),
      wellPanel(
        h5(helpText("Table 2: Simulated catch and F by gear, including recreational catches.")),
        br(),
        tableOutput("CatchGearTable")
      )
    )
  )

# full width results table
allocations_resultspanel_wide <- 
  fluidRow(
    column(12,
      conditionalPanel("output.hide_panel",
        wellPanel(
          h5(helpText("Table 3: Forecast scenarios. Comparison between the simulated scenario (highlighted row) and the basis of ICES advice for 2020.")), 
          br(),
          DT::dataTableOutput("forecastTable")
        )
      )
    )
  )


# ----------------------------
# main user interface

ui <- 
  navbarPage(
    # tab title
    windowTitle = "Sea bass catch allocation tool",

    # navbar title
    title =
      div(img(src = 'Sea_bass_Negative_LOGO.png',
              style = "margin-top: -14px; padding-right:10px;padding-bottom:10px", 
              height = 60)),
  
    tabPanel(
      "Instructions",
      includeMarkdown("Instructions.Rmd")
    ),
  
    tabPanel(
      "Allocations",
      sidebarLayout(
        sidebarPanel = allocations_inputpanel,
        mainPanel = allocations_resultspanel
      ),
      allocations_resultspanel_wide
    ),

    tabPanel(
      "Useful links",
      includeMarkdown("UsefulLinks.Rmd")  
     ),

    # extra tags, css etc
    tags$style(type = "text/css", "body {padding-top: 70px;}"),
    tags$head(tags$style(HTML('#go{background-color:#dd4814}'))),
    theme = shinytheme("united"),
    position = "fixed-top",

    tags$script(HTML("var header = $('.navbar > .container-fluid');
     header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-tools-prod/seabass-catch-allocation-tool\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
     console.log(header)"))
  )
