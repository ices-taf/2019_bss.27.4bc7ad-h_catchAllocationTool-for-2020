#####-------------------------
### Issues
#- No seasonality in recreational
#-
#-

#####-------------------------
### Libraries
require(dplyr)
require(ggplot2)
require(shiny)
require(rhandsontable)
require(markdown)
require(shinythemes)
require(Matrix)


#####-------------------------
### Additional functions
source("utilities.R")

#####-------------------------
### Read in required data

## Initial population 
#TEMP# Only had population numbers at Jan 2020 out to age 16+. Here manually expanded the 16+ to fill ages up to 20.
pop_age_2020 <- read.csv("data/pop_age_2020.csv")
# H.Cons Retained mean weights from 'Bass47_STF 2019 assessment 20190514.xlsx'
weights_age <- read.csv("data/weights_age.csv")
weights_age_rec <- read.csv("data/weights_age_Rec.csv")

## Fleet size (no vessel by gear)
noVessels <- read.csv("data/Number_Vessels.csv")


## Fleet selectivity by age
# assuming this is catch selectivity (i.e. L+D)
selectivity_age <- read.csv("data/selectivity_age_16+.csv")
# Use advice forecast discard F selectivity for all fleet (quantity of disards by fleet from discard proportion estimates, below)
discard_Sel <- read.csv("data/discard_selectivity.csv")

## Discards
# discard proportions by gear (from last 3 years of French and English data)
# adjusted to match assumed total discard rate in the advice forecast
# no discard selectivites available, so projection simply uses gear landings selectivities
# discard proportion is used on the results to divide total catch by gear in landings and discards by gear (only C@A will be presented, no L@A or D@A)
discard_prop <- read.csv("data/discard_proportions.csv")

## Recreational fisheries
# F multipliers for bag limits and closed seasons
RecFs <- read.csv("data/BagLimitFs.csv")
# Fbar of recreational fishery in 2012
Fbar_rec_2012 <- 0.0604
# Fbar of recreational fishery in 2019
Fbar_rec_2019 <- 0.018829873
# F at age of recreational fishery in 2019
# F_age_rec_2019 <- c(0.000,0.000,0.001,0.001,0.006,0.013,0.013,0.023,0.015,0.022,0.020,
#                     0.023,0.024,0.021,0.022,0.023,0.025)
F_age_rec_2019 <- c(0,	0.000205609,	0.00118539,	0.001176412,	0.006351395,	0.013477775,	0.012815346,
                    0.023498693,	0.014668892,	0.022002183,	0.020376389,	0.023425403,	0.023519036,
                    0.021145238,	0.021742123,	0.022936007,	0.024790632)

# Z at age from the ICES advice forecasts
# This is used to estimate total Recreational catch
Advice_Z_age_2020 <- read.csv("data/ICESadvice_Forecast_Zs.csv")
# Forecasted Catch at age
AdviceForecastCatchAge <- read.csv("data/AdviceForecastCatchAge.csv")


## Natural mortality 
# per year
Myr <- 0.24
# per Month
M <- Myr/12

## ICES advice (http://ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/bss.27.4bc7ad-h.pdf)
# Options for MAP
ICESadvMSY <- 1946
ICESadvMSYlow <- 1634
# Recreational catches from catch scenario (not used as a limit, just for comparison)
ICESadvMSYRec <- 412
ICESadvMSYlowRec <- 346

#####-------------------------
### Create table structure
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan21")
rowNames <- list("12" = c(month.name), "1" = c("Year"))
# set up with default values?
#defaultDF <- read.csv("data/CatchGear.csv", row.names = 1)

#####-------------------------00000000000000000000000000000000-------------------------#####
### SERVER function
#####-------------------------00000000000000000000000000000000-------------------------#####

source("utilities.R")

server <- function(input, output) {

  #####-------------------------
  ### Dynamic input sections

  # Code to select the recreational options from section boxes  
  output$SelectOpenSeason <- renderUI({
    selectInput(
      "OpenSeason", 
      label= div(style = "font-size:13px","Duration of open season"), 
      choices = c("0 months"  = 1,
                  "3 months"  = 2,
                  "6 months"  = 3,
                  "7 months"  = 4,
                  "9 months"  = 5,
                  "10 months" = 6,
                  "12 months" = 7),
      width = "40%",
      selected = 4)
  })

  output$SelectBagLimit <- renderUI({
    selectInput(
      "BagLimit", 
      label= div(style = "font-size:13px","Bag limit size"), 
      choices = c("1 Fish"  = 1,
                  "2 Fish"  = 2,
                  "3 Fish"  = 3,
                  "4 Fish"  = 4,
                  "5+ Fish" = 5),
      width = "40%",
      selected = 1)
  })


  #####-------------------------
  ### Reactive section
  
  # process options, but do not run forecast
  reactiveData <- reactive({
    # Get advice value and total Z from advcie forecast
    ICESadvOpt <- input[["AdviceType"]] 
    if (input[["AdviceType"]] == "MSY") {
      ICESadv <- ICESadvMSY
      totZ <- Advice_Z_age_2020[, "MSY_Z"]
    } else {
      ICESadv <- ICESadvMSYlow
      totZ <- Advice_Z_age_2020[, "MSYlow_Z"]
    }

    ## Get multiplier
    RecF <- RecFs[as.numeric(input$OpenSeason), as.numeric(input$BagLimit)+1]
    # on first loop, RecF is not initialised, so put in a dummy value to avoid errors
    if (length(RecF) == 0) {
      RecF <- RecFs[1,1]
    }
    
    ## calculate recreational F based on management measures
    # uses selected multiplier and 2012+2019 F@A and Fbar to estimate 2020 F@A
    f_age_rec_2020 <- 
      cbind.data.frame(Age = weights_age_rec$Age[1:length(F_age_rec_2019)], 
                       f_age_rec_2020 = RecF * F_age_rec_2019 * Fbar_rec_2012 / Fbar_rec_2019)

    # Mean F for recreational ages 4-15
    FbarRec <- mean(f_age_rec_2020[5:16, 2])

    # Monthly F values to use as estimates of recreational mortality in the monthly forecast (final rec catch values determined from annually below)
    f_age_rec_2020_month <- f_age_rec_2020; f_age_rec_2020_month[,2] <- f_age_rec_2020_month[,2]/12
    
    # Get recreational catch at age and total catch
    catchRec_n <- pop_age_2020[,"N"]*(1-exp(-totZ)) * ((f_age_rec_2020[,2])/totZ)
    recCatch <- sum(catchRec_n*weights_age_rec[,2])

  
    # Calculate what is left for the commercial fleets
    ICESadvComm <- ICESadv - recCatch  
    
    ## Monthly or Annual forecast
    Monthly <- input$TimeStep == 12
    
    ## Fleet catches
    if (!is.null(input$table)) {
      CatchGear <- hot_to_r(input$table)      
    } else {
      # initialise CatchGear variable to stop errors
      CatchGear <- data.frame(Demersal_Trawl = 0, 
                               Gill_Nets = 0,
                               Hooks_and_Lines = 0,
                               Seines = 0)
    }

    names(CatchGear) <- gsub(" ","_",names(CatchGear))
    # Calculate TOTAL
    CatchGear[13,] <- apply(CatchGear[1:12,], 2, sum, na.rm=T)
    
    catches <- CatchGear
    catches[is.na(catches)] <- 0
    for (ii in 1:(nrow(catches)-1)) catches[ii,] <- catches[ii,]*noVessels[,2]
    CatchGear[13,] <- apply(CatchGear[1:12,], 2, sum, na.rm=T)
    
    # collect outputs
    list(CatchGear = CatchGear,
         FbarRec = FbarRec, 
         recCatch = recCatch,
         Monthly = Monthly, 
         ICESadvComm = ICESadvComm, 
         catches = catches,
         f_age_rec_2020_month = f_age_rec_2020_month,
         f_age_rec_2020 = f_age_rec_2020,
         catchRec_n = catchRec_n,
         recCatch = recCatch,
         ICESadv = ICESadv,
         ICESadvOpt = ICESadvOpt
         )
  })

  # run forecast on click "Run Simulation"
  reactiveForecast <- eventReactive(input$go, {
    dat <- reactiveData()

    with(dat,
        runForecast(
          months = months,
          selectivity_age = selectivity_age, 
          weights_age = weights_age, 
          M = M, 
          pop_age_2020 = pop_age_2020,
          f_age_rec_2020 = f_age_rec_2020,
          f_age_rec_2020_month = f_age_rec_2020_month,
          discard_prop = discard_prop,
          discard_Sel = discard_Sel,
          catches = catches,
          Monthly = Monthly,
          ICESadvComm = ICESadvComm,
          ICESadv = ICESadv,
          ICESadvOpt = ICESadvOpt,
          Myr = Myr,
          CatchGear = CatchGear,
          recCatch = recCatch,
          catchRec_n = catchRec_n,
          FbarRec = FbarRec,
          AdviceForecastCatchAge = AdviceForecastCatchAge,
          noVessels=noVessels
        )
    )
  })


  #####-------------------------
  # Dynamic input table of catches
  
  valuesUser <- reactiveValues(data = NULL) ## assign it with NULL
  
  observeEvent(input$TimeStep, {
    req(input$TimeStep)
    valuesUser$data <- data.frame(
      row.names = rowNames[[input$TimeStep]],
      Demersal_Trawl = rep(NA, input$TimeStep),
      Gill_Nets = rep(NA, input$TimeStep), 
      Hooks_and_Lines = rep(NA, input$TimeStep),
      Seines = rep(NA, input$TimeStep),
      stringsAsFactors = FALSE
    )
  })
  
  
  # changes in numericInput sets all (!) new values
  observe({
    req(input$table)
    DF <- hot_to_r(input$table)
    DF[setdiff(rownames(DF),"TOTAL"),]
    DF["TOTAL",] <- colSums(DF[setdiff(rownames(DF),"TOTAL"),], na.rm = TRUE)
    names(DF) <- gsub("_"," ",names(DF))
    valuesUser$data <- DF
  })
  

  #####-------------------------
  ### prepare outputs

  output$table <- renderRHandsontable({
    req(valuesUser$data)
    rhandsontable(valuesUser$data, rowHeaderWidth = 90, colWidths = 119) %>%
      hot_row(nrow(valuesUser$data), readOnly = TRUE)
  })

  
  # Line that calculates the remaining of catches after the recreational options
  output$RemQuota <- renderText({
    # get data input by user
    tmpX <- valuesUser$data[setdiff(rownames(valuesUser$data), "TOTAL"), ]
    # loop over rows and scale by number of vessels per fleet
    for (ii in 1:nrow(tmpX)) tmpX[ii,] <- tmpX[ii,] * noVessels[,2]
    # sum catch and subtract from the advice
    leftOver <- round(reactiveData()$ICESadvComm - sum(tmpX, na.rm = TRUE), 0)
    
    # format output
    colour <- if (leftOver < 0) "red" else "green" 
    paste0("Quota remaining: <span style=\"color:", colour, "\"> ", leftOver," t</span>")
  })


  # Line that fetches the correct value for recreational F depending on the selections made
  output$RecF <- renderText({
    paste0(
      "For the options above, ",
      round(reactiveData()$recCatch, 0),
      " t of fish (F = ",
      round(reactiveData()$FbarRec, 3),
      ") will be removed by the recreational fishery (through catches or mortality after catch-and-release)."
    )
  })
  
  
  # plot catch at age
  output$plot <- renderPlot({
    reactiveForecast()$catchNplot
  })


  # Output to get a dynamic output table using the time step 
  output$CatchGearTable <- renderTable({
    req(input$TimeStep)
      
    if (input$TimeStep == 12) {
      reactiveForecast()$CatchGearTable
    } else {
      reactiveForecast()$CatchGearTable[c(13,14),]
    }
  })
 

  output$vclsGearTable <- renderTable({
    req(input$TimeStep)
    
    if (input$TimeStep == 12) {
      reactiveForecast()$vclsGearTable
    } else {      
      rbind( 
        c("Average monthly catch/vessel", 
          round(as.numeric(reactiveForecast()$vclsGearTable[13, -1]) / 12, 2)),
        reactiveForecast()$vclsGearTable[13, ]
      )
    }
  })
  

  # Output forecast table (table 3)
  output$forecastTable <- DT::renderDataTable(
    DT::datatable(
      reactiveForecast()$forecastTable, 
      options = list(dom = 't')) %>% 
    DT::formatStyle(
      'Basis',
      target = 'row',
      color = DT::styleEqual("Simulated Scenario",'white'),
      backgroundColor = DT::styleEqual("Simulated Scenario", '#dd4814')
      )
  )
  
  
  # output the value for the advice type choosen
  output$ICESadv <- renderText({
    paste0(
      "The initial advice is = ", 
      reactiveData()$ICESadv, " t")
      
  })
  
  # Output for the total amont available after the recreational selection
  output$ICESadvComm <- renderText({
    paste0(
      " Remaining available catch is = ", 
      round(reactiveData()$ICESadvComm,0),
      " t.")
    
  })
      
  # hiding wellPanels  
  output$hide_panel <- eventReactive(input$go, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
}
