#####-------------------------
### Issues
#- Should we work with 0-16+ or 0-20(+). N@A from forecast for 2020 goes to 16, most selectivities go to age 19, for recreational to 16.
# current simulation assumes zero selectivity for age 20, but this could be flat topped (i.e. = sel for age 19)
#- FIXED - Use recreational mean weights for use in forecast? Currently only using human consump mean weights.
#- Only catch selectivities for different gears. May need retention at age proportions for the fleets to calculate landings/discard quanitites
#- Include a button/checkbox to specify which advice to limit catches? (e.g. MAP upper or lower or Fmsy advice?)
#- No seasonality in recreational
#-
#-

#####-------------------------
### Libraries
require(dplyr)
require(ggplot2)
require(shiny)
require(rhandsontable)

#####-------------------------
### Additional functions
source("seabass-management-tool-age/utilities.R")
#source("utilities.R")

#####-------------------------
### Read in required data

## Initial population 
#TEMP# Only had population numbers at Jan 2020 out to age 16+. Here manually expanded the 16+ to fill ages up to 20.
pop_age_2020 <- read.csv("seabass-management-tool-age/data/pop_age_2020.csv")
# H.Cons Retained mean weights from 'Bass47_STF 2019 assessment 20190514.xlsx'
weights_age <- read.csv("seabass-management-tool-age/data/weights_age.csv")
weights_age_rec <- read.csv("seabass-management-tool-age/data/weights_age_rec.csv")

## Fleet selectivity by age
# assuming this is catch selectivity (i.e. L+D)
selectivity_age <- read.csv("seabass-management-tool-age/data/selectivity_age_16+.csv")

## Recreational fisheries
# F multipliers for bag limits and closed seasons
RecFs <- read.csv("seabass-management-tool-age/data/BagLimitFs.csv")
# Fbar of recreational fishery in 2012
Fbar_rec_2012 <- 0.0604
# Fbar of recreational fishery in 2019
Fbar_rec_2019 <- 0.019
# F at age of recreational fishery in 2019
F_age_rec_2019 <- c(0.000,0.000,0.001,0.001,0.006,0.013,0.013,0.023,0.015,0.022,0.020,
                    0.023,0.024,0.021,0.022,0.023,0.025)
# Z at age from the ICES advice forecasts
# This is used to estimate total Recreational catch
Advice_Z_age_2020 <- read.csv("seabass-management-tool-age/data/ICESadvice_Forecast_Zs.csv")

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
### Create default objects
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan21")
rowNames<-c(month.name,"TOTAL")
defaultDF<- data.frame(
  row.names = rowNames,
  Pelagic_Trawl=rep(NA_integer_, 13),
  Demersal_Trawl=rep(NA_integer_, 13),
  Hooks_and_Lines=rep(NA_integer_, 13),
  Gill_Nets=rep(NA_integer_, 13), 
  Seines=rep(NA_integer_, 13),
  stringsAsFactors = FALSE
)


#####-------------------------00000000000000000000000000000000-------------------------#####
### SERVER function
#####-------------------------00000000000000000000000000000000-------------------------#####
server <- function(input, output) {

  #####-------------------------
  ### Get Advice
  #TEMP# INPUT$ICESadvOpt replaces input$ICESadvOpt, which should comes from checkbox
  INPUT <- list()
  INPUT$ICESadvOpt <- "MSY"
  
  if (INPUT$ICESadvOpt=="MSY") INPUT$ICESadv <- ICESadvMSY else INPUT$ICESadv <- ICESadvMSYlow 
  
  #####-------------------------
  ### Get Recreational F
  #TEMP# INPUT$RecF replaces input$RecF, which comes from the blocked code below
  INPUT$RecF <- RecFs[3,3]
  
  # values <- reactiveValues(data = defaultDF) ## assign it with NULL
  #
  # ## changes in numericInput sets all (!) new values
  # observe({
  #   req(input$table)
  #   DF<-hot_to_r(input$table)
  #   DF[setdiff(rowNames,"TOTAL"),]
  #   DF["TOTAL",]<- colSums(DF[setdiff(rowNames,"TOTAL"),],na.rm=TRUE)
  #   values$data<-DF
  # })
  # 
  # output$table <- renderRHandsontable({
  #   req(values$data)
  #   rhandsontable(values$data, rowHeaderWidth = 100) %>%
  #     hot_row(nrow(values$data), readOnly = TRUE)
  # })
  # 
  # output$vx <- renderUI({
  #   selectInput("OpenSeason", "Select duration of open season", choices = c("0 months","3 months","6 months","7 months","9 months","10 months","12 months"),selected=1)
  # })
  # 
  # output$vy <- renderUI({
  #   selectInput("BagLimit", "Select Bag Limit size", choices = c("0 Fish","1 Fish","2 Fish","3 Fish","4 Fish","5 Fish"),selected=1)
  # })
  # 
  # vals <- reactiveValues()
  # observe({
  #   vals$vx  <- input$OpenSeason
  #   vals$vy<- input$BagLimit
  # })
  # 
  # # Line that fetches the correct value for recreational F depending on the selections made
  # output$RecF <- renderText({
  #   RecTemp <- RecFs[grepl(as.numeric(substring(vals$vx,1,2)),RecFs$OpenSeason, ignore.case = TRUE),]
  #   RecF<- RecTemp[1,as.numeric(substring(vals$vy,1,1))+2]
  #   
  #   paste("The recreational F for the options above is =",RecF )
  # })
  
  
  #####-------------------------
  ### calculate recreational F based on management measures
  # uses selected multiplier and 2012+2019 F@A and Fbar to estimate 2020 F@A
  f_age_rec_2020 <- cbind.data.frame(Age= weights_age_rec$Age[1:length(F_age_rec_2019)], 
                                     f_age_rec_2020 = INPUT$RecF*F_age_rec_2019*Fbar_rec_2012/Fbar_rec_2019)
  # Mean F for recreational ages 4-15
  FbarRec <- mean(f_age_rec_2020[5:16,2])
  
  # Monthly F values to use as estimates of recreational mortality in the monthly forecast (final rec catch values determined from annually below)
  f_age_rec_2020_month <- f_age_rec_2020; f_age_rec_2020_month[,2] <- f_age_rec_2020_month[,2]/12
  
  # Get recreational catch at age and total catch
  if(INPUT$ICESadvOpt=="MSY") totZ <- Advice_Z_age_2020[,"MSY_Z"] else totZ <- Advice_Z_age_2020[,"MSYlow_Z"]
  catchRec_n <- pop_age_2020[,"N"]*(1-exp(-totZ)) * ((f_age_rec_2020[,2])/totZ)
  recCatch <- sum(catchRec_n*weights_age_rec[,2])
  
  # Calculate what is left for the commercial fleets
  INPUT$ICESadvComm <- INPUT$ICESadv - recCatch  
  
  #####-------------------------
  ### Prepare data objects
  dat <-
    selectivity_age %>%
    left_join(weights_age) %>%
    #left_join(pop_age_2020) %>%
    mutate(M = M) #%>%
    #left_join(f_age_rec_2020) 
  
  # Gear types
  gears <- unique(selectivity_age$gear)
  
  # Population matrix (Jan 2020 to Jan 2021)
  initPop <- matrix(NA,17,13,dimnames=list(c(0:15,"16+"),months)) 
  initPop[,1] <- pop_age_2020[,"N"]
  
  #####-------------------------
  ##### Function to calculate difference between sp and fishing at a given F
  
  #TEMP# This should come from a checkbox specifying if monthly values are used or not. If not, input table should only show TOTALs.
  INPUT$Monthly <- F
  
  #TEMP# INPUT$CatchGear replaces input/ouput$CatchGear, which comes from the hands on table code below
  # This data file is not needed for the shiny operation
  INPUT$CatchGear <- read.csv("seabass-management-tool-age/data/CatchGear.csv")
  # Calculate TOTAL
  INPUT$CatchGear[13,-1] <- apply(INPUT$CatchGear[1:12,-1],2,sum)
  
  catches <- INPUT$CatchGear
  
  #####-------------------------
  ### Get fleet catches from input
  # Manually chosen as INPUT$CatchGear above
  
  # # generate dynamic UI elements
  # output$checkbox <- renderUI({
  #   checkboxGroupInput("selected_gears", "Select Gears", 
  #                      choices = gears, selected = gears[1],
  #                      inline = TRUE)
  # })
  # 
  # output$sliders <- renderUI({
  #   slider_gears <- input$selected_gears
  #   max_tonnes <- 5000 # as.integer(input$max_tonnes)
  #   lapply(slider_gears, function(x) {
  #     # keep the old value when adding new sliders
  #     old_val <- input[[gsub(" ", "_", x)]]
  #     if (is.null(old_val)) old_val <- 0
  #     
  #     sliderInput(inputId = gsub(" ", "_", x),
  #                 label = x,
  #                 min = 0, max = max_tonnes, value = old_val,
  #                 step = 50, post = "t", sep = ",")
  #     
  #     #sliderInput(inputId = gsub(" ", "_", x),
  #     #            label = x,
  #     #            min = 0, max = 2, step = 0.1, value = old_val)
  #   })
  #   
  # })
  # 
 
  # # generate outputs
  # reactiveCatches <- reactive({
  #   # get catches from input fields
  #   catches <-
  #     sapply(input$selected_gears,
  #          function(x) input[[gsub(" ", "_", x)]])
  #   
  #   # optimise f multipliers
  #   fmult <- catches_to_fmult(catches, dat, input)
  #   
  #   # compute catches
  #   calc_catches(dat, input, fmult)
  #   
  # })
  
  #####-------------------------
  ### Monthly forecast
  if (INPUT$Monthly) {
  
  # list to store results
  out <- list()
  # Switch for whether there is quota left or not (starts T, changes to F when quota used up. Rest of the months then have zero comm catch)
  switch <- T
  
  for (i in 1:(length(months)-1)) {
    # i <- 1

    #check how much of TAC has been taken
    if (switch) {
      caught <- 0
      if (i>1) for (ii in 1:(i-1)) caught <- caught + sum(out[[ii]]$gearCatches)
      remaining <- INPUT$ICESadvComm-caught
      if (remaining < sum(catches[i,-1])) {
        catches[i,-1] <- catches[i,-1] * (remaining/sum(catches[i,-1]))
        for (ii in (i+1):(length(months)-1)) catches[ii,-1] <- 0
        switch <- F  
      }
    } # end of switch loop
    
    # optimise Fmults to take the catches specified
    opt <- optim(rep(0, length(catches)), 
                 objective_func, 
                 catches = catches[i,-1],
                 data = dat,
                 M=M,
                 Frec = f_age_rec_2020_month,
                 pop=initPop[,months[i]])
    fmults <- opt$par
    
    # Use optimised fmults to get catch.n, commercial F and total Z 
    tmp <- gearCatches(fmults,dat, initPop[,i], f_age_rec_2020_month, M, repress=F)
    # # Get recreational catch at age and total catch
    # tmp$catchRec_n <- initPop[,i]*(1-exp(-tmp$total_z)) * (f_age_rec_2020[,2]/tmp$total_z)
    # tmp$recCatches <- sum(tmp$catchRec_n*weights_age_rec[,2])
    
    # Project population forward one month
    # Note, ages unchanged, for Jan2021 shifted one age older after this loop
    initPop[,i+1] <- initPop[,i]*exp(-tmp$total_z)
    
    # Save monthly results in list
    out[[i]] <- tmp; rm(tmp)
    
  } # end of loop over months
  
  } # END of Monthly simulation

  #####-------------------------
  ### Annual forecast
  if (!INPUT$Monthly) {
    
    # list to store results
    out <- list()
    
    # Cap catches at ICES advice
    if (INPUT$ICESadvComm < sum(catches[13,-1])) catches[13,-1] <- catches[13,-1] * (INPUT$ICESadvComm/sum(catches[13,-1]))
    
    # optimise Fmults to take the catches specified
    opt <- optim(rep(0, length(catches)), 
                 objective_func, 
                 catches = catches[13,-1],
                 data = dat,
                 M = Myr,
                 Frec = f_age_rec_2020,
                 pop=initPop[,1])
    fmults <- opt$par
    
    # Use optimised fmults to get catch.n, commercial F and total Z 
    tmp <- gearCatches(fmults=fmults,dat=dat, pop=initPop[,1], Frec=f_age_rec_2020, M=Myr, repress=F)
    # # Get recreational catch at age and total catch
    # tmp$catchRec_n <- initPop[,1]*(1-exp(-tmp$total_z)) * ((f_age_rec_2020[,2])/tmp$total_z)
    # tmp$recCatches <- sum(tmp$catchRec_n*weights_age_rec[,2])

    # Project population forward one month
    # Note, ages unchanged, for Jan2021 shifted one age older after this loop
    initPop[,13] <- initPop[,1]*exp(-tmp$total_z)
    
    # Save monthly results in list
    out[[1]] <- tmp; rm(tmp)
    
  } # END of Annual simulation
  
  #####-------------------------
  ### Sort results
 
  # Commercial catches
  asked <- INPUT$CatchGear[,-1]
  realised <- asked; realised[] <- 0
  if (INPUT$Monthly) {
    for (i in 1:(length(months)-1)) realised[i,] <- out[[i]]$gearCatches
    realised[13,] <- apply(realised[-13,],2,sum) 
    }
  if (!INPUT$Monthly) realised[13,] <- out[[1]]$gearCatches
  # round the values (have many decimals due to optimising Fmults)
  realised <- round(realised,0)
  totCommCatch <- sum(realised[13,])
  
  # Catch at age
  catch_n <- out[[1]]$catch_n
  if (INPUT$Monthly) for (i in 2:(length(months)-1)) catch_n <- catch_n + out[[i]]$catch_n 
  catch_n <- cbind(catch_n, catchRec_n)
  dimnames(catch_n)[[2]][6] <- "Recreational"
  
  # Commercial F and Fbar
  annualF <- out[[1]]$catch_f  
  if (INPUT$Monthly) for (i in 2:(length(months)-1)) annualF <- annualF + out[[i]]$catch_f   
  Fcomm <- apply(annualF,1,sum)  
  Fcommbar <- mean(Fcomm[5:16]) # ages 4-15
  # ICES rounding
  if (Fcommbar<0.2) Fcommbar <- round(Fcommbar,3) else Fcommbar <- round(Fcommbar,2)
  
  # Annual recreational catch and F
  # recCatch
  # FbarRec
  
  # Catch including recreational
  realised <- cbind(realised, realised[,1]); realised[,6] <- NA
  dimnames(realised)[[2]][6] <- "Recreational"; 
  realised[13,6] <- recCatch  
  realised <- round(realised,0)
  
  # Change ages for Jan 2021
  #initPop[,1] * exp(-(commF+(f_age_rec_2020[,2]*12)+Myr))
  initPop[nrow(initPop),13] <- initPop[nrow(initPop),13] + initPop[nrow(initPop)-1,13] 
  initPop[1:(nrow(initPop)-1),13] <- c(0,initPop[1:(nrow(initPop)-2),13])
  
  #####-------------------------
  ### Show outputs
  # Need to update these to use results from monthly simulation
  
  # output$values <- renderTable({
  #   reactiveCatches() %>%
  #     group_by(gear) %>%
  #     summarise(
  #       catch = sum(catch_n * Weight)
  #     )
  # })
  # 
  # output$catch_plot <- renderPlot({      
  #   ggplot(reactiveCatches(), 
  #          aes(x = Age, y = catch_n, fill = gear)) + 
  #     geom_area(position = 'stack')
  # })  
  # 
  # 
  # output$catchGear_plot <- renderPlot({      
  #   g <- ggplot(    reactiveCatches() %>%
  #                     group_by(gear) %>%
  #                     summarise(
  #                       catch = sum(catch_n * Weight)
  #                     ), aes(gear, catch))
  #   g + geom_bar(stat="identity", width = 0.5, fill="tomato") + 
  #     labs(title="Catch (t)", 
  #          subtitle="By gear") +
  #     theme(axis.text.x = element_text(angle=65, vjust=0.6))
  #   
  # })  
  # 
  # output$n_plot <- renderPlot({
  #   ggplot(pop_age_2020, aes(x = Age, y = n)) + 
  #     geom_line()
  #   
  # })
  
  #####-------------------------
  ### for debugging  
  # Don't show in final
  
  output$debug_text <- renderText({ 
    txt <- 
      lapply(names(input), 
             function(x) c(paste0(x, ":"), capture.output(str(input[[x]])), ""))
    
    txt <- unlist(txt)
    
    txt <- c("Debug window:", "-------------\n", txt)
    
    paste(txt, collapse = "\n")
  })
  
}

