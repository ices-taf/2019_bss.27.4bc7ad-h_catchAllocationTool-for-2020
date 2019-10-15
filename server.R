require(dplyr)

require(ggplot2)

require(shiny)
require(rhandsontable)

RecFs <- read.csv("data/Recreational/BagLimitFs.csv")

# Fbar of recreational fishery in 2012
Fbar_rec_2012 <- 0.0604
# Fbar of recreational fishery in 2019
Fbar_rec_2019 <- 0.019
# F at age of recreational fishery in 2019
F_age_rec_2019 <- c(0.000,0.000,0.001,
                    0.001,0.006,0.013,0.013,
                    0.023,0.015,0.022,0.020,
                    0.023,0.024,0.021,0.022,
                    0.023,0.025)

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


server <- function(input, output) {
  
  
  values <- reactiveValues(data = defaultDF) ## assign it with NULL
  
  ## changes in numericInput sets all (!) new values
  observe({
    req(input$table)
    DF<-hot_to_r(input$table)
    DF[setdiff(rowNames,"TOTAL"),]
    DF["TOTAL",]<- colSums(DF[setdiff(rowNames,"TOTAL"),],na.rm=TRUE)
    values$data<-DF
  })
  
  output$table <- renderRHandsontable({
    req(values$data)
    rhandsontable(values$data, rowHeaderWidth = 100) %>%
      hot_row(nrow(values$data), readOnly = TRUE)
    
    
  })
  
  
  output$vx <- renderUI({
    selectInput("OpenSeason", "Select duration of open season", choices = c("0 months","3 months","6 months","7 months","9 months","10 months","12 months"),selected=1)
  })
  
  output$vy <- renderUI({
    selectInput("BagLimit", "Select Bag Limit size", choices = c("0 Fish","1 Fish","2 Fish","3 Fish","4 Fish","5 Fish"),selected=1)
  })
  
  vals <- reactiveValues()
  observe({
    vals$vx  <- input$OpenSeason
    vals$vy<- input$BagLimit
  })
  
  # Line that fetches the correct value for recreational F depending on the selections made
  output$RecF <- renderText({
    RecTemp <- RecFs[grepl(as.numeric(substring(vals$vx,1,2)),RecFs$OpenSeason, ignore.case = TRUE),]
    RecF<- RecTemp[1,as.numeric(substring(vals$vy,1,1))+2]
    
    paste("The recreational F for the options above is =",RecF )
    
    
  })
  
  
  #source("seabass-management-tool-age/utilities.R")
  
  source("utilities.R")
  
  
  
  # natural mortality per time step
  
  M <- 0.24
  
  
  
  # read initial population
  
  #pop_age <- read.csv("seabass-management-tool-age/data/pop_age.csv")
  
  #selectivity_age <- read.csv("seabass-management-tool-age/data/selectivity_age.csv")
  
  #weights_age <- read.csv("seabass-management-tool-age/data/weights_age.csv")
  
  pop_age <- read.csv("data/pop_age.csv")
  
  selectivity_age <- read.csv("data/selectivity_age.csv")
  
  weights_age <- read.csv("data/weights_age.csv")
  
  # calculate F mult recreational based on management measures
  f_age_rec_2020 <- cbind.data.frame(Age= weights_age$Age[1:length(F_age_rec_2019)], 
                                     f_age_rec_2020 = RecF*F_age_rec_2019*Fbar_rec_2012/Fbar_rec_2019)
  
  
  
  dat <-
    
    selectivity_age %>%
    
    left_join(weights_age) %>%
    
    left_join(pop_age) %>%
    
    mutate(M = M) %>%
    
    left_join(f_age_rec_2020) 
  
  
  # at the moment pop age is to age 20 and selex for recreational to age 16 - temp fix
  dat$f_age_rec_2020[is.na(dat$f_age_rec_2020)] <- 
    max(dat$f_age_rec_2020[!is.na(dat$f_age_rec_2020)] )
  
  
  # set up fixed values
  
  gears <- unique(selectivity_age$gear)
  
  
  
  # generate dynamic UI elements
  
  output$checkbox <- renderUI({
    
    checkboxGroupInput("selected_gears", "Select Gears", 
                       
                       choices = gears, selected = gears[1],
                       
                       inline = TRUE)
    
  })
  
  
  
  output$sliders <- renderUI({
    
    slider_gears <- input$selected_gears
    
    max_tonnes <- 5000 # as.integer(input$max_tonnes)
    
    lapply(slider_gears, function(x) {
      
      # keep the old value when adding new sliders
      
      old_val <- input[[gsub(" ", "_", x)]]
      
      if (is.null(old_val)) old_val <- 0
      
      
      
      sliderInput(inputId = gsub(" ", "_", x), 
                  
                  label = x,
                  
                  min = 0, max = max_tonnes, value = old_val, 
                  
                  step = 50, post = "t", sep = ",")
      
      
      
      
      
      #sliderInput(inputId = gsub(" ", "_", x),
      
      #            label = x, 
      
      #            min = 0, max = 2, step = 0.1, value = old_val)
      
    })
    
  })
  
  
  
  
  
  # generate outputs
  
  
  
  reactiveCatches <- reactive({
    
    # get catches from input fields
    
    catches <- 
      
      sapply(input$selected_gears, 
             
             function(x) input[[gsub(" ", "_", x)]])
    
    
    
    # optimise f multipliers
    
    fmult <- catches_to_fmult(catches, dat, input)
    
    
    
    # compute catches
    
    calc_catches(dat, input, fmult)
    
  })
  
  
  
  output$values <- renderTable({
    
    reactiveCatches() %>%
      
      group_by(gear) %>%
      
      summarise(
        
        catch = sum(catch_n * Weight)
        
      )
    
  })
  
  
  
  output$catch_plot <- renderPlot({      
    
    ggplot(reactiveCatches(), 
           
           aes(x = Age, y = catch_n, fill = gear)) + 
      
      geom_area(position = 'stack')
    
  })  
  
  
  
  output$n_plot <- renderPlot({
    
    ggplot(pop_age, aes(x = Age, y = n)) + 
      
      geom_line()
    
  })
  
  
  
  
  
  # for debugging  
  
  output$debug_text <- renderText({ 
    
    
    
    txt <- 
      
      lapply(names(input), 
             
             function(x) c(paste0(x, ":"), capture.output(str(input[[x]])), ""))
    
    txt <- unlist(txt)
    
    
    
    txt <- c("Debug window:", "-------------\n", txt)
    
    paste(txt, collapse = "\n")
    
  })
  
}