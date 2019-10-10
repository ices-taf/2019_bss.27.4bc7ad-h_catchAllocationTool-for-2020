
server <- function(input, output) {
  
  require(dplyr)
  require(ggplot2)
  require(shiny)
  
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
  
  dat <-
      selectivity_age %>%
      left_join(weights_age) %>%
      left_join(pop_age) %>%
      mutate(M = M)

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
