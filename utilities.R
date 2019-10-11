calc_catches <- function(data, input, fmults) {
  
  data %>%
    right_join(
      data.frame(
        gear = input$selected_gears, 
        #        fmult = sapply(input$selected_gears, function(x) input[[gsub(" ", "_", x)]])
        fmult = fmults
      ), by = "gear") %>%
    group_by(Age) %>%
    mutate(
      z = sum(fmult * Selectivity) + M + sum(f_age_rec_2020)
    ) %>%
    ungroup() %>%
    mutate(
      catch_n = (n * (1-exp(-z)) * fmult * Selectivity / z) + # need to make sure recreational is not anymore in the gear list
        n * (1-exp(-z)) * f_age_rec_2020/ z
    )
}

totals <- function(data, input, fmults) {
  out <- 
    calc_catches(data, input, fmults) %>%
    group_by(gear) %>%
        summarise(
          catch = sum(catch_n * Weight)
        )
  out$catch
}

catches_to_fmult <- function(catches, data, input) {

  objective_func <- function(log_fmults, catches, data, input) {
    sum((totals(data, input, exp(log_fmults)) - catches)^2)
  }

  opt <- 
    optim(rep(0, length(catches)), 
          objective_func, 
          catches = catches,
          data = data,
          input = input)
  exp(opt$par)
}
