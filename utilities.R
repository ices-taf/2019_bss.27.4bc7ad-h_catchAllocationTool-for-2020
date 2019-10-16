# Function to optimis fleet Fmults to take the specified catches
# When optimising fmults, repress is set to T
gearCatches <- function(fmults, dat, pop, Frec, M, repress=T){
  gears <- unique(dat$gear)
  #M <- dat$M[1]
    
  fmort <- matrix(0,nrow=21,ncol=length(gears), dimnames=list(c(0:20), gears))
  for (gg in 1:length(gears)) fmort[1:sum(dat$gear==gears[gg]),gg] <- fmults[gg]*dat[dat$gear==gears[gg],"Selectivity"]
  zmort <- apply(fmort,1,sum) + Frec[,2] + M
  
  projCatch <- fmort[1,]; projCatch[] <- 0
  for (gg in 1:length(gears)) {
    datLen <- length(dat[dat$gear==gears[gg], "Selectivity"])
    projCatch[gg] <- sum((pop[1:datLen]*(1-exp(-zmort[1:datLen])) * dat[dat$gear==gears[gg], "Weight"]) * (fmort[1:datLen,gg]/zmort[1:datLen]),na.rm=T)
  }
  
  #return(sum((tac - sum((data$N[1:nrow(data)]*(1-exp(-zmort)) * data$Weight) * (fmort/zmort),na.rm=T))^2 ) )
  if (repress) return(projCatch)
  if (!repress)
  {
    catchN <- matrix(0,nrow=21,ncol=length(gears), dimnames=list(c(0:20), gears))
    for (gg in 1:length(gears)) {
      datLen <- length(dat[dat$gear==gears[gg], "gear"])
      catchN[1:datLen,gg] <- pop[1:datLen]*(1-exp(-zmort[1:datLen])) * (fmort[1:datLen,gg]/zmort[1:datLen])
    }
    
    return(list(gearCatches=projCatch, catch_n=catchN, catch_f=fmort,total_z=zmort))
  }
  
}

# Objective function for optimising fmults (sum of squares) 
objective_func <- function(log_fmults, catches, data, M, Frec, pop) {
  sum((gearCatches(fmults=log_fmults, dat=data, pop=pop, Frec=Frec, M=M, repress=T) - catches)^2)
}

##### Colin's original functions (altered for recreational by GL)

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
      catch_n = (n * (1-exp(-z)) * fmult * Selectivity / z) # 
      #  n * (1-exp(-z)) * f_age_rec_2020/ z # that was added by GL by mistake here -  
      # still need to make sure recreational is not anymore in the gear list
      # and that catch_n is also calculated for recreational but based on f_age_rec_2020 and no need for optimising
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
