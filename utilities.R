# Function to optimis fleet Fmults to take the specified catches
# When optimising fmults, repress is set to T
gearCatches <- function(fmults, dat, pop, Frec, disSel, M, repress=T){
  gears <- unique(dat$gear)
  #M <- dat$M[1]
    
  fmort <- matrix(0,nrow=17,ncol=length(gears), dimnames=list(c(0:15,"16+"), gears))
  dismort <- matrix(0,nrow=17,ncol=length(gears), dimnames=list(c(0:15,"16+"), gears))
  for (gg in 1:length(gears)) {
    fmort[,gg] <- fmults[gg]*dat[dat$gear==gears[gg],"Selectivity"]
    dismort[,gg] <- fmults[gg+length(gears)]*disSel
    }
  zmort <- apply(fmort,1,sum) + apply(dismort,1,sum) + Frec[,2] + M
  
  projCatch <- matrix(0,nrow=1,ncol=2*length(gears), dimnames=list("catch", c(paste(gears,"Land",sep="_"),paste(gears,"Dis",sep="_"))))
  for (gg in 1:length(gears)) {
    projCatch[gg] <- sum((pop*(1-exp(-zmort)) * dat[dat$gear==gears[gg], "Weight"]) * (fmort[,gg]/zmort),na.rm=T)
    # could use discard weights (only very slightly different from landings weights)
    projCatch[gg+length(gears)] <- sum((pop*(1-exp(-zmort)) * dat[dat$gear==gears[gg], "Weight"]) * (dismort[,gg]/zmort),na.rm=T)
  }
  
  #return(sum((tac - sum((data$N[1:nrow(data)]*(1-exp(-zmort)) * data$Weight) * (fmort/zmort),na.rm=T))^2 ) )
  if (repress) return(projCatch)
  if (!repress)
  {
    catchN <- matrix(0,nrow=17,ncol=length(gears), dimnames=list(c(0:15,"16+"), gears))
    for (gg in 1:length(gears)) {
      catchN[,gg] <- pop*(1-exp(-zmort)) * ((fmort[,gg]+dismort[,gg])/zmort)
    }
    landN <- matrix(0,nrow=17,ncol=length(gears), dimnames=list(c(0:15,"16+"), gears))
    for (gg in 1:length(gears)) {
      landN[,gg] <- pop*(1-exp(-zmort)) * (fmort[,gg]/zmort)
    }
    disN <- matrix(0,nrow=17,ncol=length(gears), dimnames=list(c(0:15,"16+"), gears))
    for (gg in 1:length(gears)) {
      disN[,gg] <- pop*(1-exp(-zmort)) * (dismort[,gg]/zmort)
    }
    return(list(gearCatches=projCatch, catch_n=catchN, land_n=landN, dis_n=disN, 
                catch_f=fmort+dismort, land_f=fmort, dis_f=dismort, total_z=zmort))
  }
  
}

# Objective function for optimising fmults (sum of squares) 
objective_func <- function(log_fmults, catches, data, M, Frec, disSel, pop) {
  sum((gearCatches(fmults=log_fmults, dat=data, pop=pop, Frec=Frec, disSel=disSel, M=M, repress=T) - catches)^2)
}
# added penalty to prevent negative Fmults
# objective_func <- function(log_fmults, catches, data, M, Frec, disSel, pop) {
#   otpt <- gearCatches(fmults=log_fmults, dat=data, pop=pop, Frec=Frec, disSel=disSel, M=M, repress=T)
#   if (sum(otpt<0)>0) 9e99 else sum((otpt - catches)^2)
# }

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
