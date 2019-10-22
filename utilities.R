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
# 
# calc_catches <- function(data, input, fmults) {
#   
#   data %>%
#     right_join(
#       data.frame(
#         gear = input$selected_gears, 
#         #        fmult = sapply(input$selected_gears, function(x) input[[gsub(" ", "_", x)]])
#         fmult = fmults
#       ), by = "gear") %>%
#     group_by(Age) %>%
#     mutate(
#       z = sum(fmult * Selectivity) + M + sum(f_age_rec_2020)
#     ) %>%
#     ungroup() %>%
#     mutate(
#       catch_n = (n * (1-exp(-z)) * fmult * Selectivity / z) # 
#       #  n * (1-exp(-z)) * f_age_rec_2020/ z # that was added by GL by mistake here -  
#       # still need to make sure recreational is not anymore in the gear list
#       # and that catch_n is also calculated for recreational but based on f_age_rec_2020 and no need for optimising
#     )
# }
# 
# 
# totals <- function(data, input, fmults) {
#   out <- 
#     calc_catches(data, input, fmults) %>%
#     group_by(gear) %>%
#         summarise(
#           catch = sum(catch_n * Weight)
#         )
#   out$catch
# }
# 
# catches_to_fmult <- function(catches, data, input) {
# 
#   objective_func <- function(log_fmults, catches, data, input) {
#     sum((totals(data, input, exp(log_fmults)) - catches)^2)
#   }
# 
#   opt <- 
#     optim(rep(0, length(catches)), 
#           objective_func, 
#           catches = catches,
#           data = data,
#           input = input)
#   exp(opt$par)
# }
# 

runForecast <- 
  function(months, selectivity_age, weights_age, M, 
           pop_age_2020, f_age_rec_2020,
           f_age_rec_2020_month,
           discard_prop, discard_Sel, catches,
           Monthly, ICESadvComm, ICESadv,
           Myr, CatchGear, recCatch, catchRec_n,
           FbarRec, ICESadvOpt, AdviceForecastCatchAge) {

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
  ### Monthly forecast
  if (Monthly) {
    
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
        remaining <- ICESadvComm-caught
        if (remaining < sum(catches[i,])) {
          catches[i,] <- catches[i,] * (remaining/sum(catches[i,]))
          for (ii in (i+1):(length(months)-1)) catches[ii,] <- 0
          switch <- F  
        }
      } # end of switch loop
      
      # Split catches in landings and discards
      lanDis <- c(as.numeric(catches[i,])*(1-discard_prop[,2]),as.numeric(catches[i,])*discard_prop[,2])
      
      # optimise Fmults to take the catches specified
      opt <- optim(rep(0, length(lanDis)), 
                   objective_func, 
                   catches = lanDis,
                   data = dat,
                   M=M,
                   Frec = f_age_rec_2020_month,
                   disSel = discard_Sel[,2],
                   pop=initPop[,months[i]],
                   lower=rep(0, length(lanDis)),
                   #upper= rep(100, length(lanDis)),
                   method="L-BFGS-B")
      fmults <- opt$par
      
      # Use optimised fmults to get catch.n, commercial F and total Z 
      tmp <- gearCatches(fmults,dat, initPop[,i], f_age_rec_2020_month, disSel = discard_Sel[,2], M, repress=F)
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
  if (!Monthly) {
    # list to store results
    out <- list()
    
    # Cap catches at ICES advice
    if (ICESadvComm < sum(catches[1,])) catches[1,] <- catches[1,] * (ICESadvComm/sum(catches[1,]))
    
    # Split catches in landings and discards
    lanDis <- c(as.numeric(catches[1,])*(1-discard_prop[,2]),as.numeric(catches[1,])*discard_prop[,2])
    
    # optimise Fmults to take the catches specified
    opt <- optim(rep(0, length(lanDis)), 
                 objective_func, 
                 catches = lanDis,
                 data = dat,
                 M = Myr,
                 Frec = f_age_rec_2020,
                 disSel = discard_Sel[,2],
                 pop=initPop[,1],
                 lower=rep(0, length(lanDis)),
                 #upper= rep(100, length(lanDis)),
                 method="L-BFGS-B")
    fmults <- opt$par
    
    # Use optimised fmults to get catch.n, commercial F and total Z 
    tmp <- gearCatches(fmults=fmults,dat=dat, pop=initPop[,1], Frec=f_age_rec_2020, disSel = discard_Sel[,2], M=Myr, repress=F)
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
  
  ## Commercial catches
  #Commercial Landings  #sweep(realisedCatch, 2, 1-discard_prop[,2], `*`) 
  realisedLandings <- CatchGear; realisedLandings[] <- 0
  if (Monthly) {
    for (i in 1:(length(months)-1)) realisedLandings[i,] <- out[[i]]$gearCatches[,1:length(gears)]
    realisedLandings[13,] <- apply(realisedLandings[-13,],2,sum) 
  }
  if (!Monthly) realisedLandings[13,] <- out[[1]]$gearCatches[,1:length(gears)]
  totCommLandings <- sum(realisedLandings[13,])
  #Commercial Discards    #sweep(realisedCatch, 2, discard_prop[,2], `*`) 
  realisedDiscards <- CatchGear; realisedDiscards[] <- 0
  if (Monthly) {
    for (i in 1:(length(months)-1)) realisedDiscards[i,] <- out[[i]]$gearCatches[,(length(gears)+1):(2*length(gears))]
    realisedDiscards[13,] <- apply(realisedDiscards[-13,],2,sum) 
  }
  if (!Monthly) realisedDiscards[13,] <- out[[1]]$gearCatches[,(length(gears)+1):(2*length(gears))]
  totCommDiscards <- sum(realisedDiscards[13,])
  #Commercial Catch
  realisedCatch <- realisedLandings + realisedDiscards
  totCommCatch <- sum(realisedCatch[13,])
 
  # to make perfect with advice
  if (Monthly) {
    if (sum(catches[13,], na.rm=T)>(ICESadv-recCatch)) ft <- T else ft <- F
  }
  if (!Monthly) {
    if (sum(catches[1,], na.rm=T)>(ICESadv-recCatch)) ft <- T else ft <- F
  }
  if (ft) adj <- (ICESadv-recCatch)/totCommCatch else adj <- 1
  totCommCatch <- adj*totCommCatch
  totCommLandings <- adj*totCommLandings 
  totCommDiscards <- adj*totCommDiscards 
  
  ## Catch at age
  catch_n <- out[[1]]$catch_n
  if (Monthly) for (i in 2:(length(months)-1)) catch_n <- catch_n + out[[i]]$catch_n 
  catch_n <- cbind(catch_n, catchRec_n)
  dimnames(catch_n)[[2]][6] <- "Recreational"
  
  ### F values
  ## Total
  totalF <- out[[1]]$total_z-Myr  
  if (Monthly) for (i in 2:(length(months)-1)) totalF <- totalF + out[[i]]$total_z   
  Ftotbar <- mean(totalF[5:16]) # ages 4-15
  if (Ftotbar<0.2) Ftotbar <- round(Ftotbar,3) else Ftotbar <- round(Ftotbar,2)
  
  ## Commercial F and Fbar
  catchF <- out[[1]]$catch_f  
  if (Monthly) for (i in 2:(length(months)-1)) catchF <- catchF + out[[i]]$catch_f   
  Fcomm <- apply(catchF,1,sum) # F landings + discards 
  Fcommbar <- mean(Fcomm[5:16]) # ages 4-15
  # ICES rounding
  if (Fcommbar<0.2) Fcommbar <- round(Fcommbar,3) else Fcommbar <- round(Fcommbar,2)
  ## By gear
  gearFTable <- apply(catchF[5:16,],2,mean)
  for (gg in gears) if (gearFTable[gg]<0.2) gearFTable[gg] <- round(gearFTable[gg],3) else gearFTable[gg] <- round(gearFTable[gg],2)
  
  ## Landings
  landF <- out[[1]]$land_f  
  if (Monthly) for (i in 2:(length(months)-1)) landF <- landF + out[[i]]$land_f   
  Fland <- apply(landF,1,sum) # F landings 
  Flandbar <- mean(Fland[5:16]) # ages 4-15
  # ICES rounding
  if (Flandbar<0.2) Flandbar <- round(Flandbar,3) else Flandbar <- round(Flandbar,2)
  
  ## Discards
  discF <- out[[1]]$dis_f  
  if (Monthly) for (i in 2:(length(months)-1)) discF <- discF + out[[i]]$dis_f   
  Fdis <- apply(discF,1,sum) # F discards 
  Fdisbar <- mean(Fdis[5:16]) # ages 4-15
  # ICES rounding
  if (Fdisbar<0.2) Fdisbar <- round(Fdisbar,3) else Fdisbar <- round(Fdisbar,2)
  
  ## Annual recreational catch and F
  # recCatch
  if (FbarRec<0.2) FbarRec <- round(FbarRec,3) else FbarRec <- round(FbarRec,2)
  
  # Catch including recreational
  realisedCatch <- cbind(realisedCatch, realisedCatch[,1]); realisedCatch[,6] <- NA
  dimnames(realisedCatch)[[2]][6] <- "Recreational"; 
  realisedCatch[13,6] <- recCatch  
  realisedCatch[,1:length(gears)] <- adj*realisedCatch[,1:length(gears)]
  # round the values (have many decimals due to optimising Fmults)
  totalCatch <- sum(realisedCatch[13,],na.rm=T)
  #realisedCatch <- round(realisedCatch,0)
  
  # Change ages for Jan 2021
  #initPop[,1] * exp(-(commF+(f_age_rec_2020[,2]*12)+Myr))
  initPop[nrow(initPop),13] <- initPop[nrow(initPop),13] + initPop[nrow(initPop)-1,13] 
  initPop[1:(nrow(initPop)-1),13] <- c(0,initPop[1:(nrow(initPop)-2),13])
  
  # SSB 2021
  mat <- c(0,0,0,0,0.089,0.291,0.575,0.798,0.916,0.966,0.986,0.994,0.997,0.999,0.999,1,1)
  stkwt <- c(0.00282457,0.0237327,0.0961958,0.209295,0.368655,0.569804,0.806228,1.07064,1.35577,  
             1.65483,1.96175,2.27132,2.57917,2.88175,3.17626,3.46058,3.73313)
  ssb2021 <- sum(initPop[,13]*mat*stkwt)
  


  #####-------------------------
  ### Show outputs
  
  ## Catch y gear table
  CatchGearTable <- as.matrix(realisedCatch)
  CatchGearTable <- rbind(CatchGearTable, c(gearFTable,NA))
  CatchGearTable <- cbind(CatchGearTable, rep(NA, nrow(CatchGearTable)))
  dimnames(CatchGearTable)[[1]] <- c(months[1:12], "TOTAL", "F")
  dimnames(CatchGearTable)[[2]][ncol(CatchGearTable)] <- "TOTAL"
  # F by gear
  #gearFTable <- as.data.frame(gearFTable); dimnames(gearFTable)[[2]] <- "F"
  CatchGearTable["F","Recreational"] <- FbarRec
  
  # Add total column
  CatchGearTable[,"TOTAL"] <- apply(CatchGearTable[,1:6],1,sum, na.rm=T)
  # Round
  CatchGearTable[-nrow(CatchGearTable),] <- round(CatchGearTable[-nrow(CatchGearTable),],0)
  # Add months
  CatchGearTable <- cbind(rep(NA, nrow(CatchGearTable)), CatchGearTable)
  dimnames(CatchGearTable)[[2]][1] <- "Month"
  CatchGearTable[,"Month"] <- c(months[1:12], "TOTAL", "F")

  ## Catch at age plot
  data <- as.data.frame(selectivity_age); data <- rbind(data[1:34,],data)
  levels(data$gear) <- c(levels(data$gear),"Recreational", "AdviceForecast")
  dimnames(data)[[2]][2] <- "catch_n"; data[1:17,"gear"] <- "AdviceForecast"; data[18:34,"gear"] <- "Recreational"
  for (gg in c(as.character(gears),"Recreational")) data[data$gear==gg,]$catch_n <- catch_n[,gg]
  if (ICESadvOpt=="MSY") expected <- AdviceForecastCatchAge[,"MSY"] else expected <- AdviceForecastCatchAge[,"MSYlow"]
  data[data$gear=="AdviceForecast",]$catch_n <- expected
  
  p <- 
    ggplot() + 
    geom_area(data = subset(data, gear!="AdviceForecast"), position = 'stack',   
              aes(x = Age, y = catch_n, fill = gear)) +
    geom_line(data = subset(data, gear=="AdviceForecast"), linetype=2,   
              aes(x = Age, y = catch_n))  +
    ylab("Catch at Age (thousands)")
  
  
  # ggplot(data = subset(data, gear!="AdviceForecast"),
  #             aes(x = Age, y = catch_n, fill = gear)) +
  #             ylab("Catch at Age (thousands)") +
  #             geom_area(position = 'stack')
  
  ## Forecast table outputs
  forecastTable <- matrix(NA, ncol=11,nrow=1,dimnames=list(ICESadvOpt,
                                                           c("Total Catch", "Commercial Landings", "Commercial discards", "Recreational removals", "Total F", "F Commercial landings",
                                                             "F Commercial discards", "F Recreational removals", "SSB (2021)", "% SSB change", "% Advice change")))
  forecastTable[,"Total Catch"] <- round(totCommCatch+recCatch,0)
  forecastTable[,"Commercial Landings"] <- round(totCommLandings,0)
  forecastTable[,"Commercial discards"] <- round(totCommDiscards,0)
  forecastTable[, "Recreational removals"] <- round(recCatch,0)
  forecastTable[, "Total F"] <- Ftotbar
  forecastTable[, "F Commercial landings"] <- Flandbar
  forecastTable[, "F Commercial discards"] <- Fdisbar
  forecastTable[, "F Recreational removals"] <- FbarRec
  forecastTable[, "SSB (2021)"] <- round(ssb2021,0)
  forecastTable[, "% SSB change"] <- round(100*(ssb2021-11413)/11413,1)
  if (ft) {
    if (ICESadvOpt=="MSY") forecastTable[, "% Advice change"] <- 7.8 else forecastTable[, "% Advice change"] <- -9.5
  }
  if (!ft) forecastTable[, "% Advice change"] <- 100*((totCommCatch+recCatch)-1806)/1806
  
  return(list(plot = p, CatchGearTable = CatchGearTable, forecastTable = forecastTable)) 
}
