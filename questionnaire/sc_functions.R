#June 2019 Reinhard Hoessinger
#functions which require no costomised pars (no or only defaults pars) -> can be generated outside of the server function

#######################################################################################################################
#convert  minutes of day to character time (hh:mm)
aaMin2Chr <- function (hm) {
  # hm <- 485
  if (is.na(hm)) {
    res <- NA
  } else {
    res <- paste0(formatC(floor(hm/60), width = 2, flag = '0'), ":", formatC(hm %% 60, width = 2, flag = '0'))
  }
  return(res)
}

#convert  minutes of day to text duration in 2 versions
#short: hh:mm
aaMin2DurSrt <- function (hm) {
  # hm <- 61
  if (is.na(hm)) {
    res <- NA
  } else {
    h <- if (floor(hm/60) == 0) {""} else {paste0(floor(hm/60), ":")}
    if (h == "" ) {m <- hm %% 60} else {m <- formatC(hm %% 60, width = 2, flag = '0')}
    res <- paste0(h,m)
  }
  res
  return(res)
}
#long: h std mm min
aaMin2DurLng <- function (hm) {
  # hm <- 327
  if (is.na(hm)) {
    res <- NA
  } else {
    h <- if (floor(hm/60) == 0) {""} else {paste0(floor(hm/60), " std ")}
    m <- paste0(formatC(hm %% 60, width = 2, flag = '0'), " min")
    res <- paste0(h,m)
  }
  return(res)
}

#######################################################################################################################
#get sp settings after completion of rp part
getSpSettings <- function (dpf = dpi, pgf = pgi) {

  ###########################################################################################
  #calculate tour attributes required as input for preparation of sp experiments
  
  #departure time as minutes of day (trip 1 and 2)
  dpf$Tour1Dep <- hour(as.POSIXct(dpf$Time_T1, format="%H:%M", tz = "GMT"))*60 + minute(as.POSIXct(dpf$Time_T1, format="%H:%M", tz = "GMT"))
  dpf$Tour2Dep <- hour(as.POSIXct(dpf$Time_T2, format="%H:%M", tz = "GMT"))*60 + minute(as.POSIXct(dpf$Time_T2, format="%H:%M", tz = "GMT"))
  #distance in km (trip 1, trip 2 and total tour)
  dpf$Tour1Dist <- min(100, dpf$Dist_T1) #censor implied dist to 100 km
  dpf$Tour2Dist <- min(100, dpf$Dist_T2)
  dpf$TourDist <- dpf$Tour1Dist + dpf$Tour2Dist
  #average urbanity of the tour
  dpf$TourUrb <- mean(as.numeric(dpf$Tour1Urb), as.numeric(dpf$Tour2Urb))
  #if tour dist > 30 km (single trip > 15 km): reduce urbanity to increase rural chracter of the trip -> higher speed
  if (dpf$TourDist > 30) {dpf$TourUrb <- dpf$TourUrb * 30 / dpf$TourDist}
  
  ###########################################################################################
  #implied net travel duration (aside from congestion and parking search) from reported trip distance and urbanity type:
  #trip 1, trip 2 and total
  dpf$Tour1Dur <- round(dpf$TourUrb * pr0["taUrb"] + (1 - dpf$TourUrb) * pr0["taRur"] + #access time
    + dpf$TourUrb * pr0["tfUrb"] * dpf$Tour1Dist + (1 - dpf$TourUrb) * pr0["tfRur"] * dpf$Tour1Dist, 0) #travel time
  dpf$Tour2Dur <- round(dpf$TourUrb * pr0["taUrb"] + (1 - dpf$TourUrb) * pr0["taRur"] +
    + dpf$TourUrb * pr0["tfUrb"] * dpf$Tour2Dist + (1 - dpf$TourUrb) * pr0["tfRur"] * dpf$Tour2Dist, 0)
  dpf$TourDur <- dpf$Tour1Dur + dpf$Tour2Dur
  ###########################################################################################
  #implied net travel cost (aside from toll and parking cost) from reported distance
  #trip 1, trip 2 and total
  dpf$Tour1Cost <- round(dpf$Tour1Dist * pr0["cfAll"], 1)
  dpf$Tour2Cost <- round(dpf$Tour2Dist * pr0["cfAll"], 1)
  dpf$TourCost <- dpf$Tour1Cost  + dpf$Tour2Cost 
  #get implied duration of stay from  start time of trip 2 - (start time + implied dur of trip 1)
  dpf$TourStay <- dpf$Tour2Dep - (dpf$Tour1Dep + dpf$Tour1Dur) #rh: corrected after pretest: in last term dpf$TourDur was replaced by dpf$Tour1Dur
  #peak-relation of 1st trip and 2nd trip (reference = start time + 1/2 travel time)
  #1 = before morning peak, 2 = within morning peak, 3 = between morning and evening peak, 4 = within evening peak, 5 = after evening peak
  dpf$Tour1Peak <- cut(dpf$Tour1Dep + 0.5 * dpf$Tour1Dur, breaks = c(0,360,540,960,1140,1440), labels = F)
  dpf$Tour2Peak <- cut(dpf$Tour2Dep + 0.5 * dpf$Tour2Dur, breaks = c(0,360,540,960,1140,1440), labels = F)
  #specify type of public transport for sp experients ("Bahn" and "OeV ohne Bahn"):
  if (dpf$Mode_T1 %in% c(20,21)) {dpf$SpPubTyp <- dpf$Mode_T1 #take public type primarily from trip 1
  } else if (dpf$Mode_T2 %in% c(20,21)) {dpf$SpPubTyp <- dpf$Mode_T2 #otherweise from trip 2
  } else if (dpf$TourUrb > 0 & dpf$TourDist > 20) {dpf$SpPubTyp <- 20} else {dpf$SpPubTyp <- 21} #otherweise according to trip characteristics
  
  ###########################################################################################
  #get settings for sp experments: which experiments are conducted and how many tasks of those conducted
  
  #decisions which experiments are conducted based on rp answers
  #sp1
  #(1) appropriate tour available -> condition must be met to come here
  dpf$SP1_dec <- 1
  #sp2
  #(1) mode = car or public, (2) at least one trip made within peak time
  if ((dpf$Mode_T1 %in% c(10,20,21) | dpf$Mode_T2 %in% c(10,20,21)) & (dpf$Tour1Peak %in% c(2,4) | dpf$Tour2Peak %in% c(2,4))) {dpf$SP2_dec <- 1} else {dpf$SP2_dec <- 0}
  #sp3
  #(1) use of carsharing is at least "rather conceivable" and person has license
  if (dpf$CsFuture >= 2 & dpf$license == 1) {dpf$SP3_dec <- 1} else {dpf$SP3_dec <- 0}
  #sp4
  #(1) use of park & ride is at least "rather conceivable" and person has license
  if (dpf$PrFuture >= 2 & dpf$license == 1) {dpf$SP4_dec <- 1} else {dpf$SP4_dec <- 0}
  
  #get settings required for handling of sp experiments
  #vector of decisions for sp1 to sp4 (0 or 1)
  edec <- as.numeric(dpf[, c("SP1_dec", "SP2_dec", "SP3_dec", "SP4_dec")])
  #number of choice tasks for sp1 to sp 4 (depends on which experiments are conducted)
  enum <- as.numeric(de0[de0$e_typ == paste(edec, collapse = "_"), c("SP1", "SP2", "SP3", "SP4")])
  dpf[, c("SP1_num", "SP2_num", "SP3_num", "SP4_num")] <- enum
  
  ###########################################################################################
  #set visibility and number of choice tasks for sp1 to sp4 in page handler
  for (si in 1:4) {
    # si <- 2
    pgf$p_spn[pgf$p_spi == si] <- enum[si]
    if (enum[si] > 0) {
      #at least one experiment is conducted: view page with experiments
      pgf$p_view[pgf$p_spi == si & pgf$p_nam == "P_SPi"] <- 1
      #note: P_Introi and P_VLi remain at their default value in pgf: 0 = hide, 1 = view
    } else {
      #no experiments conducted: hide all three pages: P_Introi, P_SPi, P_VLi
      pgf$p_view[pgf$p_spi == si] <- 0
    }
  }
  
  #rh: enable for development: write input vars to global environment
  # rpsp <- list(); rpsp$dpf <- dpf; rpsp$pgf <- pgf; rpsp <<- rpsp
  # save(rpsp, file = "rpsp.Rdata") # load("rpsp.Rdata")
  # dpf <- rpsp$dpf
  
  ###########################################################################################
  #prepare sp experiments (only if conducted)
  dsf <- list() #empty list as container for selected desings
  
  ###########################################################################################
  #sp1
  if(dpf$SP1_dec == 1) {
    #check if a design exists from a previous run and if so: keep it; otherwise retrieve a design from design plan
    ds1 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp1Tbl WHERE Sp1Tbl.SpPeID = '", dpf$PeID, "'"))
    if (nrow(ds1) > 0) { #design already existing: adapt number of tasks to existing design
      dpf$SP1_num <- nrow(ds1)
      pgf$p_spn[pgf$p_spi == 1] <- nrow(ds1)
    } else { #no design available: generate new design
      
      #get block number of design from db and retrieve design with this block number
      dc1 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM DcnTbl WHERE DcnTbl.si = 1 AND DcnTbl.ni = '", dpf$SP1_num, "'"))
      dd1 <- ds0[["s1"]][paste0("n", dc1$ni)][[1]][paste0("B", dc1$bi)][[1]] # dd1
      dpf$SP1_blc <- dc1$bi
      #advance block number by one and store new block number in db
      if(dc1$bi >= dc1$b0) {dc1$bi <- 1} else {dc1$bi <- dc1$bi + 1}
      dbSendStatement(conn = dbc, statement = paste0("UPDATE DcnTbl SET DcnTbl.bi = ", dc1$bi, " WHERE DcnTbl.DcID = '", dc1$DcID, "'"))

      #get ds1 as template row from db and multiply by row number of design
      ds1 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp1Tbl WHERE Sp1Tbl.S1ID = '0'"))
      ds1[, which(ds1[1,] == "txt")] <- ""
      ds1 <- ds1[rep(1, nrow(dd1)),]
      #add system infos
      ds1$SpPeID <- dpf$PeID
      ds1$set <- 1:nrow(ds1)
      
      #add values which remain constant over all tasks (availability of modes and type of public)
      ds1[, c("avl_10", "avl_20", "avl_30", "avl_40")] <- 1
      if (dpf$license == 0 | dpf$carAvl == 0) {ds1$avl_10 <- 0}    #car not available
      if (dpf$bikeAvl == 0 | dpf$TourDist > 30)  {ds1$avl_30 <- 0} #bike not available
      if (dpf$TourDist > 6)  {ds1$avl_40 <- 0}                     #walk not available
      
      #take type of public transport ("Bahn" and "OeV ohne Bahn") from SpPubTyp
      ds1$types <- paste0("pub", dpf$SpPubTyp)
      #add vars which vary accross choice tasks: public transport change as number or waiting time
      ds1$types <- paste0(ds1$types, " ", paste0("cng", round(0.5 + runif(nrow(ds1)) * 2, 0)))
      
      ###########################################################################################
      #car
      ds1$durc_10 <- round(dpf$TourDur * dd1$durc_10, 0) #congestion time
      ds1$durp_10 <- dd1$durp_10 #take parking search dur unchanged from the design
      #total travel time: add congestion time + parking search time + random variation +/- 10% to net travel time
      ds1$dur_10 <- round(ds1$durc_10 + ds1$durp_10 + dpf$TourDur * (1 + (runif(nrow(ds1))-0.5)/5),0)
      ds1$costt_10 <- round(dpf$TourCost * dd1$costt_10,1) #toll cost
      ds1$costp_10 <- dd1$costp_10 #take parking cost unchanged from the design
      #total cost: add toll cost + parking cost + random variation +/- 10% to net travel cost
      ds1$cost_10 <- round(ds1$costt_10 + ds1$costp_10 + dpf$TourCost * (1 + (runif(nrow(ds1))-0.5)/5),1)
      
      ###########################################################################################
      #public
      #note: calculate reference dur of public with average congestion factor and average parking search dur
      #this ensures average equivalence with car but independence from particular attrubute values of the car
      ds1$dur_20 <- round((dpf$TourDur * (1 + 0.18) + 7.45) * dd1$dur_20, 0)
      #apply the same logic to reference cost of public
      ds1$cost_20 <- round((dpf$TourCost * (1 + 0.18) + 3.00) * dd1$cost_20, 0)
      #split cng_20 to the two variants cng1_20 and cng2_20 (number of stops and duration)
      ds1$cng1_20[grep("cng1", ds1$types)] <- dd1$cng_20[grep("cng1", ds1$types)] #number of changes
      ds1$cng2_20[grep("cng2", ds1$types)] <- dd1$cng_20[grep("cng2", ds1$types)] * 5 #waiting time at changes: 5 min/change
      #frequency, risk of delay, and availability of seat: take unchanged from the design
      ds1$frq_20 <- dd1$frq_20
      ds1$dly_20 <- dd1$dly_20
      ds1$sit_20 <- dd1$sit_20 #change to text (no - yes) is done in screen generator

      ###########################################################################################
      #cycling and walking 
      #note: these modes dur as only attribute. it is not in the design but calculated with mode-specific factors + random variation +/- 10%
      ds1$dur_30 <- round(dpf$TourDist * pr0["tfBke"] * (1 + (runif(nrow(ds1))-0.5)/5),0)
      ds1$dur_40 <- round(dpf$TourDist * pr0["tfWlk"] * (1 + (runif(nrow(ds1))-0.5)/5),0)
      #save the design in db
      dbWriteTable(conn = dbc, name = "Sp1Tbl", value = ds1[,-1], row.names = F, overwrite = F, append = T)
      #delete design of selected person from db:
      # dbSendStatement(conn = dbc, statement = paste0("DELETE FROM Sp1Tbl WHERE Sp1Tbl.SpPeID = '", dpf$PeID, "'"))
    }
    
    #add sp1 design to dsf
    dsf[["s1"]] <- ds1
  }
  
  
  ###########################################################################################
  #sp2
  if(dpf$SP2_dec == 1) {
    #check if a design exists from a previous run and if so: keep it; otherwise retrieve a design from design plan
    ds2 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp2Tbl WHERE Sp2Tbl.SpPeID = '", dpf$PeID, "'"))
    if (nrow(ds2) > 0) { #design already existing: adapt number of tasks to existing design
      dpf$SP2_num <- nrow(ds2)
      pgf$p_spn[pgf$p_spi == 2] <- nrow(ds2)
    } else { #no design available: generate new design
      
      #get block number of design from db and retrieve design with this block number
      dc2 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM DcnTbl WHERE DcnTbl.si = 2 AND DcnTbl.ni = '", dpf$SP2_num, "'"))
      dd2 <- ds0[["s2"]][paste0("n", dc2$ni)][[1]][paste0("B", dc2$bi)][[1]] # dd2
      dpf$SP2_blc <- dc2$bi
      #advance block number by one and store new block number in db
      if(dc2$bi >= dc2$b0) {dc2$bi <- 1} else {dc2$bi <- dc2$bi + 1}
      dbSendStatement(conn = dbc, statement = paste0("UPDATE DcnTbl SET DcnTbl.bi = ", dc2$bi, " WHERE DcnTbl.DcID = '", dc2$DcID, "'"))
      
      #get ds2 as template row from db and multiply by row number of design
      ds2 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp2Tbl WHERE Sp2Tbl.S2ID = '0'"))
      ds2[, which(ds2[1,] == "txt")] <- ""
      ds2 <- ds2[rep(1, dpf$SP2_num),] # ds2 <- ds2[rep(1, nrow(dd2)),] # multiply row by number of rows in design
      #add system infos
      ds2$SpPeID <- dpf$PeID
      ds2$set <- 1:nrow(ds2)
      
      #get main RP mode and availability of SP alternatives
      ds2[, c("avl_11", "avl_21", "avl_12", "avl_22", "avl_13", "avl_23", "avl_14", "avl_24")] <- 0
      if (dpf$Mode_T1 %in% c(20,21) | dpf$Mode_T2 %in% c(20,21)) {
        ds2$RpMode <- 20
        ds2[, c("avl_21", "avl_22", "avl_23", "avl_14")] <- t(replicate(nrow(ds2), c(1,1,1,1)))
      } else {
        ds2$RpMode <- 10
        ds2[, c("avl_11", "avl_12", "avl_13", "avl_24")] <- t(replicate(nrow(ds2), c(1,1,1,1)))
      }
      
      #take type of public transport ("Bahn" and "OeV ohne Bahn") from SpPubTyp
      ds2$types <- paste0("pub", dpf$SpPubTyp)
      #specify variant of alternative 1: 1 = road congestion or public seat, 2 = pricing (road toll or ticket)
      #note: 1st half of experiments gets congestion, 2nd half gets pricing
      ds2$types <- paste0(ds2$types, " ", "var2")
      ds2$types[1:ceiling(nrow(ds2)/2)] <- gsub("var2", "var1", ds2$types[1:ceiling(nrow(ds2)/2)])
      
      #set public seat availability to a moderate off-peak level: (availability at peak trips (var1 in alt 1) is reduced later)
      ds2[, c("sit_1", "sit_2", "sit_3", "sit_4")] <- replicate(nrow(ds2), sample(c(2,3), 4, replace = T)) # 2 = eher ja, 3 = ja
      
      #set travel cost to off-peak cost = implied cost + random variation +/- 10% (cost of peak trips (var2 in alt 1) are increased later)
      #note: variation of off-peak cost was increased to +/- 20% after pretest (divide by 2.5 instead of 5)
      for (ai in 1:4) {
        ds2[, paste0("cost1_",ai)] <- replicate(nrow(ds2), round(dpf$Tour1Cost * (1 + (runif(1)-0.5)/2.5), 1))
        ds2[, paste0("cost2_",ai)] <- replicate(nrow(ds2), round(dpf$Tour2Cost * (1 + (runif(1)-0.5)/2.5), 1))
      }
      #if rp mode = public: reduce cost in alt 4 by 0.8 to make switch to car more attractive
      if(ds2$RpMode[1] == 20) {
        ds2$cost1_4 <- round(dpf$Tour1Cost * (0.8 + (runif(nrow(ds2))-0.5)/2.5), 1)
        ds2$cost2_4 <- round(dpf$Tour2Cost * (0.8 + (runif(nrow(ds2))-0.5)/2.5), 1)
      }

      ###########################################################################################
      #loop through rows in ds2 and calculate values
      for (ri in 1:nrow(ds2)) { # ri <- 1
        dsi <- ds2[ri,]
        #(1) add absolute random component (e.g. +/- 20 min): + (runif(1)-0.5)*20/0.5
        #(2) add relative random component (e.g. +/- 10%):    * (1 + (runif(1)-0.5)/5)
        #(2) round to nearest 5 min: ceiling( (x) * 0.2) / 0.2
        
        ###########################################################################################
        #trip 1
        
        #dep1_1
        dsi$dep1_1 <- ceiling( (dpf$Tour1Dep + (runif(1)-0.5) * 20/0.5 )  * 0.2) / 0.2
        #dur1_1
        if(dpf$Tour1Peak %in% c(2,4) & length(grep("var1", dsi$types)) > 0) { #peak + variant 1: car congestion or public seats
          if(dsi$RpMode == 10) { #car: increase travel time due to congestion
            #congestion time as relative factor between 15 and 60%
            dsi$durc1_1 <- round(dpf$Tour1Dur * dd2$durc1_1[ri], 0)
            dsi$dur1_1 <- dpf$Tour1Dur + dsi$durc1_1
          } else { #public (RpMode 20): decrease seat availability in public transport
            dsi$sit_1 <- dd2$sit_1[ri] #decreadse availability of public seats: 0 = nein, 1 = eher nein
            dsi$dur1_1 <- round(dpf$Tour1Dur * (1 + (runif(1)-0.5)/5), 0)
          } 
        } else { #off peak: RP dur +/- 10%
          dsi$dur1_1 <- round(dpf$Tour1Dur * (1 + (runif(1)-0.5)/5), 0)
        }
        #arr1_1
        dsi$arr1_1 <- dsi$dep1_1 + dsi$dur1_1
        
        #dep1_2
        if(dpf$Tour1Peak %in% c(2,4)) { #peak
          #negative offset from RP departure time to beginning of peak period - random component of 20 min
          if (dpf$Tour1Peak == 2) {PkBeg <- 6*60} else  {PkBeg <- 16*60}
          dsi$shift1_2 <- round(PkBeg - 0.5*dpf$Tour1Dur - runif(1)*20 - dpf$Tour1Dep, 0)
          dsi$dep1_2 <- ceiling( (dpf$Tour1Dep + dsi$shift1_2) * 0.2) / 0.2 #found to 5 min
        } else {
          dsi$dep1_2 <- ceiling( (dpf$Tour1Dep + (runif(1)-0.5) * 20/0.5 )  * 0.2) / 0.2
        }
        #dur1_2
        dsi$dur1_2 <- round(dpf$Tour1Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        #arr1_2
        dsi$arr1_2 <- dsi$dep1_2 + dsi$dur1_2
        
        #dep1_3
        if(dpf$Tour1Peak %in% c(2,4)) { #peak
          #positive offset from RP departure time to end of peak period - random component of 20 min
          if (dpf$Tour1Peak == 2) {PkEnd <- 9*60} else  {PkEnd <- 19*60}
          dsi$shift1_3 <- round(PkEnd - 0.5*dpf$Tour1Dur + runif(1)*20 - dpf$Tour1Dep, 0)
          dsi$dep1_3 <- ceiling( (dpf$Tour1Dep + dsi$shift1_3) * 0.2) / 0.2 #found to 5 min
        } else {
          dsi$dep1_3 <- ceiling( (dpf$Tour1Dep + (runif(1)-0.5) * 20/0.5 )  * 0.2) / 0.2
        }
        #dur1_3
        dsi$dur1_3 <- round(dpf$Tour1Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        #arr1_3
        dsi$arr1_3 <- dsi$dep1_3 + dsi$dur1_3
        
        #dep1_4
        dsi$dep1_4 <- ceiling( (dpf$Tour1Dep + (runif(1)-0.5) * 20/0.5 )  * 0.2) / 0.2
        #dur1_4
        #if rp mode = public: reduce dur in alt 4 by 0.8 to make switch to car more attractive
        if(dsi$RpMode == 20) {
          dsi$dur1_4 <- round(dpf$Tour1Dur * (0.8 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        } else {
          dsi$dur1_4 <- round(dpf$Tour1Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        }
        #arr1_4
        dsi$arr1_4 <- dsi$dep1_4 + dsi$dur1_4
        
        ###########################################################################################
        #trip 2
        
        #stay_1-4, dep2_1-4, and peak2_1-4
        for (ai in 1:4) {
          dsi[, paste0("stay_",ai)] <- dpf$TourStay * (1 + (runif(1)-0.5)/5)
          #note: stay_i is re-adjusted after trip 2 because it may change due to shift of departure time of trip 2
          dsi[, paste0("dep2_",ai)] <- ceiling( (dsi[, paste0("arr1_",ai)] + dsi[, paste0("stay_",ai)]) * 0.2) / 0.2
          dsi[, paste0("peak2_",ai)] <- cut(dsi[, paste0("dep2_",ai)] + 0.5 * dpf$Tour2Dur, breaks = c(0,360,540,960,1140,1440), labels = F)
        }
        
        #dep2_1: remain with dep2_1 from above calculation
        #dur2_1
        if(dsi$peak2_1 %in% c(2,4) & length(grep("var1", dsi$types)) > 0) { #peak + variant 1: car congestion or public seats
          if(dsi$RpMode == 10) { #car: increase travel time due to congestion
            #congestion time as relative factor between 15 and 60%
            dsi$durc2_1 <- round(dpf$Tour2Dur * dd2$durc2_1[ri], 0)
            dsi$dur2_1 <- dpf$Tour2Dur + dsi$durc2_1
          } else { #public (RpMode 20): decrease seat availability in public transport
            dsi$sit_1 <- dd2$sit_1[ri] #decreadse availability of public seats: 0 = nein, 1 = eher nein
            dsi$dur2_1 <- round(dpf$Tour2Dur * (1 + (runif(1)-0.5)/5), 0)
          } 
        } else { #off peak: RP dur +/- 10%
          dsi$dur2_1 <- round(dpf$Tour2Dur * (1 + (runif(1)-0.5)/5), 0)
        }
        #arr2_1
        dsi$arr2_1 <- dsi$dep2_1 + dsi$dur2_1
        
        #dep2_2
        if(dsi$peak2_2 %in% c(2,4)) { #peak
          #negative offset from RP departure time to beginning of peak period - random component of 20 min
          if (dsi$peak2_2 == 2) {PkBeg <- 6*60} else  {PkBeg <- 16*60}
          dsi$shift2_2 <- round(PkBeg - 0.5*dpf$Tour2Dur - runif(1)*20 - dsi$dep2_2, 0)
          dsi$dep2_2 <- ceiling( (dsi$dep2_2 + dsi$shift2_2) * 0.2) / 0.2 #frund to 5 min
          #check if stay_2 deminished by more than 1/3 due to earlier departure of return trip
          #if so: shift departure time after peak period (as in alt 3)
          if ((dsi$dep2_2 - dsi$arr1_2) < 2/3*dpf$TourStay) {
            #positive offset from RP departure time to end of peak period - random component of 20 min
            if (dsi$peak2_2 == 2) {PkEnd <- 9*60} else  {PkEnd <- 19*60}
            dsi$shift2_2 <- round(PkEnd - 0.5*dpf$Tour2Dur + runif(1)*20 - dsi$dep2_2, 0)
            dsi$dep2_2 <- ceiling( (dsi$dep2_2 + dsi$shift2_2) * 0.2) / 0.2 #found to 5 min
          }
        } #else: remain with dep2_2 from above calculation
        #dur2_2
        dsi$dur2_2 <- round(dpf$Tour2Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        #arr2_2
        dsi$arr2_2 <- dsi$dep2_2 + dsi$dur2_2
        
        #dep2_3
        if(dsi$peak2_3 %in% c(2,4)) { #peak
          #positive offset from RP departure time to end of peak period - random component of 20 min
          if (dsi$peak2_3 == 2) {PkEnd <- 9*60} else  {PkEnd <- 19*60}
          dsi$shift2_3 <- round(PkEnd - 0.5*dpf$Tour2Dur + runif(1)*20 - dsi$dep2_3, 0)
          dsi$dep2_3 <- ceiling( (dsi$dep2_3 + dsi$shift2_3) * 0.2) / 0.2 #found to 5 min
        } #else: remain with dep2_3 from above calculation
        #dur2_3
        dsi$dur2_3 <- round(dpf$Tour2Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        #arr2_3
        dsi$arr2_3 <- dsi$dep2_3 + dsi$dur2_3
        
        #dep2_4: remain with dep2_4 from above calculation
        #dur2_4
        #if rp mode = public: reduce dur in alt 4 by 0.8 to make switch to car more attractive
        if(dsi$RpMode == 20) {
          dsi$dur2_4 <- round(dpf$Tour2Dur * (0.8 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        } else {
          dsi$dur2_4 <- round(dpf$Tour2Dur * (1 + (runif(1)-0.5)/5), 0) #RP dur +/- 10%
        }
        #arr2_4
        dsi$arr2_4 <- dsi$dep2_4 + dsi$dur2_4
        
        ###########################################################################################
        #cost
        
        #cost1_1
        if(dpf$Tour1Peak %in% c(2,4) & length(grep("var2", dsi$types)) > 0) { #peak + variant 2: apply peak price
          #note: costt1_1 applies to both car toll and public peak price
          dsi$costt1_1 <- round(dpf$Tour1Cost * dd2$costt1_1[ri], 1)
          dsi$cost1_1 <- dpf$Tour1Cost + dsi$costt1_1
        } #else: remain with cost1_1 from above calculation
        #cost2_1
        if(dsi$peak2_1 %in% c(2,4) & length(grep("var2", dsi$types)) > 0) { #peak + variant 2: apply peak price
          #note: costt2_1 applies to both car toll and public peak price
          dsi$costt2_1 <- round(dpf$Tour2Cost * dd2$costt2_1[ri], 1)
          dsi$cost2_1 <- dpf$Tour2Cost + dsi$costt2_1
        } #else: remain with cost2_1 from above calculation
        
        ###########################################################################################
        #re-adjustments and display information     
        for(ai in 1:4) {
          # ai <- 1
          #recalculate stay_i according to possible changes in travel times
          dsi[, paste0("stay_",ai)] <- round( (dsi[, paste0("dep2_",ai)] - dsi[, paste0("arr1_",ai)]) * 0.2, 0) / 0.2
          #get formated values displayed on the screen
          dsi[, paste0("time1_",ai)] <- paste0(aaMin2Chr(dsi[, paste0("dep1_",ai)]), " - ", aaMin2Chr(dsi[, paste0("arr1_",ai)])) #trip 1
          dsi[, paste0("time2_",ai)] <- paste0(aaMin2Chr(dsi[, paste0("dep2_",ai)]), " - ", aaMin2Chr(dsi[, paste0("arr2_",ai)])) #trip 2
          dsi[, paste0("dur_",ai)] <- dsi[, paste0("dur1_",ai)] + dsi[, paste0("dur2_",ai)] #total travel dur
          dsi[, paste0("cost_",ai)] <- dsi[, paste0("cost1_",ai)] + dsi[, paste0("cost2_",ai)]
        }
        #add congestion time and toll to peak trips of alt 1
        dsi$durc_1 <- ifelse(is.na(dsi$durc1_1) & is.na(dsi$durc2_1), NA, sum(dsi$durc1_1, dsi$durc2_1, na.rm = T))
        dsi$costt_1 <- sum(dsi$costt1_1, dsi$costt2_1, na.rm = T)
        #write row with prepared design back to ds2
        ds2[ri,] <- dsi[1,]
      }
      
      #save the design in db
      dbWriteTable(conn = dbc, name = "Sp2Tbl", value = ds2[,-1], row.names = F, overwrite = F, append = T)
    }


    ###########################################################################################
  }
  
  ###########################################################################################
  #sp3
  if(dpf$SP3_dec == 1) {
    #check if a design exists from a previous run and if so: keep it; otherwise retrieve a design from design plan
    ds3 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp3Tbl WHERE Sp3Tbl.SpPeID = '", dpf$PeID, "'"))
    if (nrow(ds3) > 0) { #design already existing: adapt number of tasks to existing design
      dpf$SP3_num <- nrow(ds3)
      pgf$p_spn[pgf$p_spi == 3] <- nrow(ds3)
    } else { #no design available: generate new design
      
      #get block number of design from db and retrieve design with this block number
      dc3 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM DcnTbl WHERE DcnTbl.si = 3 AND DcnTbl.ni = '", dpf$SP3_num, "'"))
      dd3 <- ds0[["s3"]][paste0("n", dc3$ni)][[1]][paste0("B", dc3$bi)][[1]] # dd3
      dpf$SP3_blc <- dc3$bi
      #advance block number by one and store new block number in db
      if(dc3$bi >= dc3$b0) {dc3$bi <- 1} else {dc3$bi <- dc3$bi + 1}
      dbSendStatement(conn = dbc, statement = paste0("UPDATE DcnTbl SET DcnTbl.bi = ", dc3$bi, " WHERE DcnTbl.DcID = '", dc3$DcID, "'"))
      
      #get ds3 as template row from db and multiply by row number of design
      ds3 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp3Tbl WHERE Sp3Tbl.S3ID = '0'"))
      ds3[, which(ds3[1,] == "txt")] <- ""
      ds3 <- ds3[rep(1, dpf$SP3_num),] # ds3 <- ds3[rep(1, nrow(dd3)),] # multiply row by number of rows in design
      #add system infos
      ds3$SpPeID <- dpf$PeID
      ds3$set <- 1:nrow(ds3)
      
      #add values which remain constant over all tasks (availability of modes and type of public)
      ds3[, c("avl_25", "avl_50")] <- 1
      
      ###########################################################################################
      #public
      
      #dist: 1st choice set 10 to 20 km (short tour); other choice sets 30 to 100 km (long tour), rounded to 5
      ds3$dist_25[1] <-  round((8 + 16 * runif(1))           * 0.2, 0) / 0.2
      ds3$dist_25[-1] <- round((28 + 72 * runif(nrow(ds3)-1)) * 0.2, 0) / 0.2
      #dur: calculate at first reference travel dur (rural, without access dur) with random variation +/- 10%, add access dur later
      ds3$dur_25 <- pr0["tfRur"] * ds3$dist_25 * (1 + (runif(1)-0.5)/5)
      #access dur: additive value with factorial variation in 4 levels
      ds3$dura_25 <- dd3$dura_25
      #total dur: access dur + reference travel dur, rounded to 1
      ds3$dur_25 <- round(ds3$dura_25 + ds3$dur_25, 0)
      #cost: relative factor of reference cost from standard model with factorial variation in 4 levels
      ds3$cost_25 <- round(ds3$dist_25 * pr0["cfAll"] * dd3$cost_25, 1)
      
      ###########################################################################################
      #carsharing
      #dist equal to public
      ds3$dist_50 <- ds3$dist_25
      #dur: calculate at first reference travel dur (rural, without access dur) with random variation +/- 10%, add access dur later
      ds3$dur_50 <- pr0["tfRur"] * ds3$dist_50 * (1 + (runif(1)-0.5)/5)
      #access dur: additive value with factorial variation in 4 levels
      ds3$dura_50 <- dd3$dura_50
      #total dur: access dur + reference travel dur, rounded to 1
      ds3$dur_50 <- round(ds3$dura_50 + ds3$dur_50, 0)
      #cost: relative factor of reference cost from a model with increased cost factor (cfCsh) with factorial variation in 4 levels
      ds3$cost_50 <- round(ds3$dist_50 * pr0["cfCsh"] * dd3$cost_50, 1)
      
      #save the design in db
      dbWriteTable(conn = dbc, name = "Sp3Tbl", value = ds3[,-1], row.names = F, overwrite = F, append = T)
    }
    
    #add sp3 design to dsf
    dsf[["s3"]] <- ds3
  }
  
  ###########################################################################################
  #sp4
  if(dpf$SP4_dec == 1) {
    #check if a design exists from a previous run and if so: keep it; otherwise retrieve a design from design plan
    ds4 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp4Tbl WHERE Sp4Tbl.SpPeID = '", dpf$PeID, "'"))
    if (nrow(ds4) > 0) { #design already existing: adapt number of tasks to existing design
      dpf$SP4_num <- nrow(ds4)
      pgf$p_spn[pgf$p_spi == 4] <- nrow(ds4)
    } else { #no design available: generate new design
      
      #get block number of design from db and retrieve design with this block number
      dc4 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM DcnTbl WHERE DcnTbl.si = 4 AND DcnTbl.ni = '", dpf$SP4_num, "'"))
      dd4 <- ds0[["s4"]][paste0("n", dc4$ni)][[1]][paste0("B", dc4$bi)][[1]] # dd4
      dpf$SP4_blc <- dc4$bi
      #advance block number by one and store new block number in db
      if(dc4$bi >= dc4$b0) {dc4$bi <- 1} else {dc4$bi <- dc4$bi + 1}
      dbSendStatement(conn = dbc, statement = paste0("UPDATE DcnTbl SET DcnTbl.bi = ", dc4$bi, " WHERE DcnTbl.DcID = '", dc4$DcID, "'"))
      
      #get ds4 as template row from db and multiply by row number of design
      ds4 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp4Tbl WHERE Sp4Tbl.S4ID = '0'"))
      ds4[, which(ds4[1,] == "txt")] <- ""
      ds4 <- ds4[rep(1, dpf$SP4_num),] # ds4 <- ds4[rep(1, nrow(dd4)),] # multiply row by number of rows in design
      #add system infos
      ds4$SpPeID <- dpf$PeID
      ds4$set <- 1:nrow(ds4)
      
      #add values which remain constant over all tasks (availability of modes and type of public)
      ds4[, c("avl_15", "avl_60")] <- 1
      
      ###########################################################################################
      #car
      #dist: 60 +/- 30 km random variation, rounded to 5 (range is 30 to 90 km)
      ds4$dist_15 <- ceiling( (60 + (runif(nrow(ds4))-0.5)*30/0.5) * 0.2) / 0.2
      #dur: calculate at first reference travel dur (rural) with random variation +/- 10%, add congestion and parking search dur later
      ds4$dur_15 <- (pr0["taRur"] + pr0["tfRur"] * ds4$dist_15) * (1 + (runif(1)-0.5)/5)
      #congestion dur: relative share of reference dur with factorial variation in 4 levels
      ds4$durc_15 <- round(ds4$dur_15 * dd4$durc_15, 0)
      #parking search dur: additive value with factorial variation in 4 levels
      ds4$durp_15 <- dd4$durp_15
      #total dur: reference travel dur + congestion dur + parking search dur, rounded to 1
      ds4$dur_15 <- round(ds4$dur_15 + ds4$durc_15 + ds4$durp_15, 0)
      #cost: calculate at first travel cost as relative share of reference cost with factorial variation in 4 levels, add parking cost later
      ds4$cost_15 <- ds4$dist_15 * pr0["cfAll"] * dd4$cost_15
      #parking cost: additive value with factorial variation in 4 levels
      ds4$costp_15 <- dd4$costp_15
      
      ###########################################################################################
      #park & ride
      #dist equal to public
      ds4$dist_60 <- ds4$dist_15
      #dur: calculate at first reference travel dur (rural) with random variation +/- 10%, add congestion dur later
      ds4$dur_60 <- (pr0["taRur"] + pr0["tfRur"] * ds4$dist_60) * (1 + (runif(1)-0.5)/5)
      #dur of public to dest: relative share of reference dur with factorial variation in 4 levels
      ds4$durr_60 <- round(ds4$dur_60 * dd4$durr_60, 0)
      #total cost: reference cost + parking cost, rounded to 1
      ds4$cost_15 <- round(ds4$cost_15 + ds4$costp_15, 1)
      #congestion dur: relative share of remaining car dur (reference - public) with factorial variation in 4 levels
      ds4$durc_60 <- round((ds4$dur_60 - ds4$durr_60) * dd4$durc_60, 0)
      #total dur: reference travel dur + congestion dur, rounded to 1
      ds4$dur_60 <- round(ds4$dur_60 + ds4$durc_60, 0)
      #cost: calculate at first travel cost as relative share of reference cost with factorial variation in 4 levels, add parking cost later
      ds4$cost_60 <- ds4$dist_60 * pr0["cfAll"] * dd4$cost_60
      #parking cost: additive value with factorial variation in 4 levels
      ds4$costp_60 <- dd4$costp_60
      #cost of public to dest: reference cost * public share of reference dur with random variation +/- 10%
      #note: reference dur must be re-calculated at this point (dur_60 - durc_60) because parking cost are already included in dur_60
      ds4$costr_60 <- round(ds4$cost_60 * (ds4$durr_60 / (ds4$dur_60 - ds4$durc_60)) * (1 + (runif(1)-0.5)/5), 1)
      #total cost: reference cost + parking cost, rounded to 1
      ds4$cost_60 <- round(ds4$cost_60 + ds4$costp_60, 1)

      #save the design in db
      dbWriteTable(conn = dbc, name = "Sp4Tbl", value = ds4[,-1], row.names = F, overwrite = F, append = T)
      
    }
    
    #add sp4 design to dsf
    dsf[["s4"]] <- ds4
  }
  
  ###########################################################################################
  #return results
  # dpi = dpf; pgi = pgf; dsi = dsf
  return(list (dpf = dpf, pgf = pgf, dsf = dsf))
}

#End###################################################################################################################

