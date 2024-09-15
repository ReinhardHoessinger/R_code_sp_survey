#July 2019 Reinhard Hoessinger
#generate optimal and blocked design for sp2 experiment

#######################################################################################################################
#set work directory, load packages and data
setwd("D:/R/Projects/p84_vmoe2/design")
library(openxlsx)
library(AlgDesign) #efficient designs in R
library(pracma) #for least common multiple

options(contrasts=c("contr.sum","contr.poly"))
#note: It is prefereable to switch to orthogonal contrasts when judgments about the orthogonality of designs are of interest
rm(list = ls())

#######################################################################################################################
#load template with speficications

dsp0 <- read.xlsx(xlsxFile = "in_design.xlsx", sheet = "spec", na.strings = "")
#get design specifications
dspi <- dsp0[dsp0$spx == 2,]; dspi


########################################################################################################################
#generate full factorial 
########################################################################################################################

#generate full factorial
dff_n <- gen.factorial(levels = dspi$nlv, varNames = dspi$var)
dff_n[1:15,] # table(dff_n$durc_10)

#note: identification and removal of dominant alternatives for the candidate list is not feasible
#reason: random components are added later during preparation of choice tasks


########################################################################################################################
#get model equations
########################################################################################################################

#linear terms - main effects - template
d1eq_li_mn_t <- paste0(sapply(1:nrow(dspi), function(x) dspi$var[x]), collapse = " + ")
#linear terms - main effects - formula
d1eq_li_mn_f <- paste0("~ ", d1eq_li_mn_t) #13 coefficients
#linear terms + 2nd order interactions - formula: required for evaluation of the design
d1eq_li_i2_f <- paste0("~ (", d1eq_li_mn_t, ")^2") #79 coefficients

#polynomial terms - main effects - template
d1eq_pn_mn_t <- paste0(sapply(1:nrow(dspi), function(x) paste0("poly(", dspi$var[x],",", max(1, dspi$nlv[x]-1), ")")), collapse = " + ")
#polynomial terms - main effects - formula
d1eq_pn_mn_f <- paste0("~ ", d1eq_pn_mn_t) #44 coefficients


########################################################################################################################
#draw an optimal design from the candidate list (corresponds to the full factorial):
########################################################################################################################

#generate optimal design from factor vars based on linear model
#coerce numeric vars in candidate list to factors
dff_f <- dff_n
for(vi in 1:ncol(dff_f)) {dff_f[,vi] <- as.factor(dff_f[,vi])} # str(dff_f)
#get the design
tstart <- Sys.time()
dop_f <- try(optFederov(frml = as.formula(d1eq_li_mn_f), data = dff_f, approximate = F, criterion = "D", nTrials = 64*8)) # str(dop_f$design)
tdiff <- difftime(Sys.time(), tstart); tdiff
save(dop_f, file = "results/sp2_optimal.Rdata")


########################################################################################################################
#load and evaluate the optimal design
########################################################################################################################

# load("results/sp2_optimal.Rdata")
#evaluate the design:
edop_f <- eval.design(frml = as.formula(d1eq_li_mn_f), design = dop_f$design, confounding = T); edop_f[2:4]
# edop_f$determinant 0.4528
# edop_f$diagonality 0.808

#check attruíbute balance
table(as.matrix(dop_f$design))
#result: perfectly balanced (note: -1 and 1 are over-represented becaue sit_1 has only 2 levels)
# -1  -3   1   3 
# 768 512 768 512  


########################################################################################################################
#perform blocking
########################################################################################################################

#block the design with factor vars based on linear model and criterion = "D"
# str(dop_f$design)
#we need blocked desings with the following row numbers (tasks): 8

dblf_8  <- optBlock(frml = as.formula(d1eq_li_mn_f), withinData = dop_f$design, blocksizes = rep(8, 64), criterion = "D", nRepeats = 15)


########################################################################################################################
#prepare and evaluate blocked designs
########################################################################################################################

#get a vector of names of all blocked designs
ndbl <- "dblf_8"

#prepare and evaluate selected blocked designs
for(di in 1) {
  # di <- 1
  
  #get the selected design
  dblf_i <- get(ndbl[di]) # dblf_i$Blocks[1:2]
  dblfn_i <- dblf_i #get a copy for conversion into numeric design
  
  #convert factor vars in blocked design to numeric vars
  for (bi in 1:length(dblfn_i$Blocks)) { # bi <- 1
    for(vi in 1:ncol(dblfn_i$Blocks[[bi]])) {dblfn_i$Blocks[[bi]][,vi] <- as.numeric(levels(dblfn_i$Blocks[[bi]][,vi])[dblfn_i$Blocks[[bi]][,vi]])}
  } # str(dblfn_i$Blocks[1])
  for(vi in 1:ncol(dblfn_i$design)) {dblfn_i$design[,vi] <- as.numeric(levels(dblfn_i$design[,vi])[dblfn_i$design[,vi]])} # str(dblfn_i$design)
  
  #evaluate the blocked design with numeric vars (which was generated from blocking with factor vars) based on polynomial model
  edblfn_i <- eval.blockdesign(frml = as.formula(d1eq_pn_mn_f), design = dblfn_i$design,
                               blocksizes = rep(nrow(dblfn_i$Blocks[[1]]), length(dblfn_i$Blocks)))
  print(edblfn_i)
  
  #shuffle row order of blocked design
  for (bi in 1:length(dblfn_i$Blocks)) { # bi <- 1
    dblfn_i$Blocks[[bi]] <- dblfn_i$Blocks[[bi]][sample(1:nrow(dblfn_i$Blocks[[bi]]), size = nrow(dblfn_i$Blocks[[bi]]), replace = F),]
  } # dblfn_i$Blocks[1:10]
  
  #write final numeric design to environment
  assign(gsub("dblf", "dblfn", ndbl[di]), dblfn_i) # dblfn_i$Blocks[1:10]
}

#save final blocked designs of SP1
#note: optimal design generated with factor vars, blocking also with factor vars, then transformed to numeric and row order randomised
save(dblfn_8, file = "results/sp2_blocked.Rdata")
# dblfn_8$Blocks[1:5]

########################################################################################################################
#replace levels by factors and save the final design
########################################################################################################################

# load("results/sp2_blocked.Rdata")
load("results/in_ds0.Rdata") #ds0 already comprises sp1 designs, now add sp2 designs

#get designs of sp1
for (ni in c(8)) {
  # ni <- 8
  #get blocks of selected design
  db0 <- get(paste0("dblfn_", ni))
  db0 <- db0$Blocks # db0[1]
  #specify number of blocks in ds0$C
  ds0$C[nrow(ds0$C)+1,] <- c(2, ni, length(db0), 1)
  
  #loop over all blocks bi
  for (bi in 1:length(db0)) {
    # bi <- 1
    #select block bi
    dbi <- db0[paste0("B", bi)][[1]]
    
    #loop over all vars in block dbi and substitute the levels by the factors from design plan
    for (vi in names(dbi)) {
      # vi <- names(dbi)[2]
      mtc <- data.frame(ls = as.numeric(dspi[dspi$var == vi,grep("ls", names(dspi))]), fc = as.numeric(dspi[dspi$var == vi,grep("fc", names(dspi))]))
      mtc <- mtc[!is.na(mtc$ls),]
      dbi[,vi] <- mtc$fc[match(dbi[,vi], mtc$ls)]
    }
    db0[paste0("B", bi)][[1]] <- dbi
  }
  ds0[["s2"]][[paste0("n", ni)]] <- db0
}

########################################################################################################################
#check before saving
#get a selected design (ni = numer of experiments, bi = block)
ni <- c(8)[1]
bi <- 42
ds0[["s2"]][paste0("n", ni)][[1]][paste0("B", bi)][[1]]

#save list of designs with sp2 included
save(ds0, file = "results/in_ds0.Rdata")

#End###################################################################################################################
