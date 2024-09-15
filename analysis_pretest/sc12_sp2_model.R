#Sept 2019 Reinhard Hoessinger
#vmoe: estimate sp2 model from pretest data

#######################################################################################################################
#set work directory, load packages and empty workspace
setwd("D:/R/Projects/p84_vmoe2/analysis/pretest")
rm(list = setdiff(ls(), c('constLL', 'beta')))
source('D:/R/Projects/p02_Functions/DcFunctions.R') #estimation of dc models with Hess code
source("D:/R/Projects/p02_Functions/CorrMatrix.R") #semparatial correlation
source("D:/R/Projects/p02_Functions/DataHandling.R") #generate dummy vars

#######################################################################################################################
#general settings, load and prepare data
#######################################################################################################################

#set input file and directories
ufl <- "in_sp2_models.txt" #txt file mit utility functions
set.seed(31415926)
#load data from vmoe pretest
load("data/dat_v01.Rdata")
#note: models are based on dat_v01.Rdata = db_ive_eldon_2019_08_30.Rdata


########################################################################################################################
#prepare sp2 data for estimation
#######################################################################################################################

#allocate tour dist from rp to sp2
dsp2$TourDist <- dp2$TourDist[match(dsp2$PeID, dp2$PeID)]

#calculate deviation from rp for: departure time of trip 1, departure time of trip 2, duration of stay
#allocate rp departure time of trip 1 from person data to sp2 data
dsp2$Tour1Dep <- dp2$Tour1Dep[match(dsp2$PeID, dp2$PeID)]
#calculate positive and negative deviations from rp departure time in separate vars (negative deviations turned to positive)
dsp2$pdep1_1 <- dsp2$dep1_1 - dsp2$Tour1Dep; dsp2$pdep1_1[dsp2$pdep1_1 < 0] <- 0
dsp2$ndep1_1 <- - (dsp2$dep1_1 - dsp2$Tour1Dep); dsp2$ndep1_1[dsp2$ndep1_1 < 0] <- 0
dsp2$pdep1_2 <- dsp2$dep1_2 - dsp2$Tour1Dep; dsp2$pdep1_2[dsp2$pdep1_2 < 0] <- 0
dsp2$ndep1_2 <- - (dsp2$dep1_2 - dsp2$Tour1Dep); dsp2$ndep1_2[dsp2$ndep1_2 < 0] <- 0
dsp2$pdep1_3 <- dsp2$dep1_3 - dsp2$Tour1Dep; dsp2$pdep1_3[dsp2$pdep1_3 < 0] <- 0
dsp2$ndep1_3 <- - (dsp2$dep1_3 - dsp2$Tour1Dep); dsp2$ndep1_3[dsp2$ndep1_3 < 0] <- 0
dsp2$pdep1_4 <- dsp2$dep1_4 - dsp2$Tour1Dep; dsp2$pdep1_4[dsp2$pdep1_4 < 0] <- 0
dsp2$ndep1_4 <- - (dsp2$dep1_4 - dsp2$Tour1Dep); dsp2$ndep1_4[dsp2$ndep1_4 < 0] <- 0

#allocate rp departure time of trip 2 from person data to sp2 data
dsp2$Tour2Dep <- dp2$Tour2Dep[match(dsp2$PeID, dp2$PeID)]
#calculate positive and negative deviations from rp departure time in separate vars (negative deviations turned to positive)
dsp2$pdep2_1 <- dsp2$dep2_1 - dsp2$Tour2Dep; dsp2$pdep2_1[dsp2$pdep2_1 < 0] <- 0
dsp2$ndep2_1 <- - (dsp2$dep2_1 - dsp2$Tour2Dep); dsp2$ndep2_1[dsp2$ndep2_1 < 0] <- 0
dsp2$pdep2_2 <- dsp2$dep2_2 - dsp2$Tour2Dep; dsp2$pdep2_2[dsp2$pdep2_2 < 0] <- 0
dsp2$ndep2_2 <- - (dsp2$dep2_2 - dsp2$Tour2Dep); dsp2$ndep2_2[dsp2$ndep2_2 < 0] <- 0
dsp2$pdep2_3 <- dsp2$dep2_3 - dsp2$Tour2Dep; dsp2$pdep2_3[dsp2$pdep2_3 < 0] <- 0
dsp2$ndep2_3 <- - (dsp2$dep2_3 - dsp2$Tour2Dep); dsp2$ndep2_3[dsp2$ndep2_3 < 0] <- 0
dsp2$pdep2_4 <- dsp2$dep2_4 - dsp2$Tour2Dep; dsp2$pdep2_4[dsp2$pdep2_4 < 0] <- 0
dsp2$ndep2_4 <- - (dsp2$dep2_4 - dsp2$Tour2Dep); dsp2$ndep2_4[dsp2$ndep2_4 < 0] <- 0

#allocate rp duration of stay from person data to sp2 data
dsp2$TourStay <- dp2$TourStay[match(dsp2$PeID, dp2$PeID)]
dsp2$TourStay[dsp2$TourStay < 0] <- 0
#note: there are 2 negative durations due to incorrect calc of dpf$TourStay in sc_functions.R line 46 (already corrected)
#calculate positive and negative deviations from rp duration of stay in separate vars (negative deviations turned to positive)
dsp2$stay_1[dsp2$stay_1 < 0] <- 0
dsp2$pstay_1 <- dsp2$stay_1 - dsp2$TourStay; dsp2$pstay_1[dsp2$pstay_1 < 0] <- 0
dsp2$nstay_1 <- - (dsp2$stay_1 - dsp2$TourStay); dsp2$nstay_1[dsp2$nstay_1 < 0] <- 0
dsp2$stay_2[dsp2$stay_2 < 0] <- 0
dsp2$pstay_2 <- dsp2$stay_2 - dsp2$TourStay; dsp2$pstay_2[dsp2$pstay_2 < 0] <- 0
dsp2$nstay_2 <- - (dsp2$stay_2 - dsp2$TourStay); dsp2$nstay_2[dsp2$nstay_2 < 0] <- 0
dsp2$stay_3[dsp2$stay_3 < 0] <- 0
dsp2$pstay_3 <- dsp2$stay_3 - dsp2$TourStay; dsp2$pstay_3[dsp2$pstay_3 < 0] <- 0
dsp2$nstay_3 <- - (dsp2$stay_3 - dsp2$TourStay); dsp2$nstay_3[dsp2$nstay_3 < 0] <- 0
dsp2$stay_4[dsp2$stay_4 < 0] <- 0
dsp2$pstay_4 <- dsp2$stay_4 - dsp2$TourStay; dsp2$pstay_4[dsp2$pstay_4 < 0] <- 0
dsp2$nstay_4 <- - (dsp2$stay_4 - dsp2$TourStay); dsp2$nstay_4[dsp2$nstay_4 < 0] <- 0

#coerce dur_i to numeric (stored as chr)
for (vi in c("dur_1", "dur_2", "dur_3", "dur_4")) {dsp2[, vi] <- as.numeric(dsp2[, vi])}


########################################################################################################################
#check corrs and semipartial corrs
########################################################################################################################

#make dsp1 to corr dataset
dcor <- dsp2

#vector of all attributes
varx <- c("avl_11", "avl_21", "avl_12", "avl_22", "avl_13", "avl_23", "avl_14", "avl_24",
  "pdep1_1", "pdep1_2", "pdep1_3", "pdep1_4", "ndep1_1", "ndep1_2", "ndep1_3", "ndep1_4",
  "pdep2_1", "pdep2_2", "pdep2_3", "pdep2_4", "ndep2_1", "ndep2_2", "ndep2_3", "ndep2_4",
  "pstay_1", "pstay_2", "pstay_3", "pstay_4", "nstay_1", "nstay_2", "nstay_3", "nstay_4",
  "dur_1", "dur_2", "dur_3", "dur_4", "cost_1", "cost_2", "cost_3", "cost_4", 
  "load_1", "load_2", "load_3", "load_4", "costt_1") # dcor[1:10, varx]
#corr matrix of all attributes
# round(cor(dcor[, varx], use = "pairwise.complete.obs"),3)

#get dependent var y as dummies of choice var
dcor <- cbind(dcor, aaDummies(var = dcor$chc, dmy = "chc")) # dcor[1:10,]
vary <- names(dcor)[grep("chc_", names(dcor))]
#get control vars z
varz <- c()

#calculate semi-partial corrs and prepare for display
spc <- aaCorrSemi(dcor, varx, vary, varz); spc[1:20,]

########################################################################################################################
#get summary statistics of all attributes
rspi <- data.frame(var = "", Mean = NA, StDev = NA, Min = NA, Median = NA, Max = NA, NAs = NA, stringsAsFactors = F)
res22 <- rspi[0,]
for(vi in varx) { # vi <- varx[2]
  res22 <- rbind(res22, rspi)
  res22$var[nrow(res22)] <- vi
  res22$Mean[nrow(res22)] <- round(mean(dcor[,vi], na.rm = T),2)
  res22$StDev[nrow(res22)] <- round(sd(dcor[,vi], na.rm = T),2)
  res22$Min[nrow(res22)] <- round(min(dcor[,vi], na.rm = T),2)
  res22$Median[nrow(res22)] <- round(median(dcor[,vi], na.rm = T),2)
  res22$Max[nrow(res22)] <- round(max(dcor[,vi], na.rm = T),2)
  res22$NAs[nrow(res22)] <- round(length(dcor[is.na(dcor[,vi]),vi]),2)
}
#write to csv
write.table(res22, file = "results/res_descriptives_v02.csv", sep = ";", row.names = F, append = T) #from db_ive_eldon_2019_09_05.Rdata


########################################################################################################################
#estimate the model
########################################################################################################################

#specify model dataset, utility function, and par vector (create dat1, mdi1, beta)
dat0 <- dsp2 #320
#truncate model dataset to tour distance < 60 km
# dat0 <- dsp2[dsp2$TourDist < 60,] #256

#check for missing vals in attributes (note: shhould be replaced by zeros)
# table(is.na(dat0[, varx]))
#constants only model: mdi1<-aaPrepDatMod(ufl,0,dat0);beta<-mdi1$beta0;aaRunModel();constLL<-mdo1$maximum;constLL

mdi1 <- aaPrepDatMod(ufl = ufl, mod = 3, dat = dat0); beta <- mdi1$beta0; mdi1 # dat1[1:15,]
aaRunModel(hess = F) # beta <- mdo1$estimate
mdr1 <- aaModOutput(mdo1) # summary(mdo1)$estimate
#save model and data as R object: save(mdo1, dat1, file = 'results/m00.Rdata')

#write output to Excel
rnam <- "m05: SP2 all displayed attributes except departure time of trip 2"
xfl <- "D:/R/Projects/p84_vmoe2/analysis/pretest/results/sp_mod_viewer.xlsm"
aaResToExcel(xfl, dat1, mdr1, rnam)

#End###################################################################################################################
