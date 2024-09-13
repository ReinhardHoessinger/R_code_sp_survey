#Jan 2019 Reinhard Hoessinger
#get facilities for shiny app

#######################################################################################################################
#set work directory, load packages and data
# setwd("D:/R/Projects/p84_vmoe2/shiny")
# setwd("/home/iveweb/applications/vmoe")
# setwd("/home/hoessing/apps/shiny/vmoe")
# list.files(getwd())

library(shiny)
library(shinyhelper) #for help dialogue boxes
library(shinyBS) #for bsPopover and bsTooltip
library(shinydashboard) #for placing icons using valueBox()
library(shinyjs) #for hidden text fields (shinyjs::hidden) and disabeled()
library(shinyWidgets) #for confirmSweetAlert (also prettyRadioButtons but not used)
library(shinyTime) #for time input field

library(RMySQL) #for communication with MySQL database
library(openxlsx) #for communication with Excel
library(lubridate) #for transfer of time format to min of day

#libraries which are currently not used
#note: sweetalertR is not available via install.packages(), it must be installed from github as follows:
# install.packages("remotes")
# remotes::install_github("timelyportfolio/sweetalertR")
# library(sweetalertR) #for alert dialog box but possibly no 'cancel' option -> use confirmSweetAlert from shinyWidgets

#directory of input data
dir0 <- "in_vmoe2.xlsx"

#######################################################################################################################
#set parameters for calculations
pr0 <- c(
  taUrb = 6, taRur = 3, #access time (urban, rural)
  tfUrb = 1.7, tfRur = 1.2, #factor for net travel time [min/km] for car and public (urban, rural)
  tfCon = 1.2, #factor for average congestion time (varies between 0 and 40%)
  tPark = 8, #average time for parking search (varies between 0 and 16 min)
  tfBke = 5, tfWlk = 10, #factor for travel time [min/km] for bike and walk (12 and 6 km/h, respectively)
  cfAll = 0.25, #factor for net travel cost [Euro/km] for car and public
  cfCsh = 0.25, #factor for net travel cost [Euro/km] for carsharing (same as car and public)
  cfTol = 1.1, #factor for average toll cost (varies between 0 and 20%)
  cPark = 5, #average parking cost (varies between 0 and 10 Euro)
  mpBeg = 360, mpEnd = 540, epBeg = 960, epEnd = 1140 #begin and end of morning and evening peak (minute of day)
)

#######################################################################################################################
#create a reactive object can be shared between all sessions
#required for session counter: https://gist.github.com/trestletech/9926129
vals <- reactiveValues(count = 0)

#######################################################################################################################
#get default vector of check variables which command conditional pages, e.g. warnings of invalid input
con0 <- c(
  NumPers = 0, NumUnd6 = 0, Num6To17 = 0, StatWalk = 0, HhLicense = 0, HhCars = 0, HhCsMemb = 0, #household page
  birth = 0, ptCard = 0, #persons page
  Dist_T1 = 0, Dist_T2 = 0, Dur_T1 = 0, Dur_T2 = 0, Time_T2 = 0 #trip page
) 

#######################################################################################################################
#load auxillary tables for online forms

#handling of forms (order, visibility, appearance)
pg0 <- read.xlsx(xlsxFile = dir0, sheet = "pages", na.strings = "")
#large text passages for HTML
dx0 <- read.xlsx(xlsxFile = dir0, sheet = "text", na.strings = "")
#table with number of tasks depending on which SP experiments are conducted
de0 <- read.xlsx(xlsxFile = dir0, sheet = "exper", na.strings = "")
#table with vars to be checked and messages if input is missing
req0 <- read.xlsx(xlsxFile = dir0, sheet = "req", na.strings = "")
#screen layout (replace NAs by "")
sc0 <- read.xlsx(xlsxFile = dir0, sheet = "screen", na.strings = "", detectDates = T)
for (ni in names(sc0)) {sc0[is.na(sc0[, ni]), ni] <- ""}; rm(ni) # str(sc0)

#list with code blocks: dataframes are for for match, named vectors are for radiobuttons and checkboxes
cd00 <- read.xlsx(xlsxFile = dir0, sheet = "codes", na.strings = "")
cls <- list()
cls$cd01 <- cd00[cd00$sel == 1,c("nam", "val")] #interview state
cls$cd02 <- cd00[cd00$sel == 2,c("nam", "val")] #conditional color of interview state
cls$cd03 <- setNames(cd00$val[cd00$sel == 3], cd00$nam[cd00$sel == 3]) #yes/no
cls$cd04 <- setNames(cd00$val[cd00$sel == 4], cd00$nam[cd00$sel == 4]) #tour: main purpose
cls$cd05 <- setNames(cd00$val[cd00$sel == 5], cd00$nam[cd00$sel == 5]) #trip: type of origin
cls$cd06 <- setNames(cd00$val[cd00$sel == 6], cd00$nam[cd00$sel == 6]) #trip: public service interval
cls$cd07 <- setNames(cd00$val[cd00$sel == 7], cd00$nam[cd00$sel == 7]) #trip: main mode
cls$cd08 <- setNames(cd00$val[cd00$sel == 8], cd00$nam[cd00$sel == 8]) #trip: availability of public (if not used)
cls$cd09 <- setNames(cd00$val[cd00$sel == 9], cd00$nam[cd00$sel == 9]) #household: public means at nearest station
cls$cd10 <- setNames(cd00$val[cd00$sel == 10], cd00$nam[cd00$sel == 10]) #household: freq. of delivery and service
cls$cd11 <- setNames(cd00$val[cd00$sel == 11], cd00$nam[cd00$sel == 11]) #household: net income
cls$cd12 <- setNames(cd00$val[cd00$sel == 12], cd00$nam[cd00$sel == 12]) #gender
cls$cd13 <- setNames(cd00$val[cd00$sel == 13], cd00$nam[cd00$sel == 13]) #educational level
cls$cd14 <- setNames(cd00$val[cd00$sel == 14], cd00$nam[cd00$sel == 14]) #eployment status
cls$cd15 <- setNames(cd00$val[cd00$sel == 15], cd00$nam[cd00$sel == 15]) #car availabilty
cls$cd16 <- setNames(cd00$val[cd00$sel == 16], cd00$nam[cd00$sel == 16]) #public transport tickets
cls$cd17 <- setNames(cd00$val[cd00$sel == 17], cd00$nam[cd00$sel == 17]) #cs/rp used in the past
cls$cd18 <- setNames(cd00$val[cd00$sel == 18], cd00$nam[cd00$sel == 18]) #cs/pr conceivable in the future
cls$cd95 <- setNames(cd00$val[cd00$sel == 95], cd00$nam[cd00$sel == 95]) #no
cls$cd97 <- setNames(cd00$val[cd00$sel == 97], cd00$nam[cd00$sel == 97]) #don't know
#code list for availability of seats in SP1 and SP2:
cls$sit <- data.frame(cd = c(0,1,2,3), na = c("nein", "eher nein", "eher ja", "ja"), stringsAsFactors = F)

#list of varname vectors required for SavePeToR (store input vals of ui in person record)
vls <- list()
vls$vhhl <- c("hTel", "hMail", "NumPers", "NumUnd6", "Num6To17", "StatWalk", "StatMeans", "HhCars", "HhLicense", "HhCsMemb", "HhDeliver", "HhService", "HhIncome")
vls$vprs <- c("birth", "gender", "educ", "employ", "bikeAvl", "license", "carAvl", "ptCard", "CsUsed", "CsFuture", "PrUsed", "PrFuture")
vls$vrp1 <- c("TourYn")
vls$vrp2 <- c("Tour2Purp", "TourPeak", "Tour1Purp", "Tour1Urb", "Tour2Urb")
vls$vrp3 <- c("Modes_T1", "Stop_T1", "Dist_T1",
              "PubInt_T1", "PubSecDsc_T1", "PubCost_T1", "PubCostNa_T1", "CarCong_T1", "CarCongNa_T1",
              "CarPark_T1", "CarParkNa_T1", "CarCost_T1", "CarCostNa_T1")
vls$vrp4 <- c("Modes_T1", "Stop_T1", "Dist_T1",
              "PubInt_T1", "PubSecDsc_T1", "PubCost_T1", "PubCostNa_T1", "CarCong_T1", "CarCongNa_T1",
              "CarPark_T1", "CarParkNa_T1", "CarCost_T1", "CarCostNa_T1", "Modes_T2", "Stop_T2",
              "Dist_T2", "PubInt_T2", "PubSecDsc_T2", "PubCost_T2", "PubCostNa_T2", "CarCong_T2",
              "CarCongNa_T2", "CarPark_T2", "CarParkNa_T2", "CarCost_T2", "CarCostNa_T2")

#file with design factors
load("in_ds0.Rdata") # ds0$C

#######################################################################################################################
#connect to database with request data
#close all open connections to avoid that too many connections are opened (only 16 can be opened)
#try(dbDisconnect(dbc))
try(lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect))

#set new connection to db either on iveweb-server or on ive-server
if (substring(getwd(),1,13) == "D:/R/Projects" | substring(getwd(),1,12) == "/home/iveweb") {
  dbc <- dbConnect(
    MySQL(),
    dbname = "iveweb_02",
    host = "iveweb.boku.ac.at",
    port = 3306,
    user = "iveweb", password = "KwttwLxi6lul/LCq"
  )
} else {
  dbc <- dbConnect(
    MySQL(),
    dbname = "vmoe2",
    host = "localhost",
    port = 3306,
    user = "vmoe2", password = "VoWiToLi2019vmoe2"
  )
}

#End###################################################################################################################
