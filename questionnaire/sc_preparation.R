#Jan 2019 Reinhard Hoessinger
#data processing for shiny

#######################################################################################################################
#read adress data from ZMR and change encoding to repair specal characters
# dad <- read.csv("D:/R/Projects/p84_vmoe2/sample/MobilitaetserhebungBmvit.csv", sep = ";", encoding = "UTF-8") # dad[1:10,]
# write.xlsx(dad, "D:/R/Projects/p84_vmoe2/sample/MobilitaetserhebungBmvit.xlsx")

#######################################################################################################################
#get a key file with passwords of length size
# size <- 20000
# dk0 <- character()
# for (i in 1:size) {dk0 <- c(dk0, paste0(sample(letters[1:26], 2, replace = T), collapse = ""))}
# for (i in 1:length(dk0)) {dk0[i] <- paste0(dk0[i], "-", paste0(sample(1:9,4,replace = T), collapse = ""))}
# dk0 <- data.frame(PeID = 1:size, pwd = dk0, stringsAsFactors = F); dk0[1:20,]
# dk0 <- dk0[!duplicated(dk0$pwd),][1:13003,]
# 
# save(dk0, file = dkn)
# write.table(dk0, file = "D:/R/Projects/p84_vmoe2/sample/keys.csv", sep = ";")
#save keys as csv-template for database
# write.table(dk0, file = "zz_templates/KeysTmp.csv", col.names = T, sep = ";", na = "", row.names = F) #save input in csv


#######################################################################################################################
#DO NOT OVERWRITE THE DATA IN IVE-ELDON DATABASE!!!
#######################################################################################################################

#initialise tables in db with test datasets
#Household:
# db_dh <- read.xlsx(xlsxFile = dir0, sheet = "hhld", na.strings = "")
# dbWriteTable(conn = dbc, name = "HholdsTbl", value = db_dh, row.names = F, overwrite = T, append = F)
# #Person:
# db_dp <- read.xlsx(xlsxFile = dir0, sheet = "pers", na.strings = ""); db_dp[, which(db_dp[2,] == "txt")] <- ""
# dbWriteTable(conn = dbc, name = "PersonsTbl", value = db_dp, row.names = F, overwrite = T, append = F)
# #SP1:
# db_sp1 <- read.xlsx(xlsxFile = dir0, sheet = "sp1", na.strings = "")
# dbWriteTable(conn = dbc, name = "Sp1Tbl", value = db_sp1[1,], row.names = F, overwrite = T, append = F)
# #SP2:
# db_sp2 <- read.xlsx(xlsxFile = dir0, sheet = "sp2", na.strings = "")
# dbWriteTable(conn = dbc, name = "Sp2Tbl", value = db_sp2[1,], row.names = F, overwrite = T, append = F)
# #SP3:
# db_sp3 <- read.xlsx(xlsxFile = dir0, sheet = "sp3", na.strings = "")
# dbWriteTable(conn = dbc, name = "Sp3Tbl", value = db_sp3[1,], row.names = F, overwrite = T, append = F)
# #SP4:
# db_sp4 <- read.xlsx(xlsxFile = dir0, sheet = "sp4", na.strings = "")
# dbWriteTable(conn = dbc, name = "Sp4Tbl", value = db_sp4[1,], row.names = F, overwrite = T, append = F)
# #Log:
# db_log <- read.xlsx(xlsxFile = dir0, sheet = "log", na.strings = "")
# dbWriteTable(conn = dbc, name = "LogTbl", value = db_log[1,], row.names = F, overwrite = T, append = F)
#block counter for desings
# dc0 <- ds0$Dc
# dc0 <- cbind(DcID = 1:10, dc0)
# dbWriteTable(conn = dbc, name = "DcnTbl", value = dc0, row.names = F, overwrite = T, append = F)

#End###################################################################################################################
