#Reinhard Hoessinger August 2016
#functions for data handling

#######################################################################################################################
#get dummies for unique codes of a variable
aaDummies <- function (var, dmy = "dmy_", ref = NULL, keepNA = F) { #specify variable, prefix of dummies, code(s) to be excluded, keep NAs
  out <- data.frame(v1 = rep(0, length(var)))
  cds <- unique(var)
  cds <- cds[!is.na(cds)]
  if (length(ref) > 0) {cds <- cds[!cds %in% ref]}
  cds <- cds[order(cds)]
  for (ci in cds) {
    # ci <- 1
    out$dmy_i <- 0
    out$dmy_i[var == ci] <- 1
    names(out)[names(out) == "dmy_i"] <- paste0(dmy, ci)
  }
  if (keepNA == T) {out[is.na(var),] <- NA}
  return(out[, -1])
}

#######################################################################################################################
#convert character number to numeric
aaChr2Num <- function (cn, dfl = NULL, min = NULL, max = NULL) { #set default if cn is invalid as well as min and max
  # cn <- " 5 bis 7,5 Kilometer"; dfl = 10; min = 1; max = 100
  #(1) replace everything but numbers, ",", and "." by blanks, (2) convert "," to "."., (3) remove leading and trailing blanks
  cn <- trimws(gsub(",", ".", gsub("[^0-9.,]", " ", cn)))
  #replace double blanks by single blanks
  while(length(grep("  ",cn)) > 0) {cn <- gsub("  ", " ",cn)}
  #combine multiple numbers in a vector and calculate mean
  cn <- as.numeric(unlist(strsplit(cn," ")[[1]]))
  if(length(cn) > 1) {cn <- mean(cn)}
  res <- cn
  #if dfl is set and res is NA -> take default value
  if (length(dfl) > 0) { if (is.na(res)) {res <- dfl} }
  if (!is.na(res)) {
    #if min and/or max is set and res < min and/or res > max: take default value
    if (length(min) > 0) {if (res < min){res <- min}}
    if (length(max) > 0) {if (res > max){res <- max}}
  }
  return(res)
}

#######################################################################################################################
#get moving average in 4 different versions
aaMovingAvg <- function(dat, rng, dir = c('fore', 'back', 'both', 'symm')[4]) {
  #note: for dir = both and symm rng is applied to both sides -> size of rolling window doubles
  mav <- rep(NA, length(dat))
  for (ri in 1:length(dat)) {
    #censor forward and backward range to the edges of the dataset
    rf <- min(rng, length(dat) - ri)
    rb <- min(rng, (ri - 1))
    #note:   dir == 'both' causes that censored rf and rb remain unchanged -> no if condition required
    if      (dir == 'fore') {rb <- 0}
    else if (dir == 'back') {rf <- 0}
    else if (dir == 'symm') {
      rf <- min(rf, rb)
      rb <- rf
    }
    mav[ri] <- mean(dat[(ri-rb):(ri+rf)], na.rm = T)
  }
  return(mav)
}

#######################################################################################################################
#write an object to CSV
aaWriteCsv <- function (file, dat, rown = F, coln = NULL, app = F) {
  # file <- 'dat.csv'; dat <- dat; rown = F; coln = NULL
  if(length(coln) == 0) {
    if (rown == F) {coln <- T} else {coln <- NA}
  }
  write.table(dat, file, sep = ';', row.names = rown, col.names =  coln, na = "", append = app) # coln = c(F, T)
}

############################################################################################
#write an object to Excel using openxlsx
aaWriteXls <- function (file, sheet, dat, rnam = F, new = F, app = F) {
  # file = "results/res_descriptives.xlsx"; sheet = "dat"; dat = res_1; new = F; app = T
  if(!'package:openxlsx' %in% search()) {library(openxlsx)}
  # file <- 'wm_descriptives.xlsx'; sheet <- sht
  wbk <- loadWorkbook(file = file)
  if (new == T) {addWorksheet(wbk, sheet)}
  if (new == F & app == T) {xro <- nrow(read.xlsx(file, sheet, skipEmptyRows = F)) + 3} else {xro <- 1}
  #note: append is only possible if the sheet already exists (new == F)
  writeData(wb = wbk, sheet = sheet, x = dat, startRow = xro, rowNames = rnam)
  saveWorkbook(wbk, file = file, overwrite = T)
}

#End########################################################################################
