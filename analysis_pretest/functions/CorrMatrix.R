#Reinhard Hoessinger August 2016
#function to generate correlation matrices (bivariate and semipartial)
library(ppcor)

############################################################################################
#get a bivariate correlation matrix
aaCorrBivar <- function (ddat, varl) { #specify dataset, list of variables
  #generate a dataset ddat with selected variables varl
  ddat <- ddat[, varl]
  #check variables according to statistical criteria: keep only numeric variables with sd > 0
  vars <-  character()
  for (var in names(ddat)) {
    if (is.numeric(ddat[, var])) {
      if (length(ddat[!is.na(ddat[, var]), var] > 1)) {
        if (sd(ddat[, var], na.rm = T) > 0) {vars <- c(vars, var)}
      }
    }
  }
  ddat <- ddat[, vars] #all numeric variables with sd > 0
  #generate a dataset with means and sds
  dmat <- data.frame(t(colMeans(ddat, na.rm = T)))
  dmat <- rbind(dmat, apply(ddat, 2, function(x) sd(x, na.rm = T)))
  dmat$Label[1:2] <- c("Means", "SDs")
  dmat$Varname <- NA
  #get frequency matrix
  mfrq <- as.matrix(ddat)
  mfrq[!is.na(mfrq)] <- 1
  mfrq[is.na(mfrq)] <- 0 # table(mfrq)
  dfrq <- as.data.frame(crossprod(mfrq))
  dfrq$Label <- "N"
  dfrq$Varname <- rownames(dfrq)
  #get correlation matrix
  mcor <- cor(ddat, use = "pairwise.complete.obs")
  dcor <- as.data.frame(mcor)
  dcor$Label <- "CORR"
  dcor$Varname <- rownames(dcor)
  #merge all datasets and put Label and Varname on first place
  dmat <- rbind(dmat, dfrq, dcor)
  dmat <- dmat[, c("Label", "Varname", names(dmat)[1 : (ncol(dmat) -2)])]
  return(dmat)
}

############################################################################################
#get table with semipartial correlations
aaCorrSemi <- function(ddat, varx, vary, varz = NULL, b = 1, e = 20) { #specify dataset, full list of predictors, y and z vars
  # ddat <- dt3; b = 1; e = 20
  #check varx according to statistical criteria: only vars not included in vary and varz with numeric scale and sd > 0
  varx <- varx[!varx %in% vary & !varx %in% varz]
  vars <-  character()
  for (var in varx) {
    # var <- varx[i]; var
    ddat[is.nan(ddat[, var]) | is.infinite(ddat[, var]), var] <- NA
    if (is.numeric(ddat[, var]) & length(ddat[, var] > 1)) {
      if (sd(ddat[, var], na.rm = T) > 0) {vars <- c(vars, var)}
    }
  }
  #replace NAs by mean values
  for (vi in c(vars, vary, varz)) {ddat[which(is.na(ddat[, vi])), vi] <- mean(ddat[, vi], na.rm = T)} # table(is.na(ddat[, c(vars, vary, varz)]))
  #get empty dataset for results
  spc <- data.frame(matrix(data = NA, nrow = length(vars), ncol = length(vary) + 4))
  names(spc) <- c("vars", "tmax", "tmid", "xx", vary)
  spc$vars <- vars
  #get semipartial correlations for all x,y
  # xi <- vars[1]; yi <- vary[1]
  for (xi in vars) {
    for (yi in vary) {
      if (length(varz) == 0) {spi <- try(cor.test(ddat[, xi], ddat[, yi], method = "pearson"), silent = T)} else {
        spi <- try(spcor.test(ddat[, xi], ddat[, yi], ddat[, varz], method = "pearson"), silent = T)}
      #check for error and continue even if error occurs
      if(isTRUE(class(spi) == "try-error")) {NULL} else {spc[spc$vars == xi, yi] <- round(spi$statistic, 1)}
    }
  } # spc[1:10, 1:8]
  #prepare the result: if only one y the sapply otherwise apply
    if (length(vary) == 1) {
    spc$tmax <- sapply(spc[, vary], function(x) max(abs(x), na.rm = T))
    spc$tmid <- sapply(spc[, vary], function(x) round(mean(abs(x), na.rm = T), 1))
  } else {
    spc$tmax <- apply(spc[, vary], 1, function(x) max(abs(x), na.rm = T))
    spc$tmid <- apply(spc[, vary], 1, function(x) round(mean(abs(x), na.rm = T), 1))
  }
    spc$xx <- ""
  names(spc) <- c("vars", "tmax", "tmid", " ", substr(vary, b, e))
  spc <- spc[order(spc$tmax, decreasing = T), ]
  return(spc)
}

#END###################################################################################################################
