#Nov2017 Reinhard Hoessinger
#own functions for discrete choice models based on Hess code and Basil's models

pkg <- c('data.table', 'foreach', 'maxLik', 'numDeriv', 'openxlsx', 'sandwich')
sapply(pkg, require, character.only = T); rm(pkg)

#######################################################################################################################
#prepare data and model for estimation
aaPrepDatMod <- function (ufl, mod, dat, meth = 'mlhs', draws = 0) {
  # ufl = ufl; mod = 0; dat = dat0; meth = 'mlhs'; draws = 0
  ######################################################################################
  #read model settings from text file and select a model
  md0 <- scan(ufl, what = 'character', sep = '\n', blank.lines.skip = F)
  mdb <- which(md0 == paste0('[', mod, ']'))[1] + 1
  mde <- which(md0 == ''); mde <- min(mde[mde >= mdb]) - 1
  mdi <- md0[mdb:mde]
  mdi <- data.frame(matt = sapply(1:length(mdi), function(x) strsplit(mdi[x], ' | ', fixed = T)[[1]][1]),
                    mval = sapply(1:length(mdi), function(x) strsplit(mdi[x], ' | ', fixed = T)[[1]][2]), stringsAsFactors = F)
  #get sys names of selected model mdi
  sys0 <- mdi$mval[mdi$matt == 'sys'] #can currently include the keywords avl, rnd, lcs
  sys0 <- unlist(strsplit(gsub(' ', '', sys0), ','))
  sys1 <- gsub(".*=", '', sys0); names(sys1) <- gsub("=.*", '', sys0)
  #get settings of selected model mdi
  set0 <- mdi$mval[mdi$matt == 'set'] #can currently include the keywords avl, rnd, lcs
  set0 <- unlist(strsplit(gsub(' ', '', set0), ','))
  set1 <- gsub(".*=", '', set0); names(set1) <- gsub("=.*", '', set0); set1 <- c(id = mod, set1)
  mtyp <- set1['typ']
  #get alternative declarations
  altd <- sapply(grep('^u', mdi$matt), function(x) strsplit(mdi$mval[x], ': ', fixed = T)[[1]][1])
  altd <- unlist(sapply(1:length(altd), function(x) strsplit(altd[x], '#')))
  #get identifiers of alternatives: must correspond to values of chc var
  alt <- as.numeric(altd[!is.na(as.numeric(altd))]) #take only numeric elements of altd (no c* of d*)
  #get a list with model input: utility functions with avl var and equation
  mdi1 <- list()
  meqs0 <- sapply(grep('^u', mdi$matt), function(x) strsplit(mdi$mval[x], ': ', fixed = T)[[1]][2])
  avl <- gsub("\\[|\\].*", "", meqs0)
  #add attributes to mdi1
  mdi1$mset <- set1
  mdi1$mdat <- attr(dat, 'dat')
  mdi1$mnum <- NA
  mdi1$malt <- as.numeric(alt)
  mdi1$meqs <- gsub('.*] ', "", meqs0)
  mdi1$mavl <- avl
  mdi1$pfix <- gsub(' ', '', unlist(strsplit(mdi$mval[mdi$matt == 'fix'], ',')))
  #if model is a latent class model:
  if (mtyp == 'lcs') {
    cmem <- as.numeric(gsub('c', '', altd[grep('c', altd)])) #class-dependency of alternatives
    calt <- sapply(grep('^c', mdi$matt), function(x) strsplit(mdi$mval[x], ': ', fixed = T)[[1]][1])
    ceqs <- sapply(grep('^c', mdi$matt), function(x) strsplit(mdi$mval[x], ': ', fixed = T)[[1]][2])
    mdi1$ceqs <- ceqs #class membership equations
    mdi1$calt <- as.numeric(calt)
    mdi1$cmem <- as.numeric(cmem)
  }
  #if model is a nested logit model:
  if (mtyp == 'nst') {
    npar <- gsub(' ', '', unlist(strsplit(mdi$mval[mdi$matt == 'nst'], ','))) #also works if rnd does not exist (no mixed model)
    nmem <- as.numeric(gsub('n', '', altd[grep('n', altd)])) #nest membership of alternatives
    mdi1$npar <- npar
    mdi1$nmem <- as.numeric(nmem)
  } else {npar = NULL}
  ######################################################################################
  #get data and system vars (generate if missing, rename if name differs from sys name)
  dat <- data.table(dat)
  if (sys1['ID'] == 0) {dat[, ID := 1:nrow(dat)]} else if (sys1['ID'] != 'ID') {dat[, ID := dat[, sys1['ID'], with = F]]}
  if (sys1['wgt'] == 0) {dat[, wgt := 1]} else if (sys1['wgt'] != 'wgt') {dat[, wgt := dat[, sys1['wgt'], with = F]]}
  if (sys1['chc'] != 'chc') {dat[, chc := dat[, sys1['chc'], with = F]]}
  avu <- unique(avl)
  for(ai in 1:length(avu)) { if(!avu[ai] %in% names(dat)) { dat[, avu[ai] := 1] } }
  #add row ID, choice ID, and set ID in dependence of panel specification
  dat[, RowID := 1:nrow(dat)]
  setkey(dat, RowID)
  dat <- dat[order(dat$ID), ]
  ndID <- nchar(dat[, max(ID)]) #specify number of leading zeros in ID
  dat[, ID := paste0('i', formatC(ID, width = ndID, flag = '0'))]
  dat[, ChcID := sequence(rle(dat$ID)$lengths)]
  #if panel == F: treat data as cross-sectional -> give each row a unique ID
  if(set1['pan'] == 0) {
    nChcID <- nchar(max(dat$ChcID)) #specify number of leading zeros in repetition section of ID
    dat[, ID := paste0(ID, 'c', formatC(ChcID, width = nChcID, flag = '0'))]
  }
  Nind <- length(unique(dat$ID))
  Nchc <- nrow(dat)
  dat[, LikID := ID]
  #get a character vector of varnames and parameters
  varpar <- paste(mdi1$meqs, mdi1$ceqs, collapse =  ' ') #also works if ceqs does not exist (no lat class model)
  #note: use word boundary \b to find exact matches of log and exp
  varpar <- gsub("\\+.|\\*.|\\-.|\\/.|\\(.|\\).|\\^.|\\blog\\b|\\bexp\\b", ' ', varpar) #remove control characters by blanks
  varpar <- trimws(varpar) #remove leading and trailing whitespace
  while (length(grep('  ', varpar)) > 0) {varpar <- gsub('  ', ' ', varpar)} #remove multiple blanks by single blanks
  varpar <- unique(unlist(strsplit(varpar, ' ')))
  #identify model vars -> those elements in varpar which match a varname in the dataset
  vmod <- intersect(names(dat), varpar)
  vmod <- c('RowID', 'ID', 'LikID', 'ChcID', 'wgt', 'chc', avu, vmod)
  #remove unused vars from dat and replace NAs in remaining vars by mean values
  dat <- copy(dat[, vmod, with = F])
  for (vi in vmod) { # vi <- vmod[14]
    dat[is.na(eval(as.name(vi))), (vi) := dat[, mean(eval(as.name(vi)), na.rm = T)]] # table(is.na(dat))
  }
  #get random vars (if rnd = 1)
  if (set1['rnd'] == 1) {
    vrnd <- gsub(' ', '', unlist(strsplit(mdi$mval[mdi$matt == 'rnd'], ','))) #also works if rnd does not exist (no mixed model)
    Ndraws <- draws
    dat1 <<- aaRndDraws(vrnd, copy(dat), meth, Nind, Ndraws)
  } else {
    vrnd = NULL
    Ndraws <- 0
    dat1 <<- copy(dat)
  }
  mdi1$mnum <- c(Nind = Nind, Nchc = Nchc, Ndraws = Ndraws)
  ######################################################################################
  #identify parameters and get a named vector with start values (all zeros)
  prn <- setdiff(varpar, c(vmod, vrnd))
  prn <- c(prn[order(prn)], npar)
  mdi1$beta0 <- rep(0, length(prn))
  names(mdi1$beta0) <- prn
  mdi1$beta0[names(mdi1$beta0) %in% npar] <- 1 #set default of nest parameters to 1
  return(mdi1)
}

#######################################################################################################################
#add radmon variables to the dataset for mixed logit models
aaRndDraws <- function(rnd, dat, meth, Nind, Ndraws) {
  # dat <- dat1
  #define dimensions of integral
  dims <- length(rnd)
  #get draws (MLHS draws are proposed by Hess et al., 2006)
  if (meth == 'mlhs')   {draws <- as.matrix(qnorm(draw$mlhs(Ndraws, dims, Nind)))}
  if (meth == 'halton') {draws <- as.matrix(qnorm(draw$halton(Ndraws * Nind, dims)))}
  if (meth == 'unif')   {draws <- matrix(runif(Nind * Ndraws * dims), nrow = Nind * Ndraws, byrow = T)}
  colnames(draws) <- rnd
  #turn draws into data.table, add LikID for merge with dat
  draws <- data.table(draws)
  draws[, LikID := rep(unique(dat$LikID), each = Ndraws)]
  draws[, DrawID := rep(1:Ndraws, Nind)]
  ndDrw <- nchar(Ndraws) #specify number of leading zeros in draws section of LikID
  draws[, LikID := paste0(LikID, 'd', formatC(DrawID, width = ndDrw, flag = '0'))]
  draws$DrawID <- NULL
  #expand data.table to replicate each row R times (R = number of draws), add mergeID for merge with draws
  dat <- dat[rep(1:nrow(dat), each = Ndraws), ]
  dat[, DrawID := rep(1:Ndraws, nrow(dat)/Ndraws)]
  dat[, LikID := paste0(LikID, 'd', formatC(DrawID, width = ndDrw, flag = '0'))] #= ID + DrawID/100000
  #merge draws with  dat
  setkey(dat, LikID)
  setkey(draws, LikID)
  dat <- dat[draws]
  return(dat)
}

#######################################################################################################################
#estimate the model
aaRunModel <- function(hess = F) {
  start <- Sys.time() #get starting time
  message("maxLik_start ",  start)
  #estimate the model
  mtyp <- mdi1$mset['typ']
  if (mtyp == 'mnl') {fLL <- aaLogLikMnl} else if (mtyp == 'lcs') {fLL <- aaLogLikLcs} else if (mtyp == 'nst') {fLL <- aaLogLikNst}
  mdo1 <- maxLik(fLL, start = beta, fixed = mdi1$pfix, method = "BFGS", print.level = 3, iterlim = 10000)
  #replace default hessian from maxLik by newly calculated hessian -> very little difference (only if hess = 1)
  if(hess == T) {
    message("hesse_start ",  Sys.time())
    LogLik_sum <- function(beta_est) {sum(fLL(beta_est))}
    mdo1$hessian <- numDeriv::hessian(func = LogLik_sum, x = mdo1$estimate)
    message("hesse_end ",  Sys.time())
  }
  runtime <- round(difftime(Sys.time(), start, units = 'mins'), 1)
  attr(mdo1, 'mdi') <- mdi1
  attr(mdo1, 'beta') <- beta
  attr(mdo1, 'runtime') <- runtime
  mdo1 <<- mdo1
  message("runtime ",  paste(runtime, attr(runtime, 'units')))
}

########################################################################################################################
#evaluate the utility function of a multinomial logit with parameters in beta and compute likelihood
aaLogLikMnl <- function(beta, rtyp = 1) { #multinomial logit
  #write current pars to environment
  beta_i <<- beta
  #get a copy of dat1
  dati <- copy(dat1)
  #evaluate utility functions
  dati[, exv_c := 0] #exp(V) of chosen alternative
  dati[, exv_s := 0] #sum of exp(V) over all alternatives
  #loop through alternatives and compute utilities of alternatives
  for (ei in 1:length(mdi1$meqs)) {
    # ei <- 1
    ai <- mdi1$malt[ei]
    avl_i <- mdi1$mavl[ei]
    setnames(dati, avl_i, 'avl_i')
    #calculate exv_i in consideration of avl_i
    dati[, exv_i := with(as.list(beta), eval(parse(text = mdi1$meqs[ei]), dati))]
    dati[exv_i >  700,  exv_i :=  700]
    dati[exv_i < -700,  exv_i := -700]
    dati[, exv_i := avl_i * exp(exv_i)]
    setnames(dati, 'avl_i', avl_i)
    #add choice_prob_i to sum_choice_prob
    dati[, exv_c := exv_c + (chc == ai) * exv_i]
    dati[, exv_s := exv_s + exv_i]
    setnames(dati, 'exv_i', paste0('exv_', ai))
  }
  #compute weighted choice probability of chosen alternative
  dati[, prb_c := (exv_c / exv_s) ^ wgt]
  #get product of probs across choices of same individual and draws (likelihood at ID and draw level)
  #note: prod(prb) corresponds to exp(sum(log(prb))): 3*4*5 == exp(sum(log(3)+log(4)+log(5)))
  dati[, Lik := prod(prb_c), by = LikID]
  #censor Lik to exp(-700)
  dati[Lik == 0,  Lik := exp(-700)]
  #note: dati can include multiple rows per ID -> result from repeated trips per ID and/or random draws
  if (rtyp == 1) {output <- dati[, log(mean(Lik)), by = ID][[2]]} #estimation: return LogLik at ID level (1 row per ID)
  if (rtyp == 2) {output <- dati} #posterior analysis: return complete dati for further analysis
  return(output)
}
# dati[200:250, .SD, .SDcols = c(1,6, grep("avl",names(dati)), grep("exv_",names(dati)))]

########################################################################################################################
#evaluate the utility function of a latent class model with parameters in beta and compute likelihood
aaLogLikLcs <- function(beta, rtyp = 1) { #latent class
  #write current pars to environment
  beta_i <<- beta
  #get a copy of dat1
  dati <- copy(dat1)
  #loop through classes and compute utilities of alternatives by class
  for (lj in mdi1$calt) {
    # lj <- 1
    dati[, exv_c_lj := 0] #exp(V) of chosen alternative
    dati[, exv_s_lj := 0] #sum of exp(V) over all alternatives
    #loop through alternatives of class lj and compute utilities of alternatives given class_j
    for (ei in which(mdi1$cmem == lj)) {
      # ei <- 1
      ai <- mdi1$malt[ei]
      avl_i <- mdi1$mavl[ei]
      setnames(dati, avl_i, 'avl_i')
      #calculate exv_i_lj in consideration of avl_i
      dati[, exv_i_lj := with(as.list(beta), eval(parse(text = mdi1$meqs[ei]), dati))]
      dati[exv_i_lj >  700,  exv_i_lj :=  700]
      dati[exv_i_lj < -700,  exv_i_lj := -700]
      dati[, exv_i_lj := avl_i * exp(exv_i_lj)]
      setnames(dati, 'avl_i', avl_i)
      #add choice_prob_i to sum_choice_prob
      dati[, exv_c_lj := exv_c_lj + (chc == ai) * exv_i_lj]
      dati[, exv_s_lj := exv_s_lj + exv_i_lj]
      setnames(dati, 'exv_i_lj', paste0('exv_', ai, '_l', lj))
    }
    #compute weighted choice probability of chosen alternative
    dati[, prb_c_lj := (exv_c_lj / exv_s_lj) ^ wgt]
    setnames(dati, 'exv_c_lj', paste0('exv_c_l', lj))
    setnames(dati, 'exv_s_lj', paste0('exv_s_l', lj))
    setnames(dati, 'prb_c_lj', paste0('prb_c_l', lj))
  }
  #compute utilities of class membership and sum(utilities) = denominator
  dati[, exv_ls := 0]
  for (lj in mdi1$calt) {
    # lj <- 1
    ceqsi <- mdi1$ceqs[lj]
    dati[, exv_lj := with(as.list(beta), eval(parse(text = ceqsi), dati))]
    dati[exv_lj >  700,  exv_lj :=  700]
    dati[exv_lj < -700,  exv_lj := -700]
    dati[, exv_lj := exp(exv_lj)]
    dati[, exv_ls := exv_ls + exv_lj]
    setnames(dati, 'exv_lj', paste0('exv_l', lj))
  }
  #compute unconditinal likelihood at individual level as product of probabilities across choices of the individual times
  #class memership probability and take the sum accross classes: sum_j(prod_ind(P_choice | class_j) * mean(P_class_j))
  dati[, prb_c := 0]
  for (lj in mdi1$calt) {
    # lj <- 1
    setnames(dati, paste0('prb_c_l', lj), 'prb_c_lj')
    setnames(dati, paste0('exv_l', lj), 'exv_lj')
    dati[, prb_lj := exv_lj / exv_ls]
    dati[, prb_c := mean(prb_c) + prod(prb_c_lj) * mean(prb_lj), by = LikID]
    setnames(dati, 'prb_c_lj', paste0('prb_c_l', lj))
    setnames(dati, 'exv_lj', paste0('exv_l', lj))
    setnames(dati, 'prb_lj', paste0('prb_l', lj))
  }
  #note: prod(prb) by LikID was already computed above, thus take here the mean(prb) by LikID
  dati[, Lik := mean(prb_c), by = LikID]
  #censor Lik to exp(-700)
  dati[Lik == 0,  Lik := exp(-700)]
  #note: dati can include multiple rows per ID -> result from random draws
  if (rtyp == 1) {output <- dati[, log(mean(Lik)), by = ID][[2]]} #estimation: return LogLik at ID level (1 row per ID)
  if (rtyp == 2) {output <- dati} #posterior analysis: return complete dati for further analysis
  return(output)
}

########################################################################################################################
#evaluate the utility function of a nested logit with parameters in beta and compute likelihood
aaLogLikNst <- function(beta, rtyp = 1) { #nested logit
  #write current pars to environment
  beta_i <<- beta
  #get a copy of dat1
  dati <- copy(dat1)
  #loop through nests and compute lambda = 1/my_nest
  for (nj in unique(mdi1$nmem)) {
    # nj <- 1
    la_nj <- 1 / beta[mdi1$npar][nj]
    dati[, exv_s_nj := 0] #sum of exp(V) over all alternatives by nest
    #loop through alternatives of nest nj and compute utilities
    for (ei in which(mdi1$nmem == nj)) {
      # ei <- 1
      ai <- mdi1$malt[ei]
      avl_i <- mdi1$mavl[ei]
      setnames(dati, avl_i, 'avl_i')
      #calculate exv_i_nj in consideration of avl_i
      dati[, exv_i_nj := with(as.list(beta), eval(parse(text = mdi1$meqs[ei]), dati))]
      dati[, exv_i_nj := exv_i_nj / la_nj] #divide utility by nest par
      dati[exv_i_nj >  700,  exv_i_nj :=  700]
      dati[exv_i_nj < -700,  exv_i_nj := -700]
      dati[, exv_i_nj := avl_i * exp(exv_i_nj)]
      setnames(dati, 'avl_i', avl_i)
      #compute sum(exp(utility)) by nest
      dati[, exv_s_nj := exv_s_nj + exv_i_nj]
      setnames(dati, 'exv_i_nj', paste0('exv_', ai, '_n', nj))
    }
    #note: exv_s_nj = 0 causes an error in logLik (happens if all alternatives of a nest are unavailable) 
    dati[exv_s_nj < 1e-304,  exv_s_nj := 1e-304] #~exp(-700)
    setnames(dati, 'exv_s_nj', paste0('exv_s_n', nj))
  }
  #compute sum (utilities to the power of nest par) across all nests
  dati[, exv_ns := 0] #sum of exp(V) over all alternatives
  for (nj in unique(mdi1$nmem)) {
    # nj <- 1
    la_nj <- 1 / beta[mdi1$npar][nj]
    setnames(dati, paste0('exv_s_n', nj), 'exv_s_nj')
    dati[, exv_ns := exv_ns + exv_s_nj ^ la_nj]
    setnames(dati, 'exv_s_nj', paste0('exv_s_n', nj))
  }
  #compute choice probabilities of all alternatives
  dati[, prb_c := 0]
  for (nj in unique(mdi1$nmem)) {
    # nj <- 1
    la_nj <- 1 / beta[mdi1$npar][nj]
    setnames(dati, paste0('exv_s_n', nj), 'exv_s_nj')
    #loop through alternatives of nest nj and compute choice probabilities
    for (ei in which(mdi1$nmem == nj)) {
      # ei <- 1
      ai <- mdi1$malt[ei]
      setnames(dati, paste0('exv_', ai, '_n', nj), 'exv_i_nj')
      dati[, prb_i := exv_i_nj * exv_s_nj ^ (la_nj - 1) / exv_ns]
      dati[, prb_c := prb_c + (chc == ai) * prb_i]
      setnames(dati, 'prb_i', paste0('prb_', ai))
      setnames(dati, 'exv_i_nj', paste0('exv_', ai, '_n', nj))
    }
    setnames(dati, 'exv_s_nj', paste0('exv_s_n', nj))
  }
  #compute weighted choice probability of chosen alternative
  dati[, prb_c := prb_c ^ wgt]
  #get product of probs across choices of same individual and draws (likelihood at ID and draw level)
  #note: prod(prb) corresponds to exp(sum(log(prb))): 3*4*5 == exp(sum(log(3)+log(4)+log(5)))
  dati[, Lik := prod(prb_c), by = LikID]
  #censor Lik to exp(-700)
  dati[Lik == 0,  Lik := exp(-700)]
  #note: dati can include multiple rows per ID -> result from repeated trips per ID and/or random draws
  if (rtyp == 1) {output <- dati[, log(mean(Lik)), by = ID][[2]]} #estimation: return LogLik at ID level (1 row per ID)
  if (rtyp == 2) {output <- dati} #posterior analysis: return complete dati for further analysis
  return(output)
}

#######################################################################################################################
#print model output (adapted version based on Basils code)
#note: full version with aditional figures -> D:\R\Projects\p74_LogLik_Hess_Code\templates_basil\joint_est_v01\source_code.R
aaModOutput <- function(mdo1, rob = T) {
  # rob = T
  mset <- attr(mdo1, 'mdi')$mset
  mtyp <- mset['typ']
  if (mtyp == 'mnl') {fLL <- aaLogLikMnl} else if (mtyp == 'lcs') {fLL <- aaLogLikLcs} else if (mtyp == 'nst') {fLL <- aaLogLikNst}
  est <- mdo1$estimate
  varcov <- vcov(mdo1)
  meat1 <- meat(mdo1)
  bread1 <- bread(mdo1)
  meat1[is.na(meat1)] <- 0
  bread1[is.na(bread1)] <- 0
  Npars <- summary(mdo1)$NActivePar
  iterations <- mdo1$iterations
  zeroLL <- sum(fLL(attr(mdo1, 'mdi')$beta0))
  finalLL <- sum(fLL(est))
  params <- length(attr(mdo1, 'beta')) - sum(mdo1$fixed)
  rho2zero <- 1 - finalLL/zeroLL
  if (rob == T) {
    robvarcov <- sandwich(mdo1, bread1, meat1)
    se_rob <- sqrt(diag(robvarcov))
    t_rob <- est/se_rob
    coeffs <- t(rbind(est, se_rob, t_rob))
  } else {
    se_val <- sqrt(diag(varcov))
    t_val <- est/se_val
    coeffs <- t(rbind(est, se_val, t_val))
  }
  coeffs[mdo1$fixed, -1] <- NA
  if (!exists("constLL")) {constLL <- NA; rho2const <- NA} else {rho2const <- 1 - finalLL/constLL}
  #get output objects rhead, rpars
  rhead <- c('spec', 'data', 'n_indivs', 'n_choices', 'n_draws', 'n_params', 'LL_null', 'LL_con', 'LL_fin')
  rhead <- c(rhead, 'rho2_null', 'rho2_con', 'dur_min')
  rhead <- data.frame(Label = rhead, Value = rep(NA, length(rhead)), stringsAsFactors = F)
  rhead$Label[1] <- paste0(mset[1:2], collapse = '|')
  rhead$Label[1] <- paste0(rhead[1,1], '|', paste0(sapply(3:length(mset), function(x) paste0(names(mset)[x], '=', mset[x])), collapse = '|'))
  rhead$Label[2] <- paste0('data: ', attr(mdo1, 'mdi')$mdat)
  rvals <- c(NA, NA, attr(mdo1, 'mdi')$mnum, Npars, zeroLL, constLL, finalLL, rho2zero, rho2const)
  rvals <- c(rvals, as.numeric(attr(mdo1, 'runtime')))
  rhead$Value <- rvals
  rpars <- data.frame(cbind(parname = rownames(coeffs), as.data.frame(coeffs))); rownames(rpars) <- NULL
  mdr1 <- list(rhead = rhead, rpars = rpars)
  print(data.frame(Label = rhead[, 1], Value = round(rhead[, -1], 3)))
  print(data.frame(parname = rpars[, 1], round(rpars[, -1], 3)))
  return(mdr1)
}

#######################################################################################################################
#write model results to Excel
aaResToExcel <- function(xfl, dat, mod, nam = 'MNL default', rshr = F, rnam = NULL) {
  # xfl = xfl; dat = dat1; mod = mdr1; nam = 'MNL default'; rshr = F
  #calculate choice shares and name alternatives (if names are available)
  if (rshr == T) {
    ralt <- as.data.frame(round(prop.table(table(dat1$chc))*100, 2))
    names(ralt) <- c("choice", "share")
    if (length(rnam) > 0) {
      ralt$choice <- names(rnam)[match(as.numeric(labels(ralt$choice)), rnam)]
    }
  }
  #write output to Excel
  #get references
  xsh <- read.xlsx(xlsxFile = xfl, sheet = 'Viewer')
  xro <- nrow(xsh)
  xbk <- loadWorkbook(file = xfl)
  xsz <- max(nrow(mod$rhead), nrow(mod$rpars))
  #write data
  writeData(xbk, 'Viewer', c('a', rep('i', xsz+2)), startCol = 1, startRow = xro+2)
  writeData(xbk, 'Viewer', nam, startCol = 2, startRow = xro+2)
  writeData(xbk, 'Viewer', mod$rhead, startCol = 2, startRow = xro+3)
  writeData(xbk, 'Viewer', mod$rpars, startCol = 5, startRow = xro+3)
  if (rshr == T) {writeData(xbk, 'Viewer', ralt, startCol = 11, startRow = xro+3)}
  saveWorkbook(xbk, file = xfl, overwrite = T)
}

########################################################################################################################
#calculate choice probabilities from predefined parameters and utility functions
aaChoiceProbs <- function() {
  #extract meqs and beta from model attributes (model object must exist in the environment)
  smdi <- attr(mdo1, 'mdi')
  sbeta <- mdo1$estimate
  #if the model does not include latent classes -> conventional multinomial logit
  mtyp <- smdi$mset['typ']
  #identify model type (mnl or lcs; nst is not yet implemented)
  if (mtyp == 'mnl') { #multinomial logit
    dat <- aaLogLikMnl(sbeta, rtyp = 2)
    #loop through alternatives and compute choice probabilities: P_choice_i
    for (ai in unique(smdi$malt)) {
      # ai <- 1
      setnames(dat, paste0('exv_', ai), 'exv_i')
      dat[, prb_i := (exv_i / exv_s)]
      setnames(dat, 'exv_i', paste0('exv_', ai))
      setnames(dat, 'prb_i', paste0('prb_', ai))
    }
  } else if (mtyp == 'lcs') { #latent class model
    #if the model does not include latent classes -> conventional multinomial logit
    dat <- aaLogLikLcs(sbeta, rtyp = 2)
    #loop through classes and compute choice probabilities and probabilities of class membership
    for (lj in smdi$calt) {
      # lj <- 1
      #loop through alternatives and compute conditional choice probabilities: P_choice_i | class_j
      for (ai in smdi$malt[smdi$cmem == lj]) {
        # ai <- 1
        setnames(dat, paste0('exv_', ai, '_l', lj), 'exv_i_lj')
        setnames(dat, paste0('exv_s_l', lj), 'exv_s_lj')
        dat[, prb_i_lj := (exv_i_lj / exv_s_lj)] #P_choice_i|class_j
        setnames(dat, 'exv_i_lj', paste0('exv_', ai, '_l', lj))
        setnames(dat, 'exv_s_lj', paste0('exv_s_l', lj))
        setnames(dat, 'prb_i_lj', paste0('prb_', ai, '_l', lj))
      }
    }
    #compute unconditional choice probability:s sum( P_choice_i | class_j * P_class_j )
    for (ai in unique(smdi$malt)) { # ai <- 1
      dat[, prb_i := 0]
      for (lj in smdi$cmem[smdi$malt == ai]) { # lj <- 1
        setnames(dat, paste0('prb_', ai, '_l', lj), 'prb_i_lj')
        setnames(dat, paste0('prb_l', lj), 'prb_lj')
        dat[, prb_i := prb_i + prb_i_lj * prb_lj]
        setnames(dat, 'prb_i_lj', paste0('prb_', ai, '_l', lj))
        setnames(dat, 'prb_lj', paste0('prb_l', lj))
      }
      setnames(dat, 'prb_i', paste0('prb_', ai))
    }
  }
  #if dat includes random vars: get mean of choice probs per individual and choice over all draws 
  if (smdi$mset['rnd'] == 1) {
    vres <- names(dat)[-which(names(dat) %in% c('ID', 'LikID', 'ChcID'))]
    dat <- dat[, lapply(.SD, mean), by = c('ID', 'ChcID') , .SDcol = vres]
  }
  return(dat)
}

########################################################################################################################
#calculate simulation results from choice probabilities
aaGetSimResult <- function() {
  #extract meqs from model attributes (model object must exist in the environment)
  smdi <- attr(mdo1, 'mdi')
  #get template for identification of probability vars
  valts <- c('prb_[[:digit:]].*$')
  #get choice probabilities and identify model type (mnl or lcs)
  dsm0 <- aaChoiceProbs() # sum(aaLogLikLcs(beta))
  mtyp <- smdi$mset['typ']
  if (mtyp == 'lcs') {
    valts <- c(valts, paste0('prb_[[:digit:]]_l', smdi$calt)) #search strings for choice probs
    vclsp <-  names(dsm0)[grep('prb_l[[:digit:]].*$', names(dsm0))] #varnames of class membership probs
  }
  ncls <- paste0('c', c(0, smdi$calt)) #header of class-subsections in output list (c0, c1 etc.)
  #prepare result list
  rs0 <- list()
  for (li in seq_along(ncls)) {
    # li <- 1
    rs0[[li]] <- list()
    names(rs0)[[li]] <- ncls[li]
  }
  #get results for each class (class_0 = mnl)
  for (ri in seq_along(ncls)) {
    # ri <- 1
    valtp <- names(dsm0)[grep(valts[ri], names(dsm0))]
    #get aggregated results (1 row)
    rsma <- dsm0[, apply(.SD, 2, mean), .SDcols = valtp] # round(rsma, 3)
    #get individual results (1 row per observation) and add predicted mode
    rsmi <- dsm0[, c('chc', valtp), with = F] # rsmi
    rsmi[, prd_ci := max.col(rsmi[, valtp, with = F], ties.method = 'first')]
    prd_ci <- paste0('prd_', ncls[ri])
    setnames(rsmi, 'prd_ci', prd_ci)
    #get hit rate
    hitt <- table(rsmi[, chc], unlist(rsmi[, ..prd_ci])) # hitt
    hitf <- sum(hitt[colnames(hitt)[col(hitt)] == rownames(hitt)[row(hitt)]])
    hitr <- hitf / sum(hitt) # hitr
    #write results in list
    rs0[[ri]]$rsma <- rsma
    #get class membership probs (only if latent class model and only in global result (c0))
    if (mtyp == 'lcs' & ri == 1) {
      rsca <- dsm0[, apply(.SD, 2, mean), .SDcols = vclsp] # round(rsca, 3)
      rs0[[ri]]$rsca <- rsca
    }
    rs0[[ri]]$rsmi <- rsmi
    rs0[[ri]]$hitt <- hitt
    rs0[[ri]]$hitr <- hitr
  }
  return(rs0)
}

#######################################################################################################################
#compute posterior distribution of random parameters = most likely parameter value for each individual
aaPostPars <- function(dat, prnd, pnam) {
  # dat <- dat1; prnd <- prnd[1]; pnam <- attr(prnd, 'pnam')[1]
  #note: the procedure follows Train, K., 1999: Customer-Specific Taste Parameters and Mixed Logit
  #prnd is an expression which returns individual values for the random par
  #keep only 1st choice per individual and draw, because Lik is already the product of all choice probs per individual and draw
  pbeta <- mdo1$estimate
  dpi <- copy(dat[!duplicated(LikID), ])
  dpi[, Par_rnd := with(as.list(pbeta), eval(parse(text = prnd, dpi)))]
  dpi <- dpi[, .(ID, Par_rnd, Lik)]
  #weight all random pars (-> draws) with corresponding likelihood by multiplication with Lik at each draw
  dpi[, ParLik := Par_rnd * Lik]
  #divide mean(weighted_pars) by mean(Lik) -> average weighted par (corresponds to sum(weighted_pars) / sum(Lik))
  dpi[, Par_mv := mean(ParLik) / mean(Lik), by = ID]
  #compute var(weighted_pars) as squared deviation from mean(weighted_pars) at individual draws
  dpi[, ParLik_var := (Par_rnd - Par_mv)^2 * Lik]
  #aggregate to individual level
  dpa <- dpi[, lapply(.SD, mean, na.rm = TRUE), by = ID]
  dpa[, Par_sd := sqrt(ParLik_var / Lik)]
  dpa <- dpa[, .(ID, Par_mv, Par_sd)]
  setnames(dpa, c('Par_mv', 'Par_sd'), paste0(pnam, c('', '_sd')))
  return(dpa)
}

#######################################################################################################################
#draw libary
draw <- list()
#MLHS
draw$mlhs <- function(N,d,i){ #N = number of draws/ind, d = number of random vars, i = number of individuals
  #function to shuffle the elements of the vector
  shuffle <- function(inv){
    out <- inv[rank(runif(length(inv)))];
    out}
  #generate MLHS draws
  temp <- seq(0,N-1)/N;
  out <- matrix(0,N*i,d);
  j <- 1;
  k <- 1;
  while(j<i+1){
    k <- 1;
    while(k<d+1){
      out[(1+N*(j-1)):(N*j),k] <- shuffle(temp+runif(1)/N);
      k <- k+1}
    j <- j+1}
  out}

#HALTON
draw$halton <- function(n,d){ #n = number of draws/ind * number of individuals, d = number of random vars
  #function to get a halton element
  haltonelement <- function(prime,element){
    H <- 0;
    power <- (1/prime);
    while(element>0){
      digit <- (element%%prime);
      H <- H+digit*power;
      element <- element%/%prime;
      power <- power/prime}
    H}
  #function to get a halton sequence
  haltonsequence <- function(prime,lengthvec){
    i <- 1;
    out <- 0;
    while(i<lengthvec+1){
      out[i] <- haltonelement(prime,i);
      i <- i+1}
    out}
  #generate halton draws
  prime <- 2;
  out <- (haltonsequence(prime,n));
  i <- 2;
  while(i<d+1){
    k <- 0
    while(k<1){
      prime <- prime+1;
      if(sum(prime/1:prime==prime%/%1:prime)==2) k <- 1;
    }
    out <- cbind(out,haltonsequence(prime,n));
    i <- i+1}
  out}

#End###################################################################################################################
