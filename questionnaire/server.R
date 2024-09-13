#online survey tool: present an html form on https, save input data in a MySQL database and load it from there

#######################################################################################################################
#define server logic to handle input and output data

vmoe_sf <- shinyServer(func = function(input, output, session) {
  #source script with pages in shiny environment
  source("sc_pages.R", local = T, encoding = "UTF-8")
  source("sc_functions.R", local = T, encoding = "UTF-8")
  #initialise those objects which must remain within the local function environment
  isolate(vals$count <- vals$count + 1) #session counter: add 1 every time when a session opens
  Log_chk <- 0 #check login: 0 = not yet tried, 1 = failed (wrong pwd), 2 = success (correct pwd), 3 = administrator
  Mis_chk <- 0 #check missing input: 0 = no missings, > 0 = number of missing items
  Con_chk <- con0 #check for violation of consistency rules
  Bnt_chk <- list() #check if action button has been clicked (storage of latest value)
  Tme_chk <- Sys.time() #check time passed since a specified event
  dhi <- data.frame() #current household
  dph <- data.frame() #table with all persons in current household
  dpi <- data.frame() #current person
  dsi <- list() #list with designs of current person
  dsii <- data.frame() #design of current person and current experiment
  pgi <- pg0 #page handler with individual settings: commands SP experiments depending on RP answers
  pgii <- 1 #row of current page in pgi
  PgID <- 1 #ID of current page in pg0
  SP_i <- 0 #number of current SP experiment (1 to 4)
  SPi_j <- 1 #index of current choice task of current experiment
  SPi_n <- 1 #total number of choice tasks in current experiment
  SPij_chcA <- 0 #value of current choice task: 0 = no choice made, 1 = no alternative selected, >1 = specific alternative selected
  SPij_msg <- 0 #control var for warning message of current choice task: 0 = hide, 1 = view
  SPij_avl <- 1 #vector with codes of available choice options in current choice task
  
  
  ###########################################################################################
  #functions on session level
  ###########################################################################################
  observe_helpers() #required for helpers
  #allow reconnecting to new sessions (suggestion of Simona for websocket, if client is a proxy)
  session$allowReconnect(TRUE)
  
  #identify browser via java script and if browser is MS internet explorer: throw a warning
  # https://community.rstudio.com/t/determining-what-the-sessions-browser-is/6927
  # http://www.javascriptkit.com/javatutors/navigator.shtml
  observeEvent(input$usrAgt, {
    MIE_chk <- 0
    #note: MSIE is identified in the user agent string by "MSIE" (until IE 10) or "Trident" (IE 11)
    if (length(grep("MSIE", input$usrAgt)) > 0 ) {MIE_chk <- 1}
    if (length(grep("Trident", input$usrAgt)) > 0 ) {MIE_chk <- 1}
    output$MIE_chk <- renderText(MIE_chk)
    outputOptions(output, "MIE_chk", suspendWhenHidden = F)
  })

  #check if url includes the pwd as parameter and if so: pass it to the pwd input field in the ui
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['pwd']])) {updateTextInput(session, inputId = "pwd", value = query[['pwd']])}
  })
  
  ###########################################################################################
  #get next page or previous page according to page order
  HandlePages <- function(rel = 0, fix = NULL) {
    # rel = 1; fix = "P_Switch"
    #get new PgID depending on relative or fixed PgID
    if (length(fix) > 0) {PgID <- pgi$PgID[pgi$p_nam == fix]} else {PgID <- PgID + rel}
    if (rel >= 0) {dir = 1} else {dir = -1} #identify direction of change if primary target page is hided
    #check if target page is hided and if so: move in direction of dir to next page which is not hided
    while(pgi$p_view[PgID] == 0) {PgID <- PgID + dir}
    PgID <<- PgID
    pgii <<- pgi[PgID,]
    Mis_chk <<- 0 #clear check of missing input wheneven the page changes
    output$Mis_chk <- renderText(Mis_chk)
    outputOptions(output, "Mis_chk", suspendWhenHidden = F)
    #check if page is SP and if so: prepare SP settings
    if (pgii$p_nam == "P_SPi") {
      if (rel == 0) {
        #if rel == 0: stay within current experiment and get previous or next task (handled in observeEvent(input$SPi_back(/next))
        page <- getP_SPi(SP_f = SP_i)
      } else {
        #if rel <> 0: get 1st task of next experiment or last task of previous experiment
        SP_i <<- pgii$p_spi
        SPi_n <<- pgii$p_spn
        if (rel > 0) {SPi_j <<- 1}
        if (rel < 0) {SPi_j <<- SPi_n}
        page <- getP_SPi(SP_f = SP_i)
        #reset choice procedure for new experiment
        SPij_chcA <<- 0
        SPij_msg <<- 0
        output$SPij_msg <- renderText(SPij_msg)
        outputOptions(output, "SPij_msg", suspendWhenHidden = F)
      }
    }
    #check if page is VL (rating of SP attributes) and if so: prepare settings
    else if (pgii$p_nam == "P_VLi") {
      #get index of corresponding SP experiment and number of choice tasks of this experiment
      SP_i <<- pgii$p_spi
      SPi_n <<- pgi$p_spn[pgi$p_nam == "P_SPi" & pgi$p_spi == pgii$p_spi]
      page <- getP_VLi(SP_f = SP_i)
    }
    #all other pages without customised pars (no pars or default pars only): call page by name of the function
    else {
      page <- get(paste0("get", pgii$p_nam))()
    }
    #get output objects
    #ui of current page
    output$page_curr <- renderUI(
      wellPanel(value = "page_all", id = "page_body",
      #page header
      div(pgii$p_lbl, id = "page_head"),
      #if person section is reached: view name of selected person on top-right
      if (PgID >= 5) {div(dpi$pName, style = "text-align:right;padding-right:5px")},
      #page body
      page
      )
    )
  }
  
  ###########################################################################################
  #handle login
  HandleLogin <- function () { # Log_chk <- 2 to simulate success
    #p <- parent.env(environment()) # p = parent environment, notation: p$x
    #get pwd from ui and check for special user types
    pwd <- input$pwd # pwd <- "01co_1234"
    #retrieve household record from db which matches the pwd
    sqlSelHh <- paste0("SELECT * FROM HholdsTbl WHERE HholdsTbl.pwd = '", pwd, "'")
    dhf <- dbGetQuery(conn = dbc, statement = sqlSelHh)
    #check if pwd matches a HhID in the household table. if not: prompt a warning
    if (nrow(dhf) == 0) {
      Log_chk <- 1
    } else {
      #check if household is admin and if so: view session counter
      if (dhf$utyp == 1) {Log_chk <- 3} else {Log_chk <- 2}
      #write household record to shiny environment and get next page
      dhi <<- dhf
      CallPersAction()
      HandlePages(rel = 1)
    }
    
    #get output vars
    output$Log_chk <- renderText(Log_chk)
    outputOptions(output, "Log_chk", suspendWhenHidden = F)
  }
  
  ###########################################################################################
  #check selected input vars
  #note: check always saved input in data, not input objects in ui
  CheckInput <- function (dat = dpi, page) { #page = name of page
    # dat = dpf; page = "P_RP3"; dat$Time_T1 <- ""
    misNum <- 0
    misMsg <- "Bitte noch folgendes ausfüllen:<ul style='padding-left: 20px'>"
    #get check vars for current page and perform checks
    reqi <- req0[req0$vpge == page, ]
    vchk <- reqi$vnam
    #check specified vars and give warning if input is missing
    for(vchk_i in vchk) {
      # vchk_i <- vchk[1]
      if (is.na(dat[, vchk_i]) | dat[, vchk_i] == "") {
        misNum <- misNum + 1
        misMsg <- paste0(misMsg, "<li>", reqi$vmsg[reqi$vnam == vchk_i], "</li>")
      }
    }
    misMsg <- paste0(misMsg, "</ul style='padding-left:20px;'>")
    #get output: view conditional panel which indicates missing info
    Mis_chk <<- misNum #set check of missing input as number of missing items
    output$Mis_chk <- renderText(misNum)
    output$Mis_msg <- renderText(misMsg)
    outputOptions(output, "Mis_chk", suspendWhenHidden = F)
  }
  
  ###########################################################################################
  #save selected input vars from ui in R data record
  #note: ipo = vector of input objects in HTML, ipv = corresponding vector of input vars in dpi
  #person level
  SavePeToR <- function (dpf = dpi, ipo, ipv = NULL) {
    if (length(ipv) == 0) {ipv <- ipo} #if varnames are not specified they correspond to names of input objects
    #add selected inputs to dpi
    for(ipo_i in ipo){
      ipv_i <- ipv[ipo == ipo_i]
      ipo_i <- input[[ipo_i]]
      #make sure that ipo_i has a length of one to be stored in dpi
      if (length(ipo_i) == 0) {ipo_i <- NA}
      else if (length(ipo_i) > 1) {ipo_i <- paste(ipo_i, collapse = " ")}
      dpf[1, ipv_i] <- ipo_i
    }
    dpi <<- dpf
  }
  #household level
  SaveHhToR <- function (dhf = dhi, ipo, ipv = NULL) {
    if (length(ipv) == 0) {ipv <- ipo} #if varnames are not specified they correspond to names of input objects
    #add selected inputs to dhi
    for(ipo_i in ipo){
      ipv_i <- ipv[ipo == ipo_i]
      ipo_i <- input[[ipo_i]]
      #make sure that ipo_i has a length of one to be stored in dhi
      if (length(ipo_i) == 0) {ipo_i <- NA}
      else if (length(ipo_i) > 1) {ipo_i <- paste(ipo_i, collapse = " ")}
      dhf[1, ipv_i] <- ipo_i
    }
    dhi <<- dhf
  }
  
  ###########################################################################################
  #save R data record in db
  #person level
  SavePeToDb <- function (dpf = dpi) {
    #update not-NA fields fields of dpf in PersonsTbl (NAs are incorrectly saved as 0 in the db-table)
    dpcol <- names(dpf)[which(!is.na(dpf))][-c(1,2)]
    sqlUpdPe <- paste0("UPDATE PersonsTbl SET ", paste0(dpcol, " = '", dpf[, dpcol], "'", collapse = ", "), " WHERE PersonsTbl.PeID = '", dpf$PeID, "'")
    dbSendStatement(conn = dbc, statement = sqlUpdPe)
  }
  #household level
  SaveHhToDb <- function (dhf = dhi) {
    #update not-NA fields fields of dhf in HholdsTbl (NAs are incorrectly saved as 0 in the db-table)
    dhcol <- names(dhf)[which(!is.na(dhf))][-c(1,2)] #exempt HhID and user type from update
    sqlUpdHh <- paste0("UPDATE HholdsTbl SET ", paste0(dhcol, " = '", dhf[, dhcol], "'", collapse = ", "), " WHERE HholdsTbl.HhID = '", dhf$HhID, "'")
    dbSendStatement(conn = dbc, statement = sqlUpdHh)
  }

  ###########################################################################################
  #save data when browser tab is closed without "send" button
  onSessionEnded(function(dpf = dpi) {
    #session counter: substract 1 every time when a session is closed
    isolate(vals$count <- vals$count - 1)
    #save dpi on exit only if the person has started the survey but not regularly completed (PeState == 1)
  })
  
  ###########################################################################################
  #after all general functions are generated: display ui with login
  #note: the functions below apply to particular pages and are called from above functions
  HandlePages(fix = "P_Login")
  
  
  ###########################################################################################
  #P_Login:
  ###########################################################################################
  
  #page is in sc_pages.R
  
  #click start button: execute login and proceed to first page
  observeEvent(input$Login_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandleLogin()
    }
  })
  
  
  ###########################################################################################
  #P_Test
  ###########################################################################################
  
  #page for test of ui elements (should be hided during runtime)
  #page is in sc_pages.R
  
  #click next button (back button is not provided because this is the 1st page after login)
  observeEvent(input$Test_back, {
    HandlePages(rel = -1)
  })
  
  #click next button (back button is not provided because this is the 1st page after login)
  observeEvent(input$Test_next, {
    HandlePages(rel = 1)
  })
  
  
  ###########################################################################################
  #P_Switch
  ###########################################################################################
  
  #page with overview of questionnaires 
  getP_Switch <- function (dhf = dhi) {
    #add HhState and PeState as text for overview table
    dhf$stnam <- cls$cd01$nam[match(dhf$HhState, cls$cd01$val)]
    dhf$stcol <- cls$cd02$nam[match(dhf$HhState, cls$cd02$val)]
    sqlSelPe_hh <- paste0("SELECT PeID, PeHhID, pName, gender, PeState FROM PersonsTbl WHERE PersonsTbl.PeHhID = '", dhf$HhID[1], "'")
    dph <- dbGetQuery(conn = dbc, statement = sqlSelPe_hh)
    #get dataset with all persons of the hh
    dph$stnam <- cls$cd01$nam[match(dph$PeState, cls$cd01$val)]
    dph$stcol <- cls$cd02$nam[match(dph$PeState, cls$cd02$val)]
    #write dataset with all persons in the household to shiny environment
    dph <<- dph
    #reset input values of all radiobuttons to NULL to avoid erroneous allocation to other persons filled in during the same session
    inzero <- c(
      "gender", "educ", "employ", "bikeAvl", "license", "carAvl", "CsUsed", "CsFuture", "PrUsed", "PrFuture", #P_Person
      "TourYn", #P_RP1
      "Tour1Purp", "TourPeak", "Tour2Purp", "Tour1Urb", "Tour2Urb", #P_RP2
      "Stop_T1", "PubInt_T1", "Stop_T2", "PubInt_T2", #P_RP3 + P_RP4
      "item1", "item2", "item3", "item4", "item5" #P_Attitudes
      )
    lapply(inzero, function(x) session$sendCustomMessage(type = "resetValue", message = x))
    #set values for check of change of action buttons to zero (start condition)
    lapply(
      1:nrow(dph), # ri <- 1
      function(ri) {Bnt_chk[[paste0("pGetRec_", ri)]] <<- 0}
    )
    #specify cell width of split layout
    cwd <- c("34%","34%","30%")
    
    #get page with named checkboxes
    page <- (
      #main div
      div(value = "page_Switch", style = "padding:10px",
        #session counter if user is admin
        conditionalPanel(
          condition = "output['Log_chk'] == '3'",
          div(textOutput("count"), style = "color: #27408B")
        ),
        br(),
        div(HTML(dx0$txt[dx0$lbl == "intro1_Switch"]), style = "text-align: justify"), 
        #household header
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb_h", style = "color:#27408B;",
          div(id = "tb2_t", HTML("Fragebogen-Art")),
          div(id = "tb2_t", HTML("Fortschritt")),
          ""
        ),
        #household row
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb2_r1",
          div(id = "tb2_t", HTML("Haushalts-Fragebogen")),
          div(id = "tb2_t", HTML(dhf$stnam[1]), style = paste0("color:", dhf$stcol[1], ";")),
          if (dhi$HhState == 2) {""} else {
            actionButton(inputId = "qst_hhl", label = "zum Fragebogen", icon = icon("angle-right"), style = "color: #27408B;float:right")}
        ),
        br(), br(),
        div(HTML(dx0$txt[dx0$lbl == "intro2_Switch"]), style = "text-align: justify"),
        #person header (display only if there are persons in the household)
        if (nrow(dph) > 0) {
          splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb_h", style = "color:#27408B;",
            div(id = "tb2_t", HTML("Name der Person")),
            div(id = "tb2_t", HTML("Fortschritt")),
            ""
          )
        },
        if (nrow(dph) > 0) {
        #person rows: each lapply loop refers to one row = person (only if there are persons in the household)
          lapply(
            1:nrow(dph), # ri <- 1
            function(ri) {
              splitLayout(cellWidths = c("34%","34%","30%"), cellArgs = list(style='white-space: normal;'), id = "tb2_r1", #cellWidths must correspond to cwd in getP_Switch
              div(textInput(inputId = paste0("pName_", ri), label = NULL, value = dph$pName[ri])), #make Name a text field that can be changed
              div(id = "tb2_t", HTML(dph$stnam[ri]), style = paste0("color:", dph$stcol[ri], ";")),
              if (dph$PeState[ri] == 2) {""} else {
                actionButton(inputId = paste0("pGetRec_", ri), label = "zum Fragebogen", icon = icon("angle-right"), style = "color: #27408B;float:right")}
              )
            }
          )
        },
        #add new person
        actionButton(inputId = "pers_add", label = "Person hinzufügen", icon = icon("angle-right"), style = "margin:5px;color:#27408B"),
        #ask for contact data
        br(), br(),
          div(id = "tb3_t", HTML("<font size='-1'><i>Kontaktdaten für Rückfragen (optional, muss nicht angegeben werden):</i></font size='-1'>"), style = "color:#27408B"),
          div(id = "tb3_t", textInput(inputId = "hTel",  label = NULL, value = dhf$hTel, placeholder = "Telefonnummer", width = "200px"), style = "display:inline-block"),
          div(id = "tb3_t", textInput(inputId = "hMail", label = NULL, value = dhf$hMail, placeholder = "e-Mail-Adresse", width = "200px"), style = "display:inline-block"),
        br(), br(),
        actionButton(inputId ="Switch_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
        actionButton(inputId = 'Switch_close', label = "ausloggen", icon = icon("sign-out-alt"), style = "color:#27408B;float:right",
          onclick = "window.open('http://www.rali.boku.ac.at/verkehr', '_self')"),
        bsPopover(id = "Switch_close", title = "", placement = "top", trigger = "hover",
          content = "Sie können sich später wieder einloggen und weitermachen. Alle bisherigen Angaben bleiben gespeichert.")
      )
    )
    return(page)
  }
  
  ###########################################################################################
  #events of P_Switch
  
  #note:
  #(1) next button is not provided: respondents proceed by going to household or person questionnaires
  #(2) Switch_close is directly handled within the button: switch to IVe homepage
  
  #click back button
  observeEvent(input$Switch_back, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      ChngPersName()
      SaveHhToR(dhi, ipo = c("hTel", "hMail"))
      SaveHhToDb()
      HandlePages(rel = -1)
    }
  })
  
  #click button to go to household form
  observeEvent(input$qst_hhl, {
    #update HhState and HhDate of dhi
    dhi$HhState <<- 1 #in progress
    dhi$HhDate <<- as.character(Sys.Date())
    #save dpf in df and write it as as dpi to shiny environment
    SaveHhToDb(dhi)
    #get next page
    ChngPersName()
    SaveHhToR(dhi, ipo = c("hTel", "hMail"))
    HandlePages(fix = "P_Household")
  })
  
  #note: the following functions CallPersAction and ChngPersName provide the functionality for handling the continuous person list
  
  #click button to go to person questionnaire of selected person (row)
  CallPersAction <- function() {
    #note: the lapply function makes observeEvent react specifically to the selected row in the continuous form
    observeEvent(
      lapply(1:nrow(dph),
        function(ri) {
          #check if a button in the person subform has been clicked and if so: go to corresponding person questionnaire
          rx <- as.numeric(paste0(0, input[[paste0("pGetRec_", ri)]])) #paste0(0,) avoids error when input$pGetRec_i is not yet initialised
          #compare rx with Bnt_chk$pGetRec_i (which stores the latest value of input$pGetRec_i) to check if button has been clicked
          if (rx != Bnt_chk[[paste0("pGetRec_", ri)]]) {
            #update Bnt_chk$pGetRec_i to current value of input$pGetRec_i (for next check)
            Bnt_chk[[paste0("pGetRec_", ri)]] <<- rx
            #save changes and get person page
            ChngPersName()
            SaveHhToR(dhi, ipo = c("hTel", "hMail"))
            pid <- dph$PeID[ri]
            #get record of selected person
            sqlSelPe <- paste0("SELECT * FROM PersonsTbl WHERE PersonsTbl.PeID = '", pid, "'")
            dpf <- dbGetQuery(conn = dbc, statement = sqlSelPe)
            #update PeState and PeDate of dpf only if PeState is not 2
            #note: the if condition is necessary because GetPersRec() is called every time when the switchboard is opened
            if (difftime(Sys.time(), Tme_chk) > 1) {
              Tme_chk <<- Sys.time()
              if (dpf$PeState != 2) { #not yet finished: either in progress (1) or no appropriate tour (gets a 2nd chance)
                dpf$PeState <- 1 #in progress
                dpf$PeDate <- as.character(Sys.Date())
                dpf$PeQst <- 1 #online questionnaire
                #save dpf in db and write it as as dpi to shiny environment
                dpi <<- dpf
                SavePeToDb()
                #get next page
                HandlePages(fix = "P_Person") #skip household page and go to person page
                #log time of page change
                dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
                dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
              }
            }
          }
        }
      ), {} #the body of observeEvent function must exist as {} but remains empty
    )
  }
  
  #update any changes in the person names whenever the switch form is left
  ChngPersName <- function() {
    lapply(1:nrow(dph),
      function(ri) {
        rn <- input[[paste0("pName_", ri)]]
        if (length(rn) > 0) {
          pid <- dph$PeID[ri]
          #update person record in db
          SqlPeNam <- paste0("UPDATE PersonsTbl SET pName = '", rn, "' WHERE PersonsTbl.PeID = '", pid, "'")
          dbSendStatement(conn = dbc, statement = SqlPeNam)
        }
      }
    )
  }
  
  #add a new person to the household
  observeEvent(input$pers_add, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      sqlAddPe <- paste0("SELECT * FROM PersonsTbl WHERE PersonsTbl.PeID = '0'")
      dpn <- dbGetQuery(conn = dbc, statement = sqlAddPe)
      #remove entries from new record (if there are any)
      for (vi in 4:ncol(dpn)) { ifelse (is.numeric(dpn[,vi]), dpn[,vi] <- NA, dpn[,vi] <- "") }
      #add system infos
      dpn$PeHhID <- dhi$HhID
      dpn$PeState <- 1
      dpn$PeDate <- as.character(Sys.Date())
      dpn$pName <- paste0("Person ", nrow(dph)+1)
      
      #add new person to fb
      dbWriteTable(conn = dbc, name = "PersonsTbl", value = dpn[,-1], row.names = F, overwrite = F, append = T)
      ChngPersName()
      CallPersAction()
      HandlePages(fix = "P_Switch")
    }
  })

  
  ###########################################################################################
  #P_Household
  ###########################################################################################
  
  #page for household questions
  #page is in sc_pages.R
  
  ###########################################################################################
  #perform plausibility checks
  #check for zero or negative value of number of persons in household
  observeEvent(input$NumPers, {
    Con_chk["NumPers"] <<- 0
    if (!is.na(input$NumPers) & sum(0,input$NumPers, na.rm = T) <= 0) {Con_chk["NumPers"] <<- 1}
    output$NumPers_chk <- renderText(Con_chk["NumPers"])
    outputOptions(output, "NumPers_chk", suspendWhenHidden = F)
  })
  
  #check for negative value of number of children under 6
  observeEvent(input$NumUnd6, {
    Con_chk["NumUnd6"] <<- 0
    if (!is.na(input$NumUnd6) & sum(0,input$NumUnd6, na.rm = T) < 0) {Con_chk["NumUnd6"] <<- 1}
    output$NumUnd6_chk <- renderText(Con_chk["NumUnd6"])
    outputOptions(output, "NumUnd6_chk", suspendWhenHidden = F)
  })
  
  #check for negative value of number of persons between 6 and 17
  observeEvent(input$Num6To17, {
    Con_chk["Num6To17"] <<- 0
    if (!is.na(input$Num6To17) & sum(0,input$Num6To17, na.rm = T) < 0) {Con_chk["Num6To17"] <<- 1}
    output$Num6To17_chk <- renderText(Con_chk["Num6To17"])
    outputOptions(output, "Num6To17_chk", suspendWhenHidden = F)
  })
  
  #check for zero or negative value of wakling time to next public station
  observeEvent(input$StatWalk, {
    Con_chk["StatWalk"] <<- 0
    if (!is.na(input$StatWalk) & sum(0,input$StatWalk, na.rm = T) <= 0) {Con_chk["StatWalk"] <<- 1}
    output$StatWalk_chk <- renderText(Con_chk["StatWalk"])
    outputOptions(output, "StatWalk_chk", suspendWhenHidden = F)
  })
  
  #check for negative value of number of cars in the hhold
  observeEvent(input$HhCars, {
    Con_chk["HhCars"] <<- 0
    if (!is.na(input$HhCars) & sum(0,input$HhCars, na.rm = T) < 0) {Con_chk["HhCars"] <<- 1}
    output$HhCars_chk <- renderText(Con_chk["HhCars"])
    outputOptions(output, "HhCars_chk", suspendWhenHidden = F)
  })
  
  #check for negative value of number of persons with car license
  observeEvent(input$HhLicense, {
    Con_chk["HhLicense"] <<- 0
    if (!is.na(input$HhLicense) & sum(0,input$HhLicense, na.rm = T) < 0) {Con_chk["HhLicense"] <<- 1}
    output$HhLicense_chk <- renderText(Con_chk["HhLicense"])
    outputOptions(output, "HhLicense_chk", suspendWhenHidden = F)
  })
  
  #check for negative value of number of persons with carsharing membership
  observeEvent(input$HhCsMemb, {
    Con_chk["HhCsMemb"] <<- 0
    if (!is.na(input$HhCsMemb) & sum(0,input$HhCsMemb, na.rm = T) < 0) {Con_chk["HhCsMemb"] <<- 1}
    output$HhCsMemb_chk <- renderText(Con_chk["HhCsMemb"])
    outputOptions(output, "HhCsMemb_chk", suspendWhenHidden = F)
  })
  
  #click back button
  observeEvent(input$Household_back, {
    #save input in R and db
    SaveHhToR(dhi, ipo = vls$vhhl)
    SaveHhToDb()
    HandlePages(fix = "P_Switch")
  })
  #click submit button
  observeEvent(input$Household_submit, {
    SaveHhToR(dhi, ipo = vls$vhhl)
    #check required vars
    CheckInput(dat = dhi, page = "P_Household")
    if (Mis_chk == 0 & sum(Con_chk[c("NumPers", "NumUnd6", "Num6To17", "StatWalk", "HhLicense", "HhCars", "HhCsMemb")],na.rm = T) == 0) {
      confirmSweetAlert(session = session, inputId = "HhConfirm",
        text = "Wenn Sie auf OK klicken, werden die Angaben zum Haushalt gespeichert und können nicht mehr geändert werden.
          Sie kommen zurück zur Haushaltsübersicht.",
        btn_labels = c("Abbrechen", "OK")
      )
    }
  })
  
  observeEvent(input$HhConfirm, {
    if (isTRUE(input$HhConfirm)) {
      dhi$HhState <<- 2 #finished
      dhi$HhDate <<- as.character(Sys.Date())
      SaveHhToR(dhi, ipo = vls$vhhl)
      SaveHhToDb(dhi)
      HandlePages(fix = "P_Switch")
    }
  }, ignoreNULL = T)
  
  
  ###########################################################################################
  #P_Person
  ###########################################################################################
  
  #page for personal questions
  #page is in sc_pages.R
  
  #check for validity of year of birth
  #note: get warning before page change change but hide it immediately after year is valid
  observeEvent(input$birth, {
    if (!is.na(input$birth) & (sum(0,input$birth, na.rm = T) < 1930 | sum(0,input$birth, na.rm = T) > 2002)) {
      Con_chk["birth"] <<- 1
    } else {
      Con_chk["birth"] <<- 0
      output$birth_chk <- renderText(Con_chk["birth"])
      outputOptions(output, "birth_chk", suspendWhenHidden = F)
    }
  })
  
  observeEvent(input$ptCard, {
    Con_chk["ptCard"] <<- 0
    if (length(input$ptCard) > 1 & 0 %in% input$ptCard) {Con_chk["ptCard"] <<- 1}
    output$ptCard_chk <- renderText(Con_chk["ptCard"])
    outputOptions(output, "ptCard_chk", suspendWhenHidden = F)
  })
  
  #click back button
  observeEvent(input$Person_back, {
    #save input in R and db
    SavePeToR(dpi, ipo = vls$vprs)
    SavePeToDb()
    HandlePages(fix = "P_Switch") #skip household page and go back to switchboard
  })
  
  #click submit button
  observeEvent(input$Person_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      SavePeToR(dpi, ipo = vls$vprs)
      #if year of birth is invalid: view warning now
      output$birth_chk <- renderText(Con_chk["birth"])
      outputOptions(output, "birth_chk", suspendWhenHidden = F)
      #check required vars
      CheckInput(page = "P_Person")
      if (Mis_chk == 0 & sum(Con_chk[c("birth", "ptCard")], na.rm = T) == 0) {
        SavePeToDb()
        #check if sp experiments exist for this person and if so: skip rp pages; otherwise go to next page
        ds1 <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp1Tbl WHERE Sp1Tbl.SpPeID = '", dpi$PeID, "'"))
        if (nrow(ds1) == 0) {
          HandlePages(rel = 1)
        } else {
          #get number of choice tasks for SP1 to SP 4 from person table and proceed to SP1
          for (si in 1:4) { # si <- 1
            pgi$p_spn[pgi$p_spi == si & pgi$p_nam == "P_SPi"] <<- as.numeric(dpi[, paste0("SP", si, "_num")])
            #handle page viewer depending on number of choice tasks
            if (pgi$p_spn[pgi$p_spi == si & pgi$p_nam == "P_SPi"] > 0) {
              #at least one experiment is conducted: view page with experiments
              pgi$p_view[pgi$p_spi == si & pgi$p_nam == "P_SPi"] <<- 1
              #note: P_Introi and P_VLi remain at their default value in pgi: 0 = hide, 1 = view
            } else {
              #no experiments conducted: hide all three pages: P_Introi, P_SPi, P_VLi
              pgi$p_view[pgi$p_spi == si] <<- 0
            }
          }
          HandlePages(fix = "P_Intro1")
        }
        #log time of page change
        dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
        dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
      } #proceed to next page only of no missing items were detected
    }
  })
  
  
  ###########################################################################################
  #P_RP1
  ###########################################################################################
  
  #page for selection of the rp tour
  #page is in sc_pages.R
  
  #insert answer regarding availability of tour 
  observeEvent(input$TourYn, {
    dpi$PeState <<- ifelse(input$TourYn == 1, 1, 3) #1 = appropriate tour available -> in progresss, 3 = no appropriate tour
  })
  
  #click back button (remove this button at runtime - avoid going back to login page)
  observeEvent(input$RP1_back, {
    SavePeToR(dpi, ipo = vls$vrp1)
    #save person record to db because calling a person from the switchboard overrides dpi with a record from the db
    SavePeToDb()
    HandlePages(rel = -1) #skip household page and go back to switchboard
  })
  
  #click next button: get decision on sp experiments and prepare experiments
  observeEvent(input$RP1_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      #save input and check for completeness: continue only if all obligatory answers are given
      SavePeToR(dpi, ipo = vls$vrp1)
      CheckInput(page = "P_RP1")
      if (Mis_chk == 0) {
        SavePeToDb()
        #if no appropriate tour available : go back to switchboard, otherwise proceed to next page
        if (dpi$TourYn == 0) {HandlePages(fix = "P_Switch")} else {HandlePages(rel = 1)}
        #log time of page change
        dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
        dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
      }
    }
  })
  
  
  ###########################################################################################
  #P_RP2
  ###########################################################################################
  
  #page for selection of the rp tour
  #page is in sc_pages.R
  
  #click back button (remove this button at runtime - avoid going back to login page)
  observeEvent(input$RP2_back, {
    SavePeToR(dpi, ipo = vls$vrp2)
    #save person record to db because calling a person from the switchboard overrides dpi with a record from the db
    SavePeToDb()
    HandlePages(rel = -1) #skip household page and go back to switchboard
  })
  
  #click next button: get decision on sp experiments and prepare experiments
  observeEvent(input$RP2_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      #save input and check for completeness: continue only if all obligatory answers are given
      SavePeToR(dpi, ipo = vls$vrp2)
      CheckInput(page = "P_RP2")
      if (Mis_chk == 0) {
        SavePeToDb()
        HandlePages(rel = 1)
        #log time of page change
        dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
        dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
      }
    }
  })
  
  
  ###########################################################################################
  #P_RP3
  ###########################################################################################
  
  #page for description of the rp trips
  #page is in sc_pages.R
  
  ###########################################################################################
  #perform plausibility checks
  
  #handle time format of reported departure time of trip 1 and 2
  observeEvent(input$Time_T1, {
    tmt1 <- format(input$Time_T1, format="%H:%M", tz = "GMT")
    if (tmt1 == "00:00") {dpi$Time_T1 <<- ""} else {dpi$Time_T1 <<- as.character(tmt1)}
  })
  observeEvent(input$Time_T2, {
    tmt2 <- format(input$Time_T2, format="%H:%M", tz = "GMT")
    if (tmt2 == "00:00") {dpi$Time_T2 <<- ""} else {dpi$Time_T2 <<- as.character(tmt2)}
  })
  #handle time format of preferred departure time of trip 1
  observeEvent(input$Pref_T1, {
    tmt1 <- format(input$Pref_T1, format="%H:%M", tz = "GMT")
    if (tmt1 == "00:00") {dpi$Pref_T1 <<- ""} else {dpi$Pref_T1 <<- as.character(tmt1)}
  })
  observeEvent(input$Pref_T2, {
    tmt2 <- format(input$Pref_T2, format="%H:%M", tz = "GMT")
    if (tmt2 == "00:00") {dpi$Pref_T2 <<- ""} else {dpi$Pref_T2 <<- as.character(tmt2)}
  })
  #distance of trip 1 and 2: main input quantity for reference vals of sps
  observeEvent(input$Dist_T1, {
    Con_chk["Dist_T1"] <<- 0
    if (!is.na(input$Dist_T1)) {
      dist1 <- ceiling(as.numeric(gsub(",", ".", input$Dist_T1)))
      if (dist1 < 1) {Con_chk["Dist_T1"] <<- 1}
      #if Dur_T1 is not empty: check for valid ratio dist/dur: should be between 0.05 (3 kmh) and 1.67 (100 kmh)
      if (dpi$Dur_T1 != "") {
        Con_chk["Dur_T1"] <<- 0
        dur1 <- hour(input$Dur_T1)*60 + minute(input$Dur_T1) #dur in minutes
        vel1 <- input$Dist_T1 / dur1
        #if dur is valid: hide warning immediately
        if (vel1 < 0.05 | vel1 > 1.67) {Con_chk["Dur_T1"] <<- 1}
        output$Dur_T1_chk <- renderText(Con_chk["Dur_T1"])
        outputOptions(output, "Dur_T1_chk", suspendWhenHidden = F)
      }
    }
    output$Dist_T1_chk <- renderText(Con_chk["Dist_T1"])
    outputOptions(output, "Dist_T1_chk", suspendWhenHidden = F)
  })
  observeEvent(input$Dist_T2, {
    Con_chk["Dist_T2"] <<- 0
    if (!is.na(input$Dist_T2)) {
      dist2 <- ceiling(as.numeric(gsub(",", ".", input$Dist_T2)))
      if (dist2 < 1) {Con_chk["Dist_T2"] <<- 1}
      #if Dur_T2 is not empty: check for valid ratio dist/dur: should be between 0.05 (3 kmh) and 1.67 (100 kmh)
      if (dpi$Dur_T2 != "") {
        Con_chk["Dur_T2"] <<- 0
        dur2 <- hour(input$Dur_T2)*60 + minute(input$Dur_T2) #dur in minutes
        vel2 <- input$Dist_T2 / dur2
        #if dur is valid: hide warning immediately
        if (vel2 < 0.05 | vel2 > 1.67) {Con_chk["Dur_T2"] <<- 1}
        output$Dur_T2_chk <- renderText(Con_chk["Dur_T2"])
        outputOptions(output, "Dur_T2_chk", suspendWhenHidden = F)
      }
    }
    output$Dist_T2_chk <- renderText(Con_chk["Dist_T2"])
    outputOptions(output, "Dist_T2_chk", suspendWhenHidden = F)
  })
  
  #duration of trip 1 and 2: cross-validate with distance
  observeEvent(input$Dur_T1, {
    tmt1 <- format(input$Dur_T1, format="%H:%M", tz = "GMT")
    if (tmt1 == "00:00") {dpi$Dur_T1 <<- ""} else {
      #save dur in dpi
      dpi$Dur_T1 <<- as.character(tmt1)
      #if Dist_T1 is not empty: check for valid ratio dist/dur: should be between 0.05 (3 kmh) and 1.67 (100 kmh)
      if(!is.na(input$Dist_T1)) {
        Con_chk["Dur_T1"] <<- 0
        dur1 <- hour(input$Dur_T1)*60 + minute(input$Dur_T1) #dur in minutes
        vel1 <- input$Dist_T1 / dur1
        #if dur is valid: hide warning immediately
        if (vel1 < 0.05 | vel1 > 1.67) {Con_chk["Dur_T1"] <<- 1}
        output$Dur_T1_chk <- renderText(Con_chk["Dur_T1"])
        outputOptions(output, "Dur_T1_chk", suspendWhenHidden = F)
      }
    }
  })
  observeEvent(input$Dur_T2, {
    tmt2 <- format(input$Dur_T2, format="%H:%M", tz = "GMT")
    if (tmt2 == "00:00") {dpi$Dur_T2 <<- ""} else {
      #save dur in dpi
      dpi$Dur_T2 <<- as.character(tmt2)
      #if Dist_T2 is not empty: check for valid ratio dist/dur: should be between 0.05 (3 kmh) and 1.67 (100 kmh)
      if(!is.na(input$Dist_T2)) {
        Con_chk["Dur_T2"] <<- 0
        dur2 <- hour(input$Dur_T2)*60 + minute(input$Dur_T2) #dur in minutes
        vel2 <- input$Dist_T2 / dur2
        #if dur is valid: hide warning immediately
        if (vel2 < 0.05 | vel2 > 1.67) {Con_chk["Dur_T2"] <<- 1}
        output$Dur_T2_chk <- renderText(Con_chk["Dur_T2"])
        outputOptions(output, "Dur_T2_chk", suspendWhenHidden = F)
      }
    }
  })
  
  #used modes of trip 1 and 2: derive main mode and view or hide additional questions
  #note: observe is also triggered if all modes are deselected (unlike observeEvent), therefore used here
  observe({
    if (length(input$Modes_T1) == 0) {ModQst_T1 <- 0} else {
      mods1 <- c(20,21,10,30,40)[sapply(c(20,21,10,30,40), function(x) x %in% input$Modes_T1)]
      #save main mode in dpi according to priority rule: Bahn > anderer Oev > Pkw > Rad > zu Fuss
      dpi$Mode_T1 <<- mods1[1]
      #get var which controls if additional question on car and/or public are displayed
      if ((20 %in% mods1 | 21 %in% mods1) & 10 %in% mods1) {ModQst_T1 <- 3}
      else if (20 %in% mods1 | 21 %in% mods1)              {ModQst_T1 <- 2}
      else if (10 %in% mods1)                              {ModQst_T1 <- 1}
      else                                                 {ModQst_T1 <- 0}
    }
    output$ModQst_T1 <- renderText(ModQst_T1)
    outputOptions(output, "ModQst_T1", suspendWhenHidden = F)
  })
  #mode of trip 2
  observe({
    if (length(input$Modes_T2) == 0) {ModQst_T2 <- 0} else {
      mods2 <- c(20,21,10,30,40)[sapply(c(20,21,10,30,40), function(x) x %in% input$Modes_T2)]
      #save main mode in dpi according to priority rule: Bahn > anderer Oev > Pkw > Rad > zu Fuss
      dpi$Mode_T2 <<- mods2[1]
      #get var which controls if additional question on car and/or public are displayed
      if ((20 %in% mods2 | 21 %in% mods2) & 10 %in% mods2) {ModQst_T2 <- 3}
      else if (20 %in% mods2 | 21 %in% mods2)              {ModQst_T2 <- 2}
      else if (10 %in% mods2)                              {ModQst_T2 <- 1}
      else                                                 {ModQst_T2 <- 0}
    }
    output$ModQst_T2 <- renderText(ModQst_T2)
    outputOptions(output, "ModQst_T2", suspendWhenHidden = F)
  })
  
  #click back button
  observeEvent(input$RP3_back, {
    #save input in R and db
    SavePeToR(dpi, ipo = vls$vrp3)
    SavePeToDb()
    HandlePages(rel = -1)
  })
  
  #click submit button
  observeEvent(input$RP3_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      SavePeToR(dpi, ipo = vls$vrp3)
      #check required vars
      CheckInput(page = "P_RP3")
      #continue with warning or change of page
      if (Mis_chk == 0 & sum(Con_chk[c("Dist_T1")], na.rm = T) == 0) {
        #save rp-sp settings in db and proceed to sp
        SavePeToDb()
        HandlePages(rel = 1)
        #log time of page change
        dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
        dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
      } #proceed to next page only of no missing items were detected
    }
  })
  
  
  ###########################################################################################
  #P_RP4
  ###########################################################################################
  
  #note: RP4 is just an extension of RP3 with a second column for the return trip
  
  #click back button
  observeEvent(input$RP4_back, {
    #save input in R and db
    SavePeToR(dpi, ipo = vls$vrp4)
    SavePeToDb()
    HandlePages(rel = -1)
  })
  
  #click submit button
  observeEvent(input$RP4_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      SavePeToR(dpi, ipo = vls$vrp4)
      #check required vars
      CheckInput(page = "P_RP4")
      #check validity of departure time of trip 2 in relation to departure time and dur of trip 1
      Con_chk["Time_T2"] <<- 0
      tmm1 <- hour(input$Time_T1) * 60 + minute(input$Time_T1)
      tdm1 <- hour(input$Dur_T1) * 60 + minute(input$Dur_T1)
      tmm2 <- hour(input$Time_T2) * 60 + minute(input$Time_T2)
      if (tmm2 > 0 & (tmm2 - tmm1 - tdm1 <= 0)) {Con_chk["Time_T2"] <<- 1}
      output$Time_T2_chk <- renderText(Con_chk["Time_T2"])
      outputOptions(output, "Time_T2_chk", suspendWhenHidden = F)
      #continue with warning or change of page
      if (Mis_chk == 0 & sum(Con_chk[c("Dist_T1", "Dist_T2", "Time_T2")], na.rm = T) == 0) {
        #make sure that Dist_T1 and Dist_T2 are numeric for calc of designs
        dpi$Dist_T1 <<- ceiling(as.numeric(gsub(",", ".", input$Dist_T1)))
        dpi$Dist_T2 <<- ceiling(as.numeric(gsub(",", ".", input$Dist_T2)))
        #get settings for sp experiments and write output to shiny environment
        out <- getSpSettings(dpi, pgi)
        dpi <<- out$dpf
        pgi <<- out$pgf
        dsi <<- out$dsf
        #save rp-sp settings in db and proceed to sp
        SavePeToDb()
        HandlePages(rel = 1)
        #log time of page change
        dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
        dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
      } #proceed to next page only of no missing items were detected
    }
  })
  
  
  ###########################################################################################
  #P_Intro1 to P_Intro4
  ###########################################################################################
  
  #intro pages for sp experiments (each experiment has its own intro)
  #pages are in sc_pages.R
  
  #note: fast double-click on back or next button of intro pages can cause an error in the lappply function of the sp page generator
  #avoid double-clicks by waiting time of 1 second before next click is accepted
  
  #intro1
  observeEvent(input$Intro1_back, {
    #skip rp part because rp tour should not change after sp experiments have been developed
    HandlePages(fix = "P_Person")
  })
  
  observeEvent(input$Intro1_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = 1)
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    }
  })
  
  #intro2
  observeEvent(input$Intro2_back, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = -1)
    }
  })
  
  observeEvent(input$Intro2_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = 1)
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    }
  })
  
  #intro3
  observeEvent(input$Intro3_back, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = -1)
    }
  })
  
  observeEvent(input$Intro3_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = 1)
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    }
  })
  
  #intro4
  observeEvent(input$Intro4_back, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = -1)
    }
  })
  
  observeEvent(input$Intro4_next, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      HandlePages(rel = 1)
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    }
  })
  
  
  ###########################################################################################
  #P_SPi
  ###########################################################################################
  
  #page for sp choice tasks (only one page for all experiments and choice tasks)
  getP_SPi <- function (dpf = dpi, SP_f = SP_i, chcNo = 1) {
    # SP_f = 1; SPi_j <- 1; chcNo = 1
    #chcNo = kind of "no choice": 0 = none (forced choice) 1 = "no choice" as col, 2 = "no choice" as row with radiobutton
    ###########################################################################################
    #load design from db (is available from previius session) or from design plan
    #note: repeated choices within an experiment are stored in dsii (only in R but not in the db)
    #on exit of an experiment (back and forth) two actions are trigggered:
    #(1) dsii is stored to the db and (2) dsii is reset to an empty data.frame
    #the following code checks if dsii is empty which means that a new experiment starts
    if(nrow(dsii) == 0) {
      #check if a design is already stored in the db from a previous session
      dsx <- dbGetQuery(conn = dbc, statement = paste0("SELECT * FROM Sp", SP_f, "Tbl WHERE Sp", SP_f, "Tbl.SpPeID = '", dpf$PeID, "'"))
      if (nrow(dsx) > 0) {
        #if so: take the design from the db which may include choices from the previous session
        dsii <<- dsx
      } else {
        #otherwise: get new design from design plan
        dsii <<- dsi[[paste0("s", SP_f)]]
      }
    }
    
    ###########################################################################################
    #load template for screen from Excel
    sci <- sc0[sc0$sp == SP_f,]
    rX0 <- min(which(sci$nr >= 0)) #1st row with attributes taken from design plan
    rXd <- min(which(sci$nr >= 1)) #1st row with attributes displayed on the screen
    cx0 <- which(names(sci) == "na") #col with attribute names = leftmost col whose infos are displayed on the screen
    #identify rows with meta infos
    rcod <- which(sci$na == "code"); ricn <- which(sci$na == "icon"); rlbl <- which(sci$na == "label")
    rclr <- which(sci$na == "color"); rtyp <- which(sci$na == "typ"); ravl <- which(sci$na == "avl")
    
    ###########################################################################################
    #apply individual settings to the screen
    
    #settings specified in the types variable of the design
    types <- unlist(strsplit(dsii$types[SPi_j], " "))
    #specify type of public transport as "Bahn or "ÖV ohne Bahn"
    if ("pub20" %in% types) {
      for (cj in grep("PubTyp", sci[rtyp,])) { # cj <- 7
        ptyp <- gsub("PubTyp", "Bahn", sci[rlbl, cj])
        sci[c(ricn, rlbl), cj] <- c("i200_PubTrn.png",	ptyp)
      }
    }
    if ("pub21" %in% types) {
      for (cj in grep("PubTyp", sci[rtyp,])) { # cj <- 7
        ptyp <- gsub("PubTyp", "ÖV ohne Bahn", sci[rlbl, cj])
        sci[c(ricn, rlbl), cj] <- c("i201_PubLoc.png",	ptyp)
      }
    }
    
    ###########################################################################################
    #replace attribute names in sci by values from design plan
    #note: remove mismatches resulting from variants, e.g. cng1_20 and cng2_20
    for (cj in (cx0+1):ncol(sci)) {
      for (ri in rX0:nrow(sci)) {
        # cj <- 8; ri <- 5
        nij <- sci[ri,cj]
        if (length(names(dsii[SPi_j,])[names(dsii[SPi_j,]) == nij]) > 0) {
          #if match is found: take value from design only if it is not NA (set NAs to blank which makes the row remove)
          if (!is.na(dsii[SPi_j,][1, nij]) ) {sci[ri,cj] <- dsii[SPi_j,][1, nij]} else {sci[ri,cj] <- ""}
        }
      }
    }
    
    #format duration attributes with short dration (s): hh:mm (not in use)
    
    #format duration attributes with long duration (l): h std mm min
    for (ri in grep("l", sci$st)) { # ri <- 7; cj <- 8
      for (cj in (cx0+1):ncol(sci)) {
        if (sci[ri,cj] != ""){ sci[ri,cj] <- aaMin2DurLng(as.numeric(sci[ri,cj])) }
      }
    }
    
    #format cost attributes (c): "," as decimal and 2 digits after decimal (coerced to string)
    for (ri in grep("c", sci$st)) { # ri <- 7; cj <- 8
      for (cj in (cx0+1):ncol(sci)) {
        if (sci[ri,cj] != "") {sci[ri,cj] <- format(as.numeric(sci[ri,cj]), nsmall = 2, decimal.mark = ",")}
      }
    }
    
    #convert availability of seat to text (o): 0,1 -> nein, ja
    for (ri in grep("o", sci$st)) { # ri <- 7; cj <- 8
      for (cj in (cx0+1):ncol(sci)) {
        if (sci[ri,cj] != "") {sci[ri,cj] <- cls$sit$na[match(sci[ri,cj], cls$sit$cd)]}
      }
    }
    
    #remove attributes with zero values (r): set zero values to blank such that row is remved later
    for (ri in grep("r", sci$st)) { # ri <- 7; cj <- 8
      for (cj in (cx0+1):ncol(sci)) {
        if (sci[ri,cj] == "0" | sci[ri,cj] == "0,00") {sci[ri,cj] <- ""}
      }
    }

    ###########################################################################################
    #prepare sci for display: add style infos to specified rows
    #bold
    for (ri in which(sci$st %in% "b")) {
      for (ci in cx0:ncol(sci)) {if(sci[ri,ci] != "") {sci[ri,ci] <- paste0("<b>", sci[ri,ci], "</b>")}}
    }
    #indent attribute names of those attributes which are a subset of other attributes
    for (ri in grep("i", sci$st)) {
      sci[ri,cx0] <- paste0(paste0(rep("&nbsp",5), collapse = ""), sci[ri,cx0])
    }
    #save dimension info as vector for usage in viewer
    rdi <- sci$di
    #remove cols left from attribute names and empty cols on the right
    sci <- sci[, cx0:ncol(sci)]
    sci <- sci[, which(sci[rcod,] != "")]
    
    #if chcNo = 1: define "none of these alternatives" as additional column on the right
    if (chcNo == 1) {
      sci$anx <- ""
      sci$anx[c(rcod, ricn, rlbl, rclr, rtyp, ravl)] <- c(1, "", "<br/><br/>keine dieser<br/>Alternativen", "black", 0, 1)
    }
    
    #identify cols of available alternatives
    cavl <- which(sci[ravl,] == 1)
    #get codes of available alternatives and write it to parent envir for updateRadioButtons
    SPij_avl <<- as.numeric(sci[rcod, cavl])
    #identify cols and rows to be displayed: attribute names + available alternatives (cols) and attributes (rows) of these alternatives
    cView <- c(1, cavl)
    rView <- sapply(1:nrow(sci), function(x) paste0(sci[x, cavl], collapse = "") != "")
    rView <- which(rView == T); rView <- rView[rXd:length(rView)] 
    #set width of splitLayout depending on number of alternatives
    cjw <-                         c("28%","14%","14%","14%","14%","14%"); cxc <- c("30%","128%") #max = 5 alternatives
    if (length(cavl) == 4) {cjw <- c("28%","18%","18%","18%","18%");       cxc <- c("30%","166%")}
    if (length(cavl) == 3) {cjw <- c("28%","24%","24%","24%");             cxc <- c("30%","230%")}
    if (length(cavl) <= 2) {cjw <- c("28%","36%","36%");                   cxc <- c("30%","340%")}
    #alternative set with wider attibute column (may be required for comprehensive labels)
    # cjw <-                         c("37%","12%","12%","12%","12%","12%"); cxc <- c("38%","112%") #max = 5 alternatives
    # if (length(cavl) == 4) {cjw <- c("37%","15%","15%","15%","15%");       cxc <- c("38%","144%")}
    # if (length(cavl) == 3) {cjw <- c("37%","20%","20%","20%");             cxc <- c("38%","192%")}
    # if (length(cavl) <= 2) {cjw <- c("37%","30%","30%");                   cxc <- c("38%","288%")}
    #set size of mode icons
    iht <- "60px"; iwd = "70px"
    
    #get experiment viewer as dynamic div with choice tasks
    page <- (
      #main div with introcuction to choice tasks
      div(value = "page_SPi_main", style = "padding: 10px",
        # div(HTML(gsub("#SPi_n#", pgii$p_spn, dx0$txt[dx0$lbl == paste0("intro_SP", SP_i)])), style = "text-align: justify"),
        
        #get header with icons and labels: each lapply loop refers to one col: 1 col for labels + 1 col for each alternative (max = 5)
        #note: do.call is required to allocate the objects returned from lapply to individual sections (cols) of splitLayout
        #all arguments to do.call must be passed via one list; two lists can be concated by c
        do.call(
          what = splitLayout,
          args = c(
            lapply(cView, function(cj) {
              if (cj == 1) {div(paste0("Wahlsituation ", SPi_j, " von ", pgii$p_spn), id = "subpage_head")}
              else {if (sci[rcod,cj] == 1) {div(id = "rb_l1", HTML(sci[rlbl,cj]), style = paste0("color: ", sci[rclr,cj]))}
              else {div(id = "rb_l1", img(src = sci[ricn,cj], height = iht, width = iwd), br(), HTML(sci[rlbl,cj]), style = paste0("color: ", sci[rclr,cj]))}}
            }),
            list(cellWidths = cjw, cellArgs = list(style='white-space: normal;'), style = "border-bottom: 1px solid gainsboro;")
          )
        ),
        
        #get rows with attributes:
        #each outer lapply loop refers to one row = attribute (view only rows with attributes of available alternatives)
        #each inner lapply loop refers to one col = alternative (max = 5)
        lapply(rView, function(ri) { # ri <- 7
          vi <- which(rView == ri) #get a consecutive number for each row to allocate alternating background col below
          do.call(
            what = splitLayout,
            args = c(
              lapply(cView, function(cj) { # cj <- 2
                #1st col: attribute name
                if (cj == 1) {div(id = "cs_l", HTML(sci$na[ri]))} 
                #other cols: attribute values
                else {
                  #if available: add dimensions to non-empty attribute values with subordinate format
                  if (rdi[ri] != "" & sci[ri,cj] != "") {
                    splitLayout(id = "cs_v",
                      div(HTML(sci[ri,cj]), style = "text-align:right"),
                      div(HTML(paste0("<font size=2;font color=#A6A6A6>", rdi[ri], "</font size=2;font color=#A6A6A6>")), style = "text-align:left")
                    )
                  } else {div(id = "cs_v", HTML(ifelse(sci[ri,cj] == "", "&nbsp", sci[ri,cj])), style = "text-align:center")}
                }
              }),
              list(cellWidths = cjw, cellArgs = list(style='white-space: normal;'), id = ifelse(vi %% 2 == 0, "cs2_r2", "cs2_r1"))
            )
          )
        }),
        
        #get radiobuttons for choice, 
        div(id = "cs_c", splitLayout(cellWidths = cxc,
          div(id = "rb_t", HTML("Ihre Wahl:"), style = "color: #27408B; font-weight: bold"),
          div(id = "rb_b", radioButtons("SPij_chcY", label = NULL, choiceValues = SPij_avl, choiceNames = rep("", length(SPij_avl)),
            selected = dsii$chc[SPi_j], inline = T))
          ),
        #define "no alternative" as additional row with a single radiobutton
          if (chcNo == 2) {
            splitLayout(id = "cs_c0", cellWidths = c("180px","400px"), # relative: c("26%","65%")
            div(id = "rb_t", HTML("...keine dieser Alternativen"), style = "color: #27408B"),
            div(id = "rb_b", radioButtons("SPij_chcN", label = NULL, choiceValues = 1, choiceNames = "", selected = dsii$chc[SPi_j], inline = T))
          )}
        ),
        #get text fields for comments and warning if choice is missing
        div(textInput("SPij_rmk", label = NULL,
          placeholder = paste0("Anmerkung zur Wahl", if(chcNo > 0) {" (Wenn Sie 'keine dieser Alternativen' gewählt haben bitte hier begründen)"}),
          value = dsii$rmk[SPi_j], width = "100%"), style = "padding-top:5px;padding-bottom:5px"
        ),
        div(actionButton(inputId ="SPi_back", label = "zurück", icon = icon("angle-left"), style = "color: red"),
        actionButton(inputId ="SPi_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right"),
        conditionalPanel(condition = "output['SPij_msg'] == '1'",
          div(id = "warning", HTML("Bitte treffen Sie eine Wahl, bevor Sie weiter gehen.&nbsp")), style = "margin-top:6px;float:right")
        )
      )
    )
    return(page)
  }
  
  ###########################################################################################
  #events of P_SPi
  
  #click radiobutton to select a specific alternative
  #update radiobutton for "no alternative" to appear empty and save choice in environment
  #note: this event is also triggered by switching to another task (back or next) where SPij_chcY is not empty
  #this ensures that the warning message disappears when one switches to another task
  observeEvent(input$SPij_chcY, {
    updateRadioButtons(session, "SPij_chcN", "", choiceValues = 1, choiceNames = "", selected = "", inline = T)
    SPij_chcA <<- input$SPij_chcY
    SPij_msg <<- 0
    output$SPij_msg <- renderText(SPij_msg)
    outputOptions(output, "SPij_msg", suspendWhenHidden = F)
  })
  
  #click radiobutton to select no alternative
  #update radiobuttons of alternatives to appear empty and save choice = 1 in function environment
  observeEvent(input$SPij_chcN, {
    updateRadioButtons(session, "SPij_chcY", "", choiceValues = SPij_avl, choiceNames = rep("", length(SPij_avl)), selected = 1, inline = T)
    SPij_chcA <<- 1
    SPij_msg <<- 0
    output$SPij_msg <- renderText(SPij_msg)
    outputOptions(output, "SPij_msg", suspendWhenHidden = F)
  })
  
  #click back button:
  observeEvent(input$SPi_back, {
    #wait 0.5 sec to avoid error cause by double-click (dsii has 0 rows if click is too fast)
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      #check if choice was made in current session; if yes: save before going back
      if (SPij_chcA > 0)            {dsii$chc[SPi_j] <<- SPij_chcA} #coice was made in current session -> save in dpi
      if (!is.null(input$SPij_rmk)) {dsii$rmk[SPi_j] <<- input$SPij_rmk} #save remarks in corresponding var
      #if current task is not the 1st: remain with experiment and go back to previous task
      if (SPi_j > 1) {
        SPi_j <<- SPi_j - 1
        HandlePages(rel = 0)
        #if current task is the 1st:
      } else {
        #save choices in db (replace existing block)
        tbl <- paste0("Sp", SP_i, "Tbl")
        dbSendStatement(conn = dbc, statement = paste0("DELETE FROM ", tbl, " WHERE ", tbl, ".SpPeID = '", dpi$PeID, "'"))
        dbWriteTable(conn = dbc, name = tbl, value = dsii, row.names = F, overwrite = F, append = T)
        #reset dsii for previous experiment
        dsii <<- data.frame()
        #go back to previous page
        HandlePages(rel = -1)
      }
    }
  })
  
  #click next button:
  observeEvent(input$SPi_next, {
    #wait 0.5 sec to avoid error cause by double-click (dsii has 0 rows if click is too fast)
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      #check if choice exists; if yes: save and proceed to next task (if not: prompt a warning)
      if (!is.na(dsii$chc[SPi_j]) | SPij_chcA > 0) { #coice exists from previous session (saved in dsi) or from current session
        if (SPij_chcA > 0)            {dsii$chc[SPi_j] <<- SPij_chcA} #coice was made in current session -> save in dpi
        if (!is.null(input$SPij_rmk)) {dsii$rmk[SPi_j] <<- input$SPij_rmk} #save text comment
        #if current task is not the last: go to next task
        #note: check for nrow(dsii) instead of pgii$p_spn to avoid error if the design has less than pgii$p_spn rows for whatever reason
        if (SPi_j < nrow(dsii)) {
          SPi_j <<- SPi_j + 1
          HandlePages(rel = 0)
          #reset choice procedure for next task
          SPij_chcA <<- 0
          #if current task is the last:
        } else {
          #save choices in db (replace existing block)
          tbl <- paste0("Sp", SP_i, "Tbl")
          dbSendStatement(conn = dbc, statement = paste0("DELETE FROM ", tbl, " WHERE ", tbl, ".SpPeID = '", dpi$PeID, "'"))
          dbWriteTable(conn = dbc, name = tbl, value = dsii, row.names = F, overwrite = F, append = T)
          #reset dsii for next experiment
          dsii <<- data.frame()
          #go to 1st task of next experiment or to next page
          HandlePages(rel = 1)
          #log time of page change
          dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
          dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
        }
        #no choice available from previous session or from current session: prompt a warning
      } else {SPij_msg <<- 1}
      output$SPij_msg <- renderText(SPij_msg)
      outputOptions(output, "SPij_msg", suspendWhenHidden = F)
    }
  })
  
  
  ###########################################################################################
  #P_VLi
  ###########################################################################################
  
  #page for rating values of attributes (only one page for all experiments)
  getP_VLi <- function (dpf = dpi, SP_f = SP_i) {
    # dpf = dpi; SP_f = 1
    #load design plan with all choice tasks of the person
    dsii <- dsi[[paste0("s", SP_f)]]
    #load screen for allocation of varnames to attributes
    sci <- sc0[sc0$sp == SP_f,]
    ravl <- which(sci$na == "avl")
    sci <- cbind(sci[, c("nr", "na")], sci[, grep("avl_", sci[ravl,])])
    #identify and remove unavailabile altarnatives
    for (cj in grep("avl_", sci[ravl,])) { # cj <- 3
      sci[1, cj] <- max(dsii[,sci[ravl, cj]])
    }
    sci <- sci[, sci[1,] != 0]
    cavl <- grep("avl_", sci[ravl,])
    sci <- sci[sci$nr > 0,]
    sci <- sci[sapply(1:nrow(sci), function(x) paste0(sci[x, cavl], collapse = "") != ""), c("nr", "na")]
    #get named vector of atttributes for checkbox group
    SPi_vl_avl <- setNames(object = sci$nr, nm = sci$na)
    #suggest values from previous sessions as selected values (note: as.character is necessary for empty values)
    SPi_vl_sel <- unlist(strsplit(as.character(dpf[, paste0("SP", SP_f, "_val")]), " "))
    
    #get page with named checkboxes
    page <- (
      #main div with rating of attributes
      div(value = "page_VLi", style = "padding: 10px",
        div(HTML(dx0$txt[dx0$lbl == "intro_VL"]), style = "text-align: justify"), 
        div(style = "padding-left: 10px",
          checkboxGroupInput(inputId = "SPi_vl", label = NULL, choices = SPi_vl_avl, selected = SPi_vl_sel)
        ),
        br(),
        actionButton(inputId ="VLi_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
        actionButton(inputId ="VLi_next", label = "weiter", icon = icon("angle-right"), style = "color:#27408B;float:right")
      )
    )
    dpi <<- dpf
    return(page)
  }
  
  ###########################################################################################
  #events of P_VLi
  
  #click back button
  observeEvent(input$VLi_back, {
    #save rating values in dpi before the page changes
    SavePeToR(dpi, ipo = "SPi_vl", ipv = paste0("SP", SP_i, "_val"))
    SavePeToDb()
    HandlePages(rel = -1)
  })
  
  #click next button
  observeEvent(input$VLi_next, {
    #save rating values in dpi before the page changes
    SavePeToR(dpi, ipo = "SPi_vl", ipv = paste0("SP", SP_i, "_val"))
    SavePeToDb()
    HandlePages(rel = 1)
    #log time of page change
    dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
    dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
  })
  
  
  ###########################################################################################
  #P_Attitudes
  ###########################################################################################
  
  #page with attitude questions
  getP_Attitudes <- function (dpf = dpi) {
    
    #define items with corresponding question text
    itmLs <- c(
      item1 = "Einstellungsfrage 1 mit sehr langem Text der einen Zeilenumbruch erfordert",
      item2 = "Einstellungsfrage 2",
      item3 = "Einstellungsfrage 3",
      item4 = "Einstellungsfrage 4",
      item5 = "Einstellungsfrage 5"
    )
    
    #define cellWidths in splitLayout
    cxc <- c("42%","89%")
    
    #get dynamic div with attitude questions
    page <- (
      #main div with introduction to attitude questions
      div(value = "page_Attitudes_main", style = "padding: 10px",
        div(HTML("Einleitungstext zu Einstellungsfragen."), style = "text-align: justify"),
        br(),
        #subpanel with attitude questions
        wellPanel(value = "page_Attitudes_sub", id = "subpage_body",
          #get common header for all questions in top row
          splitLayout(cellWidths = c("40%","10%","10%","10%","10%","10%"), cellArgs = list(style='white-space: normal;'), id = "rb_h",
            "",
            div(id = "rb_l1", HTML("lehne voll ab")),
            div(id = "rb_l1", HTML("lehne eher ab")),
            div(id = "rb_l1", HTML("neutral")),
            div(id = "rb_l1", HTML("stimme eher zu")),
            div(id = "rb_l1", HTML("stimme voll zu"))
          ),
          #get rows with questions: each lapply loop refers to one row = question
          lapply(1:length(itmLs), function(qi) {
            splitLayout(cellWidths = cxc, cellArgs = list(style='white-space: normal;'), id = ifelse(qi %% 2 == 0, "rb2_r2", "rb2_r1"),
            div(id = "rb_t", HTML(itmLs[qi])), div(id = "rb_b",
            radioButtons(names(itmLs)[qi], label = NULL, choiceValues = 1:5, choiceNames = rep("", 5),  selected = dpf[, names(itmLs)[qi]], inline = T)))
          }),
          #if input is missing: prompt warning message indicating missing items
          conditionalPanel(
            condition = "output.Mis_chk > '0'",
            h5(id = "warning", htmlOutput("Mis_msg"))
          ),
          #add action buttons at the bottom
          br(),
          actionButton(inputId ="Attitudes_back", label = "zurück", icon = icon("angle-left"), style = "color: red"),
          actionButton(inputId ="Attitudes_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")          
        )
      )
    )
    dpi <<- dpf
    return(page)
  }
  
  ###########################################################################################
  #events of P_Attitudes
  
  #click back button
  observeEvent(input$Attitudes_back, {
    SavePeToR(dpi, ipo = c("item1", "item2", "item3", "item4", "item5"))
    SavePeToDb()
    HandlePages(rel = -1)
  })
  
  #click next button
  observeEvent(input$Attitudes_next, {
    #save input and check for completeness: continue only if all obligatory answers are given
    SavePeToR(dpi, ipo = c("item1", "item2", "item3", "item4", "item5"))
    CheckInput(page = "P_Attitudes")
    if (Mis_chk == 0) {
      SavePeToDb()
      HandlePages(rel = 1)
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    } #proceed to next page only of no missing items were detected
  })
  
  
  ###########################################################################################
  #P_Farewell
  ###########################################################################################
  
  #last page with option to write remarks
  #page is in sc_pages.R
  
  #click back button
  observeEvent(input$Farewell_back, {
    if (difftime(Sys.time(), Tme_chk) > 1) {
      Tme_chk <<- Sys.time()
      SavePeToR(dpi, ipo = c("remark"))
      SavePeToDb()
      HandlePages(rel = -1)
    }
  })
  
  #click next button
  observeEvent(input$Farewell_submit, {
    confirmSweetAlert(session = session, inputId = "PeConfirm",
      text = "Wenn Sie auf OK klicken, werden die Angaben zu dieser Person gespeichert und können nicht mehr geändert werden.
        Die anderen Personen können weiterhin ausfüllen.",
      btn_labels = c("Abbrechen", "OK")
    )
  })
  
  observeEvent(input$PeConfirm, {
    if (isTRUE(input$PeConfirm)) {
      dpi$PeState <<- 2
      dpi$PeDate <<- as.character(Sys.Date())
      SavePeToR(dpi, ipo = c("remark"))
      SavePeToDb()
      HandlePages(fix = "P_Switch")
      #log time of page change
      dlg <- data.frame(LgPeID = dpi$PeID, page = ifelse(is.na(pgii$p_spi), pgii$p_nam, paste0(pgii$p_nam, "_", pgii$p_spi)), time = Sys.time())
      dbWriteTable(conn = dbc, name = "LogTbl", value = dlg, row.names = F, overwrite = F, append = T)
    }
  }, ignoreNULL = T)
  
  
  ###########################################################################################
  #get general output vars
  ###########################################################################################
  
  #make hidden output vars available for the ui
  #session counter: reactively update the client
  output$count <- renderText(paste0("Number of current sessions: ", vals$count))
  
})

#End###################################################################################################################
