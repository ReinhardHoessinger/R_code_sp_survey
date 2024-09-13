#May 2019 Reinhard Hoessinger
#page functions which require no costomised pars (no or only defaults pars) -> can be generated outside of the server function

#######################################################################################################################
#P_Login: login page
getP_Login <- function () {
  
  #default pwd: (1) if user is already logged in an goes back to login page, (2) if shiny is local, (3) otherwise NULL
  if (nrow(dhi) > 0) {dpwd <- dhi$pwd} else if (substring(getwd(),1,3) == "D:/") {dpwd <- "01co_1234"} else {dpwd <- NULL}
  
  page <- (
    div(value = "page_Login", style = "padding: 10px",
      br(),
      #if browser is not MSIE: show intro and login
      conditionalPanel(
        condition = "output['MIE_chk'] == '0'",
        div(HTML(dx0$txt[dx0$lbl == "intro_Login"]), style = "text-align: justify"),
        br(),
        div(id = "tb_h", "Geben Sie Ihren Haushalts-Code ein:", style = "color:#27408B;"),
        div(id = "tb3_r1", style = "padding-top:10px;padding-bottom:10px",
          div(textInput(inputId = "pwd", label = NULL, placeholder = "Ihr Code", value = dpwd, width = 250),
            style = "display:inline-block"),
          actionButton(inputId ="Login_next", label = "zur Befragung", icon = icon("sign-in-alt"), style = "color: #27408B"),
          br(),
          conditionalPanel(
            condition = "output['Log_chk'] == '1'", style = "display:inline-block",
            div(id = "warning", "Haushalts-Code falsch - bitte nochmals versuchen.")
          )
        )
      ),
      #if browser is MSIE: show warning and instruction how to participate
      conditionalPanel(
        condition = "output['MIE_chk'] == '1'",
        div(HTML(dx0$txt[dx0$lbl == "MSIE_Login"]), style = "text-align: justify")
      ),
      #links to further information
      br(),
      div(a("Empfehlungsschreiben der Auftraggeber", href = "BOKU-Empfehlungsschreiben.pdf", target = "_blank")),
      div(a("Erklärung zum Datenschutz", href = "BOKU-Datenschutz.pdf", target = "_blank")),
      br(),
      #contact
      splitLayout(cellWidths = c("20%", "30%", "30%", "18%"), cellArgs = list(style='white-space: normal;'),
        div(HTML("Durchführung und Kontakt:"), style = "color:#27408B"),
        div(HTML("Universität für Bodenkultur Wien</br>Institut für Verkehrswesen</br>A-1190 Wien, Peter-Jordan-Straße 82")),
        div(HTML("Florian Aschauer (Erhebungsleiter)</br>Tel.: 0664 110 3642</br>erhebung@boku.ac.at")),
        div(img(src = "img_BOKU.png", width = "70px"), style = "text-align:right")
      ),
      br(),
      #client
      div(HTML("Auftraggeber:"), style = "color:#27408B"),
      splitLayout(cellWidths = c("26%","20%","28%","24%"), cellArgs = list(style='white-space: normal;'),
        div(img(src = "img_BMVIT.png", width = "200px"), style = "text-align:left;overflow-x:hidden;"),
        div(img(src = "img_ASFINAG.png", width = "150px"), style = "text-align:center;overflow-x:hidden;"),
        div(img(src = "img_OEBB.png", width = "100px"), style = "text-align:center;overflow-x:hidden;"),
        div(img(src = "img_VMOE.png", width = "200px"), style = "text-align:right;overflow-x:hidden;")
      )
    )
  )
  return(page)
}

#######################################################################################################################
#P_Test: page with tests of help functions
getP_Test <- function () {
  page <- (
    div(value = "page_Test", style = "padding: 10px",
      br(),
      div("Filterfrage mit Subfrage:"),
      #Template for filter question
      textInput("testA", "Filterfrage: Subfrage erscheint bei Antwort 'a':", "", width = 300),
      conditionalPanel(
        condition = "input.testA == 'a'",
        textInput("testB", "Subfrage kommt hier hin:", "", width = 300)
      ),
      br(),
      div("Label und Textfeld in einer Zeile:"),
      "Label: ",
      div(textInput("test", label = NULL, placeholder = "Textfeld", width = 300), style = "display:inline-block"),
      br(),
      div("Auf andere website verlinken:"),
      #Template for hyperlink
      #note: target = "_blank" opens new browser tab
      helpText(a("IVe homepage in separatem Tab öffnen", href = "http://www.rali.boku.ac.at/verkehr", target = "_blank")),
      br(),
      div("Verschiedene Arten von Hilfsinformationen:"),
      br(),
      #Template for helper: click an icon to get help info
      #note: if type = "markdown" content = name of your markdown file (e.g. ClickHelp.md)
      helper(div("Helper: Hilfstext erscheint beim Anklicken als Layer", width = 300),
       icon = "info", colour = "#27408B", title = "Ihr Erhebungsteam:", content = "Hier kommt der Hlfetext hin.", type = "inline", size = "m"
      ),
      br(),
      
      #Template for popover to text: hovver over the text to get help info
      #note: bsTooltip works similar as bsPopover but has no content argument
      div("Tooltip: Hilfstext erscheint, wenn die Maus über Symbol streicht", width = 300, style = "display:inline-block"),
      bsButton("tt01", label = "", icon = icon("info"), style = "info", size = "extra-small"),
      bsPopover(id = "tt01", title = "Ihr Erhebungsteam:", content = "Hier kommt der Hlfetext hin.", placement = "top", trigger = "hover"),
      br(),br(),
      
      div("Verschiedene Arten von Zeitangaben:"),
      timeInput("Test_time_1", "Zeitangabe als timeInput", seconds = F),
      "Zeitangabe als Text (hh:mm):",
      div(textInput("Test_time_2", NULL, width = 60), style = "display:inline-block"),
      br(),
      
      actionButton(inputId = "Test_back", label = "zurück", icon = icon("angle-left"), style = "color: red"),
      #Template for popover to action butto: hover over the button to get help info
      actionButton(inputId = "Test_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right"),
      bsPopover(id = "Test_next", title = "", content = "weiter zur Befragung", placement = "right", trigger = "hover")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Household: household characteristics
getP_Household <- function (dhf = dhi) {
  #organise page as splitlayout with 2 cols (labels + values)
  cwd <- c("50%", "48%")
  #prepare default input for multigroup checkbox
  StatMeans_sel <- unlist(strsplit(as.character(dhf$StatMeans), " "))
  
  page <- (
    div(value = "page_Household", style = "padding: 10px",
      div(HTML("<b>Bitte beantworten Sie folgende Fragen für Ihren Haushalt:</b>"), style = "text-align: justify"), 
      br(),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
        div(id = "tb3_t", HTML("Wie viele Personen leben ständig in Ihrem Haushalt? Sie selbst eingeschlossen")),
        div(
          div(id = "tb3_t", numericInput(inputId = "NumPers", label = HTML("Personen insgesamt"), min = 1, value = dhf$NumPers, width = "120px"), style = "display:inline-block"),
          div(id = "tb3_t", numericInput(inputId = "NumUnd6", label = HTML("davon Kinder unter 6 Jahren"), min = 0, value = dhf$NumUnd6, width = "120px"), style = "display:inline-block"),
          div(id = "tb3_t", numericInput(inputId = "Num6To17", label = HTML("davon 6 bis 17 Jahre"), min = 0, value = dhf$Num6To17, width = "120px"), style = "display:inline-block"),
          conditionalPanel(condition = "output.NumPers_chk > '0'", h5(id = "warning", "Anzahl der Personen muss größer als Null sein")),
          conditionalPanel(condition = "output.NumUnd6_chk > '0'", h5(id = "warning", "Anzahl der Kinder unter 6 kann nicht negativ sein")),
          conditionalPanel(condition = "output.Num6To17_chk > '0'", h5(id = "warning", "Anzahl der Personen zwischen 6 und 17 kann nicht negativ sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Wie weit ist die nächste Haltestelle der öffentlichen Verkehrsmittel (ÖV) zu Fuß entfernt? (Minuten)")),
        div(
          div(id = "tb3_t", numericInput(inputId = "StatWalk", label = NULL, min = 1, value = dhf$StatWalk, width = "120px")),
          conditionalPanel(condition = "output.StatWalk_chk > '0'", h5(id = "warning", "Anzahl der Gehminuten muss größer als Null sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Welche Verkehrsmittel halten an dieser Haltestelle? (Mehrfachnennungen möglich)")),
        div(id = "tb3_t", checkboxGroupInput(inputId = "StatMeans", label = NULL, choices = cls$cd09, selected = StatMeans_sel))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Anzahl der Pkws inkl. Dienstwagen in Ihrem Haushalt")),
        div(
          div(id = "tb3_t", numericInput(inputId = "HhCars", label = NULL, min = 0, value = dhf$HhCars, width = "120px")),
          conditionalPanel(condition = "output.HhCars_chk > '0'", h5(id = "warning", "Anzahl der Dienstwagen kann nicht negativ sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Anzahl der Personen mit Pkw-Führerschein")),
        div(
          div(id = "tb3_t", numericInput(inputId = "HhLicense", label = NULL, min = 0, value = dhf$HhLicense, width = "120px")),
          conditionalPanel(condition = "output.HhLicense_chk > '0'", h5(id = "warning", "Anzahl der Dienstwagen kann nicht negativ sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Anzahl der Personen mit Carsharing-Mitgliedschaft")),
        div(
          div(id = "tb3_t", numericInput(inputId = "HhCsMemb", label = NULL, min = 0, value = dhf$HhCsMemb, width = "120px")),
          conditionalPanel(condition = "output.HhCsMemb_chk > '0'", h5(id = "warning", "Anzahl der Mitgliedschaften kann nicht negativ sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Wie häufig hat Ihr Haushalt in den letzten 3 Monaten Lieferdienste (z.B. Pakete, Essen) in Anspruch genommen?")),
        div(id = "tb3_t", radioButtons(inputId = "HhDeliver", label = NULL, choices = cls$cd10, selected = dhf$HhDeliver))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Wie häufig hat Ihr Haushalt in den letzten 3 Monaten Handwerker oder Dienstleister in Anspruch genommen?")),
        div(id = "tb3_t", radioButtons(inputId = "HhService", label = NULL, choices = cls$cd10, selected = dhf$HhService))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Wie hoch ist das monatliche Nettoeinkommen ihres Haushalts?")),
        div(id = "tb3_t", radioButtons(inputId = "HhIncome", label = NULL, choices = cls$cd11, selected = dhf$HhIncome))
      ), 
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="Household_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Household_submit", label = "fertig", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Person: person characteristics
getP_Person <- function (dpf = dpi) {
  #organise page as splitlayout with 2 cols (labels + values)
  cwd <- c("50%", "48%")
  cwd3 <- c("13%", "37%", "48%")
  #prepare default input for multigroup checkbox
  ptCard_sel <- unlist(strsplit(as.character(dpf$ptCard), " "))
  
  page <- (
    div(value = "page_Person", style = "padding: 10px",
      div(HTML("<b>Bitte beantworten Sie folgende Fragen zu Ihrer Person:</b>"), style = "text-align: justify"), 
      br(),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
        div(id = "tb3_t", HTML("Geburtsjahr")),
        div(
          div(id = "tb3_t", numericInput(inputId = "birth", label = NULL, min = 1930, value = dpf$birth, width = "150px")),
          conditionalPanel(condition = "output.birth_chk > '0'", h5(id = "warning", "Das Geburtsjahr muss zwischen 1930 und 2002 liegen."))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Geschlecht")),
        div(id = "tb3_t", radioButtons(inputId = "gender", label = NULL, choices = cls$cd12, selected = dpf$gender))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Höchster Schulabschluss")),
        div(id = "tb3_t", radioButtons(inputId = "educ", label = NULL, choices = cls$cd13, selected = dpf$educ))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Art der Beschäftigung")),
        div(id = "tb3_t", radioButtons(inputId = "employ", label = NULL, choices = cls$cd14, selected = dpf$employ))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Ist ein Fahrrad für Sie verfügbar?")),
        div(id = "tb3_t", radioButtons(inputId = "bikeAvl", label = NULL, choices = cls$cd03, selected = dpf$bikeAvl))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Besitzen Sie einen Führerschein für Pkw?")),
        div(id = "tb3_t", radioButtons(inputId = "license", label = NULL, choices = cls$cd03, selected = dpf$license))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Ist ein Pkw für Sie verfügbar?")),
        div(id = "tb3_t", radioButtons(inputId = "carAvl", label = NULL, choices = cls$cd15, selected = dpf$carAvl))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Welche Zeit- oder Ermäßigungskarten für öffentliche Verkehrsmittel besitzen Sie? (Mehrfachnennungen möglich)")),
        div(
          div(id = "tb3_t", checkboxGroupInput(inputId = "ptCard", label = NULL, choices = cls$cd16, selected = ptCard_sel)),
          conditionalPanel(condition = "output.ptCard_chk > '0'", h5(id = "warning", "Bitte die Antwort auf Konsistenz prüfen."))
        )
      ),
      
      #questions about carsharing and park & ride
      div(id = "tb3_t", HTML("</br><font color=#27408B>Inwiefern sind folgende Verkehrsangebote für Sie von Bedeutung?</font color=#27408B>")),
      #row with headers
      splitLayout(cellWidths = cwd3, cellArgs = list(style='white-space: normal;'), id = "tb_h", style = "color:#27408B;",
        "",
        div(id = "tb3_t", HTML("Habe Ich in der Vergangenheit schon genutzt:")),
        div(id = "tb3_t", HTML("Kann ich mir vorstellen, in Zukunft zu nutzen:"))
      ),
      splitLayout(cellWidths = cwd3, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Car Sharing")),
        div(id = "tb3_t", radioButtons(inputId = "CsUsed", label = NULL, choices = cls$cd17, selected = dpf$CsUsed)),
        div(id = "tb3_t", radioButtons(inputId = "CsFuture", label = NULL, choices = cls$cd18, selected = dpf$CsFuture))
      ),
      splitLayout(cellWidths = cwd3, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Park & Ride")),
        div(id = "tb3_t", radioButtons(inputId = "PrUsed", label = NULL, choices = cls$cd17, selected = dpf$PrUsed)),
        div(id = "tb3_t", radioButtons(inputId = "PrFuture", label = NULL, choices = cls$cd18, selected = dpf$PrFuture))
      ),
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="Person_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Person_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_RP1: selection of rp tour
getP_RP1 <- function (dpf = dpi) {
  #organise questions at the bottom as splitlayout with 2 cols (labels + values)
  cwd <- c("50%", "48%")
  
  page <- (
    div(value = "page_RP1", style = "padding: 10px",
      #introduction of tour
      div(HTML(dx0$txt[dx0$lbl == "intro1_RP1"]), style = "text-align:justify"),
      div(id = "rb_l1", img(src = "img_Tour.png", height = 300)),
      div(HTML(dx0$txt[dx0$lbl == "intro2_RP1"]), style = "text-align:justify"),
      #selection of tour
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
        div(id = "tb3_t", HTML("Denken sie bitte an den letzten Monat: Haben sie eine Tour unternommen, die den genannten Kriterien entspricht?")),
        div(id = "tb3_t", radioButtons("TourYn", label = NULL, choices = cls$cd03, selected = dpf$TourYn))
      ),
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="RP1_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="RP1_next", label = "weiter", icon = icon("angle-right"), style = "color:#27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_RP2: selection of rp tour
getP_RP2 <- function (dpf = dpi) {
  #organise questions at the bottom as splitlayout with 2 cols (labels + values)
  cwd <- c("50%", "48%")
  
  page <- (
    div(value = "page_RP2", style = "padding: 10px",
      #introduction of tour
      div(HTML(dx0$txt[dx0$lbl == "intro1_RP2"]), style = "text-align:justify"),
      #description of tour
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
        div(id = "tb3_t", HTML("Was war der Hauptzweck dieser Tour?")),
        div(id = "tb3_t", radioButtons("Tour2Purp", label = NULL, choices = cls$cd04, selected = dpf$Tour2Purp))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Wurde ein Weg dieser Tour (Hin- oder Rückweg) während der Hauptverkehrszeit zurückgelegt?")),
        div(id = "tb3_t", radioButtons("TourPeak", label = NULL, choices = cls$cd03, selected = dpf$TourPeak))
      ),
      
      #tour description (header is not required)
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Wo war der Startpunkt der Tour?")),
        div(id = "tb3_t", radioButtons(inputId = "Tour1Purp", label = NULL, choices = cls$cd05, selected = dpf$Tour1Purp))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Liegt der Startpunkt innerhalb einer der folgenden Städte: Wien, Graz, Linz, Salzburg, Innsbruck, Klagenfurt")),
        div(id = "tb3_t", radioButtons(inputId = "Tour1Urb", label = NULL, choices = cls$cd03, selected = dpf$Tour1Urb))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Liegt das Hauptziel innerhalb einer dieser Städte?</br>
          <font size='-1'>(das Hauptziel ist dort, wo der Hauptzweck ausgeübt wurde)</font size='-1'>")),
        div(id = "tb3_t", radioButtons(inputId = "Tour2Urb", label = NULL, choices = cls$cd03, selected = dpf$Tour2Urb))
      ),
      
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="RP2_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="RP2_next", label = "weiter", icon = icon("angle-right"), style = "color:#27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_RP3: description rp trips
getP_RP3 <- function (dpf = dpi) {
  #organise page as splitlayout with 3cols (labels + trip 1 + trip 2)
  
  #coerce text vars from db to POSIXct for display in timeInput field
  t1dep <- as.POSIXct(dpf$Time_T1, format="%H:%M", tz = "GMT")
  t1prf <- as.POSIXct(dpf$Pref_T1, format="%H:%M", tz = "GMT")
  t1dur <- as.POSIXct(dpf$Dur_T1, format="%H:%M", tz = "GMT")
  #get multiselection of modes
  mod1_sel <- unlist(strsplit(as.character(dpf$Modes_T1), " "))
  cwd <- c("48%", "25%", "25%")
  
  page <- (
    div(value = "page_RP3", style = "padding: 10px",
      div(HTML(dx0$txt[dx0$lbl == "intro1_RP3"]), style = "text-align: justify"), 
      
      #header of trip description
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb_h", style = "color:#27408B;",
        div(id = "tb3_t", ""),
        div(id = "tb3_t", HTML("<b>Hinweg:</b>")),
        ""
      ),
      
      #trip description
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Abfahrtszeit (Uhrzeit)")),
        div(id = "tb3_t", timeInput(inputId = "Time_T1", label = NULL, seconds = F, minute.steps = 5, value = t1dep)),
        ""
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Was wäre Ihre bevorzugte Abfahrtszeit, wenn es keinen Stau und keine Überlastung im öffentlichen Verkehr gäbe?")),
        div(id = "tb3_t", timeInput(inputId = "Pref_T1", label = NULL, seconds = F, minute.steps = 5, value = t1prf)),
        ""
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Gab es auf dem Weg einen Zwischenstopp?")),
        div(id = "tb3_t", radioButtons(inputId = "Stop_T1", label = NULL, choices = cls$cd03, selected = dpf$Stop_T1)),
        ""
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Distanz des Weges (auf ganze Kilometer runden)")),
        div(
          div(id = "tb3_t", numericInput(inputId = "Dist_T1", label = NULL, min = 1, value = dpf$Dist_T1, width = "125px")),
          conditionalPanel(condition = "output.Dist_T1_chk > '0'", h5(id = "warning", "Distanz muss mindestens 1 km sein"))
        ),
        ""
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Fahrtdauer in Stunden und Minuten</br><font size='-1'>Bitte schätzen Sie die reine Fahrzeit ohne Zwischenaufenthalt, 
                               aber mit Verspätungen im ÖV sowie Stau und Parkplatzsuche.</font size='-1'>")),
        div(
          div(id = "tb3_t", timeInput(inputId = "Dur_T1", label = HTML("&nbspStunden | Minuten"), seconds = F, minute.steps = 1, value = t1dur)),
          conditionalPanel(condition = "output.Dur_T1_chk > '0'", h5(id = "warning", "Bitte Distanz und Dauer prüfen!"))
        ),
        ""
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Verwendete Verkehrsmittel auf dem Weg</br>
         <font size='-1'>Bitte geben Sie alle verwendeten Verkehrsmittel an.</font size='-1'>")),
        div(id = "tb3_t", checkboxGroupInput(inputId = "Modes_T1", label = NULL, choices = cls$cd07, selected = mod1_sel)),
        ""
      ),
      #conditional questions if car was used
      conditionalPanel(
        condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
        div(id = "tb3_t", HTML("<font color=#27408B><b>Zusatzfragen zum Pkw:</b></font color=#27408B>")),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
          div(id = "tb3_t", HTML("Zeitverlust durch Stau (Minuten):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(numericInput(inputId = "CarCong_T1", label = NULL, min = 0, value = dpf$CarCong_T1, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarCongNa_T1", label = NULL, choices = c(keiner = 95), selected = dpf$CarCongNa_T1), style = "display:inline-block"))
          ),
          ""
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
          div(id = "tb3_t", HTML("Geschätzte Dauer für Parkplatzsuche (Minuten):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(numericInput(inputId = "CarPark_T1", label = NULL, min = 0, value = dpf$CarPark_T1, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarParkNa_T1", label = NULL, choices = cls$cd95, selected = dpf$CarParkNa_T1), style = "display:inline-block"))
          ),
          ""
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
          div(id = "tb3_t", HTML("Geschätzte Fahrtkosten inkl. Maut- und Parkkosten (Euro):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(textInput(inputId = "CarCost_T1", label = NULL, placeholder = "Euro", value = dpf$CarCost_T1, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarCostNa_T1", label = NULL, choices = cls$cd97, selected = dpf$CarCostNa_T1), style = "display:inline-block"))
          ),
          ""
        )
      ),
      #conditional questions if public was used
      conditionalPanel(
        condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
        div(id = "tb3_t", HTML("<font color=#27408B><b>Zusatzfragen zum öffentlichen Verkehrsmittel:</b></font color=#27408B>")),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
          div(id = "tb3_t", HTML("Bedienungshäufigkeit der ÖV-Verbindung")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", radioButtons(inputId = "PubInt_T1", label = NULL, choices = cls$cd06, selected = dpf$PubInt_T1))
          ),
          ""
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
          div(id = "tb3_t", HTML("Haben Sie die Fahrt für aktive Nebenaktivitäten genutzt? (nicht nur Musik hören) Wenn Ja: bitte beschreiben")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", textInput(inputId = "PubSecDsc_T1", label = NULL, value = dpf$PubSecDsc_T1, width = "200px"))
          ),
          ""
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
          div(id = "tb3_t", HTML("Geschätzte Fahrtkosten für eine einfache Fahrt (Euro)")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(textInput(inputId = "PubCost_T1", label = NULL, placeholder = "Euro", value = dpf$PubCost_T1, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "PubCostNa_T1", label = NULL, choices = cls$cd97, selected = dpf$PubCostNa_T1), style = "display:inline-block"))
          ),
          ""
        )
      ),
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="RP3_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="RP3_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_RP4: description rp trips
getP_RP4 <- function (dpf = dpi) {
  #organise page as splitlayout with 3cols (labels + trip 1 + trip 2)
  
  #coerce text vars from db to POSIXct for display in timeInput field
  t1dep <- as.POSIXct(dpf$Time_T1, format="%H:%M", tz = "GMT")
  t2dep <- as.POSIXct(dpf$Time_T2, format="%H:%M", tz = "GMT")
  t1prf <- as.POSIXct(dpf$Pref_T1, format="%H:%M", tz = "GMT")
  t2prf <- as.POSIXct(dpf$Pref_T2, format="%H:%M", tz = "GMT")
  t1dur <- as.POSIXct(dpf$Dur_T1, format="%H:%M", tz = "GMT")
  t2dur <- as.POSIXct(dpf$Dur_T2, format="%H:%M", tz = "GMT")
  #get multiselection of modes
  mod1_sel <- unlist(strsplit(as.character(dpf$Modes_T1), " "))
  mod2_sel <- unlist(strsplit(as.character(dpf$Modes_T2), " "))

  cwd <- c("48%", "25%", "25%")
  
  page <- (
    div(value = "page_RP4", style = "padding: 10px",
      div(HTML(dx0$txt[dx0$lbl == "intro1_RP4"]), style = "text-align: justify"), 

      #header of trip description
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb_h", style = "color:#27408B;",
        div(id = "tb3_t", ""),
        div(id = "tb3_t", HTML("<b>Hinweg:</b>")),
        div(id = "tb3_t", HTML("<b>Rückweg:</b>"))
      ),
      
      #trip description
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Abfahrtszeit (Uhrzeit)")),
        div(id = "tb3_t", disabled(timeInput(inputId = "Time_T1", label = NULL, seconds = F, minute.steps = 5, value = t1dep))),
        div(id = "tb3_t", timeInput(inputId = "Time_T2", label = NULL, seconds = F, minute.steps = 5, value = t2dep))
      ),
      #check departure time of trip 2
      conditionalPanel(
        condition = "output.Time_T2_chk > '0'",
        splitLayout(cellWidths = c("33%", "65%"), cellArgs = list(style='white-space: normal;'),
          "",
          h5(id = "warning", HTML("Abfahrtszeit des Rückweges muss später sein als Abfahrtszeit und Fahrtdauer des Hinweges"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Was wäre Ihre bevorzugte Abfahrtszeit, wenn es keinen Stau und keine Überlastung im öffentlichen Verkehr gäbe?")),
        div(id = "tb3_t", disabled(timeInput(inputId = "Pref_T1", label = NULL, seconds = F, minute.steps = 5, value = t1prf))),
        div(id = "tb3_t", timeInput(inputId = "Pref_T2", label = NULL, seconds = F, minute.steps = 5, value = t2prf))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Gab es auf dem Weg einen Zwischenstopp?")),
        div(id = "tb3_t", disabled(radioButtons(inputId = "Stop_T1", label = NULL, choices = cls$cd03, selected = dpf$Stop_T1))),
        div(id = "tb3_t", radioButtons(inputId = "Stop_T2", label = NULL, choices = cls$cd03, selected = dpf$Stop_T2))
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Distanz des Weges (auf ganze Kilometer runden)")),
        div(
          div(id = "tb3_t", disabled(numericInput(inputId = "Dist_T1", label = NULL, min = 1, value = dpf$Dist_T1, width = "125px"))),
          conditionalPanel(condition = "output.Dist_T1_chk > '0'", h5(id = "warning", "Distanz muss mindestens 1 km sein"))
        ),
        div(
          div(id = "tb3_t", numericInput(inputId = "Dist_T2", label = NULL, min = 1, value = dpf$Dist_T2, width = "125px")),
          conditionalPanel(condition = "output.Dist_T2_chk > '0'", h5(id = "warning", "Distanz muss mindestens 1 km sein"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
        div(id = "tb3_t", HTML("Fahrtdauer in Stunden und Minuten</br><font size='-1'>Bitte schätzen Sie die reine Fahrzeit ohne Zwischenaufenthalt, 
          aber mit Verspätungen im ÖV sowie Stau und Parkplatzsuche.</font size='-1'>")),
        div(
          div(id = "tb3_t", disabled(timeInput(inputId = "Dur_T1", label = HTML("&nbspStunden | Minuten"), seconds = F, minute.steps = 1, value = t1dur))),
          conditionalPanel(condition = "output.Dur_T1_chk > '0'", h5(id = "warning", "Bitte Distanz und Dauer prüfen!"))
        ),
        div(
          div(id = "tb3_t", timeInput(inputId = "Dur_T2", label = HTML("&nbspStunden | Minuten"), seconds = F, minute.steps = 1, value = t2dur)),
          conditionalPanel(condition = "output.Dur_T2_chk > '0'", h5(id = "warning", "Bitte Distanz und Dauer prüfen!"))
        )
      ),
      splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
        div(id = "tb3_t", HTML("Verwendete Verkehrsmittel auf dem Weg</br>
          <font size='-1'>Bitte geben Sie alle verwendeten Verkehrsmittel an.</font size='-1'>")),
        div(id = "tb3_t", disabled(checkboxGroupInput(inputId = "Modes_T1", label = NULL, choices = cls$cd07, selected = mod1_sel))),
        div(id = "tb3_t", checkboxGroupInput(inputId = "Modes_T2", label = NULL, choices = cls$cd07, selected = mod2_sel))
      ),
      #conditional questions if car was used
      conditionalPanel(
        condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3' || output.ModQst_T2 == '1' || output.ModQst_T2 == '3'",
        div(id = "tb3_t", HTML("<font color=#27408B><b>Zusatzfragen zum Pkw:</b></font color=#27408B>")),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
          div(id = "tb3_t", HTML("Zeitverlust durch Stau (Minuten):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(disabled(numericInput(inputId = "CarCong_T1", label = NULL, min = 0, value = dpf$CarCong_T1, width = "75px")), style = "display:inline-block"),
              div(disabled(checkboxGroupInput(inputId = "CarCongNa_T1", label = NULL, choices = c(keiner = 95), selected = dpf$CarCongNa_T1)), style = "display:inline-block"))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '1' || output.ModQst_T2 == '3'",
            div(id = "tb3_t", div(numericInput(inputId = "CarCong_T2", label = NULL, min = 0, value = dpf$CarCong_T2, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarCongNa_T2", label = NULL, choices = c(keiner = 95), selected = dpf$CarCongNa_T2), style = "display:inline-block"))
          )
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
          div(id = "tb3_t", HTML("Geschätzte Dauer für Parkplatzsuche (Minuten):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(disabled(numericInput(inputId = "CarPark_T1", label = NULL, min = 0, value = dpf$CarPark_T1, width = "75px")), style = "display:inline-block"),
              div(disabled(checkboxGroupInput(inputId = "CarParkNa_T1", label = NULL, choices = cls$cd95, selected = dpf$CarParkNa_T1)), style = "display:inline-block"))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '1' || output.ModQst_T2 == '3'",
            div(id = "tb3_t", div(numericInput(inputId = "CarPark_T2", label = NULL, min = 0, value = dpf$CarPark_T2, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarParkNa_T2", label = NULL, choices = cls$cd95, selected = dpf$CarParkNa_T2), style = "display:inline-block"))
          )
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
          div(id = "tb3_t", HTML("Geschätzte Fahrtkosten inkl. Maut- und Parkkosten (Euro):")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '1' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(disabled(textInput(inputId = "CarCost_T1", label = NULL, placeholder = "Euro", value = dpf$CarCost_T1, width = "75px")), style = "display:inline-block"),
              div(disabled(checkboxGroupInput(inputId = "CarCostNa_T1", label = NULL, choices = cls$cd97, selected = dpf$CarCostNa_T1)), style = "display:inline-block"))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '1' || output.ModQst_T2 == '3'",
            div(id = "tb3_t", div(textInput(inputId = "CarCost_T2", label = NULL, placeholder = "Euro", value = dpf$CarCost_T2, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "CarCostNa_T2", label = NULL, choices = cls$cd97, selected = dpf$CarCostNa_T2), style = "display:inline-block"))
          )
        )
      ),
      #conditional questions if public was used
      conditionalPanel(
        condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3' || output.ModQst_T2 == '2' || output.ModQst_T2 == '3'",
        div(id = "tb3_t", HTML("<font color=#27408B><b>Zusatzfragen zum öffentlichen Verkehrsmittel:</b></font color=#27408B>")),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1", style = "border-top:1px solid gainsboro;",
          div(id = "tb3_t", HTML("Bedienungshäufigkeit der ÖV-Verbindung")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", disabled(radioButtons(inputId = "PubInt_T1", label = NULL, choices = cls$cd06, selected = dpf$PubInt_T1)))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '2' || output.ModQst_T2 == '3'",  
            div(id = "tb3_t", radioButtons(inputId = "PubInt_T2", label = NULL, choices = cls$cd06, selected = dpf$PubInt_T2))
          )
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r2",
          div(id = "tb3_t", HTML("Haben Sie die Fahrt für aktive Nebenaktivitäten genutzt? (nicht nur Musik hören) Wenn Ja: bitte beschreiben")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", disabled(textInput(inputId = "PubSecDsc_T1", label = NULL, value = dpf$PubSecDsc_T1, width = "200px")))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '2' || output.ModQst_T2 == '3'",
            div(id = "tb3_t", textInput(inputId = "PubSecDsc_T2", label = NULL, value = dpf$PubSecDsc_T2, width = "200px"))
          )
        ),
        splitLayout(cellWidths = cwd, cellArgs = list(style='white-space: normal;'), id = "tb3_r1",
          div(id = "tb3_t", HTML("Geschätzte Fahrtkosten für eine einfache Fahrt (Euro)")),
          conditionalPanel(
            condition = "output.ModQst_T1 == '2' || output.ModQst_T1 == '3'",
            div(id = "tb3_t", div(disabled(textInput(inputId = "PubCost_T1", label = NULL, placeholder = "Euro", value = dpf$PubCost_T1, width = "75px")), style = "display:inline-block"),
              div(disabled(checkboxGroupInput(inputId = "PubCostNa_T1", label = NULL, choices = cls$cd97, selected = dpf$PubCostNa_T1)), style = "display:inline-block"))
          ),
          conditionalPanel(
            condition = "output.ModQst_T2 == '2' || output.ModQst_T2 == '3'",
            div(id = "tb3_t", div(textInput(inputId = "PubCost_T2", label = NULL, placeholder = "Euro", value = dpf$PubCost_T2, width = "75px"), style = "display:inline-block"),
              div(checkboxGroupInput(inputId = "PubCostNa_T2", label = NULL, choices = cls$cd97, selected = dpf$PubCostNa_T2), style = "display:inline-block"))
          )
        )
      ),
      br(),
      #if input is missing: prompt warning message indicating missing items
      conditionalPanel(
        condition = "output.Mis_chk > '0'",
        h5(id = "warning", htmlOutput("Mis_msg"))
      ),
      #add action buttons at the bottom
      actionButton(inputId ="RP4_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="RP4_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Intro1: introduction to sp1 experiments
getP_Intro1 <- function () {
  page <- (
    div(value = "page_Intro1", style = "padding: 10px",
      div(img(src = "img_flower_01.JPG", width = "400px"), style = "margin-left:10px;float:right"),
      div(HTML(dx0$txt[dx0$lbl == "intro_SP1"]), style = "text-align: justify"), 
      actionButton(inputId ="Intro1_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Intro1_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Intro2: introduction to sp2 experiments
getP_Intro2 <- function () {
  page <- (
    div(value = "page_Intro2", style = "padding: 10px",
      div(img(src = "img_flower_02.JPG", width = "400px"), style = "margin-left:10px;float:right"),
      div(HTML(dx0$txt[dx0$lbl == "intro_SP2"]), style = "text-align: justify"), 
      br(),br(),br(),br(),br(),br(),br(),
      actionButton(inputId ="Intro2_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Intro2_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Intro3: introduction to sp3 experiments
getP_Intro3 <- function () {
  page <- (
    div(value = "page_Intro3", style = "padding: 10px",
      div(img(src = "img_flower_03.JPG", width = "400px"), style = "margin-left:10px;float:right"),
      div(HTML(dx0$txt[dx0$lbl == "intro_SP3"]), style = "text-align: justify"), 
      br(),br(),br(),br(),br(),br(),
      actionButton(inputId ="Intro3_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Intro3_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Intro4: introduction to sp4 experiments
getP_Intro4 <- function () {
  page <- (
    div(value = "page_Intro4", style = "padding: 10px",
      div(img(src = "img_flower_04.JPG", width = "400px"), style = "margin-left:10px;float:right"),
      div(HTML(dx0$txt[dx0$lbl == "intro_SP4"]), style = "text-align: justify"),
      br(),br(),br(),br(),br(),br(),
      actionButton(inputId ="Intro4_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Intro4_next", label = "weiter", icon = icon("angle-right"), style = "color: #27408B;float:right")
    )
  )
  return(page)
}

#######################################################################################################################
#P_Farewell: farewell with option to insert mail address, therefore reactive with respect to dpi
getP_Farewell <- function (dpf = dpi) {
  #get page with prefilled anwers from previous participation
  page <- (
    #main div with farewell
    div(value = "page_Farewell", style = "padding: 10px",
      div(HTML(dx0$txt[dx0$lbl == "intro_Farewell"]), style = "text-align: justify"), 
      textAreaInput(inputId = "remark", label = NULL, height = "200px", value = dpf$remark),
      br(),
      actionButton(inputId ="Farewell_back", label = "zurück", icon = icon("angle-left"), style = "color:red"),
      actionButton(inputId ="Farewell_submit", label = "fertig", icon = icon("envelope"), style = "color:#27408B;float:right")
    )
  )
  return(page)
}

#End###################################################################################################################
