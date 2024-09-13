
#Jan 2019 Reinhard Hoessinger
#online survey tool: present an html form on https, save input data in a MySQL database and load it from there

#######################################################################################################################
#define ui for dataset viewer app

vmoe_ui <- shinyUI(fluidPage(title = "VMÖ Befragung",
  theme = "vmoe_styles.css",
  useShinyjs(),
  
  #######################################################################################################################
  #set styles in page header (currently not used because all general styles are specified in the css file)
  # tags$head(tags$style("label{color:#27408B;font-weight:normal;}")),
  
  #######################################################################################################################
  #add JavaScript code to the app
  #note: JavaScript functions can be placed (1) in a separate js file or (2) directly in the ui using tags$script()
  #(1) separate js file: the following function identifies the user's web browser
  includeScript("www/vmoe_scripts.js"),
  #(2) directly in the ui: the following  function resets the input value of a radiobutton to NULL (empty value)
  tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {Shiny.onInputChange(variableName, null);});"),
  #note: the code between the quotes corresponds exactly to the code that would be written in the js file

  #display current page generated in the server
  div(uiOutput("page_curr"))
))

#End###################################################################################################################
