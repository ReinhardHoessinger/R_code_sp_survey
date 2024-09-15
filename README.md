# A tour-based SP-off-RP survey for combined time period and mode choice
This repository includes all R scripts and auxillary files to
- develop an algorithmic experimental design using the R package AlgDesign;
- run an interactive online questionnaire using the R package shiny to define the server logic and the html user interface; this part also includes the construction of the choice sets immediately during the interview between the RP and SP part using as input (i) the attributes of the RP tour provided by the respondent, (ii) the design matrix, and (iii) a function that processes both inputs to suitable choice sets being displayed on the screen; and
- conduct an interim analysis of the pretest data using a self-written R code for discrete choice analyses.

Please note: The production system also involves a MySQL database, which is linked to the R environment using the package RMySQL. The database is not part of this repository.
