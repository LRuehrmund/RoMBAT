rm(list = ls(all = TRUE)) #clearing environment (Variablenspeicher leeren)

# Values to adjust:
work_directory <- "C:/Users/Antti/Desktop/HiWi KAI/R" #SLASH "/" needed instead of BACKSLASH "\" (Windows standard) #work-directory (Arbeitsverzeichnis), path of the folder with the data file
export_directory <- paste(work_directory, "/output/",sep ="") #export directory (Exportverzeichnis), path of the folder for the export of the p-value table
file_name <- "example_data.xlsx" #name of the data file (with file extension (e.g. ".xlsx", or ".xlsm"))
sheet_name <- "example_table" #name of the excel sheet in which the data is found
sample_type_comparison <- c("Infected","Control","Roomair") #name of the sample types (e.g. infected and controll) that shall be visualized
height_browser_window <- '700px' #height of the figure (for fullscreen: notebooks <700px, bigger screens ~850px, can be bigger or smaller, based on the screen, try it!)

#Notes: 
# -for the sample types that are supposed to be compared, there have to be the same amount of timepoints (e.g measurement days)
# -for the scatterplot, the timepoints (e.g. measurement days) must be given as numbers as strings
# -lines without values cause the name of the sample types to disappear in the boxplot => avoid empty lines in excel file

#import libraries
library(readxl)   #provides excel-file import
library(tibble)   #necessary for dplyr, ggpubr, plotly
library(dplyr)    #necessary for ggpubr
library(ggpubr)   #provides better boxplot functionality
library(xtable)   #necessary for shiny
library(shiny)    #provides User Interface
library(plotly)   #provides scatterplot

setwd(work_directory) #setting the workdirectory
import_data <- read_xlsx(file_name,sheet_name) #import of the data file
dimension_import <<- dim(import_data) #dimensions of the imported data

# hints for programming ---------------------------------------------------
# -you may not use umlauts, not even in comments
# -adjust version number of the .R-file when you change the code to mark older files as outdated  

# 1. Mapping of samples to timepoints (e.g. measurement days) and sample types -----------------------

### mapping of samples to first excel column (timepoints (e.g. measurement days))
temp_number_of_import_lines <- dimension_import[1]
temp_adress_import <- matrix(nrow=temp_number_of_import_lines,ncol=temp_number_of_import_lines)
temp_ueberschriebenes_import_data <- import_data[,1]
search_value <- vector(length=temp_number_of_import_lines)
temp_length_koordinaten_tage <- vector(length=temp_number_of_import_lines)
temp_timepoint_values <- vector(length=temp_number_of_import_lines)
search_index <- 1
j <- 1
while (search_index < Inf) { # every value will be looked at, till all values are NA and min() returns Inf (as their is no minimal value to be found)
  search_value <- as.character(import_data[search_index,1])  # defines the value which will be looked for (in the first column of imported data)
  temp_timepoint_values[j] <- search_value # saving all different timepoint values
  temp_adress_import[j,1:length(which(import_data[,1]==search_value))] <- which(import_data[,1]==search_value) # all lines where the value was found, will be safed
  temp_length_koordinaten_tage[j] <- length(which(import_data[,1]==search_value)) # amount of the found lines
  temp_ueberschriebenes_import_data[temp_adress_import[j,1:temp_length_koordinaten_tage[j]],] <- NA # the found lines will be set NA to not find them again
  search_index <- min(which(temp_ueberschriebenes_import_data != search_value)) # finding the index of the next search_value to repeat the loop
  j <- j+1
}
timepoint_values <- temp_timepoint_values[temp_timepoint_values != FALSE]
anzahl_unterschiedlicher_werte_messtag <- j-1
laenge_zeilennummer_messtag <- temp_length_koordinaten_tage[1:anzahl_unterschiedlicher_werte_messtag]
# if there is only one point in time (e.g. measurement day) the zeilennummer_messtag array needs to have 2 dimensions, otherwise some requests in evaluating code dont work
if (anzahl_unterschiedlicher_werte_messtag == 1){
  zeilennummer_messtag <- temp_adress_import[1:2,1:max(laenge_zeilennummer_messtag)]
} else {
  zeilennummer_messtag <- temp_adress_import[1:anzahl_unterschiedlicher_werte_messtag,1:max(laenge_zeilennummer_messtag)]
}

rm(temp_number_of_import_lines,temp_timepoint_values,temp_adress_import,temp_length_koordinaten_tage,temp_ueberschriebenes_import_data,j,search_index,search_value) # deleting temporary variables


### mapping of samples to second excel column (sample types)
temp_number_of_import_lines <- dimension_import[1]
temp_namen <- vector(length=temp_number_of_import_lines)
temp_adress_import <- matrix(nrow=temp_number_of_import_lines,ncol=dimension_import[1])
temp_ueberschriebenes_import_data <- import_data[,2]
temp_length_koordinaten_tage <- vector(length=temp_number_of_import_lines)
search_index <- 1
j <- 1
while (search_index < Inf) { # every value will be looked at, till all values are NA and min() returns Inf (as their is no minimal value to be found)
  search_value <- as.character(import_data[search_index,2])  # defines the value which will be looked for (in the first column of imported data)
  temp_namen[j] <- search_value # saving all different values of sample_types 
  temp_adress_import[j,1:length(which(import_data[,2]==search_value))] <- which(import_data[,2]==search_value) # all lines where the value was found, will be safed
  temp_length_koordinaten_tage[j] <- length(which(import_data[,2]==search_value)) # amount of the found lines
  temp_ueberschriebenes_import_data[temp_adress_import[j,1:temp_length_koordinaten_tage[j]],] <- NA # the found lines will be set NA to not find them again
  search_index <- min(which(temp_ueberschriebenes_import_data != search_value)) # finding the index of the next search_value to repeat the loop
  j <- j+1
}
werte_probentypen <- temp_namen[temp_namen != FALSE]
zu_vergleichende_probentypen <- match(sample_type_comparison, werte_probentypen)
anzahl_unterschiedlicher_werte_probentyp <- j-1
laenge_zeilennummer_probentyp <- temp_length_koordinaten_tage[1:anzahl_unterschiedlicher_werte_probentyp]
# if there is only one sample type the zeilennummer_probentyp array needs to have 2 dimensions, otherwise some requests in evaluating code dont work
if (anzahl_unterschiedlicher_werte_probentyp == 1){
  zeilennummer_probentyp <- temp_adress_import[1:2,1:max(laenge_zeilennummer_probentyp)]
} else {
  zeilennummer_probentyp <- temp_adress_import[1:anzahl_unterschiedlicher_werte_probentyp,1:max(laenge_zeilennummer_probentyp)]
}

rm(temp_number_of_import_lines,temp_namen,temp_adress_import,temp_length_koordinaten_tage,temp_ueberschriebenes_import_data,j,search_index,search_value) # deleting temporary variables

### mapping of samples to first and second excel column (timepoints and sample types)
temp_adress_import <- matrix(nrow = dimension_import[1],ncol = dimension_import[1])
temp_bedeutung_zeilennummer_beides <- vector(length = dimension_import[1])
temp_ueberschriebenes_import_data <- vector(mode = "character", length = dimension_import[1])
for (i in (1:dimension_import[1])){
  temp_ueberschriebenes_import_data[i] <- paste(as.character(import_data[i,1:2]),collapse = "")
}
search_value <- vector(length = dimension_import[1])
temp_length_koordinaten <- vector(length = dimension_import[1])
search_index <- 1
j <- 1
while (search_index < Inf) { # every value will be looked at, till all values are NA and min() returns Inf (as their is no minimal value to be found)
  search_value <- paste(as.character(import_data[search_index,1:2]), collapse = "")  # search_value wird initialisert (welcher Wert wird im Tibble gesucht)
  temp_adress_import[j,1:length(which(temp_ueberschriebenes_import_data==search_value))] <- which(temp_ueberschriebenes_import_data==search_value) # Alle Indizes wo der gesuchte Wert auftritt werden gespeichert
  temp_bedeutung_zeilennummer_beides[j] <- search_value
  temp_length_koordinaten[j] <- length(which(temp_ueberschriebenes_import_data==search_value)) # Wie viele Werte sind das (wird benoetigt um nicht mit NA zu adressieren)
  temp_ueberschriebenes_import_data[temp_adress_import[j,1:temp_length_koordinaten[j]]] <- NA # Hier wird das durchsuchte tibble NA gesetzt um die Adressen nicht nochmal zu finden
  search_index <- min(which(temp_ueberschriebenes_import_data != search_value)) # Die Adresse des neues Wertes wird gesucht
  j <- j+1
}
anzahl_unterschiedlicher_werte_beides <- j-1
laenge_zeilennummer_beides<- temp_length_koordinaten[1:anzahl_unterschiedlicher_werte_beides]
zeilennummer_beides <- temp_adress_import[1:anzahl_unterschiedlicher_werte_beides,1:max(laenge_zeilennummer_beides)]
bedeutung_zeilennummer_beides <- temp_bedeutung_zeilennummer_beides[1:length(laenge_zeilennummer_beides)]
rm(temp_bedeutung_zeilennummer_beides,temp_adress_import,temp_length_koordinaten,temp_ueberschriebenes_import_data,j,search_index,search_value) # Vektoren loeschen

length_sample_type_comparison <- length(sample_type_comparison) #count of the sample types that shall be compared 

sample_type_comparison_pair <- matrix(nrow = sum(1:(length_sample_type_comparison-1)), ncol = 2)
adresse_1 <- 1
lauf <- 1
for (zaehler_1 in ((length_sample_type_comparison-1):1)){
  adresse_2 <- adresse_1+1
  for (zaehler_2 in (1:zaehler_1)){
    sample_type_comparison_pair[lauf,1] <- sample_type_comparison[adresse_1]
    sample_type_comparison_pair[lauf,2] <- sample_type_comparison[adresse_2]
    
    adresse_2 <- adresse_2 + 1 
    lauf <- lauf + 1
  }
  adresse_1 <- adresse_1 + 1
}

### mapping of samples to third excel column (only necessary for data normalization (e.g. for patient identification))
temp_number_of_import_lines <- dimension_import[1]
temp_adress_import <- matrix(nrow=temp_number_of_import_lines,ncol=temp_number_of_import_lines)
temp_ueberschriebenes_import_data <- import_data[,3]
search_value <- vector(length=temp_number_of_import_lines)
temp_length_koordinaten_patienten <- vector(length=temp_number_of_import_lines)
temp_patient_values <- vector(length=temp_number_of_import_lines)
search_index <- 1
j <- 1
while (search_index < Inf) { # every value will be looked at, till all values are NA and min() returns Inf (as their is no minimal value to be found)
  search_value <- as.character(import_data[search_index,3])  # defines the value which will be looked for (in the first column of imported data)
  temp_patient_values[j] <- search_value # saving all different timepoint values
  temp_adress_import[j,1:length(which(import_data[,3]==search_value))] <- which(import_data[,3]==search_value) # all lines where the value was found, will be safed
  temp_length_koordinaten_patienten[j] <- length(which(import_data[,3]==search_value)) # amount of the found lines
  temp_ueberschriebenes_import_data[temp_adress_import[j,1:temp_length_koordinaten_patienten[j]],] <- NA # the found lines will be set NA to not find them again
  search_index <- min(which(temp_ueberschriebenes_import_data != search_value)) # finding the index of the next search_value to repeat the loop
  j <- j+1
}
patient_values <- temp_patient_values[temp_patient_values != FALSE]
anzahl_unterschiedlicher_werte_patient <- j-1
laenge_zeilennummer_patient <- temp_length_koordinaten_patienten[1:anzahl_unterschiedlicher_werte_patient]
# if there is only one point patient, the zeilennummer_patient array needs to have 2 dimensions, otherwise some requests in evaluating code dont work
if (anzahl_unterschiedlicher_werte_patient == 1){
  zeilennummer_patient <- temp_adress_import[1:2,1:max(laenge_zeilennummer_messtag)]
} else {
  zeilennummer_patient <- temp_adress_import[1:anzahl_unterschiedlicher_werte_patient,1:max(laenge_zeilennummer_patient)]
}

rm(temp_number_of_import_lines,temp_patient_values,temp_adress_import,temp_length_koordinaten_patienten,temp_ueberschriebenes_import_data,j,search_index,search_value) # deleting temporary variables


# 2. Colour adjustment -------------------------------------------------------
fill <- vector(length = 0)
# Dem plot-Befehl muessen fuer jede gezeichnete Box die Farbe mit dem Hinweis "fill" uebergeben werden
# Diese Schleifen uebergeben die Farben fuer die ersten beiden Probentypen dediziert und danach aus einer Palette
for (j in (1:length_sample_type_comparison)){
  for (i in (1:anzahl_unterschiedlicher_werte_messtag)){
    if (j == 1){
      fill <- c(fill, "cornflowerblue")
    }
    if (j == 2){
      fill <- c(fill, "blue2")
    }
    if (j > 2 && j < 9){
      fill <- c(fill, palette.colors(n=j,palette = "Okabe-Ito")[j])
    }
    if (j >= 9){
      fill <- c(fill, palette.colors(n=j-7,palette = "ggplot2")[j-7])
    }
  }
}
alpha <- 1 #Transparentwert(Alpha) fuer die Boxen (1 ist nicht transparent)
alpha_violin <- 0.3 #Transparent(Alpha) fuer die Violinplots (0 ist komplett transparent)


# 3. GUI Code ----------------------------------------------------------------

# 3.1. Definition of the UI -------------------------------------------------------
# You can modify the standard values in the UI in this Code
# For that you have to change the "value" argument in the specific line
ui <- navbarPage("Visualizing RoMBATs data",
          tabPanel("Boxplots",
              sidebarLayout(
                  sidebarPanel(h3('Options for boxplots:'),
                               selectInput('gewaehlte_substanz', 'substance:', colnames(import_data[,4:dimension_import[2]]), selected = colnames(import_data[,4])),
                               checkboxInput('draw_brackets','draw significance brackets',value = TRUE),
                               checkboxInput('draw_violin_plots', 'draw violin plots', value = FALSE), 
                               checkboxInput('normalize_data','normalize on given time point', value = FALSE),
                               conditionalPanel(condition = "input.normalize_data == true",
                                                selectInput('time_point_to_normalize_on','time point to normalize data on:', timepoint_values, selected = timepoint_values[5])),
                               numericInput('grenzwert','highest p-value that is shown in the plot:',value = 0.05), 
                               actionButton('less','previous substance'),
                               actionButton('more','next substance'),
                               h3('Options for p-value format:'),
                               checkboxInput('scientific','p-value displayed in scientific format', value = TRUE),
                               conditionalPanel(condition = "input.scientific == false",
                                                numericInput('pvalue_format','p-value number of digits displayed', value=7)),
                               h3('Table of p-values:'),
                               actionButton('export','export table'),
                               tableOutput('export_table'),
                               width = 3), #Ende sidebarPanel
                  mainPanel(
                      plotOutput('plot1', height = height_browser_window)
                      ,width = 9) #Ende mainPanel
                  )#Ende sidebarLayout
              ),#Ende tabPanel boxplots
          tabPanel("3D-Scatterplot",
              sidebarLayout(
                  sidebarPanel(
                      selectInput('scatter_substanz1', 'x_Achse:', colnames(import_data[,1:dimension_import[2]]), selected = colnames(import_data[,1])),
                      selectInput('scatter_substanz2', 'y-Achse:', colnames(import_data[,1:dimension_import[2]]), selected = colnames(import_data[,4])),
                      selectInput('scatter_substanz3', 'z-Achse:', colnames(import_data[,1:dimension_import[2]]), selected = colnames(import_data[,5])),
                      selectInput('scatter_dots', 'Punkte:', colnames(import_data[,1:dimension_import[2]]), selected = colnames(import_data[,2])),
                      width = 2
                  ), #Ende sidebarPanel
                  mainPanel(
                      plotlyOutput('scatterplot3d' ,height = height_browser_window),
                      width = 10
                  ) #Ende mainPanel
            )#Ende sidebarLayout
        )#Ende tabPanel 3D-scatterplots
    )#Ende navbarPage


# 3.2. Definition of the Server (events that are triggered when there is interaction with the UI)----------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$less, {
    substanz_auswahl <- which(colnames(import_data) == as.character(input$gewaehlte_substanz))
    substanz <- substanz_auswahl - 1
    if (substanz < 4){
      substanz <- 4
    }
    updateSelectInput(session, 'gewaehlte_substanz', selected = colnames(import_data[,substanz]))
  })
  
  observeEvent(input$more, {
    substanz_auswahl <- which(colnames(import_data) == as.character(input$gewaehlte_substanz))
    substanz <- substanz_auswahl + 1
    if (substanz > dimension_import[2]){
      substanz <- dimension_import[2]
    }
    updateSelectInput(session, 'gewaehlte_substanz', selected = colnames(import_data[,substanz]))
  })
  
  output$plot1 <- renderPlot({
    
    substanz <- which(colnames(import_data) == as.character(input$gewaehlte_substanz))

    # Die Schleife wird ausgefuehrt wenn einer unten aufgefuehrten Werte veraendert wird
    reaktivitaet <- input$gewaehlte_substanz
    reaktivitaet <- input$grenzwert
    reaktivitaet <- input$draw_violin_plots
    reaktivitaet <- input$draw_brackets
    scientific <- input$scientific
    pvalue_format <- input$pvalue_format
    normalize_data <- input$normalize_data
    time_point_to_normalize_on <- input$time_point_to_normalize_on
    
    grenzwert <- input$grenzwert
    # grenzwert <- grenzwert/(dim(p_values_vgl_beides)[1]*dim(p_values_vgl_beides)[2]) # korrigierter grenzwert nach bonferroni korrektur
    

#3.2.1 normalize data ----------------------------------------------------

    # numeric_normalized_data muss in das tibble gesteckt werden und dort Werte ueberschreiben
    # funktioniert das auch wenn die tage nicht sortiert sind?
    import_data_normalized <- import_data
    import_data_safety <- import_data
    adress_time_point_to_normalize_on <- which(timepoint_values==time_point_to_normalize_on)
    numeric_normalized_data <- vector(length =(dimension_import[2]-3))
    if (normalize_data){
      for (i in (1:anzahl_unterschiedlicher_werte_messtag)){
        for (j in (1:anzahl_unterschiedlicher_werte_patient)){
          browser()
          numeric_normalized_data <- as.numeric(import_data[zeilennummer_patient[j,i],4:length(import_data[zeilennummer_patient[j,i],])])/as.numeric(import_data[zeilennummer_patient[j,adress_time_point_to_normalize_on],4:length(import_data[zeilennummer_patient[j,i],])])
          numeric_normalized_data[numeric_normalized_data==Inf] <- 0 # wenn etwas durch 0 geteilt wird
          numeric_normalized_data[is.nan(numeric_normalized_data)] <- 1 # wenn 0 durch 0 geteilt wird
          for (k in (1:(dimension_import[2]-3))){
            import_data_normalized[zeilennummer_patient[j,i],k+3] <- numeric_normalized_data[k]
          }
        }
         #4. Spalte zur Sortierung der Daten um einzelne Patienten zu sortieren
      }
      import_data <- import_data_normalized
    }

#  3.2.2 calculation p-values -----------------------------------------------------
    lauf_stat_test <- 1
    adresse_1 <- 1
    
    p_values_vgl_beides <- vector(length = sum(1:(anzahl_unterschiedlicher_werte_beides-1)),mode = "numeric")
    namen_p_values_vgl_beides <- vector(length = sum(1:(anzahl_unterschiedlicher_werte_beides-1)), mode = "character")
    
    for (zaehler_1 in ((anzahl_unterschiedlicher_werte_beides-1):1)){
      adresse_2 <- adresse_1+1
      for (zaehler_2 in (1:zaehler_1)){
        if (is.na(import_data[1,substanz]) == FALSE && laenge_zeilennummer_beides[adresse_1] != 1 && laenge_zeilennummer_beides[adresse_2] != 1) { #Es muss mehr als ein Wert fuer die p-Wert Berechnung untersucht werden
          temp_stat_test <- wilcox.test(as.numeric(unlist(import_data[zeilennummer_beides[adresse_1,1:laenge_zeilennummer_beides[adresse_1]],substanz])),
                                        as.numeric(unlist(import_data[zeilennummer_beides[adresse_2,1:laenge_zeilennummer_beides[adresse_2]],substanz])))
          p_values_vgl_beides[lauf_stat_test] <- unlist(temp_stat_test[3])
          if (is.nan(p_values_vgl_beides[lauf_stat_test])){
            p_values_vgl_beides[lauf_stat_test] <- 3 # wenn der Ausgangswert keinen gueltigen statistischen Test zulaesst (z.B. zu viele 0en) dann gibt es den Fehlercode "3"
          }
        } else{
          p_values_vgl_beides[lauf_stat_test] <- 2 # wenn der Ausgangswert Na ist (nicht vorhanden), dann nimmt p_values_vgl_probentypen den Fehlercode "2" an
        }
        # Namen fuer die Zellen, auskommentiert, da Leistungshungrig
        'namen_p_values_vgl_beides[lauf_stat_test] <- paste("Vergleich zwischen",
                                                                  colnames(import_data[1]),as.character(import_data[zeilennummer_beides[adresse_1,1],1]),
                                                                  colnames(import_data[2]),as.character(import_data[zeilennummer_beides[adresse_1,2],2]),
                                                                  "und",
                                                                  colnames(import_data[1]),as.character(import_data[zeilennummer_beides[adresse_2,1],1]),
                                                                  colnames(import_data[2]),as.character(import_data[zeilennummer_beides[adresse_2,2],2]),
                                                                  "fuer",
                                                                  colnames(import_data[substanz]))'
        lauf_stat_test <- lauf_stat_test + 1
        adresse_2 <- adresse_2+1
      }
      adresse_1 <- adresse_1+1
    }
    rm(temp_stat_test, adresse_1, adresse_2, lauf_stat_test)
    
# data conditioning for plot command -------------------------------------------------------------    
    lauf <- 1
    adresse_1 <- 1
    lauf_comp_probentyp <- 1
    lauf_comp_probentag <- 1
    comp_probentyp <- matrix(nrow = dimension_import[1], ncol = 3)
    comp_probentag <- matrix(nrow = dimension_import[1], ncol = 3)
    comparisons_table <- matrix(ncol = 3, nrow = 2) # falls keine Tabelle erstellt wird (der Grenzwert zu niedrig ist) fuehrt die Initialisierung hier dazu, dass trotzdem ein plot gezeichnet wird
    comparisons_manuell <- matrix() # falls keine Signifikanzen ueber dem Grenzwert liegen, fuehrt die Initialisierung hier dazu, dass trotzdem ein plot gezeichnet wird
    for (zaehler_1 in ((anzahl_unterschiedlicher_werte_beides-1):1)){
      adresse_2 <- adresse_1 + 1
      for (zaehler_2 in (1:zaehler_1)){
        # Bedingung if Schleife: Wenn der p-value unter dem korrigierten Grenzwert liegt und sich der p-value entweder am gleichen Tag oder beim selben Probentyp unterscheidet
        if ((p_values_vgl_beides[lauf] <= grenzwert) && (import_data[zeilennummer_beides[adresse_1],1]==import_data[zeilennummer_beides[adresse_2],1] || import_data[zeilennummer_beides[adresse_1],2] == import_data[zeilennummer_beides[adresse_2],2])){
          
          # An welchem Probentag treten Signifikanzen zwischen den Probentypen auf
          if (import_data[zeilennummer_beides[adresse_1],1]==import_data[zeilennummer_beides[adresse_2],1]){
            comp_probentyp[lauf_comp_probentyp,1] <- as.character(import_data[zeilennummer_beides[adresse_1],2]) # Wert 1 fuer comparison im plot
            comp_probentyp[lauf_comp_probentyp,2] <- as.character(import_data[zeilennummer_beides[adresse_2],2]) # Wert 2 fuer comparison im plot
            comp_probentyp[lauf_comp_probentyp,3] <- as.character(import_data[zeilennummer_beides[adresse_2],1]) # der Tag an dem sich die Probentypen signifikant unterscheiden
            lauf_comp_probentyp <- lauf_comp_probentyp + 1
          }
          # Bei welchen Probentypen treten Signifikanzen zwischen den Probentagen auf
          if (import_data[zeilennummer_beides[adresse_1],2]==import_data[zeilennummer_beides[adresse_2],2]){
            comp_probentag[lauf_comp_probentag,1] <- as.character(import_data[zeilennummer_beides[adresse_1],1]) # Wert 1 fuer comparison im plot
            comp_probentag[lauf_comp_probentag,2] <- as.character(import_data[zeilennummer_beides[adresse_2],1]) # Wert 2 fuer comparison im plot
            comp_probentag[lauf_comp_probentag,3] <- as.character(import_data[zeilennummer_beides[adresse_2],2]) # der Probentyp der sich an den unterschiedlichen Messtagen signifikant unterscheidet
            lauf_comp_probentag <- lauf_comp_probentag + 1
          }
        }
        adresse_2 <- adresse_2 + 1 
        lauf <- lauf + 1
      }
      adresse_1 <- adresse_1 + 1
    }
    comp_probentag_ohne_NA <- comp_probentag[!is.na(comp_probentag[,1]),]
    # um Fehlermeldungen zu vermeiden die nrow erzeugt wenn es nur eine oder keine Reihe gibt, wird eine Variable verwendet mit ueberpruefung
    nrow_comp_probentag_ohne_NA <- nrow(comp_probentag_ohne_NA)
    if (is.null(nrow_comp_probentag_ohne_NA)||nrow_comp_probentag_ohne_NA==0){
      nrow_comp_probentag_ohne_NA <- 1
      #Falls es nur einen Messtag gibt, muss trotzdem irgendwas in comp_probentag stehen, damit das nachfolgende Programm laeuft
      comp_probentag[1,] = c(99,99,99)
    }
    
    comp_probentyp_ohne_NA <- comp_probentyp[!is.na(comp_probentyp[,1]),]
    # um Fehlermeldungen zu vermeiden, die nrow erzeugt wenn es nur eine oder keine Reihe gibt, wird eine Variable verwendet mit ueberpruefung
    nrow_comp_probentyp_ohne_NA <- nrow(comp_probentyp_ohne_NA)
    if (is.null(nrow_comp_probentyp_ohne_NA)){
      nrow_comp_probentyp_ohne_NA <- 1
    }

    adresse_comparisons_tage <- matrix(nrow = dimension_import[1], ncol = length_sample_type_comparison)
    adresse_comparisons_typen <- matrix(nrow = dimension_import[1], ncol = sum(1:(length_sample_type_comparison-1)))
    
    # Plot Schleife fuer alle Tage fuer alle angegebenen Probentypen
    if (!is.na(comp_probentag[1,1])){

      # Es werden die Probentypen die verglichen werden sollen abgespeichert und anschlieszend werden alle relevanten Signifikanzen herausgesucht (Signifikanzen fuer gegebene Probentypen an allen Tagen und zwischen den selben Tagen fuer die Probentypen)
      for (m in (1:length_sample_type_comparison)){
        l <- 1
        for (k in (1:nrow_comp_probentag_ohne_NA)){
          if (comp_probentag[k,3] == sample_type_comparison[m]){
            adresse_comparisons_tage[l,m] <- k 
            # Die Reihen in "adresse_comparisons_tage" geben an, in welcher Reihe der comp_probentag Matrix ein Signifikanzwert fuer die relevanten Probentypen steht
            # Die Spalten in "adresse_comparisons_tage" geben an, fuer welchen Probentyp das ist
            # Beispiel: Bei 3 unterschiedlichen angegebenen Probentypen stehen die fuer den 3. relevanten Probentyp in Spalte 3
            l <- l + 1
          }
        }
      }
      m <- 1
      for (zaehler_1 in ((length_sample_type_comparison-1):1)){
        for (zaehler_2 in (1:zaehler_1)){
          l <- 1
          for (k in (1:nrow_comp_probentyp_ohne_NA)){
            if (!(FALSE %in% (sample_type_comparison_pair[m,] %in% comp_probentyp[k,1:2]))){
              adresse_comparisons_typen[l,m] <- k
              # In den Zellen der Matrix "adresse_comparisons_typen" stehen die Reihen, in denen in der comp_probentag Matrix ein Signifikanzwert fuer die relevanten Probentypen steht
              # Die Spalten in "adresse_comparisons_typen" geben an, zwischen welchen Probentypen das ist
              # Beispiel: Bei 4 unterschiedlichen angegebenen Probentypen stehen die Signifikanzen zwischen 1. und 2. Probentyp in der ersten Spalte und zwischen 2. und 3. Probentyp in Spalte 4 (Spalte1: 1 zu 2, Spalte2: 1 zu 3, Spalte3: 1 zu 4, Spalte4: 2 zu 3, Spalte5: 2 zu 4, Spalte6: 3 zu 4)
              l <- l + 1
            }
          }
          m <- m+1
        }
      }

      # Ermittlung der Anzahl der Signifikanzklammern insgesamt 
      length_adresse_comparisons <- 0
      for (m in (1:length_sample_type_comparison)){
        if (!is.na(adresse_comparisons_tage[1,m])){
          length_adresse_comparisons <- length_adresse_comparisons + length(adresse_comparisons_tage[!is.na(adresse_comparisons_tage[,m]),m])
        }
      }
      for (m in (1:sum(1:(length_sample_type_comparison-1)))){
        if (!is.na(adresse_comparisons_typen[1,m])){
          length_adresse_comparisons <- length_adresse_comparisons + length(adresse_comparisons_typen[!is.na(adresse_comparisons_typen[,m]),m])
        }
      }

      comparisons_manuell <- matrix(ncol = 4, nrow = length_adresse_comparisons)

      #if Bedingung stellt sicher, dass 2 Zeilen in der Tabelle erstellt werden => ermoeglicht eine gute Darstellung auch wenn weniger Zeilen mit Werten gefuellt werden
      if (length_adresse_comparisons>1) {comparisons_table <- matrix(ncol = 3, nrow = length_adresse_comparisons)}
      else {comparisons_table <- matrix(ncol = 3, nrow = 2)}
      
      adress_of_all_displayed_values <- vector(mode = "numeric", length = 0)
      adresse1 <- 1
      adresse2 <- 0
      for (m in (1:length_sample_type_comparison)){
        adresse2 <- adresse2+length(zeilennummer_probentyp[zu_vergleichende_probentypen[m],which(!is.na(zeilennummer_probentyp[zu_vergleichende_probentypen[m],]))])
        adress_of_all_displayed_values[adresse1:adresse2] <- zeilennummer_probentyp[zu_vergleichende_probentypen[m],]
        adresse1 <- adresse2+1
      }

            empty_space <- ""
      l <- 1
      pos_value <- vector(mode = "numeric", length = dim(adresse_comparisons_tage)[2]) #Position der Klammer als Faktor (beginnt fuer alle Probentypen mit 1 und steigert sich dann unabhaengig voneinander um 0.25 je Klammer)
      scale_value <- vector(mode = "numeric", length = dim(adresse_comparisons_tage)[2]) #der maximale Wert
      for (m in (1:dim(adresse_comparisons_tage)[2])){
        scale_value[m] <- max(import_data[zeilennummer_probentyp[zu_vergleichende_probentypen[m],],substanz], na.rm = TRUE) # der maximale Wert der ueberhaupt ins Diagramm gezeichnet wird (Klammerzeichnung beginnt ueber dem maximalen Wert aller Probentypen)
      }
      for (m in (1:dim(adresse_comparisons_tage)[2])){
        pos_value[m] <- 1.2
        k <- 1
        ziel <- length(which(!is.na(adresse_comparisons_tage[,m])))
        while (k <= ziel){
          adresse_1 <- which(bedeutung_zeilennummer_beides == paste(comp_probentag[adresse_comparisons_tage[k,m],1],comp_probentag[adresse_comparisons_tage[k,m],3],sep = ""))
          adresse_2 <- which(bedeutung_zeilennummer_beides == paste(comp_probentag[adresse_comparisons_tage[k,m],2],comp_probentag[adresse_comparisons_tage[k,m],3],sep = ""))
          comparisons_manuell[l,1] <- paste(empty_space,as.character(comp_probentag[adresse_comparisons_tage[k,m],1]),empty_space,sep = "") # Name zur Zuweisung im Plotbefehl
          comparisons_manuell[l,2] <- paste(empty_space,as.character(comp_probentag[adresse_comparisons_tage[k,m],2]),empty_space,sep = "") # Name zur Zuweisung im Plotbefehl
          comparisons_manuell[l,3] <- as.character(wilcox.test(as.numeric(unlist(import_data[zeilennummer_beides[adresse_1,1:laenge_zeilennummer_beides[adresse_1]],substanz])),
                                                               as.numeric(unlist(import_data[zeilennummer_beides[adresse_2,1:laenge_zeilennummer_beides[adresse_2]],substanz])))[3]) # statistischer Test gibt in der dritten Spalte [3] den p-Wert aus 
          comparisons_manuell[l,4] <- pos_value[m]*max(scale_value)+scale_value[m]-max(scale_value) # Positionierung der Signifikanzklammer im Plot
          comparisons_table[l,1] <- paste(as.character(comp_probentag[adresse_comparisons_tage[k,m],3]),"_",as.character(comp_probentag[adresse_comparisons_tage[k,m],1]),sep = "") # Bezeichnung fuer die erste Spalte der Tabelle im GUI
          comparisons_table[l,2] <- paste(as.character(comp_probentag[adresse_comparisons_tage[k,m],3]),"_",as.character(comp_probentag[adresse_comparisons_tage[k,m],2]),sep = "") # Bezeichnung fuer die zweite Spalte der Tabelle im GUI
          comparisons_table[l,3] <- comparisons_manuell[l,3] # p-Wert fuer die Tabelle im GUI
          l <- l+1
          pos_value[m] <- pos_value[m] + 0.25
          k <- k + 1
        }
        #Um die Probentage trotz gleicher Bezeichnung nebeneinander darzustellen, wird eine steigende Anzahl an Leerzeichen vor und hinter die Tagesbezeichnung eingefuegt => empty_space
        #Beispiel: Probentyp 1 ist der erste Tag "1", fuer Probentyp 2 ist der dritte Tag " 3 ", fuer Probentyp 4 ist der vierte Tag "   4   "
        empty_space <-paste(empty_space," ", sep = "") 
      }
      pos_value2 <- max(pos_value)
      scale_value2 <- max(scale_value)
      zaehler <- 1 # Zaehler fuer die Spalten der Matrix adresse_comparisons_typen
      for (m in (1:(length_sample_type_comparison-1))){ # Einmal alle Probentypen (Bei 4 Probentypen laeuft die Schleife von 1 bis 4)
        for (i in ((m+1):length_sample_type_comparison)){ # Die Probentypen die damit interargieren koennen (Bei 4 Probentypen laeuft die Schleife von 2 bis 4 und 3 bis 4 und nur 4)
          k <- 1
          ziel <- length(which(!is.na(adresse_comparisons_typen[,zaehler])))
          while (k <= ziel){ # Die Zeilen der Matrix ohne NA
            empty_space1 <- ""
            empty_space2 <- ""
            o <- 1
            while (o < (m)){
              empty_space1 <- paste(empty_space1," ", sep = "")
              o <- o+1
            }
            for (o in (1:(i-1))){
              empty_space2 <- paste(empty_space2," ", sep = "")
            }

            adresse_1 <- which(bedeutung_zeilennummer_beides == paste(comp_probentyp[adresse_comparisons_typen[k,zaehler],3],comp_probentyp[adresse_comparisons_typen[k,zaehler],1],sep = ""))
            adresse_2 <- which(bedeutung_zeilennummer_beides == paste(comp_probentyp[adresse_comparisons_typen[k,zaehler],3],comp_probentyp[adresse_comparisons_typen[k,zaehler],2],sep = ""))
            comparisons_manuell[l,1] <- paste(empty_space1,as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],3]),empty_space1,sep = "") # Name zur Zuweisung im Plotbefehl
            comparisons_manuell[l,2] <- paste(empty_space2,as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],3]),empty_space2,sep = "") # Name zur Zuweisung im Plotbefehl
            comparisons_manuell[l,3] <- as.character(wilcox.test(as.numeric(unlist(import_data[zeilennummer_beides[adresse_1,1:laenge_zeilennummer_beides[adresse_1]],substanz])),
                                                                 as.numeric(unlist(import_data[zeilennummer_beides[adresse_2,1:laenge_zeilennummer_beides[adresse_2]],substanz])))[3]) # statistischer Test gibt in der dritten Spalte [3] den p-Wert aus 
            comparisons_manuell[l,4] <- pos_value2*scale_value2 # Positionierung der Signifikanzklammer im Plot
            comparisons_table[l,1] <- paste(as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],1]),"_",as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],3]),sep = "")
            comparisons_table[l,2] <- paste(as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],2]),"_",as.character(comp_probentyp[adresse_comparisons_typen[k,zaehler],3]),sep = "")
            comparisons_table[l,3] <- comparisons_manuell[l,3]
            pos_value2 <- pos_value2 + 0.25
            l <- l + 1
            k <- k + 1
          }
          zaehler = zaehler + 1
        }
      }
    }

    temp_adresse = vector(mode = "numeric")
    empty_space <- ""
    plot_data <- tibble()
    m <- 1
    
    # Die folgende Schleife bereitet die Daten fuer den plot-Befehl auf, => Die Messtage von verschiedenen Probentypen muessen unterschieden werden, dazu wird empty_space genutzt 
    for (i in (1:length_sample_type_comparison)){
      temp_adresse <- zeilennummer_probentyp[zu_vergleichende_probentypen[i],!is.na(zeilennummer_probentyp[zu_vergleichende_probentypen[i],])] #damit keine NAs als Daten an den plot Befehl uebergeben werden
      plot_data <- bind_rows(plot_data,tibble(as_tibble("",column_name = colnames(import_data[,1])),import_data[temp_adresse,2:length(import_data)])) # Die tibbles werden verbunden
      
      for (i in (1:length(temp_adresse))){
        plot_data[m,1] <- paste(empty_space,as.character(import_data[temp_adresse[i],1]),empty_space, sep = "") # die Werte der ersten Spalte werden mit hinzugefuegten Leerzeichen ueberschrieben
        m <- m+1
      }
      #Um die Probentage trotz gleicher Bezeichnung nebeneinander darzustellen, wird eine steigende Anzahl an Leerzeichen vor und hinter (damit steht der Text in der Mitte) die Tagesbezeichnung eingefuegt => empty_space
      #Beispiel: Probentyp 1 ist der erste Tag "1", fuer Probentyp 2 ist der dritte Tag " 3 ", fuer Probentyp 4 ist der vierte Tag "   4   "
      empty_space <- paste(empty_space," ", sep = "") #hier wird je Probentyp immer ein Leerzeichen angefuegt
    }
    
    colour_adress <- (1:length_sample_type_comparison)*anzahl_unterschiedlicher_werte_messtag
    x_pos <- (1:length_sample_type_comparison)*anzahl_unterschiedlicher_werte_messtag-anzahl_unterschiedlicher_werte_messtag/2+0.5
    y_pos <- min(plot_data[,substanz])-0.1*max(plot_data[,substanz])-min(plot_data[,substanz])

# plot command -------------------------------------------------------------

        plot <- ggboxplot(plot_data, x = colnames(plot_data[1]), y = colnames(import_data[substanz]), ylab = "Teilchen [MS-Einheit]", xlab = "Day",
                      title=paste("Vergleich der Probenahmetage fuer", colnames(import_data[substanz])," mit den Probetypen"), color = "black", palette = "npg") +
      geom_vline(xintercept = ((1:(length_sample_type_comparison-1))*anzahl_unterschiedlicher_werte_messtag)+0.5) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(fill = fill, alpha = alpha) +
      annotate("text", x = x_pos, y = y_pos , label = sample_type_comparison, parse = TRUE, colour = fill[colour_adress],fontface = 2, size = 9)
    if (input$draw_brackets == TRUE){
      if (!rlang::is_empty(comparisons_manuell)){
        if (!is.na(comparisons_manuell)){
          global.comparisons_manuell <<- comparisons_manuell
          comparisons_manuell[,3] <- format(round(as.numeric(comparisons_manuell[,3]),digits = pvalue_format), scientific = scientific)
          plot <- plot + stat_pvalue_manual(tibble(group1 = comparisons_manuell[,1],group2 = comparisons_manuell[,2], p = comparisons_manuell[,3]), label = "p", y.position = as.numeric(comparisons_manuell[,4]))
        }
      }
    }
    if (input$draw_violin_plots == TRUE){
      plot <- plot + geom_violin(fill = "red", alpha = alpha_violin, width = 1)
    }
    print(ggpar(plot, legend = "none"))
    global.p_values_vgl_beides <<- p_values_vgl_beides
    global.comparisons_table <<- comparisons_table#
    
    import_data <- import_data_safety
  })
  
  # Schreiben der p-values der Tabelle in eine Datei
  observeEvent(input$export, {
    write.csv(global.table,paste(export_directory,file_name,"_",input$gewaehlte_substanz,"_mit_",input$grenzwert,".csv",sep = ""))
    
  })
  
  output$export_table <- renderTable({
    #Damit dieser Schritt ausgefuehrt wird, muessen die Variablen aufgerufen werden, die Werte in der Tabelle veraendern
    reaktivitaet <- input$gewaehlte_substanz
    reaktivitaet <- input$grenzwert
    reaktivitaet <- input$draw_violin_plots
    reaktivitaet <- input$draw_brackets
    scientific <- input$scientific
    pvalue_format <- input$pvalue_format
    normalize_data <- input$normalize_data
    time_point_to_normalize_on <- input$time_point_to_normalize_on
    pre_table <- global.comparisons_table
    
    if (pvalue_format <= 0){
      pre_table[,3] <- format(as.numeric(pre_table[,3]), scientific = scientific)
    }
    else {
      pre_table[,3] <- format(round(as.numeric(pre_table[,3]),digits = pvalue_format), scientific = scientific)
    }
    colnames(pre_table) <- c("Type_Day1", "Type_Day2", "p-value")
    #browser()
    #table <- xtable(pre_table[,1:3])
    table <- pre_table[,1:3]
    global.table <<- table
  })

# 3.2.2 Scatterplot -------------------------------------------------------------

  output$scatterplot3d <- renderPlotly({
    
    achse1 <- which(colnames(import_data) == as.character(input$scatter_substanz1))
    achse2 <- which(colnames(import_data) == as.character(input$scatter_substanz2))
    achse3 <- which(colnames(import_data) == as.character(input$scatter_substanz3))
    scatter_dots <- which(colnames(import_data) == as.character(input$scatter_dots))
    
    # if the data is not numeric, but of string type (for example "infected"/"control") then the data has to be treated as string
    if (any(is.na(as.numeric(unlist(import_data[,achse1]))))){
      x_plot <- unlist(import_data[,achse1])
    }
    else{ # if the data is numeric it will be treated as numeric and therefor displayed with color gradient
      x_plot <- as.numeric(unlist(import_data[,achse1]))
    }
    
    # if the data is not numeric, but of string type (for example "infected"/"control") then the data has to be treated as string
    if (any(is.na(as.numeric(unlist(import_data[,achse2]))))){
      y_plot <- unlist(import_data[,achse2])
    }
    else{ # if the data is numeric it will be treated as numeric and therefor displayed with color gradient
      y_plot <- as.numeric(unlist(import_data[,achse2]))
    }
    
    # if the data is not numeric, but of string type (for example "infected"/"control") then the data has to be treated as string
    if (any(is.na(as.numeric(unlist(import_data[,achse3]))))){
      z_plot <- unlist(import_data[,achse3])
    }
    else{ # if the data is numeric it will be treated as numeric and therefor displayed with color gradient
      z_plot <- as.numeric(unlist(import_data[,achse3]))
    }
    
    # if the data is not numeric, but of string type (for example "infected"/"control") then the data has to be treated as string
    if (any(is.na(as.numeric(unlist(import_data[,scatter_dots]))))){
      color_plot <- unlist(import_data[,scatter_dots])
    }
    else{ # if the data is numeric it will be treated as numeric and therefor displayed with color gradient
      color_plot <- as.numeric(unlist(import_data[,scatter_dots]))
    }
   
    plot3d <- plot_ly(import_data,x = x_plot, y = y_plot, z = z_plot,
                      color = color_plot, colors = "Set1")
    plot3d <- add_markers(plot3d)
    plot3d <- layout(plot3d, scene = list(xaxis = list(title = paste("x-Achse:",input$scatter_substanz1)),
                                          yaxis = list(title = paste("y-Achse:",input$scatter_substanz2)),
                                          zaxis = list(title = paste("z-Achse:",input$scatter_substanz3))))
    })
  
}

shinyApp(ui,server)