# Importation des packages utilisés
library(shiny)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(ggplot2)
library(fastDummies)
library(DT)
library(dplyr)
library(randomForest)
library(rpart)
library(klaR)
library(e1071)
library(MASS)
library(xtable)
library(ROCR)
library(caret)
library(DataExplorer)
library(shinyWidgets)
library(FSinR)
library(caTools)
library(FNN)
library(mgcv)
library(vip)
library(shinythemes)
library(e1071)
library(imputeTS)



# Création de  d'un menu latéral qui s'affiche/se cache à l'aide d'un bouton. 
# Il se compose de 3 parties, Une partie pour la lecture et visualization de donnés
# Une deuxiémme pour l'analyse de données
# et une troisième partie conistant à l'entrainement de modèles pour faire la prédiction.

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis application"),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization" ),
      menuItem("Modèles de classification supervisée", tabName ="prediction")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 1. Lecture de la donnée
      tabItem(tabName = "readData",
              h1("Lecture des données"),
              
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              fluidRow(
                column(3,
                       h3("Parameters"),
                       
                       # Choix si la data contient une en tete
                       radioButtons(inputId = "header", 
                                    label = "Header",
                                    choices = c("Yes" = TRUE,
                                                "No" = FALSE),
                                    selected = TRUE, inline=T),
                       
                       # # Choix du séparateur
                       radioButtons(inputId = "sep", 
                                    label = "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = "\t", inline=T),
                       
                       # Choix de la quote
                       radioButtons(inputId = "quote", 
                                    label= "Quote",
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = "", inline=T),
                       
                       
                       
                       
                       
                ),
                # Une preview de la donnée séléctionnée
                column(9,
                       h3("File preview"),
                       dataTableOutput(outputId = "preview")
                )
              ), 
              # Button permettant de valider le choix de la donnée et
              # déclenchant le reste des analyses.
              div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center"),
              
              
              
              
      ),
      #2. Visualization des tables de l'analyse de donnés
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              
              
              mainPanel(
                tabsetPanel(
                  tabPanel( title = "visualisation",uiOutput("myoutput")
                  ),
                  # Résumé de la donnée
                  tabPanel(title ="summary",
                           verbatimTextOutput("summary")
                  ),
                  # Pourcentage des valeurs manquantes
                  tabPanel( "pourcentage de données manquantes" 
                            ,plotOutput(outputId="NAN")),
                  # Distribution des variables
                  tabPanel("Variables qualitatives",plotOutput(outputId="Plotbar")),
                  tabPanel("variables quantitatives",plotOutput(outputId="Plothisto")),
                  
                  # Corrélation entre les variales quantitatives
                  tabPanel("correlation",plotOutput(outputId="PlotCorrQuatita")),
                  # AOVA pour les variables qualitatives
                  tabPanel("ANOVA", sidebarLayout(
                    sidebarPanel(
                      selectInput(
                        inputId = "variable",
                        label = "Enter la variable ",
                        choices=NULL
                        
                      ),
                      actionButton(
                        inputId = "soumettre",
                        label = "soumettre"
                      )
                    ),
                    mainPanel(  
                      plotOutput(outputId="PlotBot")
                    )
                  ))
                  
                )),
              
              
              
              
      ),  
      # 3. Volets Prédictions : Entrainement et affichage de résultats pour  modèles :
      tabItem(tabName = "prediction",
              h1("Modeles de predictions"),
              selectInput(
                # Tout d'abord, choix de la variable target à utiliser
                inputId = "variabletarget",
                label = "Enter la variable target ",
                choices=NULL
                
              ),
              # Button pour soumettre le choix de la variable
              actionButton(
                inputId = "soumettre1",
                label = "soumettre"
              ), 
              
              tabPanel("Prediction", 
                       navbarPage("Models",
                                  # 3.1 Modèle Decision Tree
                                  tabPanel("Decision Tree", 
                                           fluidRow(
                                             #Matrice de confusion
                                             column(6, h3("Matrice de confusion"),
                                                    plotOutput(outputId="dt")),
                                             
                                             column(6, h3("Métriques d'évaluation"),
                                                    tableOutput(outputId = "dt2")
                                             )
                                           ),
                                           # Courbe ROC
                                           fluidRow(
                                             column(8, h3('Courbe ROC'),
                                                    plotOutput(outputId = "dt3")
                                             )
                                             
                                           ),
                                           # Importance de variables
                                           fluidRow(
                                             column(8, h3('Features Important'),
                                                    plotOutput(outputId = "dt4")
                                             )
                                           )  
                                  ),
                                  # 3.2 Random Forest
                                  tabPanel("random forest", 
                                           fluidRow(
                                             column(6, h3("Matrice de confusion"),
                                                    plotOutput(outputId="lr")),
                                             
                                             column(6, h3("Métriques d'évaluation"),
                                                    tableOutput(outputId = "lr2")
                                             )
                                           ),
                                           fluidRow(
                                             column(8, h3('Courbe ROC'),
                                                    plotOutput(outputId = "lr3")
                                             )
                                             
                                           ),
                                           fluidRow(
                                             column(8, h3('Features Important'),
                                                    plotOutput(outputId = "lr4")
                                             )
                                             
                                           )
                                  ),
                                  # 3.3 Modèle Naive Bayes
                                  tabPanel("Naive Bayes", 
                                           fluidRow(
                                             column(6, h3("Confusion Matrix"),
                                                    plotOutput(outputId="nb")),
                                             
                                             column(6, h3('Evaluation Metrics'),
                                                    tableOutput(outputId = "nb2")
                                             )
                                           ),
                                           fluidRow(
                                             column(8, h3('ROC Curve'),
                                                    plotOutput(outputId = "nb3")
                                             )
                                             
                                           )
                                  )
                       )
              )        
              
              
              
      )
      
      
    )
    
  )
  
  
)

server <- function(input, output,session) {
  
  
  
############partieWessal


  data <- reactiveValues()
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   stringsAsFactors = TRUE,
                   nrows=10
    )
    
  },  options = list(scrollX = TRUE , dom = 't'))
  
  oko= reactiveValues(types = list())
  typesAjour=reactiveValues(types=list())
  lestypes = reactiveValues(types = c()) 
  
  ##########################################################################################
  output$files <- renderTable({head(data$table)})
  output$tabType<-renderTable({Types()})
  
  
  Types<-reactiveVal({NULL})
  
  type_stat_f<-function(x){
    
    
    newtype<-c()
    k=1
    for(i in x){
      if(i=="numeric"){
        oko$types[k]="quantitative continue"
        newtype<-c(newtype,"quantitative continue")
        
      } else {
        oko$types[k]="qualitative nominale"
        newtype<-c(newtype,"qualitative nominale")
      }
      k=k+1
    }
    newtype
  }
  
  # L'execution de pluesieurs action après la validation du choix de la données :
  observeEvent(input$actBtnVisualisation, {
    
    if(!is.null(input$dataFile$datapath)){
      # Lecture de données et son stockage dans une variable
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote)
      #Affichage d'un message de réussite
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien été lu !",
        type = "success"
      )
      
      #visualisation
      updateTabItems(session, "tabs", selected = "visualization")
      ## Choix le nom des variables à considérer pour l'ANOVA
      updateSelectInput(inputId = "variable",choices = colnames(data$table))
      ## Choix le nom des variables à considérer pour la variable target
      updateSelectInput(inputId = "variabletarget",choices = colnames(data$table))
      Type<-sapply(data$table, class)
      lestypes$types<- type_stat_f(Type)
    }
    
    #############################
    
    # Partie responsable de l'actualisation des types de variables en fonction 
    # des choix de l'utilisateur
    nomvar<-reactiveValues(names=c())
    Types<-reactiveVal({NULL})
    observeEvent(input$dataFile, {
      req(input$dataFile)
      d1<-as.data.frame(data$table)
      Nom_variable<-c()
      Type<-sapply(d1, class)
      for(i in colnames(d1)){
        Nom_variable<-c(Nom_variable,i)
      }
      typest<-lestypes$types
      
      selecttype<-c()
      k=1
      for(ii in typest){
        
        selecttype<-c(selecttype,as.character(selectInput(inputId=paste0("row_select_", k), label=NULL,selected = ii, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))))
        k=k+1
      }
      d2<-data.frame(Nom_variable,Type)
      setDT(d2)
      d2<-data.frame(d2,lestypes$types,selecttype)
      Types(d2)
    })
    
    observeEvent(input$sauvegarde, {
      rvs$observers = lapply(
        1, 
        function(i) {
          observeEvent(input[["sauvegarde"]], 
                       changerr()
          )
        }
      )
    })
    ##########################################################################################
    
    
    
    output$myTableOutput <- DT::renderDataTable({
      datatable(as.data.table(Types()), escape = FALSE, options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
      ))
    })
    
    
    ###
    
    rvs = reactiveValues(buttons = list(), observers = list())
    
    observeEvent(input$sauvesgarde,{
      print("-------------")
      print(input$row_select_1)
      print("-------------")
      
    }
    )
    
    # fonction responsable d'actualiser la base de données (en mode reactive)
    changerr<-reactive({
      
      d1<-as.data.frame(data$table)
      d2<-data$table
      nom_col<-colnames(data$table)
      Nom_variable<-c()
      Type<-sapply(d1, class)
      for(i in colnames(d1)){
        Nom_variable<-c(Nom_variable,i)
      }
      
      compteur=1
      for(o in seq_len(length(lestypes$types))){
        
        
        observeEvent(input[[paste0("row_select_", compteur)]],{
          
          
          if(is.null(input[[paste0("row_select_", compteur)]])==FALSE){
            
            oko$types[[compteur]]=input[[paste0("row_select_", compteur)]]
            lestypes$types[[compteur]]<-input[[paste0("row_select_", compteur)]]
            
            typesAjour$types[[compteur]]=input[[paste0("row_select_", compteur)]]
            
            if(input[[paste0("row_select_", compteur)]]=="qualitative nominale" || input[[paste0("row_select_", compteur)]]=="qualitative ordinal" ){
              as.factor(d2[[nom_col[compteur]]])
              
            }
            
            
          }
        })
        compteur=compteur+1
        
        data$table = as.data.frame(d2)
        
        
      }
      data(d2)
      
      
      typest<-lestypes$types
      
      selecttype<-c()
      k=1
      for(ii in typest){
        selecttype<-c(selecttype,as.character(selectInput(inputId=paste0("row_select_", k), label=NULL,selected = ii, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))))
        k=k+1
      }
      d2<-data.frame(Nom_variable,Type)
      setDT(d2)  
      
      d2<-data.frame(d2,lestypes$types,selecttype)
      Types(d2)
    })
    
    
    oko= reactiveValues(types = list())
    typesAjour=reactiveValues(types=list())
    
    observeEvent(input$sauvegarde,{
      print(oko$types)
      nom_col<-colnames(data$table)
      d2<-as.data.frame(data$table)
      compteur=1
      for(i in seq_len(length(lestypes$types))){
        print(input[[paste0("row_select_", compteur)]])
        if(is.null(input[[paste0("row_select_", compteur)]])==FALSE){
          
          if( lestypes$types[[compteur]]!=input[[paste0("row_select_", compteur)]]){
            if(input[[paste0("row_select_", compteur)]]=="qualitative nominale" || input[[paste0("row_select_", compteur)]]=="Qualitative ordinal" ){
              d2[ , nom_col[1]] <- lapply(d2[ , nom_col[1]] , factor)
              
            } 
            
            if(input[[paste0("row_select_", compteur)]]=="quantitative continue" || input[[paste0("row_select_", compteur)]]=="quantitative discrete" ){
              d2[ , nom_col[1]] <- lapply(d2[ , nom_col[1]] , numeric)
              
            } 
            
          }
          
          
          oko$types[[compteur]]=input[[paste0("row_select_", compteur)]]
          lestypes$types[[compteur]]<-input[[paste0("row_select_", compteur)]]
          typesAjour$types[[compteur]]=input[[paste0("row_select_", compteur)]]
          
          
          
          
          
          
        }
        compteur=compteur+1
      }
      
      
      data(d2)
      
      
      rvs$observers = lapply(
        1, 
        function(i) {
          observeEvent(input[["sauvegarde"]], 
                       changerr()
          )
        }
      )
      
      
      
    }
    )
    
    
    ####
    
    observeEvent(input$sauvegarde, {
      rvs$observers = lapply(
        1, 
        function(i) {
          #button pour valider le choix des types (et declencher le ajustements)
          observeEvent(input[["sauvegarde"]], 
                       changerr()
          )
        }
      )
    })
    
    # La partie qui regrooupe la visualisation des types, le choix et le button pour le sauvegarde
    output$myoutput <- renderUI({
      mainPanel(
        tabsetPanel(
          tabPanel("TYPES DES VARIABLES",
                   dataTableOutput('myTableOutput'),
                   htmlOutput("mySelection"),
                   actionButton(inputId = "sauvegarde", label = "save")
          ),))
      
      
    })
    
    # lire la données
    output$contents <- renderDataTable({data$table})
    ##############################
    dt<- data$table
    col_names <- sapply(dt, function(col)  length(unique(col)) < 10)
    dt[ , col_names] <- lapply(dt[ , col_names] , factor)
    output$summary <- renderPrint({summary(dt)
    })
    
    
    
    ############################# 
    
    # Plot de différents graphes pour la partie analyse de données
    output$NAN<-renderPlot(plot_missing(data$table))
    
    output$Plotbar<-renderPlot(plot_bar(data$table))
    
    output$Plothisto<-renderPlot(plot_histogram(data$table) )
    
    output$PlotCorrQuatita<-renderPlot(plot_correlation(data$table,
                                                        cor_args = list("use" = "pairwise.complete.obs"), type="c")) 
    
    output$PlotBot<-renderPlot(plot_boxplot(data$table, by= input$variable))    
  })   



############partieChaimae
  
  
  
}