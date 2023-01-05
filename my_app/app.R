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
      
      
    )
    
  )
  
  
)