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






############partieChaimae
  
  
  # Fonctions qui gére les classifications
  
  ################ random forest ################
  random_frst <- function(arg=NULL){
    
    #make this example reproducible
    set.seed(1)
    
    #Use 80% of dataset as training set and remaining 30% as testing set
    sample <- sample(c(TRUE, FALSE), nrow(data$table), replace=TRUE, prob=c(0.8,0.2))
    ###traitement donnees manquante pour rdf
    train <- data$table[sample, ]
    test <- data$table[!sample, ]
    df.rf=randomForest(as.formula(paste(input$variabletarget," ~ .")),train,importance=TRUE)
    
    
    
    
    
    predict_rdf <- predict(df.rf,test,type='class')
    
    predict_rdf=round(predict_rdf,0)
    
    
    table = table(test[, input$variabletarget], predict_rdf)
    
    
    Precision = table[1, 1] / sum(table[,1])
    
    Recall = table[1, 1] / sum(table[1,])
    Specificity = table[2, 2] / sum(table[2,])
    
    f1 = 2 * Precision * Recall / (Precision + Recall)
    
    missing_classerr <- mean(predict_rdf != test[, input$variabletarget])
    Accuracy = 1 - missing_classerr
    
    if (is.null(arg)){
      
      tablo <- data.frame(table(test$target, predict_rdf))
      colnames(tablo)=c("Prediction","REEL","N")
      ggplot(data = tablo , mapping = aes(x = Prediction , y =REEL )) +
        geom_tile(aes(fill = N), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", N)), vjust = 1) +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_bw() + theme(legend.position = "none")
    }
    
    else if (arg==1){data.frame(Accuracy, Precision, Recall, f1, Specificity)}
    
    else if (arg==2) { 
      # ROC-AUC Curve
      ROCPred <- prediction(predict_rdf, test[, input$variabletarget])
      ROCPer <- performance(ROCPred, measure = "tpr",x.measure = "fpr")
      
      auc <- performance(ROCPred, measure = "auc")
      auc <- auc@y.values[[1]]
      auc
      
      # Plotting curve
      plot(ROCPer, colorize = TRUE, 
           print.cutoffs.at = seq(0.1, by = 0.1), 
           main = "ROC CURVE")
      abline(a = 0, b = 1)
      
      auc <- round(auc, 4)
      legend(.6, .4, auc, title = "AUC", cex = 1)
      
    }
    
    else {
      {
        # Features Importance using the package vip
        
        fi_lr <- varImpPlot(df.rf)
        print(fi_lr)
        p1 <- vip(df.rf) + ggtitle("random forest")
        # Display plots in a grid
        grid.arrange(p1)
      }
    }
  }
  
  output$lr <- renderPlot({
    random_frst(arg = NULL)
  })
  output$lr2 <- renderTable({
    random_frst(arg = 1)
  })
  output$lr3 <- renderPlot({
    random_frst(arg=2)
  })
  output$lr4 <- renderPlot({
    random_frst(arg=3)
  }) 
  
}