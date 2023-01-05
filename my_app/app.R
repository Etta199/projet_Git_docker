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
      
      
      
      
    )
    
  )
  
  
)