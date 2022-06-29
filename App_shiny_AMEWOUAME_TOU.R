
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(haven)
library(tidyverse)
library(nycflights13)
library(expss)
library(visdat)
library(dplyr)
library(questionr)
library(shinythemes)

#Section 14a
section14a <- read_dta("C:\\Users\\DELL\\Documents\\ENSAE\\Cours ISEP 2\\Semestre 2\\Programmation avec R\\Projet_R\\Bases_données\\s14a_me_SEN_2021.dta")
new_name <- c(colnames(section14a)[1], colnames(section14a)[2],colnames(section14a)[3],
              colnames(section14a)[4], 'repondant', colnames(section14a)[6], 'event',
              'event_occur', 'nbr_pers', 'solution', 'event_time', colnames(section14a)[12],
              'menage_member', 'num_ordre', 'sexe', 'age', 'relationship',
              'resultat_interview', 'motif','langue','quiz_result', 'obs')

colnames(section14a) <- new_name
data14a <- select(section14a, interview__id, id_menage, event, event_occur, nbr_pers, num_ordre, menage_member, sexe, age, relationship, resultat_interview, solution, event_time)
data14a = apply_labels(data14a, 
                       interview__id = 'clé_interview',
                       id_menage = 'identité de ménage',
                       event = c(' Eté malade de Coronavirus' = 1,'Réduit le temps de travail à cause du coronavirus' = 2,
                                 'Subi le recul de transferts de parents vivant à l étranger du fait du coronavirus' =3,
                                 ' Subi le recul de transferts de parents vivant dans le pays du fait du coronavirus' = 4,
                                 'Subi la mévente de produits de l agriculture du fait du coronavirus' = 5,
                                 'Renoncer à une visite à des parents ou amis dans le pays du fait du coronavirus' = 6,
                                 'Renoncer à une visite à des parents ou amis hors du pays du fait du coronavirus'= 7,
                                 'Renoncer à un voyage d affaires dans le pays du fait du coronavirus' = 8,
                                 'Renoncer à un voyage d affaires hors du pays du fait du coronavirus' = 9),
                       event_occur = c('oui' = 1, 'non' = 2),
                       nbr_pers = 'nombre de personnes ayant subi impact',
                       num_ordre = 'numéro d ordre',
                       menage_member = c('oui' = 1, 'non' = 2),
                       sexe = 'quel est son sexe',
                       age = 'quel est son age',
                       relationship = 'lien de parenté avec le chef menage',
                       solution = c('oui' = 1, 'non' = 2),
                       event_time = 'combien de temps cela a duré')

data14a <- distinct(data14a)
data14a <- data14a %>% filter(!is.na(event_occur))
data14a <- select(data14a, -sexe, -age, -relationship, -id_menage, -resultat_interview)


# Section 14b

section14b <- read_dta("C:\\Users\\DELL\\Documents\\ENSAE\\Cours ISEP 2\\Semestre 2\\Programmation avec R\\Projet_R\\Bases_données\\s14b_me_SEN_2021.dta")
new_name <- c(colnames(section14b)[1], colnames(section14b)[2],colnames(section14b)[3],
              colnames(section14b)[4], 'repondant', colnames(section14b)[6], 'event','event_occur', 'date', 'annee', 'revenu', 'avoirs', 'prod_agri', 'cheptel', 'stock_aliment', 'achat_aliment', 'epargne', 'aide_parent', 'aide_gouv', 'aide_ONG', 'marier_une_fille', 'change_habit', 'cheaper_food', 'extra_job', 'emploi_chomeur', 'emploi_enfant', 'descolarisation_enfant', 'migration', 'reduction_depenses', 'credit', 'vente_actif_agric', 'vente_bien_durable', 'vente_terrain', 'louer', 'vente_vivres', 'peche', 'ente_betail', 'confiage_enfant', 'activite_spirituel', 'culture_contre_saison', 'autre_statégie', 'aucune_strategie', 'strategie', 'resultat_interview','motif','langue','quiz_result','obs')
colnames(section14b) <- new_name

data14b <- select(section14b, -grappe, -vague, -repondant, -interview__key, -resultat_interview, -motif, -langue, -quiz_result, -obs)
data14b <- data14b %>% filter(!is.na(event_occur))



# Define UI for application that draws a histogram
ui <- fluidPage(
  h2("Application shiny montrant les résultats de l'analyse sur les impacts du covid sur les ménages"),
  h3("Auteurs: AMEWOUAME Elisée et TOU Brahima"),
  #theme = shinytheme(theme = 'slate'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('var', 'choisissez une variable', choices = colnames(data14a))
      
    ),
    
    mainPanel(
      tabsetPanel( tabPanel('Section 14a', DTOutput('data14a')),
                   
                   tabPanel('Statistiques', verbatimTextOutput('summary')),
                   tabPanel('Graphes', plotOutput('barplot'))
      )
    )
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),

  

)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$data14a <- renderDT({
    data14a
  })
  
  # Résumé statistique
  
  output$summary <- renderPrint({
    summary(data14a)
  })
  
  # Graphe
  output$barplot <- renderPlot({
    barplot(table(data14a[, input$var]), col = c("lightblue","red" ))
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


