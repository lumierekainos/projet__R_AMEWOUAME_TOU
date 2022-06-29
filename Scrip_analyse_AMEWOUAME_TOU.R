#Bibliothèques
library(haven)
library(tidyverse)
library(nycflights13)
library(expss)
library(visdat)
library(dplyr)
library(questionr)
library(labelled)
#                  Analyse de la section 14A


filter <- filter(data14a, event == "Réduit le temps de travail à cause du coronavirus")
freq(filter$event_occur,valid = FALSE, total = TRUE, sort= "dec")

p = ggplot(filter) + geom_bar(aes(x = event_occur), fill = c("blue","red"))

p <- p + labs(title = "Proportion des ménages selon ceux qui ont ", subtitle = "réduit leur temps de travail ou non",
              caption = "Data source: EHCVM 2021")
p




filter <- filter(data14a, event == "Réduit le temps de travail à cause du coronavirus", event_occur == "Oui") # Je sélectionne ceux qui ont réduit leur temps de travail
freq(filter$solution,valid = FALSE, total = TRUE, sort= "dec")

filter <- filter(data14a, event == "Réduit le temps de travail à cause du coronavirus", event_occur == "Oui", solution =="Solution trouvée" )
freq(filter$event_time,valid = FALSE, total = TRUE, sort= "dec")

filter <- filter(data14a, event == "Subi la mévente de produits de l'agriculture du fait du coronavirus")

freq(filter$event_occur,valid = FALSE, total = TRUE, sort= "dec")


p = ggplot(filter) + geom_bar(aes(x = event_occur), fill = c("blue","red"))

p <- p + labs(title = "Proportion des ménages selon ceux qui ont subi la mévente des produits de l'agriculture",
              caption = "Data source: EHCVM 2021")
p

filter <- filter(data14a, event =="Subi la mévente de produits de l'agriculture du fait du coronavirus" , event_occur == "Oui") # Je sélectionne ceux qui ont subi la mévente des produits de l'agriculture

freq(filter$solution,valid = FALSE, total = TRUE, sort= "dec")

filter <- filter(data14a, event == "Subi la mévente de produits de l'agriculture du fait du coronavirus", event_occur == "Oui", solution =="Solution trouvée" )
freq(filter$event_time,valid = FALSE, total = TRUE, sort= "dec")

filter <- filter(data14a, event == "Renoncer à un voyage d'affaires hors du pays du fait du coronavirus")
freq(filter$event_occur,valid = FALSE, total = TRUE, sort= "dec")

p = ggplot(filter) + geom_bar(aes(x = event_occur), fill = c("blue","red"))

p <- p + labs(title = "Proportion des ménages selon ceux qui ont",
              subtitle = "renoncé à un voyage d'affaires hors du pays",
              caption = "Data source: EHCVM 2021")
p

filter <- filter(data14a, event == "Renoncer à un voyage d'affaires hors du pays du fait du coronavirus", event_occur == "Oui") # Je sélectionne ceux qui ont renoncé à un voyage d'affaire à l'étranger

freq(filter$solution,valid = FALSE, total = TRUE, sort= "dec")

filter <- filter(data14a, event == "Renoncer à un voyage d'affaires hors du pays du fait du coronavirus", event_occur == "Oui", solution =="Solution trouvée")

freq(filter$event_time,valid = FALSE, total = TRUE, sort= "dec")

filter1 = filter(data14a, event =="Eté malade de Coronavirus") 
freq(filter1$event_occur,
     valid = TRUE, 
     total = TRUE, 
     sort = "dec")


tab<-table(data14a$event, data14a$event_occur) #Tableau croisé
addmargins(prop.table(addmargins(tab,1),1),2)*100

p = ggplot(data14a) + geom_bar(aes(y = event, fill = event_occur))

p <- p + labs(title = "Proportion des ménages",
              subtitle = "selon chaque type d'impact",
              caption = "Data source: EHCVM 2021")




#                  Analyse de la section 14B

#Importation de la section0

section0 <- read_dta("C:\\Users\\DELL\\Documents\\ENSAE\\Cours ISEP 2\\Semestre 2\\Programmation avec R\\Projet_R\\Bases_données\\s00_me_SEN_2021.dta")

# Sélection des variables utiles
data0 <- select(section0, interview__id, s00q01, s00q04)

#Renommons les variables 

new_name <- c("interview__id", "region", "residence")
colnames(data0) <- new_name  #Remplacement 

#Labelisation des variables

data0[c("interview__id", "region", "residence")] <- to_factor(data0[c("interview__id", "region", "residence")])

#visualisation de la base
head(data0)
# Fusion des bases suivant la clé 'interview__id'
fusion<- merge(data14b,data0, by = 'interview__id')
head(fusion)

choc <- filter(fusion, event_occur == "Oui") #Nous constituons une base formée uniquement de ceux qui ont subi au moins un choc
head(choc)

#Nous allons constituer la base contenant unique les colonnes 'interview__id', 'event_occur', 'region' et 'residence'
data1 <- select(choc, interview__id, event_occur, region, residence)
head(data1)

data1 <- distinct(data1)
dim(data1)

freq(data1$region, valid = TRUE, total = TRUE)

data1.sub <- data1 %>% filter(!is.na(region))
p = ggplot(data1.sub) + geom_bar(aes(y = region), fill = 'blue')

p <- p + labs(title = ": Proportion des ménages affectés par un problème selon le milieu de résidence",
              caption = "Data source: EHCVM 2021")
p

freq(data1$residence, valid = TRUE, total = TRUE)

data1.sub <- data1 %>% filter(!is.na(residence))
p = ggplot(data1.sub) + geom_bar(aes(x = residence), fill = c('blue', 'red'))

p <- p + labs(title = ": Proportion des ménages affectés par un problème selon le milieu de résidence",
              caption = "Data source: EHCVM 2021")
p

fusion <- fusion %>% filter(!is.na(revenu))
vis_miss(fusion)

filtre <- filter(fusion, event_occur =="Oui") #Je sélectionne uniquement ceux qui ont subi un impact
freq(filtre$revenu, valid = TRUE, total = TRUE)

p = ggplot(filtre) + geom_bar(aes(x = revenu), fill = 'blue', na.rm = FALSE)

p <- p + labs(title = "Proportion dess ménages selon l'effet du choc sur les revenus",
              caption = "Data source: EHCVM 2021")
p

filtre2 <- filter(fusion, event_occur =="Oui", revenu == "Diminué") #Je sélectionne uniquement ceux qui ont subi un impact

freq(filtre2$residence, valid = TRUE, total = TRUE)

filtre2.sub <- filtre2 %>% filter(!is.na(residence))
p = ggplot(filtre2.sub) + geom_bar(aes(x = residence), fill = 'blue', na.rm = FALSE)

p <- p + labs(title = "Proportion des ménages qui ont connu une baisse du revenu selon le milieu de résidence",
              caption = "Data source: EHCVM 2021")
p

filtre2 <- filter(fusion, event_occur =="Oui", revenu == "Diminué") #Je sélectionne uniquement ceux qui ont subi un impact

freq(filtre2$region, valid = TRUE, total = TRUE)

filtre2.sub <- filtre2 %>% filter(!is.na(region))
p = ggplot(filtre2.sub) + geom_bar(aes(y = region), fill = 'blue', na.rm = FALSE)

p <- p + labs(title = "Proportion des ménages qui ont connu une baisse du revenu selon le milieu de résidence",
              caption = "Data source: EHCVM 2021")
p

freq(filtre$avoirs, valid = TRUE, total = TRUE)

p = ggplot(filtre) + geom_bar(aes(x = avoirs), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon l’effet du choc sur les avoirs",
              caption = "Data source: EHCVM 2021")
p

freq(filtre$prod_agri, valid = TRUE, total = TRUE)

filtre.sub <- filtre %>% filter(!is.na(prod_agri))
p = ggplot(filtre.sub) + geom_bar(aes(x = prod_agri), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon l’effet du choc sur la production agricole",
              caption = "Data source: EHCVM 2021")
p

freq(filtre$cheptel, valid = TRUE, total = TRUE)

filtre.sub <- filtre %>% filter(!is.na(cheptel))
p = ggplot(filtre.sub) + geom_bar(aes(x = cheptel), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon l’effet du choc sur l'effectif du cheptel",
              caption = "Data source: EHCVM 2021")
p
freq(filtre$stock_aliment, valid = TRUE, total = TRUE)

filtre.sub <- filtre %>% filter(!is.na(stock_aliment))
p = ggplot(filtre.sub) + geom_bar(aes(x = stock_aliment), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon l’effet du choc sur le stock de produits alimentaires",
              caption = "Data source: EHCVM 2021")
p

freq(filtre$achat_aliment, valid = TRUE, total = TRUE)

filtre.sub <- filtre %>% filter(!is.na(achat_aliment))
p = ggplot(filtre.sub) + geom_bar(aes(x = achat_aliment), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon l’effet du choc sur l'achat des produits alimentaires",
              caption = "Data source: EHCVM 2021")
p

values <- c() #Nous allons construire un vecteur qui contient le nombre de ménages qui ont opté pour la modalité 1 pour chaque stratégie 

values = append(values, nrow(filter(fusion, epargne ==1)))
values = append(values, nrow(filter(fusion, aide_parent ==1)))
values = append(values, nrow(filter(fusion, aide_gouv ==1)))
values = append(values, nrow(filter(fusion, aide_ONG ==1)))
values = append(values, nrow(filter(fusion, marier_une_fille ==1)))
values = append(values, nrow(filter(fusion, change_habit ==1)))
values = append(values, nrow(filter(fusion, cheaper_food ==1)))
values = append(values, nrow(filter(fusion, extra_job ==1)))
values = append(values, nrow(filter(fusion, emploi_chomeur ==1)))
values = append(values, nrow(filter(fusion, emploi_enfant ==1)))
values = append(values, nrow(filter(fusion, descolarisation_enfant ==1)))
values = append(values, nrow(filter(fusion, migration ==1)))
values = append(values, nrow(filter(fusion, reduction_depenses ==1)))
values = append(values, nrow(filter(fusion, credit ==1)))
values = append(values, nrow(filter(fusion, vente_actif_agric ==1)))
values = append(values,  nrow(filter(fusion, vente_bien_durable ==1)))
values = append(values,  nrow(filter(fusion, vente_terrain ==1)))
values = append(values,  nrow(filter(fusion, louer ==1)))
values = append(values,  nrow(filter(fusion, vente_vivres ==1)))
values = append(values,  nrow(filter(fusion, peche ==1)))
values = append(values,  nrow(filter(fusion, ente_betail ==1)))
values = append(values,  nrow(filter(fusion, confiage_enfant ==1)))
values = append(values,  nrow(filter(fusion, activite_spirituel ==1)))
values = append(values,  nrow(filter(fusion, culture_contre_saison ==1)))
values = append(values,  nrow(filter(fusion, autre_statégie ==1)))
values = append(values,  nrow(filter(fusion, aucune_strategie ==1)))


values <- values*100/length(unique(fusion$interview__id)) #Je calcule les pourcentages

length(values)

group <- colnames(data14b)[12:37] 
mydata <- data.frame(strategie = group, n= values)

mydata  #Je construis un dataframe pour y mettre les informations

p = ggplot(mydata) + geom_col(aes(y = strategie, x =n ), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon la première stratégie",
              subtitle = "adoptée pour faire face au problème",
              caption = "Data source: EHCVM 2021")
p

values <- c() #Nous allons construire un vecteur qui contient le nombre de ménages qui ont opté pour la modalité 2 pour chaque stratégie 

values = append(values, nrow(filter(fusion, epargne ==2)))
values = append(values, nrow(filter(fusion, aide_parent ==2)))
values = append(values, nrow(filter(fusion, aide_gouv ==2)))
values = append(values, nrow(filter(fusion, aide_ONG ==2)))
values = append(values, nrow(filter(fusion, marier_une_fille ==2)))
values = append(values, nrow(filter(fusion, change_habit ==2)))
values = append(values, nrow(filter(fusion, cheaper_food ==2)))
values = append(values, nrow(filter(fusion, extra_job ==2)))
values = append(values, nrow(filter(fusion, emploi_chomeur ==2)))
values = append(values, nrow(filter(fusion, emploi_enfant ==2)))
values = append(values, nrow(filter(fusion, descolarisation_enfant ==2)))
values = append(values, nrow(filter(fusion, migration ==2)))
values = append(values, nrow(filter(fusion, reduction_depenses ==2)))
values = append(values, nrow(filter(fusion, credit ==2)))
values = append(values, nrow(filter(fusion, vente_actif_agric ==2)))
values = append(values,  nrow(filter(fusion, vente_bien_durable ==2)))
values = append(values,  nrow(filter(fusion, vente_terrain ==2)))
values = append(values,  nrow(filter(fusion, louer ==2)))
values = append(values,  nrow(filter(fusion, vente_vivres ==2)))
values = append(values,  nrow(filter(fusion, peche ==2)))
values = append(values,  nrow(filter(fusion, ente_betail ==2)))
values = append(values,  nrow(filter(fusion, confiage_enfant ==2)))
values = append(values,  nrow(filter(fusion, activite_spirituel ==2)))
values = append(values,  nrow(filter(fusion, culture_contre_saison ==2)))
values = append(values,  nrow(filter(fusion, autre_statégie ==2)))
values = append(values,  nrow(filter(fusion, aucune_strategie ==2)))


values <- values*100/length(unique(fusion$interview__id)) #Je calcule les pourcentages

length(values)

group <- colnames(data14b)[12:37] 
mydata <- data.frame(strategie = group, n= values)

mydata  #Je construis un dataframe pour y mettre les informations

p = ggplot(mydata) + geom_col(aes(y = strategie, x =n ), fill = 'blue')

p <- p + labs(title = "Proportion des ménages selon la deuxième stratégie",
              subtitle = "adoptée pour faire face au problème",
              caption = "Data source: EHCVM 2021")
p