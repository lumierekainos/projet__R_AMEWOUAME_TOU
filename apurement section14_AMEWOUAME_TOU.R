# Installation des bibliothèques utiles

library(haven)
library(tidyverse)
library(nycflights13)
library(expss)
library(visdat)
library(dplyr)
library(questionr)
library(labelled)

#                          Apurement

# Section 14a

#Importation de la base

section14a <- read_dta("C:\\Users\\DELL\\Documents\\ENSAE\\Cours ISEP 2\\Semestre 2\\Programmation avec R\\Projet_R\\Bases_données\\s14a_me_SEN_2021.dta")


#Visionnage de la base
head(section14a)
dim(section14a)
new_name <- c(colnames(section14a)[1], colnames(section14a)[2],colnames(section14a)[3],
              colnames(section14a)[4], 'repondant', colnames(section14a)[6], 'event',
              'event_occur', 'nbr_pers', 'solution', 'event_time', colnames(section14a)[12],
              'menage_member', 'num_ordre', 'sexe', 'age', 'relationship',
              'resultat_interview', 'motif','langue','quiz_result', 'obs')

isTRUE(length(new_name)==length(colnames(section14a))) #Vérifie si les longueurs sont les mêmes

colnames(section14a) <- new_name
colnames(section14a)
# Nous allons à présent sélectionner uniquement les variables dont nous aurons besoin

data14a <- select(section14a, interview__id, event, event_occur, nbr_pers, num_ordre, menage_member, sexe, age, relationship, solution, event_time)
colnames(data14a) #Pour vérifier les modifications
#Labelisons à présent les variables de notre nouvelle base 

data14a[c("interview__id", "event","event_occur", "nbr_pers",     "num_ordre", "menage_member", "sexe", "age", "relationship", "solution", "event_time")] <- to_factor(data14a[c("interview__id", "event","event_occur", "nbr_pers",     "num_ordre", "menage_member", "sexe", "age", "relationship", "solution", "event_time")])

head(data14a) #Vérification du résultat

print(nrow(data14a) - nrow(distinct(data14a))) #doublons 
data14a <- distinct(data14a)

vis_miss(data14a) #visualisation des valeurrs manquantes

data14a <- data14a %>% filter(!is.na(event_occur)) #suppression des valeurs manquantes

any(is.na(data14a$event_occur)==TRUE) #Vérification

filter = filter(data14a, event_occur ==1)
vis_miss(filter)
data14a <- select(data14a, -sexe, -age, -relationship)
colnames(data14a)
write.table(data14a, 'section 14A_apuré.dta', row.names = FALSE)

# Section 14b

#Importation de la base

section14b <- read_dta("C:\\Users\\DELL\\Documents\\ENSAE\\Cours ISEP 2\\Semestre 2\\Programmation avec R\\Projet_R\\Bases_données\\s14b_me_SEN_2021.dta")


#Visionnage de la base
head(section14b)
dim(section14b)

new_name <- c(colnames(section14b)[1], colnames(section14b)[2],colnames(section14b)[3],
              colnames(section14b)[4], 'repondant', colnames(section14b)[6], 'event','event_occur', 'date', 'annee', 'revenu', 'avoirs', 'prod_agri', 'cheptel', 'stock_aliment', 'achat_aliment', 'epargne', 'aide_parent', 'aide_gouv', 'aide_ONG', 'marier_une_fille', 'change_habit', 'cheaper_food', 'extra_job', 'emploi_chomeur', 'emploi_enfant', 'descolarisation_enfant', 'migration', 'reduction_depenses', 'credit', 'vente_actif_agric', 'vente_bien_durable', 'vente_terrain', 'louer', 'vente_vivres', 'peche', 'ente_betail', 'confiage_enfant', 'activite_spirituel', 'culture_contre_saison', 'autre_statégie', 'aucune_strategie', 'strategie', 'resultat_interview','motif','langue','quiz_result','obs')
isTRUE(length(new_name)==length(colnames(section14b)))

colnames(section14b) <- new_name  #Remplacement
colnames(section14b) #vérification

data14b <- select(section14b, -grappe, -vague, -repondant, -interview__key, -resultat_interview, -motif, -langue, -quiz_result, -obs, -id_menage)
colnames(data14b)

data <- distinct(data14b)  #Je suprime les doublons s'il y en a eventuellemment
print(nrow(data14b)-nrow(data))

data14b[c("interview__id", "event", "event_occur", "date", "annee", "revenu", "avoirs", "prod_agri", "cheptel", "stock_aliment", "achat_aliment", "epargne", "aide_parent", "aide_gouv", "aide_ONG", "marier_une_fille", "change_habit", "cheaper_food", "extra_job", "emploi_chomeur", "emploi_enfant", "descolarisation_enfant", "migration", "reduction_depenses", "credit", "vente_actif_agric", "vente_bien_durable", "vente_terrain", "louer", "vente_vivres", "peche", "ente_betail", "confiage_enfant", "activite_spirituel", "culture_contre_saison", "autre_statégie", "aucune_strategie", "strategie")] <- to_factor(data14b[c("interview__id", "event", "event_occur", "date", "annee", "revenu", "avoirs", "prod_agri", "cheptel", "stock_aliment", "achat_aliment", "epargne", "aide_parent", "aide_gouv", "aide_ONG", "marier_une_fille", "change_habit", "cheaper_food", "extra_job", "emploi_chomeur", "emploi_enfant", "descolarisation_enfant", "migration", "reduction_depenses", "credit", "vente_actif_agric", "vente_bien_durable", "vente_terrain", "louer", "vente_vivres", "peche", "ente_betail", "confiage_enfant", "activite_spirituel", "culture_contre_saison", "autre_statégie", "aucune_strategie", "strategie")])

head(data14b) #Vérification

vis_miss(data14b, warn_large_data = FALSE) #Visualisation des NA

data14b <- data14b %>% filter(!is.na(event_occur))
any(is.na(data14b$event_occur) == TRUE) #Vérification

vis_miss(filter(data14b, event_occur =='Oui'), warn_large_data= FALSE)
data14b <- data14b %>% filter(!is.na(revenu))
write.table(data14b, 'section 14B_apuré.dta', row.names = FALSE)




