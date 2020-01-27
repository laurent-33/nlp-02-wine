data_achats <- read.csv('wine_achats_data.csv')
data_tarifs <- read.csv('wine_tarifs_data.csv')
str(data_achats)
summary(data_achats)
str(data_tarifs)
summary(data_tarifs)

#Q1 : Combien des clients (ID_CLIENT), depots (ID_DEPOT), types de vin (ID_VIN) et 
#tailles de bouteille (ID_BOUTEILLE) avons-nous dans le data set ?
length(levels(data_achats$ID_CLIENT))
length(levels(data_achats$ID_DEPOT))
length(levels(data_achats$ID_VIN))
length(unique(data_achats$ID_BOUTEILLE))

#Q2 : Qui sont les trois clients qui ont achete plus de bouteilles 
#(en totale) et combien ils en ont achete ?
combine = aggregate(data_achats$QUANTITE_ACHETEE, by=list(data_achats$ID_CLIENT), FUN=sum)
head(combine[order(combine$x, decreasing = T),], 3)

#Q3 : Combien de bouteilles de vin "V01" ont ete achetees depuis le 
#20 Janvier 2014 jusqu'au 15 Mai 2014 (inclus) ?
data_achats$DATA_ACHAT <- as.Date(data_achats$DATA_ACHAT, "%d/%m/%Y")
str(data_achats)
sum(
  data_achats[
    which(data_achats$DATA_ACHAT >= as.Date("20/01/2014", "%d/%m/%Y") &
          data_achats$DATA_ACHAT <= as.Date("15/05/2014", "%d/%m/%Y")) &
          data_achats$ID_VIN == "V01"
  , "QUANTITE_ACHETEE"]
)

#Q4 : Quel millesime (MILLESIME) a ete le plus vendu ?
combine = tapply(data_achats$QUANTITE_ACHETEE, data_achats$MILLESIME, sum)
head(sort(combine, decreasing = T), 1)

#Q5 : Combien de bouteilles ont ete achetees (au total) ni en casher ni en primeur ?    
sum(
  data_achats[
    which(data_achats$CACHER == "NON" & data_achats$EN_PRIMEUR == "NON")
    , "QUANTITE_ACHETEE"]
)

#Q6 : Dans quels departements, et combien en tout, le chateau n'a-t'il
# vendu aucune bouteille en primeur ? 

## Transformation de la colonne département en variable catégorielle (et non des nombres)
data_achats$DEPARTEMENT <- as.factor(data_achats$DEPARTEMENT)
str(data_achats)

## Affichage de la liste des départements ou pas de vente primeur
dep <- unique(data_achats$DEPARTEMENT)
depP <- unique(data_achats[which(data_achats$EN_PRIMEUR == "OUI"),]$DEPARTEMENT)
diffD <- setdiff(dep, depP)
diffD
length(diffD)

#Q7 : Combien de clients (uniques) ont achete des bouteilles en depot D05 ?
length(unique(data_achats[which(data_achats$ID_DEPOT == "D05"), "ID_CLIENT"]))

#Q8 : A quelle date le chateau a passe les 10000 bouteilles vendues ?
## Création d'une nouvelle table triée par date
data_achats_date_sorted <- data_achats[order(data_achats$DATA_ACHAT),
                                       c("DATA_ACHAT", "QUANTITE_ACHETEE")]

## Création d'une colonne avec les quantité cumulée de vente
data_achats_date_sorted$qte_cum <- cumsum(data_achats_date_sorted[, "QUANTITE_ACHETEE"])

## Affichage de la 1ère ligne à partir de laquelle plus de 10 000 bouteilles ont été vendues
head(data_achats_date_sorted[which(data_achats_date_sorted$qte_cum > 10000),],1)

#Q9 : Quel est le vin le plus vendu ?
library(dplyr)
data_achats %>%
  group_by(ID_VIN) %>%
  summarise(plus_vendus = sum(QUANTITE_ACHETEE)) %>%
  arrange(-plus_vendus) %>%
  head(1) %>%
  select(ID_VIN)

#Q10 : En termes de prix, quelle est la top3 de clients qui ont acheté le plus ?
## Fusion des tables pour récupérer le prix des bouteilles
data <- merge(data_achats, data_tarifs)

## Création de la colonne du prix total par ligne
data$montant_commande <- data$QUANTITE_ACHETEE * data$PRIX

## calcul de la somme par client et affichage
combine = aggregate(data$montant_commande, by=list(data$ID_CLIENT), FUN=sum)
head(combine[order(combine$x, decreasing = T),], 3)


