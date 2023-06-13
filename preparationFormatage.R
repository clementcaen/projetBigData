# on importe
library(datasets)
stat_acc_V3 <- read.csv("stat_acc_V3.csv", sep=';')
data <- stat_acc_V3

# clément
# Mettre les variables numériques sous format numériques, date sous format date
convertDate <- function (colonneCsv){
  new_date <- NULL
  for (each_date in colonneCsv){
    new_date <- append(new_date, as.POSIXct(each_date, format = "%Y-%m-%d %H:%M:%S")) # format represente les dates venant du csv ex:2009-10-04 20:50:00
  }
  return(new_date)
}
convertNumeric <- function (colonneCsv){
  new_num <- NULL
  for (each_number in colonneCsv){
    new_num <- append(new_num, as.double(each_number))
  }
  return(new_num)
}

# conversion colonnes non multi-modales
data["Num_Acc"] <- convertNumeric(data["Num_Acc"])
data["id_usa"] <- convertNumeric(data["id_usa"])
data["date"] <- convertDate(data["date"])
data["id_code_insee"] <- convertNumeric(data["id_code_insee"])
data["latitude"] <- convertNumeric(data["latitude"])
data["longitude"] <- convertNumeric(data["longitude"])
data["an_nais"] <- convertDate(data["an_nais"])
data["age"] <- convertNumeric(data["age"])
data["place"] <- convertNumeric(data["place"])

print(data)
print("Conversion réussi")
print("CHLOE")

# clément
# Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000
#habitants par région (qui servirait à l’ACP discutée dans la section 3 intitulée Analyse de
#                      données ci-dessous)
# on creer un tableau avec les données du nombre d'habitant par commune https://www.insee.fr/fr/statistiques/2044741
stat_pop <- read.csv("base-cc-evol-struct-pop-2009.csv", sep=';')
print(stat_pop)
# P09_POP: population | CODGEO: code insee | REG: id_region
# On groupe par région avec la sommme des habitants des villes (total d'habitant par région) et les divisant par 100 000 pour avoir les données pour 100 000 habitant
total_pop <- aggregate(P09_POP ~ REG, data = stat_pop, FUN = sum)
pop100000_by_reg <- NULL
pop100000_by_reg = data.frame(total_pop$REG, total_pop$P09_POP /100000)
print(pop100000_by_reg)
# On trouve tous les accidents dans la région en mergeant les 2 tableaux
names(pop100000_by_reg)[pop100000_by_reg == "CODGEO"] <- "id_code_insee"
merging_tableau <- merge(total_pop, data, by="id_code_insee")
print(merging_tableau)
# on groupe par région les accidents
accid_by_reg <- aggregate(place ~ REG, data = merging_tableau, FUN = sum) # on somme les personnes touchés par l'accidents (place) pour connaitre le nombre d'affecté par les accident par région
print(accid_by_reg)
# On trie par gravité ? (histogramme)







