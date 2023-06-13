# on importe
library(datasets)
stat_acc_V3 <- read.csv("stat_acc_V3.csv", sep=';')
data <- stat_acc_V3

# clément
# Mettre les variables numériques sous format numériques, date sous format date
convertDate <- function (colonneCsv){
  new_date <- NULL
  for (each_date in colonneCsv){
    new_date <- append(new_date, as.Date(each_date, format = "%Y-%m-%d %H:%M:%S")) # format represente les dates venant du csv ex:2009-10-04 20:50:00
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


