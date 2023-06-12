# on importe à la main
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
data["date"] <- convertDate(data["date"])
print(data["date"])
print("Conversion réussi")



