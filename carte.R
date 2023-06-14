#install.packages("sf")
#install.packages("leaflet")
#install.packages("magrittr")

library(sf)
library(leaflet)
library(geojsonio)
# représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements

# fonction isin venant d'elsa
isin <- function(x, values) {
  match(x, values, nomatch = 0) > 0
}

#acc_region <- function(donnee){
  regions <- list(
    alsace = c("67", "68"),
    aquitaine = c("24", "33", "40", "47", "64"),
    auvergne = c("03", "15", "43", "63"),
    bourgogne = c("21", "58", "71", "89"),
    bretagne = c("22", "29", "35", "56"),
    centre = c("18", "28", "36", "37", "41", "45"),
    champagne = c("08", "10", "51", "52"),
    corse = c("2A", "2B"),
    franche_comte = c("25", "39", "70", "90"),
    ile_de_france = c("75", "77", "78", "91", "92", "93", "94", "95"),
    languedoc_roussillon = c("11", "30", "34", "48", "66"),
    limousin = c("19", "23", "87"),
    lorraine = c("54", "55", "57", "88"),
    midi_pyrenees = c("09", "12", "31", "32", "46", "65", "81", "82"),
    nord_pas_de_calais = c("59", "62"),
    basse_normandie = c("14", "50", "61"),
    haute_normandie = c("27", "76"),
    pays_de_la_loire = c("44", "49", "53", "72", "85"),
    picardie = c("2", "60", "80"),
    poitou_charentes = c("16", "17", "79", "86"),
    provence_alpes_cote_d_azur = c("04", "05", "06", "13", "83", "84"),
    rhone_alpes = c("01", "07", "26", "38", "42", "69", "73", "74")
  )
  donnee$region <- ""
  for (i in seq_along(donnee)){
    each_code <- donnee$id_code_insee[i]
    dep <- substring(each_code, 1, 2)
    for (reg_name in names(regions)){
      each_region <- regions[[reg_name]]
      if(isin(dep, each_region)){ # regarde si le departement(dep) est dans cette region(each_region)
        print("Met la regions")
        print(each_region)
        donnee$region[i] <<- reg_name
      }
    }
  }
  #print(donnee$region)
  geo_region_json <- geojson_read("regions.geojson")


  # https://rstudio.github.io/leaflet/json.html
  map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>%
    addTiles() %>%
    addGeoJSON(geo_region_json)
  map

#}

stat_acc_V3 <- read.csv("stat_acc_V3.csv", sep=';')
data <- stat_acc_V3
#donnee <- acc_region(data)
