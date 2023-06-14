install.packages("sf")
install.packages("geojsonio")
install.packages("leaflet")

library(sf)
library(geojsonio)
library(leaflet)

# représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements

acc_region <- function(donnee){
  regions <- list(
    alsace = c(67, 68),
    aquitaine = c(24, 33, 40, 47, 64),
    auvergne = c(3, 15, 43, 63),
    bourgogne = c(21, 58, 71, 89),
    bretagne = c(22, 29, 35, 56),
    centre = c(18, 28, 36, 37, 41, 45),
    champagne = c(8, 10, 51, 52),
    corse = c(2A, 2B),
    franche_comte = c(25, 39, 70, 90),
    ile_de_france = c(75, 77, 78, 91, 92, 93, 94, 95),
    languedoc_roussillon = c(11, 30, 34, 48, 66),
    limousin = c(19, 23, 87),
    lorraine = c(54, 55, 57, 88),
    midi_pyrenees = c(9, 12, 31, 32, 46, 65, 81, 82),
    nord_pas_de_calais = c(59, 62),
    basse_normandie = c(14, 50, 61),
    haute_normandie = c(27, 76),
    pays_de_la_loire = c(44, 49, 53, 72, 85),
    picardie = c(2, 60, 80),
    poitou_charentes = c(16, 17, 79, 86),
    provence_alpes_cote_d_azur = c(4, 5, 6, 13, 83, 84),
    rhone_alpes = c(1, 7, 26, 38, 42, 69, 73, 74)
  )
  for (i in 0:length(donnee))
    each_code <- donnee[i]$id_code_insee
  #for (each_code in donnee[id_code_insee]){
  dep=each_code[0]+each_code[1]
  for (each_region in regions){
    if isin(dep,each_region){
      donnee[i]$region = each_region
    }
  }
  #}
  
  map <- geojson_read("C:\Users\marzh\Documents\ISEN\Cours_A3\Projet_Big_Data\regions.geojson")  
}
