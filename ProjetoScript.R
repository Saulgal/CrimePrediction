if(!require(readxlsb)) install.packages("readxlsb")
if(!require(dplyr)) install.packages("dplyr")
if(!require(leaflet)) install.packages("leaflet")
if(!require(leaflet)) install.packages("janitor")

library(readxlsb)
library(dplyr)
library(leaflet)
library(janitor)

# url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2020.zip"
# download.file(url_arq, "temp.zip", quiet = F)
# unzip("temp.zip")
# 
# base2020 <- read_xlsb("BaseCrime2020.xlsb",1)
# 
# url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2019.zip"
# download.file(url_arq, "temp.zip", quiet = F)
# unzip("temp.zip")
# 
# base2019 <- read_xlsb("BaseCrime2019.xlsb",1)
# 
# url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2018.zip"
# download.file(url_arq, "temp.zip", quiet = F)
# unzip("temp.zip")
# 
# base2018 <- read_xlsb("BaseCrime2018.xlsb",1)


bind_rows(bind_rows(base2019,base2018),base2020) -> BaseCrimeTotal
#Ajusta os formatos de data do excel
BaseCrimeTotal %>% mutate(DATAOCORRENCIA = excel_numeric_to_date(DATAOCORRENCIA))

#Exploração Inicial da tabela 
head(BaseCrimeTotal)

#Crime através do espaço
BaseCrimeTotal$popup <- paste("<b>Número BO:<b>", BaseCrimeTotal$NUMERO_BOLETIM, "<br>","<b>Veículo<b>:",BaseCrimeTotal$DESCR_MARCA_VEICULO,"<br>","<b>Data da Ocorrência:<b>",BaseCrimeTotal$DATAOCORRENCIA,"<br>","<b>Hora da Ocorrência:<b>",BaseCrimeTotal$HORAOCORRENCIA)

leaflet(BaseCrimeTotal, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = BaseCrimeTotal$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )


