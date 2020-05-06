
library(readxlsb)
url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2020.zip"
download.file(url_arq, "temp.zip", quiet = F)
unzip("temp.zip")

base2020 <- read_xlsb("BaseCrime2020.xlsb",1)

url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2019.zip"
download.file(url_arq, "temp.zip", quiet = F)
unzip("temp.zip")

base2019 <- read_xlsb("BaseCrime2019.xlsb",1)

url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/Data/BaseCrime2018.zip"
download.file(url_arq, "temp.zip", quiet = F)
unzip("temp.zip")

base2018 <- read_xlsb("BaseCrime2018.xlsb",1)
print("ok")