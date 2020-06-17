################## Importing library #########################
if(!require(readxl)) install.packages("readxl") 
if(!require(dplyr)) install.packages("dplyr")
if(!require(leaflet)) install.packages("leaflet")
if(!require(leaflet.extras)) install.packages("leaflet.extras")
if(!require(janitor)) install.packages("janitor")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(stringr)) install.packages("stringr")
if(!require(GGally)) install.packages("GGally")
if(!require(scales)) install.packages("scales")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")

library(stringr)
library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(janitor)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(caret)
library(GGally)
library(scales)
library(knitr)
library(kableExtra)



################## Importing data (with pre-processing) from github #########################
 url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/BaseCrimesFinal/BaseCrime1.zip"
 download.file(url_arq, "temp.zip", quiet = F,method='curl')
 unzip("temp.zip")
 base1 <- read_excel("BaseCrime1.xlsx")
 file.remove("temp.zip")
 
 url_arq <- "https://github.com/Saulgal/CrimePrediction/raw/BaseCrimesFinal/BaseCrime2.zip"
 download.file(url_arq, "temp.zip", quiet = F)
 unzip("temp.zip")
 base2 <- read_excel("BaseCrime2.xlsx")
 file.remove("temp.zip")
 bind_rows(base1,base2) -> BaseCrimeTotal
 
#Cleaning data
 #Keeping data that is theft and in the Sao Paulo zone
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!is.na(DATAOCORRENCIA))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(RUBRICA %in% c('Furto (art. 155) - VEICULO','Furto qualificado (art. 155, §4o.) - VEICULO','A.I.-Furto (art. 155) - VEICULO','A.I.-Furto qualificado (art. 155, §4o.) - VEICULO'))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(LATITUDE <= -23.36)
BaseCrimeTotal <- BaseCrimeTotal %>% filter(LONGITUDE <= -46.36579)
BaseCrimeTotal <- BaseCrimeTotal %>% filter(LATITUDE >= -24)
BaseCrimeTotal <- BaseCrimeTotal %>% filter(LONGITUDE >=-46.82112)
  #Removing data from other citys, but that were registered in Sao Paulo
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"OSASCO"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"SANTO ANDRÉ"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"GUARULHOS"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DIADEMA"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"BERNARDO DO CAMPO"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"S.B.C-DR OMAR CASSIM"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"ITAPECERICA"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"TABOÃO DA SERRA"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"MAUA"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"MAUÁ"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"CAETANO DO SUL"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"ITAQUAQUE"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"BARUERI"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"MAIRIPO"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"EMBU DAS ARTES"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"FERRAZ DE VASCONCEL"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"FERRAZ VASCONCELOS"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.1ª CENTRO"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.2ª SUL"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.3ª OESTE"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.4ª NORTE"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.5ª LESTE"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.6ª SANTO AMARO"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.7ª ITAQUERA"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.SEC.8ª SÃO MATEUS"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DOPE - 02ª DP DEATUR CONGONHAS"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"SÃO VICENTE"))
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!str_detect(DELEGACIA_CIRCUNSCRICAO,"DEL.POL.RPIRES DR.EVANDRO LIMA"))

  #Creating columns to use in the project
BaseCrimeTotal$DEL_NUM <- str_trim(str_replace(str_sub(BaseCrimeTotal$DELEGACIA_CIRCUNSCRICAO,end = 3),'º',''))
BaseCrimeTotal <- BaseCrimeTotal %>% mutate(DateCrime = as.Date.POSIXct(DATAOCORRENCIA))

  #Removing data that does not have LATITUDE
BaseCrimeTotal <- BaseCrimeTotal %>% filter(!is.na(LATITUDE))



#Seeing the data
BaseCrimeTotal[1:5,]%>%knitr::kable()  %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "400px")


# #Crime através do tempo
BaseCrimeTotal_dia <- BaseCrimeTotal %>% dplyr::group_by(DateCrime) %>% dplyr::summarise(Qtd=n()) %>% arrange(DateCrime)

BaseCrimeTotal_dia %>% ggplot( aes(x =  DateCrime, y = Qtd)) +
  geom_line(color = "cornflowerblue", size = 0.1) +
  geom_smooth(color = "black") +
  # fte_theme() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Date of thefts", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2020")



BaseCrimeTotal$DiaSemana <- factor(wday(BaseCrimeTotal$DATAOCORRENCIA),levels = c(1,2,3,4,5,6,7))
BaseCrimeTotal$DiaSemana <- factor(weekdays(BaseCrimeTotal$DATAOCORRENCIA))
BaseCrimeTotal$OCORRENCIA <- 1
##By year
BaseCrimeTotal %>%  ggplot(aes(x= year(DATAOCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity', color = "cornflowerblue")+
  labs(x = "Year of thefts", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2020")

##By month
BaseCrimeTotal %>% filter(year(DATAOCORRENCIA)<2020) %>%  ggplot(aes(x= month(DATAOCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity', color="cornflowerblue")+
  labs(x = "Month of thefts", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2019")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))
BaseCrimeTotal %>% filter(year(DATAOCORRENCIA)<2020) %>%  ggplot(aes(x= month(DATAOCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity', color="cornflowerblue")+facet_wrap(.~year(DATAOCORRENCIA))+
  labs(x = "Month and Year of thefts", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2019")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

##By day
BaseCrimeTotal %>% filter(year(DATAOCORRENCIA)<2020) %>%  ggplot(aes(x= day(DATAOCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity')
BaseCrimeTotal %>% filter(year(DATAOCORRENCIA)<2020) %>%  ggplot(aes(x= day(DATAOCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity')+facet_wrap(month(DATAOCORRENCIA)~year(DATAOCORRENCIA))

##By day of week

BaseCrimeTotal$DiaSemana <- factor(BaseCrimeTotal$DiaSemana, levels = c("domingo","segunda-feira","terça-feira","quarta-feira",'quinta-feira','sexta-feira','sábado'))
BaseCrimeTotal %>%  ggplot(aes(x=reorder(DiaSemana,-OCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity')

BaseCrimeTotal %>%  ggplot(aes(x=reorder(DiaSemana,-OCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity')+facet_wrap(.~year(DATAOCORRENCIA))
BaseCrimeTotal %>%  ggplot(aes(x=reorder(DiaSemana,-OCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity')+facet_wrap(month(DATAOCORRENCIA)~year(DATAOCORRENCIA))

##By hour
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
BaseCrimeTotal <- BaseCrimeTotal %>% mutate(Hora = hour(HORAOCORRENCIA))
BaseCrimeTotal$Hora <- factor(BaseCrimeTotal$Hora, level = 0:23, label = hour_format)
BaseCrimehora <- BaseCrimeTotal %>% dplyr::group_by(DiaSemana,Hora) %>% dplyr::summarise(Qtd = n())

BaseCrimehora <- BaseCrimehora %>% filter(!is.na(Qtd))
BaseCrimehora <- BaseCrimehora %>% filter(!is.na(Hora))
#BaseCrimehora$Hora <- factor(BaseCrimehora$Hora, level = 0:23, label = hour_format)
#BaseCrimehora$DayOfWeek <- mutate(DayOfWeek =wee)


ggplot(BaseCrimehora, aes(Hora, DiaSemana )) +
       geom_tile(aes(fill = Qtd), color = "white") +
       scale_fill_gradient(low = "white", high = "cornflowerblue") +
       ylab("Day of Week of Theft") +
       xlab("Hour of Theft") +
       theme(legend.title = element_text(size = 10),
                        legend.text = element_text(size = 12),
                        plot.title = element_text(size=16),
                        axis.title=element_text(size=14,face="bold"),
                        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.6)) +
       labs(fill = "Qtd",title='Number of Thefts in Sao Paulo from 2016 – 2020, by Time of Theft')

#Facet by month
BaseCrimehoraMes <- BaseCrimeTotal %>% dplyr::group_by(Mes = month(DATAOCORRENCIA),DiaSemana,Hora) %>% dplyr::summarise(Qtd = n())

BaseCrimehoraMes <- BaseCrimehoraMes %>% filter(!is.na(Qtd))
BaseCrimehoraMes <- BaseCrimehoraMes %>% filter(!is.na(Hora))



ggplot(BaseCrimehoraMes, aes(Hora, DiaSemana )) +
  geom_tile(aes(fill = Qtd), color = "white") +
  scale_fill_gradient(low = "white", high = "cornflowerblue") +
  ylab("Day of Week of Theft") +
  xlab("Hour of Theft") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.6)) +
  labs(fill = "Qtd",title='Number of Thefts in Sao Paulo from 2016 – 2020, by Time of Theft') + facet_wrap(.~Mes)


#Distribuicao espacial de crime total
Crimelonglat <- BaseCrimeTotal %>% filter(!is.na(LATITUDE))
Crimelonglat <- Crimelonglat %>% mutate(AnoOcorrencia = year(DATAOCORRENCIA))
Crimelonglat$popup <- paste("<b>Número BO:<b>", Crimelonglat$NUMERO_BOLETIM, "<br>","<b>Veículo<b>:",Crimelonglat$DESCR_MARCA_VEICULO,"<br>","<b>Data da Ocorrência:<b>",Crimelonglat$DATAOCORRENCIA,"<br>","<b>Hora da Ocorrência:<b>",Crimelonglat$HORAOCORRENCIA)


leaflet(Crimelonglat) %>% 
  setView( -46.5, -23.6, 10 ) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = Crimelonglat$popup, clusterOptions = markerClusterOptions()) %>%
  
  addHeatmap( blur = 14, max = 175, radius = 7) 

#Dados de furto por circunscrição de delegacia 
ggplot(aes(LONGITUDE, LATITUDE, color=DEL_NUM), data = Crimelonglat) + geom_point()


#Outras visualizações interessantes
BaseCrimeTotal %>%  ggplot(aes(x= DESCR_TIPO_VEICULO,y=OCORRENCIA ))+geom_bar(stat = 'identity', color = "cornflowerblue")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.6))+
  labs(x = "Type of veihicle", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2020")


BaseCrimeTotal %>%  ggplot(aes(x= reorder(as.factor(DESCR_COR_VEICULO),-OCORRENCIA),y=OCORRENCIA ))+geom_bar(stat = 'identity', color = "cornflowerblue")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.6))+
  labs(x = "Color of veihicle", y = "Number of thefts", title = "Car theft in Sao Paulo from 2016 – 2020")



####################################################################
######################         Modelagem      ######################
####################################################################


DelNum <- sort(unique(BaseCrimeTotal$DEL_NUM))
DiaCrime <- sort(as.character(unique(BaseCrimeTotal$DateCrime)))

##Tabela com todas as combinações de delegacia e data
temp <- expand.grid(DelNum, DiaCrime)
names(temp) <- c("DELEGACIA_NUM", 'DATA_CRIME')
temp <- temp[order(temp$DELEGACIA_NUM),]


##tabela com os dados espaço temporais quantificados
modelo <- BaseCrimeTotal %>% dplyr::group_by(DEL_NUM,as.character(DateCrime)) %>% dplyr::summarise(Qtd=n())
names(modelo) <- c("DELEGACIA_NUM", 'DATA_CRIME','Qtd')

##Combinação 
modelo <- merge(temp, modelo, by= c('DELEGACIA_NUM', 'DATA_CRIME'), all.x = TRUE)
modelo$Qtd <- ifelse(is.na(modelo$Qtd),0,modelo$Qtd)

modelo$DIA_SEMANA <- weekdays(as.Date(modelo$DATA_CRIME),abbreviate=TRUE)
modelo$MES <- months(as.Date(modelo$DATA_CRIME),abbreviate=TRUE)
#Função que cria um vetor iniciado com zero seguindo de uns, que usaremos para determinar quantos dias tem crimes por delegacia
pastDays <- function(x) {
  c(0, rep(1, x))
}

#Crimes por data acumulada por intervalo
modelo$CrimePast1 <- ave(modelo$Qtd, modelo$DELEGACIA_NUM, FUN= function(x) stats::filter(x, pastDays(1),side=1))
modelo$CrimePast7 <- ave(modelo$Qtd, modelo$DELEGACIA_NUM, FUN= function(x) stats::filter(x, pastDays(7),side=1))
modelo$CrimePast30 <- ave(modelo$Qtd, modelo$DELEGACIA_NUM, FUN= function(x) stats::filter(x, pastDays(30),side=1))

meanNA <- function(x){
  mean(x, na.rm = TRUE)
}

modelo$CrimePast1 <- ifelse(is.na(modelo$CrimePast1), meanNA(modelo$CrimePast1), modelo$CrimePast1)
modelo$CrimePast7 <- ifelse(is.na(modelo$CrimePast7), meanNA(modelo$CrimePast7), modelo$CrimePast7)
modelo$CrimePast30 <- ifelse(is.na(modelo$CrimePast30), meanNA(modelo$CrimePast30), modelo$CrimePast30)
modelo$trend <- ifelse(modelo$CrimePast30 == 0, 0,modelo$CrimePast7/modelo$CrimePast30)
ggcorr(modelo, label=T)


CrimeModelo <- glm(Qtd ~ CrimePast1 + CrimePast7 + CrimePast30 + trend + factor(DIA_SEMANA)+MES, data = modelo)
summary(CrimeModelo)

#Validação Modelo
# Validation set will be 20% of modelo
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = modelo$DELEGACIA_NUM, times = 1, p = 0.2, list = FALSE)

train_set <- modelo[-test_index,]
test_set <- modelo[c(test_index),]

#medidas de dispersão 
mean(modelo$Qtd)
var(modelo$Qtd)

CrimeModeloPred <- predict(CrimeModelo, test_set, type= 'response')
sqrt(mean((test_set$Qtd - CrimeModeloPred)^2)) ->RMSE
validate <- data.frame(test_set$Qtd, CrimeModeloPred)
names(validate) <- c('actual', 'predicted')

validate$bucket <- with(validate, cut(predicted, breaks=
                                        quantile(predicted, probs= seq(0, 1, 0.1)),
                                      include.lowest= TRUE, labels= c(1:10)))

validate <- aggregate(validate[, c('actual', 'predicted')], by=
                      list(validate$bucket), FUN = mean)

plot(validate$predicted, col= 'red', type= 'l', lwd= 1.5, ylab= 'No. of Crashes', xlab=
     'Predicted Crimes Decile', main= 'Actual vs. Predicted')

lines(validate$actual, col= 'blue', lwd= 1.5)

legend('topright', c('Actual', 'Predicted'), col= c('blue', 'red'), lwd= c(1.5, 1.5),
        bty= 'n')