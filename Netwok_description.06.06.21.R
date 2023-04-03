# 0 Bancos with codigo de sitio ----
library(dplyr)
# Revisão de bancos solicitados enviados
rm(list = ls())

#codigo de sitio
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
mov17 <- readxl::read_excel("movilizacion2017.xlsx")
mov18 <- readxl::read_excel("movilizacion2018.xlsx")

#file too big i split in two to join
mov17_18 <- rbind(mov17,mov18)
# write.csv(mov17_18, file="movilizacion2017-2018")
rm(list = ls())

#Reading again 17 and 18 and combining with 19
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
mov17_18 <- read.csv(file = "movilizacion2017-2018", colClasses = "character")

mov19 <- readxl::read_excel("movilizacion2019.xlsx")
mov17_18 <- mov17_18[,-1]
mov <- mov17_18
rm(mov17_18)

mov <- rbind(mov19,mov)
rm(mov19)

# write.csv(mov, file="movilizacion_final")

#Arquivo final banco movimentacao ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
m <- read.csv(file = "movilizacion_final", colClasses = "Character")
# Changing names of columns
m <- mov
rm(mov)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m16 <- m16[,-1]

colnames(m)
colnames(m16)
#aumentando codigo sitio em m16
m16$codigo.sitio.origen <-NA
m16$codigo.sitio.destino <-NA

# tirando modigo de provincia em m
m <- m[,c(-6,-16)]

colnames(m)
colnames(m16)
#same until 25 colname changing id_solicitante and nombre_solicitante
colnames(m16)
colnames(m)
m <- m[,c(1:25,39,38,26:37)]
#same until 30 changing order 32 to 31
colnames(m16)
colnames(m)
m <-m[,c(1:30,32,31,33:39)]

#colnames the same, Iwill change the colnames m16 to m
colnames(m) <- colnames(m16) 

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
# write.csv(m, file="movimentos_db.csv")

# 1 Install Packages ----
install.packages("devtools")
install.packages("epinemo")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                    '7055b1fb20190fb0411d0f730d81faccd5968f0e')

library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)
library(epinemo)

# 1 Importing Database ----
rm(list = ls())
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")

m <- read.csv(file = "movimentos_db.csv", colClasses = "character")
m <- m[,-1]

# Original dataset contain 1679686 lines
# Number of pigs and csmi of the original dataset ----
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad)
#10419708
length(unique(m$numero.certificado)) 
#1212008

# Include month and year
#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

# 1 Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11530 

# 1.2 indocumentados elimination ----
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
#3732

# 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
#1
length(unique(m$numero.certificado[m$ano == 2020]))
#156


#Eliminating anulados
m <- m[(m$estado != "anulado"),]
#Eliminating indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]
#eliminating 2016 a&2020
m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

sum(m$cantidad) #animals
# (10419708-9912840)/10419708 #4.86% Pigs
# (10419708-9904714)/10419708 #4.94% Pigs 8712 pigs were deleted because of 
# problems with duplicate operations at destiny related in 1.5 removing duplicates

length(unique(m$numero.certificado))  #CSMI
# (1212008-1190991)/1212008 #1.73% CSMI

# 21315
# 11530/21315 54.09% canceled
# (5897+3732)/21315  45.17% cadastral
# (1+156)/21315 0.74% not corresponding to study period

# Number of animals and certificates final dataset ----
sum(m$cantidad)
#9912840
length(unique(m$numero.certificado)) 
#1190991

# 1.3 Reorganizing Operation ----

table(m$operacion.origen)
unique(m$operacion.origen)

m$operacion.origen <- gsub("Faenador", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Expositor", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Reproductor de animales", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Vacunador oficial", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Incubador", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Investigador", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Distribuidor vacuna oficial", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Operador de vacunación", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Operador Traspatio-Comercial", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Importador", "Productor", m$operacion.origen)

m$operacion.origen <- gsub("Feria de Exposición Animal", "Feria de comercialización animal", m$operacion.origen)
m$operacion.origen <- gsub("Evento Deportivo -Recreativo Animal", "Feria de comercialización animal", m$operacion.origen)

m$operacion.origen <- gsub("Comercializador Directo", "Comercializador", m$operacion.origen)
m$operacion.origen <- gsub("Comercializador directo", "Comercializador", m$operacion.origen)

m$operacion.origen <- gsub("Cuarentena", "Productor", m$operacion.origen)
m$operacion.origen <- gsub("Industrializador", "Operador Industrial", m$operacion.origen)

unique(m$operacion.origen)

# destino
table(m$operacion.destino)
unique(m$operacion.destino)

m$operacion.destino <- gsub("Reproductor de animales", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Industrializador", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Expositor", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Vacunador oficial", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Operador de vacunación", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Operador Traspatio-Comercial", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Operador Movilización", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Distribuidor vacuna oficial", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Emisor de movilización oficial", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Investigador", "Productor", m$operacion.destino)
m$operacion.destino <- gsub("Incubador", "Productor", m$operacion.destino)

m$operacion.destino <- gsub("Feria de Exposición Animal", "Feria de comercialización animal", m$operacion.destino)
m$operacion.destino <- gsub("Evento Deportivo -Recreativo Animal", "Feria de comercialización animal", m$operacion.destino)

m$operacion.destino <- gsub("Comercializador Directo", "Comercializador", m$operacion.destino)
m$operacion.destino <- gsub("Comercializador directo", "Comercializador", m$operacion.destino)

m$operacion.destino <- gsub("Cuarentena", "Productor", m$operacion.destino)

unique(m$operacion.destino)

# We could look for the industrial operators in the vaccine registry to have another operacao
# Replacing produtores para operadores industriais ----

# I will take all the ID of the selfservice
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")


auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)
sum(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto, auto_lis)


table(m$producto)
library(tidyr)
# 1.4 Productive category ----
# suplementary table sow, piglet,boars
p <- m %>% group_by(producto, ano) %>%
  summarize(animals=sum(cantidad)) %>%
  spread(key="ano", value = "animals")
p <- data.frame(p)
p$mean <- rowMeans(p[,c(2,3,4)])
p$por <- (p$mean/3304280*100)
sum(p$mean)


#        producto   X2017   X2018   X2019      mean       por
# 1 Cerda levante  919335 1268072 1468531 1218646.0 36.880833
# 2   Cerda madre  173744   62298   80914  105652.0  3.197429
# 3 Cerdo levante 1229789 1514655 1693492 1479312.0 44.769572
# 4        Lechón  199419  247389  269191  238666.3  7.222945
# 5       Lechona  171525  193397  219048  194656.7  5.891046
# 6       Verraco  148211   23946   29884   67347.0  2.038175

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
# write.csv(p, file="categories.csv")

# 1.5 Aggregate without animal type ----
#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, codigo.sitio.origen, 
           identificacion.operador.origen, razon.social.operador.origen, 
           operacion.destino, provincia.destino, canton.destino, parroquia.destino, sitio.destino, 
           codigo.sitio.destino, identificacion.operador.destino, razon.social.operador.destino,
           fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))

m2 <- data.frame(m2)

# 1.6 Finding duplicates CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190991

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2$numero.certificado[duplicated(m2$numero.certificado)]

#write.csv(duplicated, file="duplicados.csv")

dupicatedc <- m2[(m2$numero.certificado %in% duplicated),]

#1.7 Removing duplicated ----
sum(m2$cantidad) #9912840
length(unique(m2$numero.certificado)) #1190991

m2 <- m2[!duplicated(m2$numero.certificado),]
sum(m2$cantidad) #9904714
length(unique(m2$numero.certificado)) #1190991

# 9912840-9904128
#we lost 8712 animais from the duplicated issue.

length(unique(m2$numero.certificado))
#1190991

# 1.8 Selfservice ----
# Porcentage of emision by self service ----
library(tidyr)
m2%>%
  group_by(ano)%>%
  filter(is.na(provincia.emision == TRUE))%>%
  summarise(n=n())%>%
  spread(key = "ano", value = n )

m2%>%
  group_by(ano)%>%
  summarise(n=n())

297949/314550
380385/394461
471247/481980

mean(0.9472,0.96431,0.97773)

# 1.9 Checking markets  ----
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)

#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen)%>%
  summarise(Propietarios=length(unique(codigo.sitio.origen))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen                 `2017` `2018` `2019`
# 1 Comercializador                    1678   2006   2300
# 2 Feria de comercialización animal     54     55     98
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47956  57190  65272

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino)%>%
  summarise(Propietarios=length(unique(codigo.sitio.destino))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7981   8140  11436
# 2 Faenador                            160    147    154
# 3 Feria de comercialización animal     75     60     79
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20785  31348  44201

noferiasorigen <- m2 %>% group_by(provincia.origen, 
                                  razon.social.operador.origen,
                                  sitio.origen,
                                  identificacion.operador.origen,
                                  operacion.origen,
                                  codigo.sitio.origen,
                                  ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen, "FERIA"))%>%
  filter(!str_detect(sitio.origen, "feria"))%>%
  filter(!str_detect(sitio.origen, "Feria"))%>%
  filter(!str_detect(sitio.origen, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen, "Asociacion de"))%>%
  filter(!str_detect(sitio.origen, "Matadero Municipal de Yantzaza"))%>%
  summarise(CSMI=n(),a=sum(as.numeric(cantidad)), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "animais")

noferiasorigen <- noferiasorigen$codigo.sitio.origen

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen = if_else(operacion.origen == "Feria de comercialización animal" &
                                       codigo.sitio.origen %in% noferiasorigen,
                                     "Productor", operacion.origen))
m2 %>%
  group_by(Year=ano, operacion.origen)%>%
  summarise(Propietarios=length(unique(codigo.sitio.origen))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen                 `2017` `2018` `2019`
# 1 Comercializador                    1678   2006   2300
# 2 Feria de comercialización animal     52     53     65
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47958  57192  65273

no_ferias_destino <- m2 %>% group_by(provincia.destino, 
                                           razon.social.operador.destino,
                                           sitio.destino,
                                           identificacion.operador.destino,
                                           operacion.destino,
                                           codigo.sitio.destino,
                                           ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino, "FERIA"))%>%
  filter(!str_detect(sitio.destino, "feria"))%>%
  filter(!str_detect(sitio.destino, "Feria"))%>%
  filter(!str_detect(sitio.destino, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino, "Asociacion de"))%>%
  filter(!str_detect(sitio.destino, "Matadero Municipal de Yantzaza"))%>%
  summarise(CSMI=n(),a=sum(as.numeric(cantidad)), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "animais")

noferiasdestino <- no_ferias_destino$codigo.sitio.destino

#removing no-ferias from erroneous destiny
m2 <- m2 %>%
  mutate(operacion.destino = if_else(operacion.destino == "Feria de comercialización animal" &
                                        codigo.sitio.destino %in% noferiasdestino,
                                      "Productor", operacion.destino))

m2 %>%
  group_by(Year=ano, operacion.destino)%>%
  summarise(Propietarios=length(unique(codigo.sitio.destino))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7981   8140  11436
# 2 Faenador                            160    147    154
# 3 Feria de comercialización animal     58     58     76
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20798  31350  44204

#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(prov=provincia.origen,
                                       canton=canton.origen,
                                       parr=parroquia.origen,
                                       nsitio= sitio.origen,
                                       rz=razon.social.operador.origen,
                                       id=identificacion.operador.origen,
                                       operac=operacion.origen, 
                                       sitio= codigo.sitio.origen, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

ferias_verdaderas_d <- m2 %>% group_by(prov=provincia.destino,
                                       canton=canton.destino,
                                       parr=parroquia.destino,
                                       nsitio= sitio.destino,
                                       rz=razon.social.operador.destino,
                                       id=identificacion.operador.destino, 
                                       operac=operacion.destino, 
                                       sitio=codigo.sitio.destino, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas2 <- ferias_verdaderas %>% 
  group_by(prov, canton,parr, nsitio, rz, id, operac, sitio, ano) %>%
  summarise(CSM=sum(as.numeric(CSMI)),a=sum(as.numeric(animais)), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "animais")


# Changing markets with less than 40 CSMI to producers

fv <- ferias_verdaderas2 %>% group_by(prov, canton, parr, rz, nsitio, sitio) %>%
  summarise(CSMI=sum(CSM), animais=sum(a)) 

length(unique(fv$sitio)) #85
#transforming to producer the ferias with less than 40 CSM
fvd <- fv$sitio[fv$CSMI < 40]

m2 <- m2 %>%
  mutate(operacion.origen = if_else(codigo.sitio.origen %in% fvd,
                                      "Productor", operacion.origen))

m2 <- m2 %>%
  mutate(operacion.destino = if_else(codigo.sitio.destino %in% fvd,
                                     "Productor", operacion.destino))

# Checking last

m2 %>%
  group_by(Year=ano, operacion.origen)%>%
  summarise(Propietarios=length(unique(codigo.sitio.origen))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen                 `2017` `2018` `2019`
# 1 Comercializador                    1677   2006   2300
# 2 Feria de comercialización animal     46     51     61
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47965  57194  65277

m2 %>%
  group_by(Year=ano, operacion.destino)%>%
  summarise(Propietarios=length(unique(codigo.sitio.destino))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7980   8140  11436
# 2 Faenador                            160    147    154
# 3 Feria de comercialización animal     49     54     62
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20807  31354  44218

# manual checking

m2$codigo.sitio.origen <- gsub("1960000620001.1903", "1960000620001.1902", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("FERIA COMERCIAL YANTZAZA 2019", "FERIA COMERCIAL GANADERA YANTZAZA", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("0460000640001.0405", "0460000640001.0403", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("MERCADO AGROGANADERO MONTUFAR", "FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("0560000620001.0509", "0560000620001.0502", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("CENTRO DE COMERCIO DE GANADO SAN MIGUEL DE SALCEDO", "FERIA DE COMERCIALIZACION DE ANIMALES MAYORES MEGASA - SALCEDO", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("0560000620001.0508", "0560000620001.0502", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("CENTRO DE COMERCIO DE GANADO SAN MIGUEL DE SALCEDO", "FERIA DE COMERCIALIZACION DE ANIMALES MAYORES MEGASA - SALCEDO", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("1060031140001.1004", "1060031140001.1003", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("Feria de Animales La Cruz", "FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("1791803450001.2202", "1791803450001.2201", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("feria de comercializacion", "REINA DEL CISNE", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("2390017130001.1703", "2390017130001.1701", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("1790953971001.2307", "1790953971001.2304", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("FERIA COMERCIAL ASOGAN SANTO DOMINGO", "FERIA COMERCIAL ASOGAN SD", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("1860001020001.1801", "1860001020001.1802", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("FERIA DE COMERCIALIZACIÓN DE PORCINOS", "Feria de Porcinos Cevallos", m2$sitio.origen)

m2$codigo.sitio.origen <- gsub("1860000800001.1802", "1860000800001.1801", m2$codigo.sitio.origen)
m2$sitio.origen <- gsub("	FERIA PORCINOS QUERO", "FERIA SANTIAGO DE QUERO", m2$sitio.origen)


# Destiny
m2$codigo.sitio.destino <- gsub("1960000620001.1903", "1960000620001.1902", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("FERIA COMERCIAL YANTZAZA 2019", "FERIA COMERCIAL GANADERA YANTZAZA", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("0460000640001.0405", "0460000640001.0403", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("MERCADO AGROGANADERO MONTUFAR", "FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("0560000620001.0509", "0560000620001.0502", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("CENTRO DE COMERCIO DE GANADO SAN MIGUEL DE SALCEDO", "FERIA DE COMERCIALIZACION DE ANIMALES MAYORES MEGASA - SALCEDO", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("0560000620001.0508", "0560000620001.0502", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("CENTRO DE COMERCIO DE GANADO SAN MIGUEL DE SALCEDO", "FERIA DE COMERCIALIZACION DE ANIMALES MAYORES MEGASA - SALCEDO", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("1060031140001.1004", "1060031140001.1003", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("Feria de Animales La Cruz", "FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("1791803450001.2202", "1791803450001.2201", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("feria de comercializacion", "REINA DEL CISNE", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("2390017130001.1703", "2390017130001.1701", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("1790953971001.2307", "1790953971001.2304", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("FERIA COMERCIAL ASOGAN SANTO DOMINGO", "FERIA COMERCIAL ASOGAN SD", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("1860001020001.1801", "1860001020001.1802", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("FERIA DE COMERCIALIZACIÓN DE PORCINOS", "Feria de Porcinos Cevallos", m2$sitio.destino)

m2$codigo.sitio.destino <- gsub("1860000800001.1802", "1860000800001.1801", m2$codigo.sitio.destino)
m2$sitio.destino <- gsub("	FERIA PORCINOS QUERO", "FERIA SANTIAGO DE QUERO", m2$sitio.destino)


# #this operations have to be changed to slaugtherhouse
# Matadero Municipal de Yantzaza
# MATADERO MUNICIPAL GAD PELILEO

m2 <- m2 %>% mutate(operacion.destino = ifelse(codigo.sitio.destino == "1960000620001.1901",
                                             "Faenador", operacion.destino))

m2 <- m2 %>% mutate(operacion.destino = ifelse(codigo.sitio.destino == "1860000640001.1801",
                                             "Faenador", operacion.destino))

# Final checkign of markets ----

m2 %>%
  group_by(Year=ano, operacion.origen)%>%
  summarise(Propietarios=length(unique(codigo.sitio.origen))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen                 `2017` `2018` `2019`
# 1 Comercializador                    1677   2006   2300
# 2 Feria de comercialización animal     45     48     53
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47965  57194  65275

m2 %>%
  group_by(Year=ano, operacion.destino)%>%
  summarise(Propietarios=length(unique(codigo.sitio.destino))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7980   8140  11436
# 2 Faenador                            160    147    154
# 3 Feria de comercialización animal     46     50     53
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20807  31354  44218

# Check point:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190991 # 1190991
# Number of animals
sum(as.numeric(m2$cantidad))
9904714 # 9904714


#write arquive with ordered Markets 
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
# write.csv(m2, file="movimentos_db_ferias.csv")

---------------------------------------------
#1.10 Checking slaughterhouses  ----
---------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
m2 <- read.csv("movimentos_db_ferias.csv", colClasses = "character")
m2 <- m2[,-1]

# Checking faenador ----
faenador <- m2 %>% group_by(prov = provincia.destino,
                              canton = canton.destino,
                              parr = parroquia.destino,
                              nsitio = sitio.destino,
                              rz = razon.social.operador.destino,
                              id = identificacion.operador.destino, 
                              operac = operacion.destino, 
                              sitio = codigo.sitio.destino, ano) %>%
  filter(operacion.destino == "Faenador") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))


faenador_sitio <- faenador %>% 
  group_by(prov, canton,parr, nsitio, rz, id, operac, sitio) %>%
  summarise(CSM=sum(as.numeric(CSMI)),
            a=sum(as.numeric(animais)),
            animais=sum(as.numeric(animais)))

# The folowing 35 faenador were changed to producer
# [1] "0160000430001.0102" "0160001590001.0101" "0160000860001.0101" "0201060910001.0201"
# [5] "0360001120001.0301" "0691745457001.0601" "0701037269.0702"    "0960005960001.0901"
# [9] "0960001110001.0901" "1060000500001.1002" "1160000240001.1105" "1160000240001.1104"
# [13] "1260000300001.1201" "1260000490001.1201" "1260001700001.1202" "1260000730001.1201"
# [17] "1260001030001.1201" "1360069910001.1301" "1360000630001.1301" "1360003220001.1301"
# [21] "1360001440001.1303" "1360001440001.1302" "1360001440001.1304" "1360001520001.1302"
# [25] "1460001690001.1401" "1760010700001.1702" "1768154770001.1702" "1705355376.1702"   
# [29] "0960006340001.2401" "1716148299.2301"    "2160000480001.2102" "2160011760001.2102"
# [33] "1104449887001.2102" "1900283712001.1901" "1900283951.1901"  

# Changing markets with less than 40 CSMI to producers

#transforming to producer the ferias with less than 40 CSM
fa <- unique(faenador$sitio[faenador$CSMI < 40])


# 35 faenadores that didnt receibed <40 CSMI a year are transformed to producers
m2 <- m2 %>%
  mutate(operacion.destino = if_else(codigo.sitio.destino %in% fa,
                                     "Productor", operacion.destino))

# Checking last

m2 %>%
  group_by(Year=ano, operacion.destino)%>%
  summarise(Propietarios=length(unique(codigo.sitio.destino))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7979   8139  11436
# 2 Faenador                            128    127    134
# 3 Feria de comercialización animal     44     48     51
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20837  31384  44249


faenador <- m2 %>% filter(!operacion.destino == "Faenador")%>%
  filter(str_detect(sitio.destino, "Matadero"))%>%
  select(codigo.sitio.destino,sitio.destino, operacion.destino)

unique(faenador$sitio.destino)
unique(faenador$codigo.sitio.destino)



#write arquive with ordered slaughterhouses 
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
# write.csv(m2, file="movimentos_db_mark_sla.csv")

#_-------------------------------------------------
  # 3.1 Descriptive characteristics ----
 --------------------------------------------------
library(tidyverse)
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2[,-1]

m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9904714

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9904714
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado)) # 1190991

# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(codigo.sitio.origen, codigo.sitio.destino))))) %>%
  spread(key="Year", value = "Propietarios")

# `2017` `2018` `2019`
# 66432  81900 100998


# 1 Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen,codigo.sitio.destino)))) 

# Year    Premises
# 1 2017     66430
# 2 2018     81900
# 3 2019    100998
100998/66430 = 152,03

# 101143/66534= 152.02%

t <- m2 %>%
  group_by(origen=operacion.origen, destino=operacion.destino, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

# Number of movements
length(unique(m2$numero.certificado))
1190991
# Number of animals
sum(as.numeric(m2$cantidad))
9904714


# 3 Number of premises by year and category ----
m_origen <- data.frame(m2$codigo.sitio.origen, m2$operacion.origen, m2$ano)
m_destino <- data.frame(m2$codigo.sitio.destino, m2$operacion.destino, m2$ano)
colnames(m_origen) <- c("sitio", "operacion", "ano")
colnames(m_destino) <- c("sitio", "operacion", "ano")
m_origen_destino <- rbind(m_origen, m_destino)

m_origen_destino %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(paste(sitio, operacion)))) %>%
  spread(key="Year", value=Premises)

premises <- m_origen_destino %>%
  group_by(Year=ano, operacion)%>%
  summarise(Premises=length(unique(paste(sitio, operacion)))) %>%
  spread(key="Year", value=Premises)

# operacion                        `2017` `2018` `2019`
# 1 Comercializador                    8085   8426  11818
# 2 Feria de comercialización animal     46     51     53
# 3 Operador Industrial                 112    115    121
# 4 Productor                         58400  73423  89171
# 5 Faenador                            128    127    134
# 
# write.csv(premises, file = "premises.csv")

#2017
# 8081+46+112+58400+128
# 66767

# 2019
# 11815+53+121+89171+134
# 101294
# 66760/101294  65.9%

# 4 Increment or decrement of these premisses 2017 vs 2019 ----
# Farms 52,74%
89158/58369
#collection 46,20%
11815/8081
#industrial 8%
121/112
# Market 15%
# 54/47
# Slaughterhouse -4%
# 145/151

# m2 <- m3

# 5 Descriptive Movements  ----
# Calculating cantidad=mean number of movements by origin and destiny

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen, destino=operacion.destino, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- round(t[,6]/sum(t$cantidad)*100,2)
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino X2017  X2018  X2019 cantidad          por
# 9  Feria de comercialización animal              Operador Industrial     0      2     13        5  0.001259455
# 4                   Comercializador              Operador Industrial     1      3     14        6  0.001511346
# 19                        Productor              Operador Industrial    88     95    107       97  0.024433434
# 13              Operador Industrial Feria de comercialización animal   624    660    717      667  0.168011345
# 1                   Comercializador                  Comercializador   443    735    968      715  0.180102117
# 8  Feria de comercialización animal Feria de comercialización animal   816   1416   1066     1099  0.276828288
# 15              Operador Industrial                        Productor   987   1768   2701     1819  0.458189860
# 5                   Comercializador                        Productor  1633   2100   2320     2018  0.508316184
# 14              Operador Industrial              Operador Industrial  3149   3281   3706     3379  0.851139933
# 11              Operador Industrial                  Comercializador  4148   4894   5557     4866  1.225701957
# 16                        Productor                  Comercializador  5632   7470   8687     7263  1.829484858
# 3                   Comercializador Feria de comercialización animal  6156   8380   9964     8167  2.057194387
# 2                   Comercializador                         Faenador 10347  13107  13546    12333  3.106572594
# 7  Feria de comercialización animal                         Faenador 16864  18522  22117    19168  4.828248072
# 6  Feria de comercialización animal                  Comercializador 19327  20995  27184    22502  5.668052907
# 12              Operador Industrial                         Faenador 18354  23611  25635    22533  5.675861530
# 20                        Productor                        Productor 24187  30212  34155    29518  7.435320670
# 10 Feria de comercialización animal                        Productor 38769  61470  86874    62371 15.710698066
# 17                        Productor                         Faenador 81836  89752 106297    92628 23.332166238
# 18                        Productor Feria de comercialización animal 81189 105988 130352   105843 26.660906757


# 6 % of movements througth markets ----
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#55.37 % af all movilizations goes througth markets  


#Number of movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#146662

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1165774

#Animals per shipment from premises to slaugtherhouses
#manual calc in excle looking at the tile

# Number of animals
t2 <- m2 %>%
  group_by(origen=operacion.origen, destino=operacion.destino, ano) %>%
  summarise(contagem = sum(cantidad)) %>%
  spread(key="ano", value = "contagem")
t2 <- data.frame(t2)

t2$X2017[is.na(t2$X2017)] <- 0
t2$X2017[is.na(t2$X2018)] <- 0
t2$X2017[is.na(t2$X2019)] <- 0
t2$cantidad <- round(rowMeans(t2[,3:5]),0)

# Calculating percentages of every mov type
t2$por <- round(t2[,6]/sum(t2[,6])*100,2)

#orderign by porcentage
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 3)), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6414   1748     4999  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal   5593   9411   7442     7482  0.23
# 13              Operador Industrial Feria de comercialización animal  10667  11349  12299    11438  0.35
# 5                   Comercializador                        Productor  10949  15648  18766    15121  0.46
# 3                   Comercializador Feria de comercialización animal  37842  55133  65086    52687  1.60
# 15              Operador Industrial                        Productor  28959  59252  70088    52766  1.60
# 16                        Productor                  Comercializador  54058  70260  81920    68746  2.08
# 11              Operador Industrial                  Comercializador  89487  65622  62591    72567  2.20
# 7  Feria de comercialización animal                         Faenador  65120  70149  91725    75665  2.29
# 2                   Comercializador                         Faenador 103052  69743  62646    78480  2.38
# 6  Feria de comercialización animal                  Comercializador  96999 104612 117197   106269  3.22
# 20                        Productor                        Productor 137100 181071 213984   177385  5.37
# 10 Feria de comercialización animal                        Productor 152443 238382 315519   235448  7.13
# 17                        Productor                         Faenador 232141 244774 299586   258834  7.84
# 18                        Productor Feria de comercialización animal 346888 445783 554458   449043 13.60
# 12              Operador Industrial                         Faenador 655342 774034 829010   752795 22.80
# 14              Operador Industrial              Operador Industrial 801422 877789 946486   875232 26.51


# Distances
td <- m2 %>%
  group_by(origen=operacion.origen, destino=operacion.destino, ano) %>%
  summarise(contagem=median(dist)) %>%
  spread(key="ano", value = "contagem")

#We have many with 0 im going to convert o to 1
m2$dist2 <- m2$dist
table(m2$dist)

# Movements winhin the same parish
420103/1190991
# 35%
summary(m2$dist)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   11.25   23.83   27.77  597.86 

m2$dist2[m2$dist2 == "0"] <- "1"
  
td <-   m2 %>%
  group_by(origen=operacion.origen, destino=operacion.destino) %>%
  summarise(median=median(dist), q1=quantile(dist, probs = c(0.25)), 
            q3=quantile(dist, probs = c(0.75)), max=max(dist)) 

td <- data.frame(td)

td$X2017[is.na(td$X2017)] <- 0
td$cantidad <- round(rowMeans(td[, 3:5]), 0)
td$median <- round(td$median,0)
td$q1 <- round(td$q1,0)
td$q3 <- round(td$q3,0)
td$max <- round(td$max,0)

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
# write.csv(t, file="t-movements.csv")
# write.csv(t2, file="t-animals.csv")
# write.csv(td, file="t-distances.csv")

# 7 Number of animals per movement in traditional farms ----
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
#5.94%

# 8 Number of animals per movement in industrial ----
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
#51.99

--------------------------------------------------
#Fig 1 Tile mean number of movements and animals per year ----

#Spanish number of movements
a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Feria de comercialización animal",
                             "Productor",
                             "Comercializador", 
                             "Operador Industrial",
                             "Faenador"))) +
  scale_y_discrete(limits=(c("Operador Industrial",
                             "Comercializador",
                             "Feria de comercialización animal",
                             "Productor"))) +
  scale_fill_gradient(low = "white",
                      high = "#8DA0CB")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destiny", y="Origin", fill="Number of movements")

a

#English number of movements ---
t$origen <- gsub("Comercializador", "Collection", t$origen)
t$origen <- gsub("Feria de comercialización animal", "Market", t$origen)
t$origen <- gsub("Operador Industrial", "Industry", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industry", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Abattoir", t$destino)


# Fig. 1A ----
a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industry",
                             "Abattoir"))) +
  scale_y_discrete(limits=(c("Industry",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = scales::comma(cantidad,accuracy = 1)))+
  labs(x="", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12),
    text = element_text(size = 14))
a

# Spanish number of animals ---
b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Feria de comercialización animal",
                             "Productor",
                             "Comercializador", 
                             "Operador Industrial"))) +
  scale_y_discrete(limits=(c("Operador Industrial",
                             "Comercializador",
                             "Feria de comercialización animal",
                             "Productor"))) +
  scale_fill_gradient(low = "white",
                      high = "#8DA0CB") +
  geom_text(aes(label = scales::comma(t2$cantidad))) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industry", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industry", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Abattoir", t2$destino)
#

# Fig. 1B ----
b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industry",
                             "Abattoir"))) +
  scale_y_discrete(limits=(c("Industry",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = scales::comma(cantidad, accuracy = 1))) +
  labs(tag = "B")+
  labs(x="Destination", y="", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12),
    text = element_text(size = 14))
b  

# Scientific notation disable on graphics labels
options(scipen = 999)

#install.packages("ggpubr")
library(ggpubr)

# Fig. 1AB ----
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
tiff(filename = "Figure_1.tiff",
     width=16, height=12, units="cm", res = 800, 
     pointsize = 6, compression = "lzw")

ggarrange(a,b, ncol = 1)

dev.off()

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$codigo.sitio.destino[m2$operacion.destino == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756

#_ --------------------------------------------------
# Calculating distance of movements----
# 1 Importing db

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) #9904714

library(tidyverse)
s_simplified <- m2 %>% 
  group_by(provincia.origen, 
           canton.origen, 
           parroquia.origen, 
           origen=paste(provincia.origen, canton.origen, parroquia.origen),
           provincia.destino, 
           canton.destino, 
           parroquia.destino, 
           destino=paste(provincia.destino, canton.destino, parroquia.destino), ano) %>%
  summarise(Freq=n())

sum(s_simplified$Freq) #1190991

# 2 creating centroids ----
origen <- data.frame(s_simplified[,1:4])
destino <- data.frame(s_simplified[,5:8])
colnames(destino) <- colnames(origen)

data <- rbind(origen, destino)

data <- data %>%
  group_by(provincia=provincia.origen, canton=canton.origen, 
           parroquia=parroquia.origen) %>%
  summarise(cantidad=n())

vigi <- data %>%
  group_by(provincia, canton, parroquia) %>%
  summarise(cantidad = sum(cantidad))

sum(vigi$cantidad , na.rm = TRUE) 

# 3 Mapa para obter centroides ----
library(rgdal)
library(gdata)

ec3<-readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# 4 Describing the political division ----
length(unique(ec3$DPA_PROVIN)) #24
length(unique(ec3$DPA_CANTON)) #224
length(unique(ec3$DPA_PARROQ)) #1040

#mapa para vigilancia 

f{
  # Mapa para vigilancia
  #Provincia
  #atualizado 27.01.2020 banco cadastro 2018-2019 com codigo de sitio
  # fazer colunas comparaveis
  ec3@data$provincia <- ec3@data$DPA_DESPRO
  vigi$p <- vigi$provincia
  
  ec3@data$provincia <- gsub("Ñ","N", ec3@data$provincia)
  ec3@data$provincia <- gsub("Ã","N", ec3@data$provincia)
  
  # Crio os comparaveis
  ec3@data$p <- trim(tolower(paste(ec3@data$provincia)))
  vigi$p <- trim(tolower(paste(vigi$p)))
  
  vigi$p <- gsub("á","a", vigi$p)
  vigi$p <- gsub("ú","u", vigi$p)
  vigi$p <- gsub("é","e", vigi$p)
  vigi$p <- gsub("í","i", vigi$p)
  vigi$p <- gsub("ó","o", vigi$p)
  vigi$p <- gsub("ñ","n", vigi$p)
  
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_p <- tolower(ec3@data$DPA_PROVIN[match(vigi$p, ec3@data$p)])
  
  sum(is.na(as.numeric(vigi$c_p)))
  vigi[is.na(as.numeric(vigi$c_p)), 5]
  # 0
  
  #Canton
  ec3@data$canton <- tolower(ec3@data$DPA_DESCAN)
  vigi$cant <- tolower(vigi$canton)
  
  vigi$cant <- gsub("á","a", vigi$cant)
  vigi$cant <- gsub("ú","u", vigi$cant)
  vigi$cant <- gsub("é","e", vigi$cant)
  vigi$cant <- gsub("í","i", vigi$cant)
  vigi$cant <- gsub("ó","o", vigi$cant)
  
  # Fazer mudancas considerando mapa padrao ouro
  ec3@data$canton <- gsub("ñ","n", ec3@data$canton)
  ec3@data$canton <- gsub("ð","n", ec3@data$canton)
  ec3@data$canton <- gsub("puebloviejo","pueblo viejo", ec3@data$canton)
  
  #tirando o parenteses
  vigi$cant <- trim(gsub("\\(.*","", vigi$cant))
  
  #mudanças para mudar vigi adaptando para ec3@data
  #vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  vigi$cant <- gsub("pelipeo","pelileo", vigi$cant)
  vigi$cant <- gsub("ñ","n", vigi$cant)
  vigi$cant <- gsub("francisco de orellana","orellana", vigi$cant)
  vigi$cant <- gsub("pelileo","san pedro de pelileo", vigi$cant)
  vigi$cant <- gsub("el empalme","empalme", vigi$cant)
  vigi$cant <- gsub("santiago de mendez","santiago", vigi$cant)
  vigi$cant <- gsub("urcuqui","san miguel de urcuqui", vigi$cant)
  vigi$cant <- gsub("marcelino mariduena", "crnel. marcelino mariduena", vigi$cant)
  vigi$cant <- gsub("yaguachi", "san jacinto de yaguachi", vigi$cant) #
  vigi$cant <- gsub("pueblobiejo","pueblo viejo", vigi$cant)
  vigi$cant <- gsub("macas","morona", vigi$cant)
  vigi$cant <- gsub("joya de los sachas","la joya de los sachas", vigi$cant) #
  vigi$cant <- gsub("puyo","pastaza", vigi$cant)
  vigi$cant <- gsub("pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("santiago de santiago de pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("rio verde","rioverde", vigi$cant)
  vigi$cant <- gsub("general antonio elizalde","gnral. antonio elizalde", vigi$cant)
  vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  #vigi$cant <- gsub("banos","banos de agua santa", vigi$cant)
  
  #crio coluna conjunta para comparar
  ec3@data$c <- trim(tolower(paste(ec3@data$provincia,ec3@data$canton)))
  vigi$c <- trim(tolower(paste(vigi$p, vigi$cant)))
  
  #caso especial la concordia cambiandole de provincia
  vigi$c <- gsub("santo domingo de los tsachilas la concordia","esmeraldas la concordia", vigi$c)
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_c <- ec3@data$DPA_CANTON[match(vigi$c, ec3@data$c)]
  
  #numero de catones sem id
  sum(is.na(as.numeric(vigi$c_c)))
  # 0
  
  #cuenta, numero e ordem deles
  sum(is.na(as.numeric(vigi$c_c)))
  vigi[is.na(as.numeric(vigi$c_c)), 8]
  
  cant <-vigi[is.na(as.numeric(vigi$c_c)), 8]
  cant
  
  #Parroquia
  
  #Criacao e transferencia dos valores a novas colunas para comparacao
  ec3@data$parroquia <- ec3@data$DPA_DESPAR
  vigi$par <- tolower(vigi$parroquia)
  
  #Modificando novas colunas por dados comparaveis
  ec3@data$parroquia <- gsub("á","a", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("é","e", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("í","i", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ó","o", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ú","u", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("Ñ","N", ec3@data$parroquia)
  
  #tirando o parenteses
  ec3@data$parroquia <- trim(gsub("\\(.*","", ec3@data$parroquia))
  vigi$par <- trim(gsub("\\(.*","", vigi$par))
  
  #ec3@data$parroquia <- gsub("ALFREDO BAQUERIZO MORENO (JUJAN)","JUJAN", ec3@data$parroquia)
  vigi$par <- gsub("á","a", vigi$par)
  vigi$par <- gsub("é","e", vigi$par)
  vigi$par <- gsub("í","i", vigi$par)
  vigi$par <- gsub("ó","o", vigi$par)
  vigi$par <- gsub("ú","u", vigi$par)
  vigi$par <- gsub("ñ","n", vigi$par)
  vigi$par <- gsub("ü","u", vigi$par)
  vigi$par <- gsub("crnl.","crnel.", vigi$par)
  
  vigi$par <- gsub("holgupin","holguin", vigi$par)
  vigi$par <- gsub("conrdoncillo","cordoncillo", vigi$par)
  vigi$par <- gsub("curticapa","curtincapa", vigi$par)
  vigi$par <- gsub("general leonidas plaza gutierrez", "gral. leonidas plaza gutierrez",vigi$par)
  #vigi$par <- gsub("guayusa", "san jose de guayusa",vigi$par) #
  #vigi$par <- gsub("puerto francisco de orel", "puerto francisco de orellana",vigi$par) #
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san jose de alluriquin", "alluriquin",vigi$par)
  vigi$par <- gsub("santo domingo", "santo domingo de los colorados",vigi$par)
  vigi$par <- gsub("santo domingo de los colorados de onzole", "santo domingo de onzole",vigi$par)
  #vigi$par <- gsub("guasaganda", "GUASAGANDA (CAB. EN GUASAGANDA CENTRO)",vigi$par)
  #vigi$par <- gsub("simon bolivar", "SIMON BOLIVAR (JULIO MORENO)",vigi$par)
  #vigi$par <- gsub("julio e. moreno", "JULIO E. MORENO (CATANAHUAN GRANDE)",vigi$par)
  #vigi$par <- gsub("san pablo", "SAN PABLO (SAN PABLO DE ATENAS)",vigi$par)
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san lorenzo de jipijapa", "jipijapa",vigi$par)
  vigi$par <- gsub("santafe", "santa fe",vigi$par)
  vigi$par <- gsub("san jose de chazo", "san jose del chazo",vigi$par)
  vigi$par <- gsub("cibijies", "cubijies",vigi$par)
  vigi$par <- gsub("crnl. carlos concha torres", "crnel. carlos concha torres",vigi$par)
  vigi$par <- gsub("la lojas", "los lojas",vigi$par)
  vigi$par <- gsub("padre juan batista aguirre", "juan bautista aguirre",vigi$par)
  vigi$par <- gsub("velazco ibarra", "velasco ibarra",vigi$par)
  vigi$par <- gsub("gnral. antonio elizalde", "gral. antonio elizalde",vigi$par)
  vigi$par <- gsub("coronel marcelino mariduenas", "coronel marcelino mariduena",vigi$par)
  vigi$par <- gsub("tarida", "tarifa",vigi$par)
  vigi$par <- gsub("san francisco de natabuela", "san fco. de natabuela",vigi$par)
  vigi$par <- gsub("dr. miguel egas cabezas", "doctor miguel egas cabezas",vigi$par)
  vigi$par <- gsub("san francisco de sigsipamba", "san  fco. de sigsipamba",vigi$par)
  vigi$par <- gsub("chiquiribamba", "chuquiribamba",vigi$par)
  vigi$par <- gsub("bolsapamba", "bolaspamba",vigi$par)
  vigi$par <- gsub("santa susana de chiviaza", "sta susana de chiviaza",vigi$par)
  vigi$par <- gsub("pablo secto", "pablo sexto",vigi$par)
  vigi$par <- gsub("pumipamba", "rumipamba",vigi$par)
  vigi$par <- gsub("pani", "pano",vigi$par)
  vigi$par <- gsub("quinsamola", "quinsaloma",vigi$par)
  vigi$par <- gsub("pelileo grande", "pelileo",vigi$par)
  vigi$par <- gsub("jujan", "alfredo baquerizo moreno",vigi$par)
  vigi$par <- gsub("triunfo dorado", "triunfo-dorado",vigi$par)
  vigi$par <- gsub("chontaduro", "rioverde",vigi$par)
  vigi$par <- gsub("24 de mayo", "sucre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("general leonidas plaza g.", "gral. leonidas plaza gutierrez",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("3 de noviembre", "tres de noviembre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("julio moreno", "santo domingo de los tsachilas",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("sta. cecilia", "santa cecilia",vigi$par) #
  #vigi$par <- gsub("el playon de san francis", "el playon de san francisco",vigi$par) #
  #vigi$par <- gsub("banos", "banos de agua santa",vigi$par) #
  vigi$par <- gsub("el guismi", "el guisme",vigi$par) #
  vigi$par <- gsub("yanzatza", "yantzaza",vigi$par) #
  vigi$par <- gsub("nobol", "narcisa de jesus",vigi$par) #
  vigi$par <- gsub("crnel.lorenzo de garaicoa", "crnel. lorenzo de garaicoa",vigi$par) #
  vigi$par <- gsub("general antonio elizalde", "gral. antonio elizalde",vigi$par) #
  
  
  # Criando novas colunas para comparacao
  ec3@data$pp <- trim(tolower(paste(ec3@data$c,ec3@data$parroquia)))
  vigi$pp <- trim(tolower(paste(vigi$c,vigi$par)))
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # Transferindo cantidad vigilancia para o data frame
  ec3@data$cantidad <- vigi$cantidad[match(ec3@data$pp, vigi$pp)]
  
  #numero de parroquias sem id
  sum(is.na(as.numeric(vigi$c_pp)))
  #227
  
  #cuenta, nombre e ordem
  sum(is.na(as.numeric(vigi$c_pp)))
  #229
  
  vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  #Transformando as parroquias urbanas atuais em parroquias anteriores a divisao
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca banos de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca cuenca de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca bellavista", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el batan", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el sagrario", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el vecino", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca gil ramirez davalos", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca huaynacapac", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca machangara", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca monay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca octavio cordero", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca palacios", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san blas", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san sebatian", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca sucre", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca totoracocha", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca yanuncay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca canaribamba", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay gualaceo daniel cordova", "azuay gualaceo gualaceo",vigi$pp)
  vigi$pp <- gsub("azuay sigsig jima", "azuay sigsig sigsig",vigi$pp)
  vigi$pp <- gsub("azuay ona ona", "azuay ona san felipe de ona",vigi$pp)
  
  vigi$pp <- gsub("bolivar guaranda guanujo", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chavez", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chaves", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabreil ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabriel ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar las naves las mercedes", "bolivar las naves las naves",vigi$pp)
  
  vigi$pp <- gsub("canar azogues aurelio bayas martinez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues azoguez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues borrero", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues san francisco", "canar azogues azogues",vigi$pp)
  
  vigi$pp <- gsub("carchi espejo 27 de septiembre", "carchi espejo el angel",vigi$pp)
  vigi$pp <- gsub("carchi montufar gonzalez suarez", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi montufar san jose", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi tulcan gonzalez suarez", "carchi tulcan tulcan",vigi$pp)
  
  vigi$pp <- gsub("chimborazo colta cajabamba", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo colta sicalpa", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano la matriz", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  
  vigi$pp <- gsub("chimborazo riobamba lizarzaburu", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba maldonado", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba velasco", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba veloz", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba yaruquies", "chimborazo riobamba riobamba",vigi$pp)
  
  vigi$pp <- gsub("cotopaxi la mana el carmen", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga el triunfo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga eloy alfaro", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga ignacio flores", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga juan montalvo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga la matriz", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga san buenaventura", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  
  vigi$pp <- gsub("el oro huaquillas ecuador", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas el paraiso", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas milton reyes", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas union lojana", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas hualtaco", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro las lajas valle hermoso", "el oro las lajas la victoria",vigi$pp)
  vigi$pp <- gsub("el oro machala el cambio", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala la providencia", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala nueve de mayo", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala puerto bolivar", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro pasaje bolivar", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje loma de franco", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje ochoa leon", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje tres cerritos", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pinas la matriz", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas la susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas pinas grande", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa jumon", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa nuevo santa rosa", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa puerto jeli", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("esmeraldas eloy alfaro esmeraldas norte", "esmeraldas eloy alfaro santa lucia de las penas",vigi$pp)
  
  vigi$pp <- gsub("esmeraldas esmeraldas luis tello", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas simon torres", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas 5 de agosto", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas bartolome ruiz", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas la concordia las villegas", "esmeraldas la concordia la villegas",vigi$pp)
  
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule la uaurora", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule padre juan bautista aguirre", "guayas daule daule",vigi$pp) #
  vigi$pp <- gsub("guayas duran el recreo", "guayas duran eloy alfaro",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil pascuales", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil bolivar", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ayacucho", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil carbo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil febres cordero", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil garcia moreno", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil letamendi", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil nueve de octubre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil olmedo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil rocafuerte", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil sucre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil tarqui", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil urdaneta", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas salitre bocana", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre central", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre grnl. vernaza", "guayas salitre el salitre",vigi$pp)
  
  vigi$pp <- gsub("imbabura antonio ante andrade marin", "imbabura antonio ante atuntaqui",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi sagrario",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra caranqui", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra guayaquil de alpachaca", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra la dolorosa del priorato", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra san francisco", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo jordan", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo san luis", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi san francisco", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra sagrario", "imbabura ibarra san miguel de ibarra",vigi$pp)
  
  vigi$pp <- gsub("loja calvas chile", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja calvas san vicente", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja catamayo san jose", "loja catamayo catamayo",vigi$pp)
  vigi$pp <- gsub("loja loja el sagrario", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja san sebastian", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja sucre", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja macara general eloy alfaro", "loja macara macara",vigi$pp)
  vigi$pp <- gsub("loja paltas lourdes", "loja paltas catacocha",vigi$pp)
  vigi$pp <- gsub("loja loja valle", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja santiago \"san salvador o james\"", "loja loja santiago",vigi$pp)
  
  
  vigi$pp <- gsub("los rios babahoyo clemente baquerizo", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo barreiro", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo dr. camilo ponce", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo el salto", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios buena fe 7 de agosto", "los rios buena fe san jacinto de buena fe",vigi$pp)
  vigi$pp <- gsub("los rios pueblo viejo san juan de iluman", "los rios pueblo viejo puebloviejo",vigi$pp)
  vigi$pp <- gsub("los rios ventanas quinsaloma", "los rios quinsaloma quinsaloma",vigi$pp)
  vigi$pp <- gsub("los rios valencia la esperanza", "los rios valencia valencia",vigi$pp)# no existe parroquia
  vigi$pp <- gsub("los rios valencia la union", "los rios valencia valencia",vigi$pp)#
  vigi$pp <- gsub("los rios valencia vergel", "los rios valencia valencia",vigi$pp)#
  
  vigi$pp <- gsub("los rios quevedo 24 de mayo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo guayacan", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo nicolas infante diaz", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san camilo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san cristobal", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo sucre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo siete de octubre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo venus del rio quevedo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  
  vigi$pp <- gsub("manabi chone santa rita", "manabi chone chone",vigi$pp)
  vigi$pp <- gsub("manabi el carmen 4 de diciembre", "manabi el carmen el carmen",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa dr. miguel moran lucio", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa manuel inocencio parrales y guale", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi manta eloy alfaro", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta tarqui", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta los esteros", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta san mateo", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi montecristi el colorado", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi general eloy alfaro", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi leonidas proano", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi anibal san andres", "manabi montecristi montecristi",vigi$pp) #agregada 14.02.2020 Network_Description.R
  
  vigi$pp <- gsub("manabi portoviejo 12 de marzo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo 18 de octubre", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo andres de vera", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo colon", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo francisco pacheco", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo picoaza", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo san pablo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo simon bolivar", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi santa ana lodana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana de vuelta larga de vuelta larga", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi sucre leonidas plaza gutierrez", "manabi sucre bahia de caraquez",vigi$pp)
  
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago tiwintza puyo", "morona santiago tiwintza santiago",vigi$pp)
  
  vigi$pp <- gsub("pichincha cayambe ayora", "pichincha cayambe san jose de ayora",vigi$pp)
  vigi$pp <- gsub("pichincha cayambe juan montalvo", "pichincha cayambe cayambe",vigi$pp)
  vigi$pp <- gsub("pichincha quito belisario quevedo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito benalcazar", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito carcelen", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito centro historico", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cotocollao", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chaupicruz", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chilibulo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chillogallo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chimbacalle", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cochapamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito comite del pueblo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito guamani", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito el condado", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito eloy alfaro", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito inaquito", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito itchimbia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la concepcion", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ecuatoriana", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ferroviaria", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la floresta", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la libertad", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la mena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito mariscal sucre", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito ponceano", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito puengasi", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito quitumbe", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito rumipamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san antonio de minas", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san bartolo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san isidro del inca", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san roque", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito solanda", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito turubamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la magdalena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san pedro de taboada", "pichincha ruminahui sangolqui",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san rafael", "pichincha ruminahui sangolqui",vigi$pp)
  
  vigi$pp <- gsub("santa elena salinas santa rosa", "santa elena salinas salinas",vigi$pp)
  vigi$pp <- gsub("santa elena santa elena ballenita", "santa elena santa elena santa elena",vigi$pp)
  
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chiguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo las mercedes", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo abraham calazacon", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chimguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio toachi", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio verde", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo zaracay", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los tsachilas", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los colorados de los colorados", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo nuevo isrrael", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  
  vigi$pp <- gsub("sucumbios lago agrio santa cruz", "sucumbios lago agrio santa cecilia",vigi$pp)
  
  vigi$pp <- gsub("tungurahua ambato atocha - ficoa", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato celiano monge", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi chico", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi loreto", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la merced", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la peninsula", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato matriz", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato pishilata", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san bartolo de pinllog", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san francisco", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pillaro ciudad nueva", "tungurahua santiago de pillaro pillaro",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pelileo pelileo grande", "tungurahua santiago de pelileo pelileo",vigi$pp)
  
  vigi$pp <- gsub("zamora chinchipe zamora el limon", "zamora chinchipe zamora zamora",vigi$pp)
  
  #transferencia de codigo do mapa os que forem mach da pro-can-parr
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # agregando os valores que foram transformados de parroquias urbanas a normais
  vigi2 <- vigi %>%
    group_by(pp) %>%
    summarise(cantidad = sum(cantidad))
  
  #Tranferindo valores agregados
  ec3@data$cantidad <- vigi2$cantidad[match(ec3@data$pp, vigi2$pp)]
  
  # Comparando numeros
  sum(vigi$cantidad, na.rm = TRUE)
  sum(vigi2$cantidad, na.rm = TRUE) -
    sum(ec3@data$cantidad, na.rm = TRUE)
  
  ####ANTERIOR AINDA NAO DELETAR
  sum(is.na(as.numeric(vigi$c_pp)))
  #157
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  par
  
  # Animais faltantes no mapa
  sum(vigi$cantidad) - sum(ec3@data$cantidad, na.rm = TRUE)
  
}

vigi$database.id <- paste(vigi$provincia, vigi$canton, vigi$parroquia)
# vigi$x <- ec3@data$x[match(vigi$c_pp, ec3@data$DPA_PARROQ)]
# vigi$y <- ec3@data$y[match(vigi$c_pp, ec3@data$DPA_PARROQ)]

# 5 Criando os centroides ----
#install.packages("rgeos")
library(rgdal)
library(rgeos)

trueCentroids <- data.frame(gCentroid(ec3,byid=TRUE))

ec3@data$x <- trueCentroids$x
ec3@data$y <- trueCentroids$y

# 6 Pasar os centroides para correspondence 
vigi$x <- ec3@data$x[match(vigi$c_pp, ec3@data$DPA_PARROQ)]
vigi$y <- ec3@data$y[match(vigi$c_pp, ec3@data$DPA_PARROQ)]

#7 Creating bank
library(epinemo)
m2$origen <- paste(m2$provincia.origen, m2$canton.origen, m2$parroquia.origen)
m2$destino <- paste(m2$provincia.destino, m2$canton.destino, m2$parroquia.destino)

m2 <- data.frame(m2)
banco <- createUniqueIds(m2,
                         from = 'origen',
                         to = 'destino')

banco$correspondence$x <- vigi$x[match(banco$correspondence$database.id, 
                                       vigi$database.id)]
banco$correspondence$y <- vigi$y[match(banco$correspondence$database.id, 
                                       vigi$database.id)]

banco$movements$x_origem <- banco$correspondence$x[match(banco$movements$origen, 
                                                         banco$correspondence$database.id)]
banco$movements$y_origem <- banco$correspondence$y[match(banco$movements$origen, 
                                                         banco$correspondence$database.id)]

banco$movements$x_destino <- banco$correspondence$x[match(banco$movements$destino, 
                                                          banco$correspondence$database.id)]
banco$movements$y_destino <- banco$correspondence$y[match(banco$movements$destino, 
                                                          banco$correspondence$database.id)]
m3 <- banco$movements

library(geosphere)
m3$dist <- distGeo(matrix(c(m3$x_origem, m3$y_origem), ncol=2),
                   matrix(c(m3$x_destino, m3$y_destino), ncol = 2))/1000
boxplot(m3$dist)
mean(m3$dist)
plot(log(m3$dist))
hist(m3$dist)
rug(m3$dist)
summary(m3$dist)

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
# write.csv(m3, file="movimentos_dist.csv")
m2 <- read.csv(file = "movimentos_dist.csv")
m2$dist <- as.numeric(m2$dist) # 


#_--------------------------------------------------
#3.2 Seasonal activity of movements on the network ----
--------------------------------------------------
# Read the m2 file ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]

m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) #9904714

  library(lubridate)
m2$fecha.inicio.vigencia <- dmy_hm(m2$fecha.inicio.vigencia)
m2$mes <- month(m2$fecha.inicio.vigencia)

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

#1 Premises by month ----

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2017)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                     (codigo.sitio.destino))))
# unique(c,d) Means the uniques of c in d
# unique(c(c,d)) means the uniques of c and d concatenated

# Year  mes   Premises
# 1 2017  01        9389
# 2 2017  02       13378
# 3 2017  03       17989
# 4 2017  04       18163
# 5 2017  05       22231
# 6 2017  06       24982
# 7 2017  07       24869
# 8 2017  08       24605
# 9 2017  09       19197
# 10 2017  10       22590
# 11 2017  11       21627
# 12 2017  12       25964

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2018)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

# Year  mes   Premises
# 1 2018  01       22717
# 2 2018  02       19408
# 3 2018  03       21706
# 4 2018  04       23439
# 5 2018  05       26843
# 6 2018  06       27442
# 7 2018  07       28585
# 8 2018  08       30571
# 9 2018  09       29351
# 10 2018  10       28605
# 11 2018  11       26686
# 12 2018  12       30742

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano==2019)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

# Year  mes   Premises
# 1 2019  01       29561
# 2 2019  02       30114
# 3 2019  03       31699
# 4 2019  04       28248
# 5 2019  05       34521
# 6 2019  06       33889
# 7 2019  07       33872
# 8 2019  08       35855
# 9 2019  09       32538
# 10 2019  10       29479
# 11 2019  11       30671
# 12 2019  12       37983


# 2 Comparing some months ----
# August 2019 vs 2018
2019 august 35855
2018 august 30571

35855/30571  17%

# Dezember 2019 vs 2018
2019 december 37983
2018 december 30742

37983/30742  24%

# 4 Montly premises analysis---- 
month2017 <- m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2017)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

mean(as.numeric(month2017$Premises)) #20415.33

month2018 <- m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2018)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

mean(as.numeric(month2018$Premises)) #26341.25

month2019 <- m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2019)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

mean(as.numeric(month2019$Premises)) #32369.17

mk_premises <- m2 %>% 
  group_by(ano,mes) %>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino))))

# 5 Montly movements analysis ----
# Mean movement
m2 %>% 
  group_by(ano) %>%
  summarise(mov=length(unique(numero.certificado)))

mov_month <- m2 %>% 
  group_by(ano, mes) %>%
  summarise(movement=n())

mov_month %>%
  group_by(ano) %>%
  summarise(mean(movement))

# ano   `mean(movement)`
# 1 2017            26212.
# 2 2018            32872.
# 3 2019            40165 

length(unique(m2$numero.certificado))

# 6 montly animals analysis ----

# Number of swine
# Mean movement
m2 %>% 
  group_by(ano) %>%
  summarise(animals=sum(as.numeric(cantidad)))

m2 %>% 
  group_by(ano) %>%
  summarise(animals= sum(as.numeric(cantidad)/12))

# ano   animals
# 1 2017  236617.
# 2 2018  275541.
# 3 2019  313235 

-------------------------------------------------------
# Seasonal on the network ----
# 7 MannKendall Tests ----

# install.packages("Kendall")
library(Kendall)
library(trend)
library(dplyr)
# 7.1 mannkendall simple premises -----
mk_premises <- m2 %>% 
  group_by(ano,mes) %>%
  summarise(premises=length(unique(c(codigo.sitio.origen),
                                    (codigo.sitio.destino))))

# Converting into a time series
pts <- ts(mk_premises$premises, start=c(2017,01), end=c(2019,12), frequency=12)
plot(pts)

#Running MannKendal test to see individual interactions
psmk <- smk.test(pts)
psmk #global
summary(psmk) #Season

#Change-point
pettitt.test(pts) # k=17

pts2 <- plot(decompose(pts))
plot(stl(pts,
         s.window="periodic"))

pts2 <- stl(pts,
    s.window="periodic")

summary(smk.test(pts2[["time.series"]][,3]))

pst3 <- data.frame(pts2[["time.series"]])
pst3$n <- 1:nrow(pst3)
library(ggplot2)
ggplot(pst3, aes(n,seasonal))+
  geom_line()+
  theme_minimal()

# Running MannKendall test to see score
#pmk <- MannKendall(pts)
# tau = 0.737, 2-sided pvalue =< 2.22e-16
#summary(pmk)

# 7.2 mannkendall simple movements -----
mk_movements <- m2 %>% 
  group_by(ano,mes) %>%
  summarise(movements=length(unique(numero.certificado)))

# Converting into a time series
mts <- ts(mk_movements$movements, start=c(2017,01), end=c(2019,12), frequency=12)
plot(mts)

#Running MannKendal test to see individual interactions
msmk <- smk.test(mts)
msmk #global
summary(msmk) #Season

#Change-point
pettitt.test(mts) # k=17
library(forecast)
plot(decompose(mts))
plot(stl(mts,
         s.window=3, t.window = 1001, robust = TRUE))

mts2 <- stl(mts,
            s.window=3, t.window = 1000, robust = FALSE)
summary(smk.test(mts2[["time.series"]][,3]))

mst3 <- data.frame(mts2[["time.series"]])
mst3$n <- 1:nrow(mst3)
mst3$n <- rep(1:12,3)

asd <- ggplot(mst3, aes(n,seasonal))+
  geom_line()+
  geom_point(col="red")+
  theme_minimal()

# With ggplot autoplot for movements
mts %>%
stl(s.window=3, t.window = 1000, robust = FALSE) %>%
  autoplot()+
  scale_color_continuous(palette = "Set2")+
  theme_minimal()+
  labs(title = "Seasonal decomposition of Time series by Loess
       Number of movements of pigs in Ecuador")+
  xlab("Month")
  
ggseasonplot(mts, polar = TRUE)

# Seasonal plot ----
# Fig 2A seasonal plot ----
seasonal.plot <- ggseasonplot(mts)+
  theme_minimal()+
  labs(tag = "A")+
  annotate("text", x=0.33, y=37000, label="k")+
  labs(y=NULL, x=NULL, color= NULL, title = NULL)+
  geom_line(size=1)+
theme(
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text = element_text(size = 11))  

# Fig 2B Seasonal subseries plot ----
seasonal.subseries <- ggsubseriesplot(mts)+
  theme_minimal()+
  labs(y=NULL, x=NULL)+
  labs(tag = "B")+
  geom_line(size=1, color="#66C2A5")+
  theme(
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 11))  

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"

library(ggpubr)
# Fig 2AB ----
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
tiff(filename = "Fig_2.tiff",
     width=12, height=10, units="cm", res = 600, 
     pointsize = 6, compression = "lzw")

ggarrange(seasonal.plot, seasonal.subseries,
          ncol=1,
          legend = "top",
          vjust = 3,
          hjust = 2)

dev.off()


# mannkendal animals
mk_animals <- m2 %>% 
  group_by(ano,mes) %>%
  summarise(animals=sum(as.numeric(cantidad)))

# Converting into a time series
ats <- ts(mk_animals$animals, start=c(2017,01), end=c(2019,12), frequency=12)
plot(ats)

#Running MannKendal test to see individual interactions
asmk <- smk.test(ats)
asmk #global
summary(asmk) #Season


#Change-point
pettitt.test(ats) # k=17

plot(decompose(pts))
plot(stl(pts,
         s.window="periodic"))

pts %>%
  stl(s.window=3, t.window = 1000, robust = FALSE) %>%
  autoplot()+
  scale_color_continuous(palette = "Set2")+
  theme_minimal()+
  labs(title = "Seasonal decomposition of Time series by Loess
       Number of pig premises in Ecuador")+
  xlab("Month")





# 8 Mann Kendal network ----------
install.packages("trend")
library(trend)
require(trend)


ats <- ts(mk_animals$animals, start=c(2017,01), end=c(2019,12), frequency=12)
plot(ats)

#Running MannKendal test to see individual interactions
asmk <- smk.test(ats)
asmk #global
summary(asmk) #Season

#Change-point
pettitt.test(ats) # k=17

plot(decompose(pts))
plot(stl(pts,
         s.window="periodic"))

#Number of premises (A), number of movements (B) ----

# and number of animals expressed by months and years. ----
# 9 Figure Numbers of swine premises, movements and animals ----
library(lubridate)
m2$month <- month(m2$mes, label = TRUE)

# Number os swine premises
g1 <-  
  m2 %>%
  group_by(Year=ano, month)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) %>%
  ggplot( aes(x=month, y= Premises, colour=Year, group=Year)) +
  geom_point() + 
  geom_line(size=1.5) +
  labs(x = NULL, y ="Premises", color="") +
  labs(tag = "A")+
  theme_minimal()+
  theme(text = element_text(size = 16))+
  scale_color_brewer(palette = "Set2")+
  annotate("text", x = 05,
           y = 30000,
           label = "k", col="black", size=5)


# graphic Number of swine movements
g2 <- m2 %>%
  group_by(year=ano, month)%>%
  summarise(movements=length(unique(numero.certificado))) %>%
  ggplot( aes(x=month, y= movements, color=year, group=year))+
  geom_point()+ geom_line(size=1.5)+
  labs(tag = "B")+
  theme_minimal()+
  labs(x = NULL, y ="Movements", color="") +
  theme(text = element_text(size = 16))+
  scale_color_brewer(palette = "Set2")+
  annotate("text", x = 05,
           y = 36000,
           label = "k", col="black", size=5)

# Number of animals
g3 <- m2 %>%
  group_by(year=ano, month)%>%
  summarise(swine=sum(as.numeric(cantidad))) %>%
  ggplot( aes(x=month, y= swine, color=year, group=year))+
  geom_point()+
  geom_line(size=1.5)+
  labs(tag = "C")+
  theme_minimal()+
  labs(x = NULL, y ="Animals", color="") +
  theme(text = element_text(size = 16))+
  scale_color_brewer(palette = "Set2")+
  annotate("text", x = 06,
           y = 290000,
           label = "k", col="black", size=5)

  
library(RColorBrewer)
brewer.pal(n=8,name="Set2")

library(ggpubr)
ggarrange(g1, g2, g3, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

z <- m2 %>%
  group_by(year=ano, month=mes) %>%
  summarise(movements=length(unique(numero.certificado)))
# 
# g2 <-
# m2 %>%
#   group_by(year=ano, month=mes) %>%
#   summarise(movements=length(unique(numero.certificado))) %>%
#   ggplot( aes(x=month, y= movements, color=month))+
#   geom_boxplot()+
#   labs(tag = "B")+
#   labs(x = NULL, y ="Movements") +
#   theme(text = element_text(size = 18))+
#   #scale_color_brewer(palette = "Paired")
#   scale_colour_grey(end = 0)
# brewer.pal(n=12,name="Set2")
# 
# 
m2 %>%
  group_by(year=ano,mes)%>%
  summarise(mean(length(unique(numero.certificado))))

# Colors specification hexagesimal
# View a single RColorBrewer palette by specifying its name
library(RColorBrewer)
display.brewer.pal(n = 8, name = 'Set2')
# Hexadecimal color specification 
brewer.pal(n = 8, name = "Set2")

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


display.brewer.pal(n = 8, name = 'Paired')
brewer.pal(n = 8, name = "Paired")

"#B3B3B3"
"#FB9A99"
"#E31A1C"

--------------------------------------------------
# Weekly and daily Temporal analysis ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]
m2 <- m2[m2$operacion.destino != "Faenador",]
#only used to know movements to slaughter
m2 <- m2[m2$operacion.destino == "Faenador",]

sum(as.numeric(m2$cantidad))
# 6407392 Total withoug slaughter
# 3497322 total only slaughter

#  Dates ----
library(lubridate)
m2$fecha.inicio.vigencia <- dmy_hm(m2$fecha.inicio.vigencia)

library(sjmisc)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Plotting the day of mobilizacion
# extracting the week day
m2$wday <- wday(m2$fecha.inicio.vigencia, label = TRUE, abbr = TRUE,
                week_start = getOption("lubridate.week.start", 1),
                locale = Sys.setlocale("LC_TIME", "C"))

m2 %>%
  mutate(week=floor_date(fecha.inicio.vigencia, unit="week"))%>%
  group_by(wday,week)%>%
  summarise(n=n())


# Fig. 3  dayly number of CSMI grouped by week ----
w <- m2 %>%
  group_by(week=floor_date(fecha.inicio.vigencia, 
                           unit = "week",
                           week_start = getOption("lubridate.week.start", 1)), 
           wday) %>%
  summarise(n=n())

#
w %>%
  group_by(week)%>%
  summarise(sum=sum(n))


week <- m2 %>%
  group_by(week=floor_date(fecha.inicio.vigencia,
                           week_start = getOption("lubridate.week.start", 1), 
                           unit = "week"))%>%
  summarise(n=n(), pigs=sum(as.numeric(cantidad)))

week %>%
  group_by(year(floor_date(week, unit = "year")))%>%
  summarize(mean_of_year=mean(n), max(n))
# `year(floor_date(week, unit = "year"))` mean_of_year
# 1                                    2016           2 
# 2                                    2017        3599.
# 3                                    2018        4778.
# 4                                    2019        5973.

# [1] 0.4618136
# > 1-(2195/4753)
# [1] 0.5381864
# > 1-(2195/4075)
# [1] 0.4613497
# > 1-(4075/4753)
# [1] 0.1426467
# > 1-(1297/4753)
# [1] 0.7271197


# Stats
wstats <- w %>%
  group_by(wday)%>%
  summarize(descr(n))
sd(c(315,600,505,1668,503,696,517))

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
# write.csv(wstats, file= "weekstats.csv")

# analyzing a time series an kendall
# Converting into a time series
summary(week$n)
# removing the first week of 2016 and last of 2019
week <- week[week$n !=2,]
week <- week[week$n !=924,]

week$nweek <- 1:156

pts <- ts(week$n, start=c(2017,1), end=c(2019,52), frequency=52)
pts <- ts(week$pigs, start=c(2017,1), end=c(2019,52), frequency=52)

pts <- ts(w$n, start=c(1,1), end=c(158,7), frequency=7)

plot(pts)
library(Kendall)
library(trend)
#Running MannKendal test to see individual interactions
psmk <- smk.test(pts)
psmk #global
summary(psmk) #Season

#Change-point
pettitt.test(pts) # k=17

plot(decompose(pts))
plot(stl(pts,
         s.window="periodic"))
stl <- stl(pts,
           s.window="periodic")

#Fig 3A. Boxplot of weekly movements ----

w2 <-  m2 %>%
  group_by(week=floor_date(fecha.inicio.vigencia, 
                           unit = "day", 
                           week_start = getOption("lubridate.week.start", 1)), 
           wday)%>%
  summarise(n=n()) %>%
  ggplot(aes(wday,n)) +
  geom_boxplot(width=0.7,
               outlier.colour = "#FC8D62",
               outlier.size = 0.4,
  )+
  scale_fill_brewer(palette = "Set2")+
  ylim(c(0,2600))+
  theme_minimal()+
  labs(tag = "A")+
  # geom_jitter(show.legend = FALSE, size=0.1,
  #             col="#66C2A5", alpha=0.8, hjust=2)+
  # # stat_summary(aes(label=round(..y..,1)),
  #                  fun = "mean", geom="text",
  #              vjust=-2)+
  labs(x=NULL, 
       y="Movements", 
       size=20)+
  theme(text = element_text(size = 12))

w2


# Fig. 3B   weekly movements----
library(RColorBrewer)

summary(week$n)
median(week$n)
# 4778

library(lubridate)
w1 <-  m2 %>%
  filter(fecha.inicio.vigencia < ("2019-12-23"))%>%
  group_by(week=floor_date(fecha.inicio.vigencia, 
                           unit = "week", 
                           week_start = getOption("lubridate.week.start", 1)), 
           wday)%>%
  summarise(n=n()) %>%
  ggplot(aes(week, n, fill=wday)) +
  geom_col()+
  scale_fill_brewer(palette = "Set2")+
  # ylim(c(0,13200))+
  theme_minimal()+
  # geom_hline(yintercept = 4778, col="black")+
  labs(x="Weeks", 
       y="Movements",
       fill=NULL,
       size=20)+
  labs(tag = "B")+
  annotate("text", x = as.POSIXct("2019-10-07"),
           y = 7500, label = "c", col="black", size=5)+
  annotate("text", x = as.POSIXct("2018-10-29"),
           y = 6200, label = "b", col="black", size=5)+
  annotate("text", x = as.POSIXct("2018-02-12"),
           y = 5200, label = "a", col="black", size=5) +
  annotate("text", x = as.POSIXct("2018-05-15", size=5),
           y = 6000, label = "K", col="red")+
  theme(text = element_text(size = 12))

w1

# Fig. 3AB  Pig movements by week ----
library(ggpubr)
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
tiff(filename = "Fig_3.tiff",
     width=16, height=10, units="cm", res = 600, 
     pointsize = 6, compression = "lzw")


ggarrange(w2,w1, heights = c(1, 1.5), nrow =2)

dev.off()



# hour of movements

m2 %>%
  mutate(horario=hour(fecha.inicio.vigencia))%>%
  group_by(operacion.origen)%>%
  summarise(descr(as.numeric(horario)))


# Collector
# 0.52*60
# 07H30
# Markets
# 0.63*60
# 07H37
# # INdustrial
# .9*60
# 10H54
# # Farms
# .68*60
# 7H40
# 
# # Slaugther 
# .1*60
# # operacion.origen        var   type  label      n NA.prc  mean    sd
# # <chr>                   <chr> <chr> <chr>  <int>  <dbl> <dbl> <dbl>
# #   1 Comercializador         dd    nume… dd     37000      0 13.1   3.87
# # 2 Feria de comercializac… dd    nume… dd     57503      0  9.80  2.06
# # 3 Operador Industrial     dd    nume… dd     67600      0 12.5   6.17
# # 4 Productor               dd    nume… dd    277885      0 12.9   3.74

#Fig.  Time of the movements ----
#only premises 
h1 <- m2 %>%
  group_by(week=floor_date(fecha.inicio.vigencia, 
                           unit = "week", 
                           week_start = getOption("lubridate.week.start", 1)), 
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, group=horario)) +
  geom_boxplot(width=0.7,
               outlier.colour = "#A6D854",
               #outlier.fill = "red",
               outlier.size = 0.8,
  )+
  labs(tag = "A")+
  scale_fill_brewer(palette="Set2")+
  theme_minimal()+
  labs(x=NULL, 
       y="Movements (week)", 
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 18))


h2 <-m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "b Trader",
                                   "Feria de comercialización animal" = "c Market",
                                   "Operador Industrial" = "a Industrial Farm",
                                   "Productor" = "d Backyard Farm")) %>%
  group_by(operacion.origen,
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, fill=operacion.origen)) +
  geom_col()+
  scale_fill_brewer(palette="Set2")+
  labs(tag = "A")+
  theme_minimal()+
  labs(x="Hour of the day", 
       y="Movements", 
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 18))
h2
library(scales)

ggarrange(h1,h2, heights = c(1.3, 1), nrow =2)



# New Fig. 8 Slaugtherhouses ----


# for this have to filter at the beggining == Faenador m2
h1 <- m2 %>%
  group_by(week=floor_date(fecha.inicio.vigencia, 
                           unit = "day", 
                           week_start = getOption("lubridate.week.start", 1)), 
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, group=horario)) +
  geom_boxplot(width=0.7,
               outlier.colour = "#A6D854",
               outlier.size = 0.8,
  )+
  labs(tag = "A")+
  scale_fill_brewer(palette="Set2")+
  theme_minimal()+
  labs(x=NULL, 
       y="Movements (week)", 
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 18))

h1
h2 <-m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "b Trader",
                                   "Feria de comercialización animal" = "c Market",
                                   "Operador Industrial" = "a Industrial Farm",
                                   "Productor" = "d Backyard Farm")) %>%
  group_by(operacion.origen,
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, fill=operacion.origen)) +
  geom_col()+
  scale_fill_brewer(palette="Set2")+
  labs(tag = "B")+
  theme_minimal()+
  labs(x="Hour of the day", 
       y="Movements", 
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 18))

h2
ggarrange(h1,h2, heights = c(1.3, 1), nrow =2)


# Fig. 4A Plot hour of movilization between premises ----
h3 <-m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "b Trader",
                                   "Feria de comercialización animal" = "c Market",
                                   "Operador Industrial" = "a Industrial",
                                   "Productor" = "d Farm")) %>%
  group_by(operacion.origen,
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, fill=operacion.origen)) +
  geom_col()+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = comma)+
  labs(tag = "A",
       title=NULL)+
  theme_minimal()+
  labs(x="Time (24-Hour)", 
       y=NULL,
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 12))
h3


# Fig. 4B hour of movilization to slaughter----
# Apply filter when importing m2 only slaughterhouses and run fecha inicio de vigencia as date

h4 <-m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "b Trader",
                                   "Feria de comercialización animal" = "c Market",
                                   "Operador Industrial" = "a Industrial",
                                   "Productor" = "d Farm")) %>%
  group_by(operacion.origen,
           horario=(hour(fecha.inicio.vigencia)))%>%
  summarise(n=n()) %>%
  ggplot(aes(horario, n, fill=operacion.origen)) +
  geom_col()+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = comma)+
  labs(tag = "B",
       title = NULL)+
  theme_minimal()+
  labs(x=NULL, 
       y=NULL, 
       size=20,
       fill=NULL)+
  theme(text = element_text(size = 12))

h3
h4
library(scales)

# Fig. 4AB ----
library(ggpubr)
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
tiff(filename = "Fig_4.tiff",
     width=10, height=10, units="cm", res = 600, 
     pointsize = 6, compression = "lzw")

ggarrange(h3, h4, nrow =2, legend = "bottom",
          heights = c(0.9, 1),
          common.legend = TRUE)

dev.off()


#_--------------------------------------------------
# 4 Figure spatial movements ----
--------------------------------------------------
# Creating the network of slaughterhouses or premises changing s or s----
library(dplyr)

# 4.1 Creating network of slaugther ----
s <- m2[m2$operacion.destino == "Faenador",]
table(s$operacion.destino)# 439988
# Resuming by operacion origen
t <- as.data.frame(table(s$operacion.origen))# 439988
t$Freq/sum(t$Freq) # [1] 0.0840932 0.1306922 0.1536406 0.6315740

#4.1.1 Creating network of premisses
s <- m2[m2$operacion.destino != "Faenador",]
table(s$operacion.destino)# 
table(s$operacion.origen)# 

#Simplify the network for provincia, canton e parroquia 
#and year this is to graphic each network by year

s_simplified <- s %>% 
  #filter(ano == 2017) %>%
  #filter(ano == 2018) %>%
  #filter(ano == 2019) %>%
  group_by(provincia.origen, canton.origen, parroquia.origen, origen=paste(provincia.origen, canton.origen, parroquia.origen),
           provincia.destino, canton.destino, parroquia.destino, destino=paste(provincia.destino, canton.destino, parroquia.destino), ano) %>%
  summarise(Freq=n())

sum(s_simplified$Freq)
#slaugtherhouses
# 2017 127401
# 2018 146919
# 2019 167595

#premises
# 2017 187149
# 2018 249469
# 2019 314385

# Slaugtherhouses: 2019 314385
library(epinemo)
library(igraph)
s_simplified <- data.frame(s_simplified)
banco <- createUniqueIds(s_simplified,
                         from = "origen",
                         to= "destino")

nodes.da.rede <- banco$correspondence$network.id
grafo <- simplify(graph_from_data_frame(banco$movements[, c("From", "To")], vertices = nodes.da.rede)) 
vcount(grafo) #nodes  Slaugther 17:818, 18:833 985, 19:865 
#                     Premises  17:1011 18:1059 19-11673
ecount(grafo) #arestas Slaugher 17:2557, 18:1797 19:3128 
#                     Premises  17:7591 18:10077 19-11679

# slaughter 17:1014
# Premises  17:7642

V(grafo)$in_degree <- degree(grafo, mode = "in")
V(grafo)$out_degree <- degree(grafo, mode = "out")
V(grafo)$all_degree <- degree(grafo, mode = "all")
page_rank_igraph <-page.rank(grafo)
V(grafo)$pagerank <- page_rank_igraph$vector

banco$correspondence$page_rank <- V(grafo)$pagerank
banco$correspondence$in_degree <- V(grafo)$in_degree
banco$correspondence$out_degree <- V(grafo)$out_degree
banco$correspondence$all_degree <- V(grafo)$all_degree

summary(banco$correspondence$in_degree)
summary(banco$correspondence$all_degree)
summary(banco$correspondence$out_degree)
summary(banco$correspondence$page_rank)

# 4.2 creating centroids ----
origen <- data.frame(s_simplified[,1:4])
destino <- data.frame(s_simplified[,5:8])
colnames(destino) <- colnames(origen)

data <- rbind(origen, destino)

data <- data %>%
  group_by(provincia=provincia.origen, canton=canton.origen, 
           parroquia=parroquia.origen, origen) %>%
  summarise(cantidad=n())

vigi <- data %>%
  group_by(provincia, canton, parroquia) %>%
  summarise(cantidad = sum(cantidad))

sum(vigi$cantidad , na.rm = TRUE)

#number of movements
# to slaugher      Premises
#2017:5222            15798
#2018:5838            20874
#2019:6512            24126

### Mapa para obter centroides
library(rgdal)
library(gdata)

ec3<-readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Describing the political division ----

length(unique(ec3$DPA_PROVIN)) #24
length(unique(ec3$DPA_CANTON)) #224
length(unique(ec3$DPA_PARROQ)) #1040

library(glue)
#mapa para vigilancia 
# Passing throug the map
f{
  # Mapa para vigilancia
  #Provincia
  #atualizado 27.01.2020 banco cadastro 2018-2019 com codigo de sitio
  # fazer colunas comparaveis
  ec3@data$provincia <- ec3@data$DPA_DESPRO
  vigi$p <- vigi$provincia
  
  ec3@data$provincia <- gsub("Ñ","N", ec3@data$provincia)
  ec3@data$provincia <- gsub("Ã","N", ec3@data$provincia)
  
  # Crio os comparaveis
  ec3@data$p <- trim(tolower(paste(ec3@data$provincia)))
  vigi$p <- trim(tolower(paste(vigi$p)))
  
  vigi$p <- gsub("á","a", vigi$p)
  vigi$p <- gsub("ú","u", vigi$p)
  vigi$p <- gsub("é","e", vigi$p)
  vigi$p <- gsub("í","i", vigi$p)
  vigi$p <- gsub("ó","o", vigi$p)
  vigi$p <- gsub("ñ","n", vigi$p)
  
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_p <- tolower(ec3@data$DPA_PROVIN[match(vigi$p, ec3@data$p)])
  
  sum(is.na(as.numeric(vigi$c_p)))
  vigi[is.na(as.numeric(vigi$c_p)), 5]
  # 0
  
  #Canton
  ec3@data$canton <- tolower(ec3@data$DPA_DESCAN)
  vigi$cant <- tolower(vigi$canton)
  
  vigi$cant <- gsub("á","a", vigi$cant)
  vigi$cant <- gsub("ú","u", vigi$cant)
  vigi$cant <- gsub("é","e", vigi$cant)
  vigi$cant <- gsub("í","i", vigi$cant)
  vigi$cant <- gsub("ó","o", vigi$cant)
  
  # Fazer mudancas considerando mapa padrao ouro
  ec3@data$canton <- gsub("ñ","n", ec3@data$canton)
  ec3@data$canton <- gsub("ð","n", ec3@data$canton)
  ec3@data$canton <- gsub("puebloviejo","pueblo viejo", ec3@data$canton)
  
  #tirando o parenteses
  vigi$cant <- trim(gsub("\\(.*","", vigi$cant))
  
  #mudanças para mudar vigi adaptando para ec3@data
  #vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  vigi$cant <- gsub("pelipeo","pelileo", vigi$cant)
  vigi$cant <- gsub("ñ","n", vigi$cant)
  vigi$cant <- gsub("francisco de orellana","orellana", vigi$cant)
  vigi$cant <- gsub("pelileo","san pedro de pelileo", vigi$cant)
  vigi$cant <- gsub("el empalme","empalme", vigi$cant)
  vigi$cant <- gsub("santiago de mendez","santiago", vigi$cant)
  vigi$cant <- gsub("urcuqui","san miguel de urcuqui", vigi$cant)
  vigi$cant <- gsub("marcelino mariduena", "crnel. marcelino mariduena", vigi$cant)
  vigi$cant <- gsub("yaguachi", "san jacinto de yaguachi", vigi$cant) #
  vigi$cant <- gsub("pueblobiejo","pueblo viejo", vigi$cant)
  vigi$cant <- gsub("macas","morona", vigi$cant)
  vigi$cant <- gsub("joya de los sachas","la joya de los sachas", vigi$cant) #
  vigi$cant <- gsub("puyo","pastaza", vigi$cant)
  vigi$cant <- gsub("pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("santiago de santiago de pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("rio verde","rioverde", vigi$cant)
  vigi$cant <- gsub("general antonio elizalde","gnral. antonio elizalde", vigi$cant)
  vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  #vigi$cant <- gsub("banos","banos de agua santa", vigi$cant)
  
  #crio coluna conjunta para comparar
  ec3@data$c <- trim(tolower(paste(ec3@data$provincia,ec3@data$canton)))
  vigi$c <- trim(tolower(paste(vigi$p, vigi$cant)))
  
  #caso especial la concordia cambiandole de provincia
  vigi$c <- gsub("santo domingo de los tsachilas la concordia","esmeraldas la concordia", vigi$c)
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_c <- ec3@data$DPA_CANTON[match(vigi$c, ec3@data$c)]
  
  #numero de catones sem id
  sum(is.na(as.numeric(vigi$c_c)))
  # 0
  
  #cuenta, numero e ordem deles
  sum(is.na(as.numeric(vigi$c_c)))
  vigi[is.na(as.numeric(vigi$c_c)), 8]
  
  cant <-vigi[is.na(as.numeric(vigi$c_c)), 8]
  cant
  
  #Parroquia
  
  #Criacao e transferencia dos valores a novas colunas para comparacao
  ec3@data$parroquia <- ec3@data$DPA_DESPAR
  vigi$par <- tolower(vigi$parroquia)
  
  #Modificando novas colunas por dados comparaveis
  ec3@data$parroquia <- gsub("á","a", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("é","e", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("í","i", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ó","o", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ú","u", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("Ñ","N", ec3@data$parroquia)
  
  #tirando o parenteses
  ec3@data$parroquia <- trim(gsub("\\(.*","", ec3@data$parroquia))
  vigi$par <- trim(gsub("\\(.*","", vigi$par))
  
  #ec3@data$parroquia <- gsub("ALFREDO BAQUERIZO MORENO (JUJAN)","JUJAN", ec3@data$parroquia)
  vigi$par <- gsub("á","a", vigi$par)
  vigi$par <- gsub("é","e", vigi$par)
  vigi$par <- gsub("í","i", vigi$par)
  vigi$par <- gsub("ó","o", vigi$par)
  vigi$par <- gsub("ú","u", vigi$par)
  vigi$par <- gsub("ñ","n", vigi$par)
  vigi$par <- gsub("ü","u", vigi$par)
  vigi$par <- gsub("crnl.","crnel.", vigi$par)
  
  vigi$par <- gsub("holgupin","holguin", vigi$par)
  vigi$par <- gsub("conrdoncillo","cordoncillo", vigi$par)
  vigi$par <- gsub("curticapa","curtincapa", vigi$par)
  vigi$par <- gsub("general leonidas plaza gutierrez", "gral. leonidas plaza gutierrez",vigi$par)
  #vigi$par <- gsub("guayusa", "san jose de guayusa",vigi$par) #
  #vigi$par <- gsub("puerto francisco de orel", "puerto francisco de orellana",vigi$par) #
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san jose de alluriquin", "alluriquin",vigi$par)
  vigi$par <- gsub("santo domingo", "santo domingo de los colorados",vigi$par)
  vigi$par <- gsub("santo domingo de los colorados de onzole", "santo domingo de onzole",vigi$par)
  #vigi$par <- gsub("guasaganda", "GUASAGANDA (CAB. EN GUASAGANDA CENTRO)",vigi$par)
  #vigi$par <- gsub("simon bolivar", "SIMON BOLIVAR (JULIO MORENO)",vigi$par)
  #vigi$par <- gsub("julio e. moreno", "JULIO E. MORENO (CATANAHUAN GRANDE)",vigi$par)
  #vigi$par <- gsub("san pablo", "SAN PABLO (SAN PABLO DE ATENAS)",vigi$par)
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san lorenzo de jipijapa", "jipijapa",vigi$par)
  vigi$par <- gsub("santafe", "santa fe",vigi$par)
  vigi$par <- gsub("san jose de chazo", "san jose del chazo",vigi$par)
  vigi$par <- gsub("cibijies", "cubijies",vigi$par)
  vigi$par <- gsub("crnl. carlos concha torres", "crnel. carlos concha torres",vigi$par)
  vigi$par <- gsub("la lojas", "los lojas",vigi$par)
  vigi$par <- gsub("padre juan batista aguirre", "juan bautista aguirre",vigi$par)
  vigi$par <- gsub("velazco ibarra", "velasco ibarra",vigi$par)
  vigi$par <- gsub("gnral. antonio elizalde", "gral. antonio elizalde",vigi$par)
  vigi$par <- gsub("coronel marcelino mariduenas", "coronel marcelino mariduena",vigi$par)
  vigi$par <- gsub("tarida", "tarifa",vigi$par)
  vigi$par <- gsub("san francisco de natabuela", "san fco. de natabuela",vigi$par)
  vigi$par <- gsub("dr. miguel egas cabezas", "doctor miguel egas cabezas",vigi$par)
  vigi$par <- gsub("san francisco de sigsipamba", "san  fco. de sigsipamba",vigi$par)
  vigi$par <- gsub("chiquiribamba", "chuquiribamba",vigi$par)
  vigi$par <- gsub("bolsapamba", "bolaspamba",vigi$par)
  vigi$par <- gsub("santa susana de chiviaza", "sta susana de chiviaza",vigi$par)
  vigi$par <- gsub("pablo secto", "pablo sexto",vigi$par)
  vigi$par <- gsub("pumipamba", "rumipamba",vigi$par)
  vigi$par <- gsub("pani", "pano",vigi$par)
  vigi$par <- gsub("quinsamola", "quinsaloma",vigi$par)
  vigi$par <- gsub("pelileo grande", "pelileo",vigi$par)
  vigi$par <- gsub("jujan", "alfredo baquerizo moreno",vigi$par)
  vigi$par <- gsub("triunfo dorado", "triunfo-dorado",vigi$par)
  vigi$par <- gsub("chontaduro", "rioverde",vigi$par)
  vigi$par <- gsub("24 de mayo", "sucre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("general leonidas plaza g.", "gral. leonidas plaza gutierrez",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("3 de noviembre", "tres de noviembre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("julio moreno", "santo domingo de los tsachilas",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("sta. cecilia", "santa cecilia",vigi$par) #
  #vigi$par <- gsub("el playon de san francis", "el playon de san francisco",vigi$par) #
  #vigi$par <- gsub("banos", "banos de agua santa",vigi$par) #
  vigi$par <- gsub("el guismi", "el guisme",vigi$par) #
  vigi$par <- gsub("yanzatza", "yantzaza",vigi$par) #
  vigi$par <- gsub("nobol", "narcisa de jesus",vigi$par) #
  vigi$par <- gsub("crnel.lorenzo de garaicoa", "crnel. lorenzo de garaicoa",vigi$par) #
  vigi$par <- gsub("general antonio elizalde", "gral. antonio elizalde",vigi$par) #
  
  
  # Criando novas colunas para comparacao
  ec3@data$pp <- trim(tolower(paste(ec3@data$c,ec3@data$parroquia)))
  vigi$pp <- trim(tolower(paste(vigi$c,vigi$par)))
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # Transferindo cantidad vigilancia para o data frame
  ec3@data$cantidad <- vigi$cantidad[match(ec3@data$pp, vigi$pp)]
  
  #numero de parroquias sem id
  sum(is.na(as.numeric(vigi$c_pp)))
  #227
  
  #cuenta, nombre e ordem
  sum(is.na(as.numeric(vigi$c_pp)))
  #229
  
  vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  #Transformando as parroquias urbanas atuais em parroquias anteriores a divisao
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca banos de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca cuenca de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca bellavista", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el batan", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el sagrario", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el vecino", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca gil ramirez davalos", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca huaynacapac", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca machangara", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca monay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca octavio cordero", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca palacios", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san blas", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san sebatian", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca sucre", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca totoracocha", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca yanuncay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca canaribamba", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay gualaceo daniel cordova", "azuay gualaceo gualaceo",vigi$pp)
  vigi$pp <- gsub("azuay sigsig jima", "azuay sigsig sigsig",vigi$pp)
  vigi$pp <- gsub("azuay ona ona", "azuay ona san felipe de ona",vigi$pp)
  
  vigi$pp <- gsub("bolivar guaranda guanujo", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chavez", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chaves", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabreil ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabriel ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar las naves las mercedes", "bolivar las naves las naves",vigi$pp)
  
  vigi$pp <- gsub("canar azogues aurelio bayas martinez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues azoguez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues borrero", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues san francisco", "canar azogues azogues",vigi$pp)
  
  vigi$pp <- gsub("carchi espejo 27 de septiembre", "carchi espejo el angel",vigi$pp)
  vigi$pp <- gsub("carchi montufar gonzalez suarez", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi montufar san jose", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi tulcan gonzalez suarez", "carchi tulcan tulcan",vigi$pp)
  
  vigi$pp <- gsub("chimborazo colta cajabamba", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo colta sicalpa", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano la matriz", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  
  vigi$pp <- gsub("chimborazo riobamba lizarzaburu", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba maldonado", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba velasco", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba veloz", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba yaruquies", "chimborazo riobamba riobamba",vigi$pp)
  
  vigi$pp <- gsub("cotopaxi la mana el carmen", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga el triunfo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga eloy alfaro", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga ignacio flores", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga juan montalvo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga la matriz", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga san buenaventura", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  
  vigi$pp <- gsub("el oro huaquillas ecuador", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas el paraiso", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas milton reyes", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas union lojana", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas hualtaco", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro las lajas valle hermoso", "el oro las lajas la victoria",vigi$pp)
  vigi$pp <- gsub("el oro machala el cambio", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala la providencia", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala nueve de mayo", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala puerto bolivar", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro pasaje bolivar", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje loma de franco", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje ochoa leon", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje tres cerritos", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pinas la matriz", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas la susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas pinas grande", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa jumon", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa nuevo santa rosa", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa puerto jeli", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("esmeraldas eloy alfaro esmeraldas norte", "esmeraldas eloy alfaro santa lucia de las penas",vigi$pp)
  
  vigi$pp <- gsub("esmeraldas esmeraldas luis tello", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas simon torres", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas 5 de agosto", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas bartolome ruiz", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas la concordia las villegas", "esmeraldas la concordia la villegas",vigi$pp)
  
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule la uaurora", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule padre juan bautista aguirre", "guayas daule daule",vigi$pp) #
  vigi$pp <- gsub("guayas duran el recreo", "guayas duran eloy alfaro",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil pascuales", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil bolivar", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ayacucho", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil carbo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil febres cordero", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil garcia moreno", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil letamendi", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil nueve de octubre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil olmedo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil rocafuerte", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil sucre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil tarqui", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil urdaneta", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas salitre bocana", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre central", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre grnl. vernaza", "guayas salitre el salitre",vigi$pp)
  
  vigi$pp <- gsub("imbabura antonio ante andrade marin", "imbabura antonio ante atuntaqui",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi sagrario",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra caranqui", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra guayaquil de alpachaca", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra la dolorosa del priorato", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra san francisco", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo jordan", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo san luis", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi san francisco", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra sagrario", "imbabura ibarra san miguel de ibarra",vigi$pp)
  
  vigi$pp <- gsub("loja calvas chile", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja calvas san vicente", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja catamayo san jose", "loja catamayo catamayo",vigi$pp)
  vigi$pp <- gsub("loja loja el sagrario", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja san sebastian", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja sucre", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja macara general eloy alfaro", "loja macara macara",vigi$pp)
  vigi$pp <- gsub("loja paltas lourdes", "loja paltas catacocha",vigi$pp)
  vigi$pp <- gsub("loja loja valle", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja santiago \"san salvador o james\"", "loja loja santiago",vigi$pp)
  
  
  vigi$pp <- gsub("los rios babahoyo clemente baquerizo", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo barreiro", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo dr. camilo ponce", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo el salto", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios buena fe 7 de agosto", "los rios buena fe san jacinto de buena fe",vigi$pp)
  vigi$pp <- gsub("los rios pueblo viejo san juan de iluman", "los rios pueblo viejo puebloviejo",vigi$pp)
  vigi$pp <- gsub("los rios ventanas quinsaloma", "los rios quinsaloma quinsaloma",vigi$pp)
  vigi$pp <- gsub("los rios valencia la esperanza", "los rios valencia valencia",vigi$pp)# no existe parroquia
  vigi$pp <- gsub("los rios valencia la union", "los rios valencia valencia",vigi$pp)#
  vigi$pp <- gsub("los rios valencia vergel", "los rios valencia valencia",vigi$pp)#
  
  vigi$pp <- gsub("los rios quevedo 24 de mayo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo guayacan", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo nicolas infante diaz", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san camilo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san cristobal", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo sucre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo siete de octubre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo venus del rio quevedo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  
  vigi$pp <- gsub("manabi chone santa rita", "manabi chone chone",vigi$pp)
  vigi$pp <- gsub("manabi el carmen 4 de diciembre", "manabi el carmen el carmen",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa dr. miguel moran lucio", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa manuel inocencio parrales y guale", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi manta eloy alfaro", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta tarqui", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta los esteros", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta san mateo", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi montecristi el colorado", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi general eloy alfaro", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi leonidas proano", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi anibal san andres", "manabi montecristi montecristi",vigi$pp) #agregada 14.02.2020 Network_Description.R
  
  vigi$pp <- gsub("manabi portoviejo 12 de marzo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo 18 de octubre", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo andres de vera", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo colon", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo francisco pacheco", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo picoaza", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo san pablo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo simon bolivar", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi santa ana lodana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana de vuelta larga de vuelta larga", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi sucre leonidas plaza gutierrez", "manabi sucre bahia de caraquez",vigi$pp)
  
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago tiwintza puyo", "morona santiago tiwintza santiago",vigi$pp)
  
  vigi$pp <- gsub("pichincha cayambe ayora", "pichincha cayambe san jose de ayora",vigi$pp)
  vigi$pp <- gsub("pichincha cayambe juan montalvo", "pichincha cayambe cayambe",vigi$pp)
  vigi$pp <- gsub("pichincha quito belisario quevedo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito benalcazar", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito carcelen", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito centro historico", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cotocollao", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chaupicruz", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chilibulo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chillogallo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chimbacalle", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cochapamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito comite del pueblo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito guamani", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito el condado", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito eloy alfaro", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito inaquito", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito itchimbia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la concepcion", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ecuatoriana", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ferroviaria", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la floresta", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la libertad", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la mena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito mariscal sucre", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito ponceano", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito puengasi", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito quitumbe", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito rumipamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san antonio de minas", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san bartolo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san isidro del inca", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san roque", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito solanda", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito turubamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la magdalena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san pedro de taboada", "pichincha ruminahui sangolqui",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san rafael", "pichincha ruminahui sangolqui",vigi$pp)
  
  vigi$pp <- gsub("santa elena salinas santa rosa", "santa elena salinas salinas",vigi$pp)
  vigi$pp <- gsub("santa elena santa elena ballenita", "santa elena santa elena santa elena",vigi$pp)
  
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chiguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo las mercedes", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo abraham calazacon", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chimguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio toachi", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio verde", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo zaracay", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los tsachilas", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los colorados de los colorados", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo nuevo isrrael", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  
  vigi$pp <- gsub("sucumbios lago agrio santa cruz", "sucumbios lago agrio santa cecilia",vigi$pp)
  
  vigi$pp <- gsub("tungurahua ambato atocha - ficoa", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato celiano monge", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi chico", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi loreto", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la merced", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la peninsula", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato matriz", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato pishilata", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san bartolo de pinllog", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san francisco", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pillaro ciudad nueva", "tungurahua santiago de pillaro pillaro",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pelileo pelileo grande", "tungurahua santiago de pelileo pelileo",vigi$pp)
  
  vigi$pp <- gsub("zamora chinchipe zamora el limon", "zamora chinchipe zamora zamora",vigi$pp)
  
  #transferencia de codigo do mapa os que forem mach da pro-can-parr
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # agregando os valores que foram transformados de parroquias urbanas a normais
  vigi2 <- vigi %>%
    group_by(pp) %>%
    summarise(cantidad = sum(cantidad))
  
  #Tranferindo valores agregados
  ec3@data$cantidad <- vigi2$cantidad[match(ec3@data$pp, vigi2$pp)]
  
  # Comparando numeros
  sum(vigi$cantidad, na.rm = TRUE)
  sum(vigi2$cantidad, na.rm = TRUE) -
    sum(ec3@data$cantidad, na.rm = TRUE)
  
  ####ANTERIOR AINDA NAO DELETAR
  sum(is.na(as.numeric(vigi$c_pp)))
  #157
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  par
  
  # Animais faltantes no mapa
  sum(vigi$cantidad) - sum(ec3@data$cantidad, na.rm = TRUE)
  
}

vigi$database.id <- paste(vigi$provincia, vigi$canton, vigi$parroquia)
vigi$x <- ec3@data$x[match(vigi$c_pp, ec3@data$DPA_PARROQ)]
vigi$y <- ec3@data$y[match(vigi$c_pp, ec3@data$DPA_PARROQ)]

# Criando os centroides ----
# install.packages("rgeos")
library(rgdal)
library(rgeos)

trueCentroids <- data.frame(gCentroid(ec3,byid=TRUE))

ec3@data$x <- trueCentroids$x
ec3@data$y <- trueCentroids$y

#Pasar os centroides para correspondence 
vigi$x <- ec3@data$x[match(vigi$c_pp, ec3@data$DPA_PARROQ)]
vigi$y <- ec3@data$y[match(vigi$c_pp, ec3@data$DPA_PARROQ)]

banco$correspondence$x <- vigi$x[match(banco$correspondence$database.id, vigi$database.id)]
banco$correspondence$y <- vigi$y[match(banco$correspondence$database.id, vigi$database.id)]

# 4.2.5
# Provincial frequency by number of movements
sum(banco$movements$Freq) #127401 2017 # 167595 2019
summary(banco$movements$Freq) 

# 2017 slaugtherhouses
# Min. 1st Qu.  Median    Mean 3rd Qu.   Max. 
# 1.00    1.00    4.00   48.79   22.00   3351.00 
# 2019
# Min. 1st Qu.  Median    Mean 3rd Qu. max
# 1.00    1.00    4.00   53.66   24.00 3825

# Premises
# 2017
# 187149

hist(log(banco$movements$Freq))
hist(banco$movements$Freq, ylim=c(0,10))

quantile(banco$movements$Freq, probs = c(0.2,0.4,0.6,0.8,1))
quantile(banco$movements$Freq, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.8952,0.9,0.95,1))
# 2017: 89.52% movements under 100 in premises 2017
quantile(banco$movements$Freq, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.8952,0.9,0.95,0.97694,1))
# 89.52, 7.6, 2.04

# Premises
# 2017: 89.52% movements under 100 in premises 2017
quantile(banco$movements$Freq, probs = c(0.96,0.9924,1))
#2019
quantile(banco$movements$Freq, probs = c(0.957,0.991245,1))


a <- banco$movements %>%
  group_by(provincia.origen)%>%
  mutate(f = ifelse(Freq >=500, "c",
                   ifelse(Freq >=100,"b","a"))) %>%
  group_by(provincia.origen,f) %>%
  summarize(count=n()) %>%
  spread(key = "f", value="count")

a$por_a <- a$a/sum(a$a, na.rm = TRUE)*100
a$por_b <- a$b/sum(a$b, na.rm = TRUE)*100
a$por_c <- a$c/sum(a$c, na.rm = TRUE)*100

# 2019
# Loja	213	18	13	16.455696
# 21	El Oro	361	35	10	12.658228
# 20	Chimborazo	139	17	6	7.594937
# 16	Carchi	144	16	5	6.329114
# 17	Cotopaxi	140	14	5	6.329114
# 18	Manabí	400	18	5	6.329114
# 19	Santo Domingo de los Tsáchilas	142	12	5	6.329114

# 2017
# 1 Loja	160	16	13	21.311475
# 2	Santo Domingo de los Tsáchilas	131	8	6	9.836066
# 3	Carchi	121	15	5	8.196721
# 4	Chimborazo	105	11	5	8.196721


b <- banco$movements %>%
  group_by(provincia.origen)%>%
  mutate(f = ifelse(Freq >=500, "c",
                    ifelse(Freq >=100,"b","a"))) %>%
  group_by(provincia.origen,f) %>%
  summarize(count=n())


c <- banco$movements %>%
  group_by(provincia.origen, parroquia.origen)%>%
  mutate(f = ifelse(Freq >=500, "c",
                    ifelse(Freq >=100,"b","a"))) %>%
  group_by(provincia.origen,parroquia.origen,f) %>%
  summarize(count=n()) %>%
  spread(key = "f", value="count")

b %>% group_by(f) %>%
  summarize(number=sum(count), por=number/2611)
2790+254+79
b %>% group_by(f) %>%
  summarize(number=sum(count) , por=number/3123)


c <- banco$movements %>%
  group_by(provincia.origen)%>%
  mutate(f = ifelse(Freq >=500, "c",
                    ifelse(Freq >=100,"b","a"))) %>%
  group_by(provincia.origen,f) %>%
  summarize(count=n()) %>%
  spread(key = "f", value="count")

# 2019
# sum(c$c, na.rm = TRUE)
# 79
# 13/79
# 0.164557
#10/79
# 0.1265823
# 6/79
# 0.07594937

# proportion of frequencies by province
a <- banco$movements %>%
  group_by(provincia.origen)%>%
  summarize(p=sum(Freq), por=p/167595)

a <- banco$movements %>%
  group_by(provincia.destino)%>%
  summarize(p=sum(Freq), por=p/167595)

# 4.3 Slaughter network graphic----
#Movements
mov <- data.frame(banco$movements$From, banco$movements$To, banco$movements$Freq)
colnames(mov) <- c("Source", "Target", "Freq")
#Premises
prem <- data.frame(banco$correspondence$network.id, banco$correspondence$y, 
                   banco$correspondence$x, banco$correspondence$all_degree)
colnames(prem) <- c("ID", "latitude", "longitude", "all_degree")

# Plotting the map base
library(maps)
# install.packages("geosphere")
library(geosphere)
library(sp)
# install.packages("shape")
library(shape) #Arrows
# install.packages("GISTools")
library(GISTools)

# Map download from GADM https://gadm.org/download_country_v3.html
m <- readRDS("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/gadm36_ECU_1_sp.rds")

# Add a point on the map for each node premise:
# points(x=prem$longitude, y=prem$latitude, pch=19,
#        cex=prem$all_degree/100, col="orange")

# Next we will generate a color gradient to use for the edges in the network. Heavier edges will be
# lighter in color.
# barplot(c(2,5,2), col=c("#B3B3B3", "#1F78B4", "#E31A1C"))
# barplot(c(2,5,2), col=c("#A6D854E6", "#1F78B4EC", "#E31A18E6"))

# Adding 5% transparency to the color F2 to the last part.
# Adding 10% transparency to the color E6 to the last part.
# Adding 20% transparency to the color CC to the last part.
# Adding 30% transparency to the color B3 to the last part.
# Adding 40% transparency to the color 99 to the last part.
# Adding 50% transparency to the color 80 to the last part.

# https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4

edge.col <- c("#A6D854CC", "#1F78B499", "#E31A1880")

# --------------------------------------------------
# 4.4 Project Using the same width line between visualization ----
width.lines <- c("0.1", "1.5", "2")

# For each mov we will use gcIntermediate() to generate the coordinates of the shortest
# arc that connects its start and end point (think distance on the surface of a sphere). After that, we
# will plot each arc over the map using lines().

# Arrows that works

plot(m, lwd=0.20,
     xlim=c(-81,-74))
north.arrow(xb=-76, yb=-3,len=0.1)
map.scale(-77.8)

#<= 100 color1, >100 & < 500 color2, >=500 color 3
# order the data frame for plotting fisrt the lines
mov <- mov[order(mov[,3]),]

# 4.5 Ploting maps and lines and arrows -----
for(i in 1:nrow(mov)) {
  node1 <- prem[prem$ID == mov[i,]$Source,]
  node2 <- prem[prem$ID == mov[i,]$Target,]
  arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude),
                         c(node2[1,]$longitude, node2[1,]$latitude),
                         n=1000, addStartEnd=TRUE )
  
  edge.ind <- ifelse(mov[i,]$Freq <= 100,1,ifelse(mov[i,]$Freq >= 500,3,2))
  lines(arc, col=edge.col[edge.ind], lwd=width.lines[edge.ind])
  
  tinter <- tail(arc,2)
  Arrows(tinter[1,1], tinter[1,2], tinter[2,1], tinter[2,2], arr.type="triangle", 
         arr.length = as.numeric(width.lines[edge.ind])*0.04,
         arr.width = as.numeric(width.lines[edge.ind])*0.06,
         arr.col=edge.col[edge.ind], lcol = edge.col[edge.ind], arr.adj=1)
}

#-----Testing-----
# f{
#   
#   # ----------TEST----------TEST----------TEST----------TEST----------TEST
#   
#   # filtering movements by frequency number
#   tab <- table(mov$Source)
#   filter <- names(tab)[tab>20]
#   prem <- prem[prem$ID %in% filter,]
#   mov <- mov[mov$Source %in% filter &
#                mov$Target %in% filter, ]
#   
#   
#   # Project Using variable width line and arrow between visualization
#   
#   
#   for(i in 1:nrow(mov)){
#     node1 <- prem[prem$ID == mov[i,]$Source,]
#     node2 <- prem[prem$ID == mov[i,]$Target,]
#     arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude),
#                            c(node2[1,]$longitude, node2[1,]$latitude),
#                            n=1000, addStartEnd=TRUE )
#     
#     edge.ind <- ifelse(mov[i,]$Freq <= 100,1,ifelse(mov[i,]$Freq >= 500,3,2))
#     lines(arc, col=edge.col[edge.ind], lwd=20*mov[i,]$Freq/max(mov$Freq))
#     
#     tinter <- tail(arc,2)
#     #Code arrow base R later I will try 
#     # arrows(tinter[1,1], tinter[1,2], tinter[2,1], tinter[2,2], type="closed", 
#     #        angle = 15, length = mov[i,]$Freq/max(mov$Freq), 
#     #        col="black") 
#     
#     Arrows(tinter[1,1], tinter[1,2], tinter[2,1], tinter[2,2], arr.type="curved", 
#            arr.length = 0.5*mov[i,]$Freq/max(mov$Freq),
#            arr.width = 0.3*mov[i,]$Freq/max(mov$Freq),
#            arr.col=edge.col[edge.ind], lcol = edge.col[edge.ind], arr.adj=1) 
#   }
#   
#   # Project How to plot in ggplot ----
#   # # http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
#   # library(rgdal)
#   # library(ggplot2)
#   # library(RColorBrewer)
#   # setwd("~/Dropbox/0.USP/3. 2017 II sem/2 MPT5803 Modelagem matemática em doenças/Projeto Modelo CSF/Modelling CSF in Equator/Epidemiological analysis")
#   # Ecmap  <- readOGR(dsn=".",layer="ECU_adm1")
#   # map.df2 <- fortify(Ecmap)
#   # 
#   # ggplot(Ecmap2.df, aes(x=latx, y=longy)) + 
#   #   geom_path(data=map.df2,aes(x=long, y=lat,group=group), colour="grey60", size=0.2)+
#   #   xlim(-82,-75) +
#   #   coord_fixed()
#   
#   
#   
#   #
#   
#   # Calculating the wide of the lines 
#   mov$length <- 20*mov$Freq/max(mov$Freq)
#   
#   mov$table <- ifelse(mov$Freq <= 100,1,ifelse(mov$Freq >= 500,3,2))
#   
#   table(mov$table)
#   
#   quantile(mov$Freq, probs = c(0.33, 0.66, 0.99))
#   
#   # Validating the ifelse
#   which(is.na(mov))
#   which(is.na(prem), arr.ind = TRUE)
#   
#   Showing the arrow types
#   xlim <- c(-5 , 5)
#   ylim <- c(-10, 10)
#   plot(0, type = "n", xlim = xlim, ylim = ylim, 
#        main = "Arrows,  type = 'curved'")
#   x0 <- runif(10, xlim[1], xlim[2])
#   y0 <- runif(10, ylim[1], ylim[2])
#   x1 <- x0+runif(10, -1, 1)
#   y1 <- y0+runif(10, -1, 1)
#   Arrows(x0, y0, x1, y1, arr.length = runif(10), code = 2, 
#          arr.type = "curved", arr.col = "black", lcol = "black", arr.adj=1)
#   
#   arr.adj
#   
#   col=edge.col[edge.ind]) 
# 
# # lines without arrows  
# for(i in 1:nrow(mov)) {
#   node1 <- prem[prem$ID == mov[i,]$Source,]
#   node2 <- prem[prem$ID == mov[i,]$Target,]
#   arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude),
#                          c(node2[1,]$longitude, node2[1,]$latitude),
#                          n=100, addStartEnd=TRUE )
#   edge.ind <- ifelse(mov[i,]$Freq <= 100,1,ifelse(mov[i,]$Freq >= 500,3,2))
#   lines(arc, col=edge.col[edge.ind], lwd=15*mov[i,]$Freq/max(mov$Freq))
# }
# 
# 
# tiff(filename="2019.tif", width=300, height=360, pointsize=12,
#      units="mm", res=400, compression="lzw")
# g1
# dev.off()
# 
# 
# dev.copy(filename="2017.tif", width=300, height=360, pointsize=12,
#          units="mm", res=300, compression="lzw")
# dev.off()
# 
# # ----------TEST----------TEST----------TEST----------TEST----------TEST
# 
# }


# 4.6 Analysis of origin and destiny premises ----
library(tidyverse)
m2$cantidad <- as.numeric(m2$cantidad)

premises <- m2 %>%
  filter(operacion.destino != "Faenador") %>%
  group_by(provincia.origen, provincia.destino) %>%
  summarise(movements=n()/3, cantidad= sum(cantidad)/3)

sum(premises$cantidad)
sum(m2$cantidad)/3

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
write.csv(premises, file = "from_to2.csv")
1190991/3

#for slaughterhouses
premises_slaug <- m2 %>%
  filter(operacion.destino == "Faenador") %>%
  group_by(provincia.origen, provincia.destino) %>%
  summarise(movements=n()/3, cantidad= sum(cantidad)/3)

premises_slaug <- data.frame(premises_slaug)

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
write.csv(premises_slaug, file = "from_to_slauh2.csv")



premises <- m2 %>%
  filter(operacion.destino != "Faenador") %>%
  group_by(provincia.origen) %>%
  summarise(movements=n(), cantidad= sum(cantidad))

premises_des <- m2 %>%
  filter(operacion.destino != "Faenador") %>%
  group_by(provincia.destino) %>%
  summarise(movements=n())

sum(premises$movements) #751003
sum(premises$cantidad) #6407395

# Cotopaxi 20.39%
158054/774784
#Santo domigno 11.04%
88361/774784


#_--------------------------------------------------
# 3.3 Describing network structure ----
###
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]

# 1 Creating network of premisses ----
m2 <- m2[m2$operacion.destino != "Faenador",]
# 751003 movements

library(tidyverse)
library(epinemo)
library(igraph)
library(Matrix)
# 2 Premise level
# 2 Number of premises by year and category ----

#Proving that the number of premises are the same 
m2 %>%
  group_by(ano, operacion.origen) %>%
  summarise(premises=length(unique(codigo.sitio.origen))) %>%
  tidyverse::spread(key="ano", value=premises)

# operacion.origen                 `2017` `2018` `2019`
# 1 Comercializador                    1323   1668   1936
# 2 Feria de comercialización animal     44     47     53
# 3 Operador Industrial                  86     90     96
# 4 Productor                         40321  49245  56423

library(tidyverse)
m2 %>%
  group_by(ano, operacion.destino) %>%
  summarise(premises=length(unique(codigo.sitio.destino))) %>%
  spread(key="ano", value=premises)

# operacion.destino                `2017` `2018` `2019`
# 1 Comercializador                    7979   8139  11436
# 2 Feria de comercialización animal     44     48     51
# 3 Operador Industrial                  88     97    110
# 4 Productor                         20837  31376  44239

library(epinemo)

# filters year 
m2017 <- m2[m2$ano == "2017",]
m2018 <- m2[m2$ano == "2018",]
m2019 <- m2[m2$ano == "2019",]

m1 <- m2[m2$mes == "01" & m2$ano == "2017",]
m_2 <- m2[m2$mes == "02" & m2$ano == "2017",]
m3 <- m2[m2$mes == "03" & m2$ano == "2017",]
m4 <- m2[m2$mes == "04" & m2$ano == "2017",]
m5 <- m2[m2$mes == "05" & m2$ano == "2017",]
m6 <- m2[m2$mes == "06" & m2$ano == "2017",]
m7 <- m2[m2$mes == "07" & m2$ano == "2017",]
m8 <- m2[m2$mes == "08" & m2$ano == "2017",]
m9 <- m2[m2$mes == "09" & m2$ano == "2017",]
m10 <- m2[m2$mes == "10" & m2$ano == "2017",]
m11 <- m2[m2$mes == "11" & m2$ano == "2017",]
m12 <- m2[m2$mes == "12" & m2$ano == "2017",]

m1 <- m2[m2$mes == "01" & m2$ano == "2018",]
m_2 <- m2[m2$mes == "02" & m2$ano == "2018",]
m3 <- m2[m2$mes == "03" & m2$ano == "2018",]
m4 <- m2[m2$mes == "04" & m2$ano == "2018",]
m5 <- m2[m2$mes == "05" & m2$ano == "2018",]
m6 <- m2[m2$mes == "06" & m2$ano == "2018",]
m7 <- m2[m2$mes == "07" & m2$ano == "2018",]
m8 <- m2[m2$mes == "08" & m2$ano == "2018",]
m9 <- m2[m2$mes == "09" & m2$ano == "2018",]
m10 <- m2[m2$mes == "10" & m2$ano == "2018",]
m11 <- m2[m2$mes == "11" & m2$ano == "2018",]
m12 <- m2[m2$mes == "12" & m2$ano == "2018",]

m1 <- m2[m2$mes == "01" & m2$ano == "2019",]
m_2 <- m2[m2$mes == "02" & m2$ano == "2019",]
m3 <- m2[m2$mes == "03" & m2$ano == "2019",]
m4 <- m2[m2$mes == "04" & m2$ano == "2019",]
m5 <- m2[m2$mes == "05" & m2$ano == "2019",]
m6 <- m2[m2$mes == "06" & m2$ano == "2019",]
m7 <- m2[m2$mes == "07" & m2$ano == "2019",]
m8 <- m2[m2$mes == "08" & m2$ano == "2019",]
m9 <- m2[m2$mes == "09" & m2$ano == "2019",]
m10 <- m2[m2$mes == "10" & m2$ano == "2019",]
m11 <- m2[m2$mes == "11" & m2$ano == "2019",]
m12 <- m2[m2$mes == "12" & m2$ano == "2019",]

# 1 Using the filtered datasets and generating the measures ----
library(epinemo)
library( igraph)
library(Matrix)
# here we assign m as the dataset that you want to analize with the f
# inside f there is all the analis
m <- m2019
f {
  m <- data.frame(m)
  m$cantidad <- as.numeric(m$cantidad)
  banco <- createUniqueIds(m,
                           from = 'codigo.sitio.origen',
                           to = 'codigo.sitio.destino')
  
  # Caregando Matrix
  banco$movements$cantidad <- as.numeric(banco$movements$cantidad)
  
  #  Cria uma matriz onde estao os uns mas guarda os ceros
  #  Acrescentando o numero de animais, aqui so trabalharemos com o numero de Atestados X=1 significa isso
  # matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
  #                        x=banco$movements$cantidad,
  #                        dims = rep(max(banco$movements$From, banco$movements$To) ,2))
  
  #lotes
  matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                         x=1,
                         dims = rep(max(banco$movements$From, banco$movements$To) ,2))
  
  
  # 2 Degree ----
  # Animais ####################
  kin <- colSums(matriz)
  kout <- rowSums(matriz)
  ktotal <- kin + kout
  
  ####
  banco$correspondence$ktotal <- ktotal
  #####
  
  summary(kin)
  summary(kout)
  summary(ktotal)

    # hist(kin)
  # hist(kout)
  # hist(ktotal)
  
  ##### eje x
  #hist(ktotal[ktotal < 10])
  
  #hist(ktotal, ylim = c(0,10))
  #boxplot(kin, kout,names= c('kin','kout'))
  
  #table(banco$movements$numero == "0")
  # limitando a rede
  #boxplot(kin, kout, ylim = c(0,20),names= c('kin','kout'))
  #title(main = "Distribucao de Grau")
  
  plot(log10(kin) ~ log10(kout))
  title(main= "kin ~ kout log10",
        sub= "geral")
  
  #plot(kin ~ kout, ylim= c(0,5000))
  #title(main= "Kin ~ Kout")
  
    # 3 Cluster coefficient ----
  library(igraph)
  cc <- clusteringCoefficient(matriz,directed =T)
    # Direccionada e igual a TRUE é uma rede direccionada
  summary(cc)
  
  
  # 4 Numero de vizinhos Neighbors ----
  matrizv <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                          dims = rep(max(banco$movements$From, banco$movements$To) ,2))
  
  matriz_nao_direcionada <-matrizv + t(matrizv)
  
  matriz_vizinhos_nao_direcionada <-((matriz_nao_direcionada >0) *1)
  
  ktotalv <- rowSums(matriz_vizinhos_nao_direcionada)
  
  summary(ktotalv)
  
    # 5 Betweeness
  library(igraph)
  propriedades <- unique(c(banco$movements$From, banco$movements$To))
  grafo <- graph_from_data_frame(banco$movements[, c("From", "To")], directed = T, vertices = propriedades)
  
  ###########
  #Funcao para atirar os loopes e as arestas multiplas
  grafo <- simplify(grafo)
  grafo
  
  # Centralidade de intermediacao
  # os nos mais centrais sao por onde pasam caminhos nos que nao sao tao conetados. 
  # A geodesica na rede e o numero menor de pasos para chegar ao no alvo.
  
  between <- betweenness(graph =grafo)
  banco$correspondence$betweenness <- between
  # 
  # summary(between)
  # 
  # boxplot(between, ylim=c(0,01))
  # #title(main = "Between")
  # 
  #boxplot(between)

  # 6  shortest paths
  spath <- mean_distance(grafo)
  
  # Zoom
  #hist(between)
  #hist(between, ylim = c(0, 10))
  
  
  # 7 Density ----
  density <-  edge_density(grafo)
  summary(density)
  
  # 8 Diameter ----
  diameter <-  diameter(grafo)
  
  
  # 9 GWCC ----
  gwcc <- components(grafo, mode = "weak")
  summary(gwcc)
  gwp <- sort(clusters(grafo, "weak")$csize, decreasing = T)[1]
  
  # 10 GSCC ----
  gscc <- components(grafo, mode = "strong")
  summary(gscc)
  gsp <- sort(clusters(grafo, "strong")$csize, decreasing = T)[1]  
  
  # 11 Clossenes 
  #CCentralidade de proximidade
  # ela se preocupa quanto distante 
  # o quanto distante o no esta das outros
  
  closeness_in <- closeness(graph =grafo, mode = 'in' )
  # closeness_out <- closeness(graph =grafo,mode = 'out' )
  
  boxplot(closeness_in, closeness_out)
  
  summary(closeness_in)
  summary(closeness_out)
  
  # banco$correspondence$closeness_in <- closeness_in
  
  
  # premises
  p <- length(unique(banco$correspondence$network.id))
  # Movements
  m <- length(unique(banco$movements$numero.certificado))
  # Degree
  d <- summary(ktotal)
  d <- t(d[4])
  d <- d[1]
  # cluster coefficient
  rcc <- summary(cc)
  rcc <- gsub("Mean   :", "", rcc)
  rcc <- gsub("  ", "", rcc)
  rcc <- as.numeric(rcc[4,2])
  #neigborh
  n <-summary(ktotalv)
  n <-t(n[4])
  n <- n[1]
  #Betweeness
   b <- summary(between)
   b <- t(b[4]) 
   b <- b[1]

# closseness 
   cl <- summary(closeness_in)
   cl <- t(cl[4])
   cl <- cl[1]

# Density
  de <- summary(density)
  de <- t(de[4])
  de <- de[1]
  
  #diameter
  di <- diameter
  
  #shortest path
  spath
  
  #Gigant weak connected conmponent
  gw <- summary(gwcc)
  g1 <- gw[1,1]
  
  gwpor <- as.numeric(gwp)/
    as.numeric(g1)*100
  
  #Gigant strong connected conmponent
  gs <- summary(gscc)
  gs <- gs[1,1]
  gs1 <- gs[2,1]
  
  gspor <- as.numeric(gsp)/
    as.numeric(gs)*100
  gspor

}
# for monts use r01, r02 ... r36
r36 <-  data.frame(p, m, d, rcc, n, b, cl, de, di, spath, gwp, gwpor, gsp, gspor)

# For years use r2017, r2018, r2019
r2019 <- data.frame(p, m, d, rcc, n, b, cl, de, di, spath, gwp, gwpor, gsp, gspor)

  # For years
  r2017
  r2018
  r2019
  
  years <- rbind(r2017, r2018, r2019)
  years[] <- lapply(years, as.character)
  years <- data.frame(years)
  years[] <- lapply(years, as.numeric)
  
  min <- unlist(t(lapply(years, min)), use.names = FALSE)
  max <- unlist(t(lapply(years, max)), use.names = FALSE)
  
  years  <- data.frame(t(years))
  colnames(years) <- c("2017", "2018", "2019")
  years$mean <- rowMeans(years)
  round(years, digits = 2)
  years$min <- min
  years$max <- max
  
  years$range <-paste("[",years$min, "-" , years$max, "]")
  
  
# 12 Monthly analysis----
  
mo <- rbind(r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24,
            r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36)
mo <- data.frame(mo)
mo[] <- lapply(mo, as.character)
mo2 <- mo
mo2[] <- lapply(mo, as.numeric)
min <- unlist(t(lapply(mo2, min)), use.names = FALSE)
max <- unlist(t(lapply(mo2, max)), use.names = FALSE)
mo2  <- data.frame(t(mo2))
colnames(mo2) <- rep(c("JAN", "FEV", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),3)
mo2$mean <- rowMeans(mo2)
mo2$min <- min
mo2$max <- max
mo2$range <-paste("[",mo2$min, "-" , mo2$max, "]")
range(mo2[13,c(2:36)])

write.csv(mo2, file="month.csv")
write.csv(mo, file="month3.csv")
write.csv(years, file="year.csv")


ym <- years[,c(-1,-2,-3,-5,-6)]
ym$mmean <- mo2$mean
ym$mrang <- mo2$range

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
# write.csv(ym, file="years_month_summary_table.csv")

# 13 Montly Graphics of network parameters----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio/")
mo <- read.csv("month3.csv", colClasses = "character")

mot <- data.frame(t(mo))

# colnames(mot) <- c("p", "m", "d", "rcc", "n", "b", "cl", "de", "di", "gw", "gwpor", "gs", "gspor")
# mot <- mot[-1,]
# mot[] <- lapply(mot, as.character)
# mot[] <- lapply(mot, as.numeric)
# 
# mot$mes <- row.names(mot)
# mot$month <- 1:36

mot <- data.frame(mo)
str(mot)
mot[] <- lapply(mot, as.numeric)

#Creating month labels
mot$month <- 1:36
mot$year <- c(rep("2017", 12),rep("2018", 12),rep("2019", 12))
mot$month2 <- rep(1:12, 3)
mot$month3 <- month(mot$month2, label = TRUE)
mot$date <- ymd(seq(ymd('2017-01-01'), ymd('2019-12-31'), by = 'month'))

# Individual metric graphics ----
# A Clustering coefficient ----
grcc <- ggplot(mot, aes(x=date,y=rcc))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Clustering coeficient")+
  # labs(tag = "A")+
  theme_minimal() + 
  theme(text = element_text(size = 10))

# Graphic facet for cc
u <- ggplot(mot, aes(x=month3, y=rcc, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "B") +
  guides(colour=FALSE)+
  labs(y = "", x ="", title="Clustering Coefficient")+
  theme_minimal() +
  theme(text = element_text(size = 12))+
  facet_grid(rows = vars(year))

#B Diameter ----
gdi <- ggplot(mot, aes(x=date,y=di))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Diameter")+
  # labs(tag = "B")+
  theme_minimal()  + 
  theme(text = element_text(size = 10))

diameter_fac <- ggplot(mot, aes(x=month3, y=di, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "A") +
  guides(colour=FALSE)+
  labs(y = "", x ="", title="Diameter")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  facet_grid(rows = vars(year))

mot$month_l <-rep(c("JAN","FEB","MAR","ABR","MAY","JUN","JUL","AGO", "SEP", "OCT", "NOV","DIC"),3)

#C Edge Density ----
gde <- ggplot(mot[3:36,], aes(x=date,y=de))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Edge density")+
  # labs(tag = "C")+
  theme_minimal()  + 
  theme(text = element_text(size = 10))

#D mean number of neighbors ----
gnn <- ggplot(mot[2:36,], aes(x=date,y=n))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Mean number of neighbors")+
  # labs(tag = "D")+
  theme_minimal()  + 
  theme(text = element_text(size = 10))

neigh_fac <- ggplot(mot, aes(x=month3, y=n, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  # labs(tag = "C") +
  guides(colour=FALSE)+
  labs(y = "", x ="", title="Mean number of neighbors")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  facet_grid(rows = vars(year))

#E Shortest Path----
gsp <- ggplot(mot[2:36,], aes(x=date,y=spath))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Shortest path")+
  # labs(tag = "E")+
  theme_minimal()  + 
  theme(text = element_text(size = 10))

shortest_fac <- ggplot(mot, aes(x=month3, y=spath, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "D") +
  guides(colour=FALSE)+
  labs(y = "", x ="", title="Shortest path")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  facet_grid(rows = vars(year))

#F  Gigant weakly component----
ggw <- ggplot(mot, aes(x=date,y=gwp))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="GWCC")+
  # labs(tag = "F")+
  theme_minimal() + 
  theme(text = element_text(size = 10))

# g Gigant strongly component ----
ggs <- ggplot(mot, aes(x=date,y=gsp))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="GSCC")+
  # labs(tag = "G")+
  theme_minimal() + 
  theme(text = element_text(size = 10))

# H Degree ----
gd <- ggplot(mot, aes(x=date,y=d))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Mean of degree")+
  theme_linedraw()+
  # labs(tag = "H")+
  theme_minimal() + 
  theme(text = element_text(size = 10))

# I Degree ----
gd <- ggplot(mot, aes(x=month,y=d))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Mean of degree")+
  theme_linedraw()+
  labs(tag = "H")+
  theme_minimal() + 
  theme(text = element_text(size = 10))

# Graphic facet for degree
t <- ggplot(mot, aes(x=month2, y=d, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "A")+
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="All Degree")+
  theme_minimal() +
  theme(text = element_text(size = 12))+
  facet_grid(rows = vars(year))

# J Mean of degree ----
gd <- ggplot(mot, aes(x=month,y=d))+
  geom_point(shape=21, colour="#FC8D62", size=2)+
  geom_line(colour="#66C2A5", size=1)+
  labs(y = NULL, x =NULL, title="Mean of degree")+
  theme_linedraw()+
  # labs(tag = "H")+
  theme_minimal() + 
  theme(text = element_text(size = 10))


# K Degree Distribution ----
# generate the network and add the ktotal to the correspodence

# List of degrees
G.degrees <- banco$correspondence$ktotal

# Let's count the frequencies of each degree
G.degree.histogram <- as.data.frame(table(G.degrees))

# Need to convert the first column to numbers, otherwise
# the log-log thing will not work (that's fair...)
G.degree.histogram[,1] <- as.numeric(paste(G.degree.histogram[,1]))

# Now, plot it!
dd <- ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
  # geom_line(alpha=0.1) +
  geom_point(colour="#FC8D62", size=0.5, shape=21) +
  scale_x_continuous("Degree",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000,3000,9000, 32000),
                     trans = "log10") +
  scale_y_continuous("Frequency",
                     breaks = c(1, 5, 30, 300, 1500, 8100, 49500),
                     trans = "log10") +
  ggtitle("Degree distribution (log-log)") +
  # labs(tag = "H")+
  theme_minimal() +
  theme(text = element_text(size = 10),
        panel.grid.minor = element_line(color = "white"))
dd

# Fig. 5 Temporal distribution of network metrics characterising the 2017–2019 monthly pig movements in Ecuador ----
library(ggpubr)
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")

tiff(filename = "Fig_5.tiff",
     width=18, height=14, units="cm", res = 600, 
     pointsize = 12, compression = "lzw")
ggarrange(grcc, gdi, gde, gnn, gsp, ggw, ggs, dd,  
          ncol=2, 
          nrow=4, 
          common.legend = FALSE)
dev.off()


# Other plots same information
ggarrange(grcc, gdi, gde, gnn, gsp, ggw, ggs, dd,  ncol=2, nrow=4, common.legend = FALSE)

# Betweeness
be <- ggplot(mot, aes(x=month,y=b))+
  geom_point(shape=21, colour="#E31A1C")+
  geom_line(colour="#E31A1C")+
  labs(y = "Values", x ="Month", title="Betweennes")+
  theme_minimal() + 
  theme(text = element_text(size = 18))

bef <- ggplot(mot, aes(x=month2, y=b, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="Betweennes")+
  labs(tag = "C")+
  theme_minimal() +
  theme(text = element_text(size = 18))+
  facet_grid(rows = vars(year))


cl <- ggplot(mot, aes(x=month,y=cl))+
  geom_point(shape=21, colour="#E31A1C")+
  geom_line(colour="#E31A1C")+
  labs(y = "Values", x ="Month", title="Closeness")+
  theme_linedraw()+
  theme(text = element_text(size = 18))

clf <- ggplot(mot, aes(x=month2, y=cl, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="Closeness")+
  labs(tag = "D")+
  theme_minimal() +
  theme(text = element_text(size = 18))+
  facet_grid(rows = vars(year))

ggarrange(t,u, bef, clf, common.legend = FALSE, legend="right")


# Plotting Diameter, clustering coefficiente,number of neighbors, shortest paths
library(ggpubr)
ggarrange(diameter_fac,u, neigh_fac, shortest_fac, common.legend = FALSE, legend="right")


#_--------------------------------------------------
#  Mann Kendal test ----
install.packages("Kendall")
library(Kendall)
library(trend)
# https://vsp.pnnl.gov/help/vsample/Design_Trend_Seasonal_Kendall.htm
colnames(mot)

#All Degree ---
# Converting into a time series
dts <- ts(mot$d, start=c(2017,01), end=c(2019,12), frequency=12)
plot(dts)
#Examining autocorrelation
acf(dts)
pacf(dts)
#Running MannKendal test to see individual interactions
dsmk <- smk.test(dts)
dsmk #global
summary(dsmk) #Season

MannKendall(dts)
summary(MannKendall(dts))
#Change-point
pettitt.test(dts) # k=17

plot(decompose(dts))
plot(stl(dts,
         s.window="periodic"))

mean(mot$d) # 17.47



# 1 Betweeneess ----
bts <- ts(mot$b, start=c(2017,01), end=c(2019,12), frequency=12)
plot(bts)
#Running MannKendal test to see individual interactions
bsmk <- smk.test(bts)
bsmk #global
summary(bsmk) #Season
#Change-point
pettitt.test(bts) # k=17
#plot
plot(decompose(nts))
plot(stl(nts,
         s.window="periodic"))
mean(mot$n) # 2.19


# 2 Closenest ----
clts - ts(mot$cl, start=c(2017,01), end=c(2019,12), frequency=12)
plot(ccts)
#Running MannKendal test to see individual interactions
clsmk <- smk.test(clts)
clsmk #global
summary(clsmk) #Season
#Change-point
pettitt.test(clts) # k=17
#plot
plot(decompose(clts))
plot(stl(clts,
         s.window="periodic"))
mean(mot$cl) # 17.47

# 3 Cluster coefficient ----
ccts <- ts(mot$rcc, start=c(2017,01), end=c(2019,12), frequency=12)
plot(ccts)
#Running MannKendal test to see individual interactions
ccsmk <- smk.test(ccts)
ccsmk #global
summary(ccsmk) #Season

MannKendall(ccts)
summary(MannKendall(ccts))

#Change-point
pettitt.test(ccts) # k=17
#plot
plot(decompose(ccts))
plot(stl(ccts,
         s.window="periodic"))
mean(mot$rcc) # 17.47

# 4 Diameter ----
dits <- ts(mot$di, start=c(2017,01), end=c(2019,12), frequency=12)
plot(dits)
#Examining autocorrelation
acf(dits)
pacf(dits)
#Running MannKendal test to see individual interactions
dismk <- smk.test(dits)
dismk #global
summary(dismk) #Season
MannKendall(dits)
summary(MannKendall(dits))
#Change-point
pettitt.test(dits) # k=17
plot(decompose(dits))
plot(stl(dits,
         s.window="periodic"))
mean(mot$di) # 17.47

# 5 Edge Density ----
dets <- ts(mot$de, start=c(2017,01), end=c(2019,12), frequency=12)
plot(dets)
#Running MannKendal test to see individual interactions
desmk <- smk.test(dets)
desmk #global
summary(desmk) #Season
#Change-point
pettitt.test(dets) # k=17
#plot
plot(decompose(dets))
plot(stl(dets,
         s.window="periodic"))
mean(mot$n) # 2.19

# 6 Number of neig ----
nts <- ts(mot$n, start=c(2017,01), end=c(2019,12), frequency=12)
plot(nts)
#Running MannKendal test to see individual interactions
nsmk <- smk.test(nts)
nsmk #global
summary(nsmk) #Season
#Change-point
pettitt.test(nts) # k=17
#plot
plot(decompose(nts))
plot(stl(nts,
         s.window="periodic"))
mean(mot$n) # 2.19

# 7 Shortest paths ----
sts <- ts(mot$spath, start=c(2017,01), end=c(2019,12), frequency=12)
plot(sts)
#Running MannKendal test to see individual interactions
ssmk <- smk.test(sts)
ssmk #global
summary(ssmk) #Season
#Change-point
pettitt.test(sts) # k=17
#plot
plot(decompose(dets))
plot(stl(dets,
         s.window="periodic"))
mean(mot$spath) # 2.19

# 8 Gigant weakly component ----
gts <- ts(mot$gwp, start=c(2017,01), end=c(2019,12), frequency=12)
plot(gts)
#Examining autocorrelation
acf(gts)
pacf(gts)

acf(PrecipGL)
pacf(PrecipGL)
#Running MannKendal test to see individual interactions
gsmk <- smk.test(gts)
gsmk #global
summary(gsmk) #Season
#Change-point
pettitt.test(gts) # k=17
#plot
plot(decompose(gts))
plot(stl(gts,
         s.window="periodic"))
mean(mot$spath) # 2.19

# 9 Gigant strongly component ----
gsts <- ts(mot$gsp, start=c(2017,01), end=c(2019,12), frequency=12)
plot(gsts)
#Examining autocorrelation
acf(gsts)
pacf(gsts)

#Running MannKendal test to see individual interactions
gsmk <- smk.test(gsts)
gsmk #global
summary(gsmk) #Season
#Change-point
pettitt.test(gsts) # k=17
#plot
plot(decompose(gsts))
plot(stl(gsts,
         s.window="periodic"))


library(tidyverse)
install.packages("timetk")
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- TRUE

mot$date <- ymd(paste(mot$year, mot$month2, "01"))
str(mot)

mot %>%
  plot_seasonal_diagnostics(date, di,
                            .feature_set = c("observed", "season",
                                             "trend", "remainder"),
                            .interactive = FALSE)
mot %>%
  plot_stl_diagnostics(
    date, di,
    .frequency = "Month", .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive = FALSE)

mot %>%
  plot_seasonal_diagnostics(
    date, di, .interactive = FALSE)


# Visualize series
taylor_30_min %>%
  plot_time_series(date, value, .interactive = FALSE)

sessionInfo()
Sys.setlocale("LC_TIME", "C")
str(mot)
# Visualize seasonality
taylor_30_min %>%
  plot_seasonal_diagnostics(date, value, .interactive = TRUE)

#_--------------------------------------------------
# Organizacion vigilancia----
# Gerando únicamente os casos v3
# --------------------------------------------------
library(dplyr)
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")

#modificando colnames de csv VEO por Rep_csv
# Estos arquivos tem os colnames certos
vge0 <- read.csv("Rep_General_EventosSan0.csv", colClasses = "character",encoding = "UTF-8")
vr0 <- read.csv("Rep_ResultadosGEV0.csv", colClasses = "character",encoding = "UTF-8")
vc0 <- read.csv("Rep_CierreGEV0.csv", colClasses = "character",encoding = "UTF-8")

#Estos arquivos tem os colnames errados, mas a informacao de cedula certa (baixados desde o sizse 11.12.2019)
vge <- read.csv("VEO1_G.csv", colClasses = "character", encoding = "UTF-8")
vr <- read.csv("VEO1_R.csv", colClasses = "character", encoding = "UTF-8")
vc <- read.csv("VEO1_C.csv", colClasses = "character",encoding = "UTF-8")

colnames(vge) <- colnames(vge0)
colnames(vr) <- colnames(vr0)
colnames(vc) <- colnames(vc0)

rm(vge0, vr0, vc0)


# Aceitabilidade vigilancia ----
#Carregando bancos
#Banco Reporte geral de eventos
vge$canton <- vge$cantón
vge$cantón <- NULL

#Banco Resultados de eventos
vr$cant_muestras <- as.numeric(vr$cant_muestras)
vr$positivos <- as.numeric(vr$positivos)
vr$negativos <- as.numeric(vr$negativos)
vr$reactivos <- as.numeric(vr$reactivos)
vr$indeterminados <- as.numeric(vr$indeterminados)
vr$canton <- vr$cantón
vr$cantón <- NULL

# Banco Cierre de eventos
vc$existentes <- as.numeric(vc$existentes)
vc$enfermos <- as.numeric(vc$enfermos)
vc$muertos <- as.numeric(vc$muertos)
vc$sacrificad <- as.numeric(vc$sacrificad)
vc$canton <- vc$cantón
vc$cantón <- NULL

# Linkage dos reportes
v1 <- left_join(vc, vr)
v2 <- left_join(v1,vge)

v2 <- v2 %>%
  filter(especie == "PORCINOS")%>%
  filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados), reactivo=sum(reactivos))
#718 eventos #854 com atualizacao 11/22/2019

#1275 com atualizacao 10/09/2020
#932 com atualizacao 10/09/2020 todas as patologias

v2$ano <- substring(v2$f_1er_enfermo,7,10)
v2 <- v2 %>% mutate(caso = ifelse(pos >=1,1,0))

v2 %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)), surtos=sum(pos >= 1, na.rm = TRUE))

#mudando nomes no banco de vigilancia para fazer match com outros bancos
colnames(v2)
names(v2)[5] <- "identificador_operador"
names(v2)[6] <- "nombre_operador"
names(v2)[11] <- "nombre_sitio"


library(stringr)
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")
vv <- read.csv(file="casos_vigilancia-2014-2018.csv", colClasses = "character")
vv$codigo.sitio2 <- str_trim(vv$codigo.sitio2)
vv[,1] <- NULL

colnames(v2)
colnames(vv)

names(vv)[5] <- "identificador_operador"
names(vv)[6] <- "nombre_operador"
names(vv)[11] <- "nombre_sitio"

vv <- vv[,-c(43, 44)]

v2$codigo.sitio2 <- 0

v2 %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)), surtos=sum(pos >= 1, na.rm = TRUE))

vv %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)), surtos=sum(pos >= 1, na.rm = TRUE))

novos <- v2[v2$orden %in% vv$orden == FALSE,]
novos <- data.frame(novos)
vf <- rbind(vv, novos)

vf %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)), surtos=sum(pos >= 1, na.rm = TRUE))

#finalmente a db está pronta, agora a procurar os códigos de sitio desde o cadastro do sql
setwd("/home/alfredo/Dropbox/0.USP/9. 2020 I sem/Projeto/Modelo espacial")
c <- read.csv(file="cadastro2019-final.csv")

# Comparação inicial
table(vv$identificador_operador %in% c$id_operador)
table(vv$codigo.sitio2 %in% c$codigo_sitio)

vf$codigo.sitio3 <- c$codigo_sitio[match(vf$identificador_operador, c$id_operador)]

library(tidyr)
vf %>%
  group_by(ano) %>%
  filter(caso == "1")%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE), y=length(unique(codigo.sitio3)) )

# Vamos importar o banco de movimentação e olhar quantos casos se encontran na red.  
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)

m2 <- data.frame(m2)
library(epinemo)
banco <- createUniqueIds(m2,
                         from = 'codigo.sitio.origen',
                         to = 'codigo.sitio.destino')

#Confirmar quantos casos movimentaram animais

table(vf$codigo.sitio3 %in% banco$correspondence$database.id)
table(vf$codigo.sitio3 %in% banco$correspondence$database.id)

vf$codigo.sitio3 <- c$codigo_sitio[match(vf$identificador_operador, c$id_operador)]

vf$network <- banco$correspondence$network.id[match(
  vf$codigo.sitio3, banco$correspondence$database.id)]

vf %>%
  group_by(ano) %>%
  filter(caso == "1")%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE), tiene.codigo.sitio=length(unique(codigo.sitio3)),
            esta.en.la.red <- length(unique(network)))

# Vamos importar as coordenadas geográficas
setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização")
vlatlong <- read.csv(file="casos_vigilancia-2014-2018-lat-long.csv")

vf$lat <- vlatlong$Latitude[match(vf$orden, vlatlong$orden)]
vf$long <- vlatlong$Longitude[match(vf$orden, vlatlong$orden)]

vf %>%
  group_by(ano) %>%
  filter(caso == "1")%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE), tiene.codigo.sitio=length(unique(codigo.sitio3)),
            esta.en.la.red <- length(unique(network)),
            cord <- length(unique(lat)))

81+82+30+39+60+35+7

setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização")
write.csv(vf, file="casos_vigilancia-2014-2020.csv")


##########
setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização/latlong")
a <- read.csv("casos_vigilancia-2014-2020.csv", colClasses = "character")
table(is.na(a$lat))
a1 <- a[is.na(a$lat) == FALSE,]

a2 <- read.csv("17S.csv", colClasses = "character")
a3 <- read.csv("18S.csv", colClasses = "character")
a4 <- read.csv("17N.csv", colClasses = "character")
a5 <- read.csv("18N.csv", colClasses = "character")
a6 <- read.csv("NA1.csv", colClasses = "character")
a7 <- read.csv("NA2.csv", colClasses = "character")

b <- rbind(a2,a3,a4,a5)
c <- rbind(a6,a7)

b$lon <- NULL
b$lat <- NULL

colnames(a1)[47] <- "long"
colnames(a1)[48] <- "lati"

a1[,1] <- NULL
b[,1] <- NULL
c[,1] <- NULL
colnames(c)[46] <- "long"
colnames(c)[47] <- "lati"

colnames(b) <- colnames(a1)
colnames(c) <- colnames(a1)

vf2 <- rbind(a1,b,c)

vf2 %>%
  group_by(ano) %>%
  filter(caso == "1")%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE), tiene.codigo.sitio=length(unique(codigo.sitio3)),
            esta.en.la.red <- length(unique(network)),
            cord <- length(unique(lati)))

write.csv(vf2, file = "casos_vigilancia-2014-2020-lat-long.csv")



# 8 Completar a db ----
library(dplyr)

setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização")
# qgis exportou alguns codigos como integer e perdío 0 inicial, vou recuperar desde a
a <- read.csv("casos_vigilancia-2014-2020.csv", colClasses = "character")
vf2 <- read.csv(file = "casos_vigilancia-2014-2020-lat-long.csv", colClasses = "character")

a$longitude <- vf2$long[match(a$orden, vf2$orden)]
a$latitude <- vf2$lati[match(a$orden, vf2$orden)]
a$long <- NULL
a$lat <- NULL

#organizar os códigos de sitio
a <- a %>% mutate(codigo.sitio2 = ifelse(codigo.sitio2 == "", NA, codigo.sitio2))
a <- a %>% mutate(codigo.sitio2 = ifelse(codigo.sitio2 == "0", NA, codigo.sitio2))

a <- a %>% mutate(codigo.sitio4 = ifelse(is.na(codigo.sitio2) == TRUE, codigo.sitio3, codigo.sitio2))
table(is.na(a$codigo.sitio4)) #true 453
a <- a %>% mutate(codigo.sitio5 = ifelse(is.na(codigo.sitio4) == TRUE, codigo.sitio3, codigo.sitio4))
table(is.na(a$codigo.sitio5)) #true 453
a <- a %>% mutate(codigo.sitio6 = ifelse(is.na(codigo.sitio5) == TRUE, identificador_operador, codigo.sitio5))
table(is.na(a$codigo.sitio6)) #true 0

#579 com codigo de sitio
# dividir o db com os casos (casos = c)
c <- a 

table(is.na(c$network))

vigi <- c %>%
  group_by(provincia, canton, parroquia) %>%
  summarise(cantidad = n())
sum(vigi$cantidad , na.rm = TRUE) 

### Mapa para obter centroides
library(rgdal)
library(gdata)

ec3<-readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Mapa para vigilancia esta função está
#melhorada para o banco de vigilancia

f{
  # Mapa para vigilancia
  #Provincia
  #atualizado 27.01.2020 banco cadastro 2018-2019 com codigo de sitio
  # fazer colunas comparaveis
  ec3@data$provincia <- ec3@data$DPA_DESPRO
  vigi$p <- vigi$provincia
  
  ec3@data$provincia <- gsub("Ñ","N", ec3@data$provincia)
  ec3@data$provincia <- gsub("Ã","N", ec3@data$provincia)
  
  # Crio os comparaveis
  ec3@data$p <- trim(tolower(paste(ec3@data$provincia)))
  vigi$p <- trim(tolower(paste(vigi$p)))
  
  vigi$p <- gsub("á","a", vigi$p)
  vigi$p <- gsub("ú","u", vigi$p)
  vigi$p <- gsub("é","e", vigi$p)
  vigi$p <- gsub("í","i", vigi$p)
  vigi$p <- gsub("ó","o", vigi$p)
  vigi$p <- gsub("ñ","n", vigi$p)
  
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_p <- tolower(ec3@data$DPA_PROVIN[match(vigi$p, ec3@data$p)])
  
  sum(is.na(as.numeric(vigi$c_p)))
  vigi[is.na(as.numeric(vigi$c_p)), 5]
  # 0
  
  #Canton
  ec3@data$canton <- tolower(ec3@data$DPA_DESCAN)
  vigi$cant <- tolower(vigi$canton)
  
  vigi$cant <- gsub("á","a", vigi$cant)
  vigi$cant <- gsub("ú","u", vigi$cant)
  vigi$cant <- gsub("é","e", vigi$cant)
  vigi$cant <- gsub("í","i", vigi$cant)
  vigi$cant <- gsub("ó","o", vigi$cant)
  
  # Fazer mudancas considerando mapa padrao ouro
  ec3@data$canton <- gsub("ñ","n", ec3@data$canton)
  ec3@data$canton <- gsub("ð","n", ec3@data$canton)
  ec3@data$canton <- gsub("puebloviejo","pueblo viejo", ec3@data$canton)
  
  #tirando o parenteses
  vigi$cant <- trim(gsub("\\(.*","", vigi$cant))
  
  #mudanças para mudar vigi adaptando para ec3@data
  #vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  vigi$cant <- gsub("pelipeo","pelileo", vigi$cant)
  vigi$cant <- gsub("ñ","n", vigi$cant)
  vigi$cant <- gsub("francisco de orellana","orellana", vigi$cant)
  vigi$cant <- gsub("pelileo","san pedro de pelileo", vigi$cant)
  vigi$cant <- gsub("el empalme","empalme", vigi$cant)
  vigi$cant <- gsub("santiago de mendez","santiago", vigi$cant)
  vigi$cant <- gsub("urcuqui","san miguel de urcuqui", vigi$cant)
  vigi$cant <- gsub("marcelino mariduena", "crnel. marcelino mariduena", vigi$cant)
  vigi$cant <- gsub("yaguachi", "san jacinto de yaguachi", vigi$cant) #
  vigi$cant <- gsub("pueblobiejo","pueblo viejo", vigi$cant)
  vigi$cant <- gsub("macas","morona", vigi$cant)
  #vigi$cant <- gsub("joya de los sachas","la joya de los sachas", vigi$cant) #
  vigi$cant <- gsub("puyo","pastaza", vigi$cant)
  vigi$cant <- gsub("pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("santiago de santiago de pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("rio verde","rioverde", vigi$cant)
  vigi$cant <- gsub("general antonio elizalde","gnral. antonio elizalde", vigi$cant)
  #vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  vigi$cant <- gsub("banos","banos de agua santa", vigi$cant)
  
  #crio coluna conjunta para comparar
  ec3@data$c <- trim(tolower(paste(ec3@data$provincia,ec3@data$canton)))
  vigi$c <- trim(tolower(paste(vigi$p, vigi$cant)))
  
  #caso especial la concordia cambiandole de provincia
  vigi$c <- gsub("santo domingo de los tsachilas la concordia","esmeraldas la concordia", vigi$c)
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_c <- ec3@data$DPA_CANTON[match(vigi$c, ec3@data$c)]
  
  #numero de catones sem id
  sum(is.na(as.numeric(vigi$c_c)))
  # 0
  
  #cuenta, numero e ordem deles
  sum(is.na(as.numeric(vigi$c_c)))
  vigi[is.na(as.numeric(vigi$c_c)), 8]
  
  cant <-vigi[is.na(as.numeric(vigi$c_c)), 8]
  cant
  
  #Parroquia
  
  #Criacao e transferencia dos valores a novas colunas para comparacao
  ec3@data$parroquia <- ec3@data$DPA_DESPAR
  vigi$par <- tolower(vigi$parroquia)
  
  #Modificando novas colunas por dados comparaveis
  ec3@data$parroquia <- gsub("á","a", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("é","e", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("í","i", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ó","o", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ú","u", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("Ñ","N", ec3@data$parroquia)
  
  #tirando o parenteses
  ec3@data$parroquia <- trim(gsub("\\(.*","", ec3@data$parroquia))
  vigi$par <- trim(gsub("\\(.*","", vigi$par))
  
  #ec3@data$parroquia <- gsub("ALFREDO BAQUERIZO MORENO (JUJAN)","JUJAN", ec3@data$parroquia)
  vigi$par <- gsub("á","a", vigi$par)
  vigi$par <- gsub("é","e", vigi$par)
  vigi$par <- gsub("í","i", vigi$par)
  vigi$par <- gsub("ó","o", vigi$par)
  vigi$par <- gsub("ú","u", vigi$par)
  vigi$par <- gsub("ñ","n", vigi$par)
  vigi$par <- gsub("ü","u", vigi$par)
  vigi$par <- gsub("crnl.","crnel.", vigi$par)
  
  vigi$par <- gsub("holgupin","holguin", vigi$par)
  vigi$par <- gsub("conrdoncillo","cordoncillo", vigi$par)
  vigi$par <- gsub("curticapa","curtincapa", vigi$par)
  vigi$par <- gsub("general leonidas plaza gutierrez", "gral. leonidas plaza gutierrez",vigi$par)
  vigi$par <- gsub("san jose de guyusa", "san jose de guayusa",vigi$par) #
  #vigi$par <- gsub("puerto francisco de orel", "puerto francisco de orellana",vigi$par) #
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san jose de alluriquin", "alluriquin",vigi$par)
  vigi$par <- gsub("el empalme", "velasco ibarra",vigi$par)
  vigi$par <- gsub("santo domingo", "santo domingo de los colorados",vigi$par)
  vigi$par <- gsub("santo domingo de los colorados de onzole", "santo domingo de onzole",vigi$par)
  #vigi$par <- gsub("guasaganda", "GUASAGANDA (CAB. EN GUASAGANDA CENTRO)",vigi$par)
  #vigi$par <- gsub("simon bolivar", "SIMON BOLIVAR (JULIO MORENO)",vigi$par)
  #vigi$par <- gsub("julio e. moreno", "JULIO E. MORENO (CATANAHUAN GRANDE)",vigi$par)
  #vigi$par <- gsub("san pablo", "SAN PABLO (SAN PABLO DE ATENAS)",vigi$par)
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san lorenzo de jipijapa", "jipijapa",vigi$par)
  vigi$par <- gsub("santafe", "santa fe",vigi$par)
  vigi$par <- gsub("san jose de chazo", "san jose del chazo",vigi$par)
  vigi$par <- gsub("cibijies", "cubijies",vigi$par)
  vigi$par <- gsub("crnl. carlos concha torres", "crnel. carlos concha torres",vigi$par)
  vigi$par <- gsub("la lojas", "los lojas",vigi$par)
  vigi$par <- gsub("padre juan batista aguirre", "juan bautista aguirre",vigi$par)
  vigi$par <- gsub("velazco ibarra", "velasco ibarra",vigi$par)
  vigi$par <- gsub("gnral. antonio elizalde", "gral. antonio elizalde",vigi$par)
  vigi$par <- gsub("coronel marcelino mariduenas", "coronel marcelino mariduena",vigi$par)
  vigi$par <- gsub("tarida", "tarifa",vigi$par)
  vigi$par <- gsub("san francisco de natabuela", "san fco. de natabuela",vigi$par)
  vigi$par <- gsub("dr. miguel egas cabezas", "doctor miguel egas cabezas",vigi$par)
  vigi$par <- gsub("san francisco de sigsipamba", "san  fco. de sigsipamba",vigi$par)
  vigi$par <- gsub("chiquiribamba", "chuquiribamba",vigi$par)
  vigi$par <- gsub("bolsapamba", "bolaspamba",vigi$par)
  vigi$par <- gsub("santa susana de chiviaza", "sta susana de chiviaza",vigi$par)
  vigi$par <- gsub("pablo secto", "pablo sexto",vigi$par)
  vigi$par <- gsub("pumipamba", "rumipamba",vigi$par)
  vigi$par <- gsub("pani", "pano",vigi$par)
  vigi$par <- gsub("quinsamola", "quinsaloma",vigi$par)
  vigi$par <- gsub("pelileo grande", "pelileo",vigi$par)
  vigi$par <- gsub("jujan", "alfredo baquerizo moreno",vigi$par)
  vigi$par <- gsub("triunfo dorado", "triunfo-dorado",vigi$par)
  vigi$par <- gsub("chontaduro", "rioverde",vigi$par)
  vigi$par <- gsub("24 de mayo", "sucre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("general leonidas plaza g.", "gral. leonidas plaza gutierrez",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("3 de noviembre", "tres de noviembre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("julio moreno", "santo domingo de los tsachilas",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("sta. cecilia", "santa cecilia",vigi$par) #
  vigi$par <- gsub("el playon de san francis", "el playon de san francisco",vigi$par) #
  vigi$par <- gsub("banos", "banos de agua santa",vigi$par) #
  vigi$par <- gsub("el guismi", "el guisme",vigi$par) #
  vigi$par <- gsub("yanzatza", "yantzaza",vigi$par) #
  vigi$par <- gsub("nobol", "narcisa de jesus",vigi$par) #
  vigi$par <- gsub("crnel.lorenzo de garaicoa", "crnel. lorenzo de garaicoa",vigi$par) #
  vigi$par <- gsub("general antonio elizalde", "gral. antonio elizalde",vigi$par) #
  
  
  # Criando novas colunas para comparacao
  ec3@data$pp <- trim(tolower(paste(ec3@data$c,ec3@data$parroquia)))
  vigi$pp <- trim(tolower(paste(vigi$c,vigi$par)))
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # Transferindo cantidad vigilancia para o data frame
  ec3@data$cantidad <- vigi$cantidad[match(ec3@data$pp, vigi$pp)]
  
  #numero de parroquias sem id
  sum(is.na(as.numeric(vigi$c_pp)))
  #227
  
  #cuenta, nombre e ordem
  sum(is.na(as.numeric(vigi$c_pp)))
  #229
  
  vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  #Transformando as parroquias urbanas atuais em parroquias anteriores a divisao
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca banos de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca cuenca de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca bellavista", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el batan", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el sagrario", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el vecino", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca gil ramirez davalos", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca huaynacapac", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca machangara", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca monay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca octavio cordero", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca palacios", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san blas", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san sebatian", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca sucre", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca totoracocha", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca yanuncay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca canaribamba", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay gualaceo daniel cordova", "azuay gualaceo gualaceo",vigi$pp)
  vigi$pp <- gsub("azuay gualaceo daniel cordova", "azuay gualaceo gualaceo toral",vigi$pp)
  vigi$pp <- gsub("azuay sigsig jima", "azuay sigsig sigsig",vigi$pp)
  vigi$pp <- gsub("azuay ona ona", "azuay ona san felipe de ona",vigi$pp)
  
  vigi$pp <- gsub("bolivar guaranda guanujo", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chavez", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chaves", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabreil ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabriel ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar las naves las mercedes", "bolivar las naves las naves",vigi$pp)
  vigi$pp <- gsub("bolivar echeandia limon", "bolivar echeandia echeandia",vigi$pp)  
  
  vigi$pp <- gsub("canar azogues aurelio bayas martinez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues azoguez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues borrero", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues san francisco", "canar azogues azogues",vigi$pp)
  
  vigi$pp <- gsub("carchi espejo 27 de septiembre", "carchi espejo el angel",vigi$pp)
  vigi$pp <- gsub("carchi montufar gonzalez suarez", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi montufar san jose", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi tulcan gonzalez suarez", "carchi tulcan tulcan",vigi$pp)
  
  vigi$pp <- gsub("chimborazo colta cajabamba", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo colta sicalpa", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano la matriz", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  
  vigi$pp <- gsub("chimborazo riobamba lizarzaburu", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba maldonado", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba velasco", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba veloz", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba yaruquies", "chimborazo riobamba riobamba",vigi$pp)
  
  vigi$pp <- gsub("cotopaxi la mana el carmen", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga el triunfo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga eloy alfaro", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga ignacio flores", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga juan montalvo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga la matriz", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga san buenaventura", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  
  vigi$pp <- gsub("el oro huaquillas ecuador", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas el paraiso", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas milton reyes", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas union lojana", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas hualtaco", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro las lajas valle hermoso", "el oro las lajas la victoria",vigi$pp)
  vigi$pp <- gsub("el oro machala el cambio", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala la providencia", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala nueve de mayo", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala puerto bolivar", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro pasaje bolivar", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje loma de franco", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje ochoa leon", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje tres cerritos", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pinas la matriz", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas la susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas pinas grande", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa jumon", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa nuevo santa rosa", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa puerto jeli", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("esmeraldas eloy alfaro esmeraldas norte", "esmeraldas eloy alfaro santa lucia de las penas",vigi$pp)
  
  vigi$pp <- gsub("esmeraldas esmeraldas luis tello", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas simon torres", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas 5 de agosto", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas bartolome ruiz", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas la concordia las villegas", "esmeraldas la concordia la villegas",vigi$pp)
  
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule la uaurora", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule padre juan bautista aguirre", "guayas daule daule",vigi$pp) #
  vigi$pp <- gsub("guayas duran el recreo", "guayas duran eloy alfaro",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil pascuales", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil bolivar", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ayacucho", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil carbo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil febres cordero", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil garcia moreno", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil letamendi", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil nueve de octubre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil olmedo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil rocafuerte", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil sucre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil tarqui", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil urdaneta", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas salitre bocana", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre central", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre grnl. vernaza", "guayas salitre el salitre",vigi$pp)
  
  vigi$pp <- gsub("imbabura antonio ante andrade marin", "imbabura antonio ante atuntaqui",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi sagrario",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra caranqui", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra guayaquil de alpachaca", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra la dolorosa del priorato", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra san francisco", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo jordan", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo san luis", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi san francisco", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra sagrario", "imbabura ibarra san miguel de ibarra",vigi$pp)
  
  vigi$pp <- gsub("loja calvas chile", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja calvas san vicente", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja catamayo san jose", "loja catamayo catamayo",vigi$pp)
  vigi$pp <- gsub("loja loja el sagrario", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja san sebastian", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja sucre", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja macara general eloy alfaro", "loja macara macara",vigi$pp)
  vigi$pp <- gsub("loja paltas lourdes", "loja paltas catacocha",vigi$pp)
  vigi$pp <- gsub("loja loja valle", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja santiago \"san salvador o james\"", "loja loja santiago",vigi$pp)
  
  
  vigi$pp <- gsub("los rios babahoyo clemente baquerizo", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo barreiro", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo dr. camilo ponce", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo dr camilo ponce", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo el salto", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios buena fe 7 de agosto", "los rios buena fe san jacinto de buena fe",vigi$pp)
  vigi$pp <- gsub("los rios pueblo viejo san juan de iluman", "los rios pueblo viejo puebloviejo",vigi$pp)
  vigi$pp <- gsub("los rios ventanas quinsaloma", "los rios quinsaloma quinsaloma",vigi$pp)
  vigi$pp <- gsub("los rios valencia la esperanza", "los rios valencia valencia",vigi$pp)# no existe parroquia
  vigi$pp <- gsub("los rios valencia la union", "los rios valencia valencia",vigi$pp)#
  vigi$pp <- gsub("los rios valencia vergel", "los rios valencia valencia",vigi$pp)#
  
  vigi$pp <- gsub("los rios quevedo 24 de mayo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo guayacan", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo nicolas infante diaz", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san camilo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san cristobal", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo sucre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo siete de octubre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo venus del rio quevedo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  
  vigi$pp <- gsub("manabi chone santa rita", "manabi chone chone",vigi$pp)
  vigi$pp <- gsub("manabi el carmen 4 de diciembre", "manabi el carmen el carmen",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa dr. miguel moran lucio", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa manuel inocencio parrales y guale", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi manta eloy alfaro", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta tarqui", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta los esteros", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta san mateo", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi montecristi el colorado", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi general eloy alfaro", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi leonidas proano", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi anibal san andres", "manabi montecristi montecristi",vigi$pp) #agregada 14.02.2020 Network_Description.R
  
  vigi$pp <- gsub("manabi portoviejo 12 de marzo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo 18 de octubre", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo andres de vera", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo colon", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo francisco pacheco", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo picoaza", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo san pablo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo simon bolivar", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi santa ana lodana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana de vuelta larga de vuelta larga", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi sucre leonidas plaza gutierrez", "manabi sucre bahia de caraquez",vigi$pp)
  
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago tiwintza puyo", "morona santiago tiwintza santiago",vigi$pp)
  
  vigi$pp <- gsub("pichincha cayambe ayora", "pichincha cayambe san jose de ayora",vigi$pp)
  vigi$pp <- gsub("pichincha cayambe juan montalvo", "pichincha cayambe cayambe",vigi$pp)
  vigi$pp <- gsub("pichincha quito belisario quevedo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito benalcazar", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito carcelen", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito centro historico", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cotocollao", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chaupicruz", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chilibulo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chillogallo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chimbacalle", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cochapamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito comite del pueblo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito guamani", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito el condado", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito eloy alfaro", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito inaquito", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito itchimbia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la concepcion", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ecuatoriana", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ferroviaria", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la floresta", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la libertad", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la mena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito mariscal sucre", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito ponceano", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito puengasi", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito quitumbe", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito rumipamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san antonio de minas", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san bartolo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san isidro del inca", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san roque", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito solanda", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito turubamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la magdalena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san pedro de taboada", "pichincha ruminahui sangolqui",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san rafael", "pichincha ruminahui sangolqui",vigi$pp)
  
  vigi$pp <- gsub("santa elena salinas santa rosa", "santa elena salinas salinas",vigi$pp)
  vigi$pp <- gsub("santa elena santa elena ballenita", "santa elena santa elena santa elena",vigi$pp)
  
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chiguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo las mercedes", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo abraham calazacon", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chimguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio toachi", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio verde", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo zaracay", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los tsachilas", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los colorados de los colorados", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo nuevo isrrael", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  
  vigi$pp <- gsub("sucumbios lago agrio santa cruz", "sucumbios lago agrio santa cecilia",vigi$pp)
  
  vigi$pp <- gsub("tungurahua ambato atocha - ficoa", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato celiano monge", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi chico", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi loreto", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la merced", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la peninsula", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato matriz", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato pishilata", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san bartolo de pinllog", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san francisco", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pillaro ciudad nueva", "tungurahua santiago de pillaro pillaro",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pelileo pelileo grande", "tungurahua santiago de pelileo pelileo",vigi$pp)
  
  vigi$pp <- gsub("zamora chinchipe zamora el limon", "zamora chinchipe zamora zamora",vigi$pp)
  
  #transferencia de codigo do mapa os que forem mach da pro-can-parr
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # agregando os valores que foram transformados de parroquias urbanas a normais
  vigi2 <- vigi %>%
    group_by(pp) %>%
    summarise(cantidad = sum(cantidad))
  
  #Tranferindo valores agregados
  ec3@data$cantidad <- vigi2$cantidad[match(ec3@data$pp, vigi2$pp)]
  
  # Comparando numeros
  sum(vigi$cantidad, na.rm = TRUE)
  sum(vigi2$cantidad, na.rm = TRUE) -
    sum(ec3@data$cantidad, na.rm = TRUE)
  
  ####ANTERIOR AINDA NAO DELETAR
  sum(is.na(as.numeric(vigi$c_pp)))
  #157
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  par
  
  # Animais faltantes no mapa
  sum(vigi$cantidad) - sum(ec3@data$cantidad, na.rm = TRUE)
  
}

vigi$dpa_despro <- paste(vigi$provincia, vigi$canton, vigi$parroquia)
a$dpa_despro <- paste(a$provincia, a$canton, a$parroquia)

#Pasar os codigos de provincia para o vigi
a$dpa_pro <- vigi$c_p[match(a$dpa_despro, vigi$dpa_despro)]
a$dpa_par <- vigi$c_pp[match(a$dpa_despro, vigi$dpa_despro)]

a$codigo.sitio7 <- paste(a$identificador_operador,a$dpa_pro, sep=".")
a$codigo.sitio8 <- paste(a$codigo.sitio7,"G1", sep = "")
a <- a %>% mutate(codigo.sitiof = ifelse(is.na(codigo.sitio5) == TRUE, codigo.sitio8, codigo.sitio5))

#identifico se foi Código sitio generado G1
a <- a %>% mutate(codigoG = ifelse(is.na(codigo.sitio5) == TRUE, 1, 0))

#vou procurar las parroquias das propriedades onde vou sortear os predios

c <- a %>% filter(caso == 1)
table(c$codigoG) #161 propriedades que não tem network

table(is.na(c$network), c$codigoG)

c %>% group_by(ano) %>%
  filter(is.na(network) == TRUE) %>%
  summarise(insertar=sum(as.numeric(codigoG)))

table(is.na(c$network))

d <- c %>% group_by(ano, dpa_par, identificador_operador, orden) %>%
  filter(ano == 2017 | ano == 2018 | ano == 2019) %>%
  summarise(sortear=sum(as.numeric(caso)))

# Sortear uma propriedade aleatoria para a movimentação na mesma parroquia
# Vou carregar o banco de movimentação
# Vamos importar o banco de movimentação e olhar quantos casos se encontran na red.  


# 9 temporal analysis----
# Read the m2 file ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]

m2 <- m2[m2$operacion.destino != "Faenador",]

sum(as.numeric(m2$cantidad))
#6407392 Total

# 1 Analysis by type of premise ----
# 1.1 analysis without direction

# 3.0 Dates ----
library(lubridate)
m2$fecha.inicio.vigencia <- dmy_hm(m2$fecha.inicio.vigencia)
plot(m2$fecha.inicio.vigencia, m2$cantidad)

# Generate a file with from, to and the number of day
# To see last day
summary(m2$fecha.inicio.vigencia)
#2017-2019
m2$nday <- 1096-round(difftime("2019-12-31 23:00:00", m2$fecha.inicio.vigencia, units = "days"),0)

#2019
round(difftime(max(m2$fecha.inicio.vigencia), min(m2$fecha.inicio.vigencia)))
m2$nday <- 366-round(difftime("2018-12-31 20:30:00", m2$fecha.inicio.vigencia, units = "days"),0)
summary(as.numeric(m2$nday))

# file
swine.ecuador <- data.frame(m2$codigo.sitio.origen, m2$codigo.sitio.destino, m2$nday)

library(epinemo)
swine.ecuador <- data.frame(swine.ecuador)
banco <- createUniqueIds(swine.ecuador,
                         from = "m2.codigo.sitio.origen",
                         to= "m2.codigo.sitio.destino")
ecuador <- data.frame(banco$movements$From, banco$movements$To, banco$movements$m2.nday)

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio/Temporal/Lenz/TemporalNetworkAccessibility/edgelists/")

write.table(ecuador, file="ecuador-2018.txt", col.names = FALSE, row.names=FALSE, sep = "\t")

write.table(ecuador, file="ecuador-collectors.txt", col.names = FALSE, row.names=FALSE, sep = "\t")
s

# Table 4 ----
m2 <- data.frame(m2)
b <- createUniqueIds(m2,
                     from = "codigo.sitio.origen",
                     to= "codigo.sitio.destino")
nodes.da.rede <- b$correspondence$network.id
grafo <- graph_from_data_frame(b$movements[, c("From", "To")], vertices = nodes.da.rede) 
vcount(grafo) #nodes   165647 
#                     
ecount(grafo) #arestas 751003
#                     

V(grafo)$in_degree <- degree(grafo, mode = "in")
V(grafo)$out_degree <- degree(grafo, mode = "out")
V(grafo)$all_degree <- degree(grafo, mode = "all")
page_rank_igraph <-page.rank(grafo)
V(grafo)$pagerank <- page_rank_igraph$vector

b$correspondence$page_rank <- V(grafo)$pagerank
b$correspondence$all_degree <- V(grafo)$all_degree
b$correspondence$in_degree <- V(grafo)$in_degree
b$correspondence$out_degree <- V(grafo)$out_degree

b$correspondence$saldo <- b$correspondence$in_degree - b$correspondence$out_degree 

sum(b$correspondence$all_degree)
write.csv(b$correspondence, file="community.correspondence.csv")


#_--------------------------------------------------
# Plotting the all degree and ----
library(readr); library(epinemo); library(igraph); library(dplyr)
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
m3 <- read_csv(file = "banco.movements.2017-2019.comu.centroids.csv")

m3 <- data.frame(m3)
b <- createUniqueIds(m3,
                     from = "origin_cod",
                     to= "destiny_cod")
nodes.da.rede <- b$correspondence$network.id
grafo <- graph_from_data_frame(b$movements[, c("From", "To")], vertices = nodes.da.rede) 
vcount(grafo) #nodes 945   
#                     
ecount(grafo) #arestas 751003*2
#                     

V(grafo)$in_degree <- degree(grafo, mode = "in")
V(grafo)$out_degree <- degree(grafo, mode = "out")
V(grafo)$all_degree <- degree(grafo, mode = "all")
page_rank_igraph <-page.rank(grafo)
V(grafo)$pagerank <- page_rank_igraph$vector

b$correspondence$page_rank <- V(grafo)$pagerank
b$correspondence$all_degree <- V(grafo)$all_degree
b$correspondence$in_degree <- V(grafo)$in_degree
b$correspondence$out_degree <- V(grafo)$out_degree

b$correspondence$balance <- b$correspondence$in_degree - b$correspondence$out_degree 

sum(b$correspondence$all_degree)

sum(b$correspondence$balance)


# write.csv(df, file="centrality_by_parish.csv")

# FigDegree  Mapa in degree, out degree, balance----
library(sp); library(ggplot2); library(tidyverse)

ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ec3 <- subset(ec3, DPA_DESPRO != "GALAPAGOS")

ec3@data$id <- rownames(ec3@data)
ec <- fortify(ec3)
ec$DPA <- ec3$DPA_PARROQ[match(ec$id, ec3@data$id)]
data <- b$correspondence

# eliminando Orellana aguarico tiputini, curarai
data$all_degree[data$database.id == "160451"] <- NA
data$all_degree[data$database.id == "220254"] <- NA
data$all_degree[data$database.id == "210751"] <- NA

data$id <- ec3@data$id[match(data$database.id, ec3@data$DPA_PARROQ)]
ec <- left_join(ec, data, by="id")

# in_degree
i <- ggplot(data=ec, aes(x=long, y=lat, group=group))+
  geom_path(data=ec3, aes(x=long, y=lat,group=group),
            colour="grey80", size=0.1)+
  geom_polygon(aes(x=long, y=lat, fill = in_degree)) +
  # scale_fill_viridis_c()+
  # scale_fill_gradient(low="white", high = "#4DA626", na.value = "white")+
  scale_fill_gradient2(high = "#66C2A5", na.value = "white")+
  xlim(-81.5,-75) +
  coord_fixed()+
  labs(tag = "A")+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "in-Degree")
i

#out degree
o <- ggplot(data=ec, aes(x=long, y=lat, group=group))+
  geom_path(data=ec3, aes(x=long, y=lat,group=group),
            colour="grey80", size=0.02)+
  geom_polygon(aes(x=long, y=lat, fill = out_degree)) +
  # scale_fill_viridis_c(direction=-1, na.value = "White")+
  # scale_fill_gradient2(low="white", high = "#d7301F", na.value = "white")+
  scale_fill_gradient2(high = "#FC8D62", na.value = "white")+
  xlim(-81.5,-75) +
  labs(tag = "B")+
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "out-Degree")

library(ggpubr)
ggarrange(i,o)

# balance plot
b <- ggplot(data=ec, aes(x=long, y=lat, group=group))+
  geom_path(data=ec3, aes(x=long, y=lat,group=group),
            colour="grey80", size=0.02)+
  geom_polygon(aes(x=long, y=lat, fill = balance)) +
  scale_fill_gradient2(high = "#E78AC3", na.value = "white")+
  xlim(-81.5,-75) +
  labs(tag = "C")+
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "Balance")

# Plot balance with parish lines ----

# Download map of borders
library(ggsn)
library(raster)
# Province limits
ecu <- getData('GADM', country='ECU', level=1)
ecu <- subset(ecu, NAME_1 != "Galápagos")
plot(ecu)

# Ecuador limits
ecu.0 <- getData('GADM', country='ECU', level=0)
ecu.0 <- subset(ecu.0, NAME_1 != "Galápagos")
plot(ecu.0)

# Seting the range
rng = range(c((-8300), (18969)))

# Fig. 6A  ----
f1 <- ggplot(data=ec, aes(x=long,
                          y=lat,
                          group=group))+
  geom_polygon(aes(x=long, y=lat, fill = balance)) +
  scale_fill_gradientn(colours = c("#d7301F", "white", "#4DA626"),
                       na.value = "grey60",
                       values = scales::rescale(c(-8300,-1944, 0, 1642, 18969)),
                       breaks = c(-8300,-1944, 0, 1642, 18969),
                       limits=c(floor(rng[1]), ceiling(rng[2]))) +
  geom_path(data=ec3, aes(x=long, y=lat,group=group),
            colour="grey80", size=0.05)+
  geom_path(data=ecu, aes(x=long, y=lat,group=group), #province limits
            colour="black", size=0.03)+
  geom_path(data=ecu.0, aes(x=long, y=lat,group=group), #ecuador limits
            colour="black", size=0.2)+
  xlim(-81,-75) +
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "Balance
degree
in - out
")+
  theme_void()+
  north(ec, symbol = 5) +
  ggsn::scalebar(ec, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, 
                 height = 0.01, border.size = 0.07)+
  theme(
    legend.key.height= unit(1.2, 'cm'),
    legend.key.width = unit(0.2, 'cm'))

    # axis.title.x = element_text(size = 8),
    # axis.title.y = element_text(size = 8),
    # axis.text = element_text(size = 8),
    # text = element_text(size = 8))



# Balance in_degree minus out_degree ----
# plot the cases over the balance 
setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização/")
v2 <- read_csv(file="casos_vigilancia-2014-2020-lat-long.csv")

v3 <- v2 %>%
  filter(ano>=2017 & ano < 2020)%>%
  filter(caso == 1)

# Calculate the limits of balance (2%)
quantile(data$balance, probs = c(0, 0.02, 0.98, 1), na.rm = TRUE)

#       0%       2%      98%     100% 
#   -8295.00 -1944.08  1642.96 18969.00 

# How many cases by year
v2 %>%
  filter(ano>=2017 & ano < 2020)%>%
  filter(caso == 1)%>%
  group_by(ano)%>%
  summarise(n())
# 1  2017    39
# 2  2018    60
# 3  2019    35
39+60+35
# Ploting the cases
ggplot()+
  geom_point(data=v3, aes(y=lati, x=long),
             size=1, shape=21, fill="red", color="black")

# Face plot of the cases by year
ggplot()+
  geom_point(data=v3, aes(y=lati, 
                          x=long, 
                          color=factor(ano)),
             size=1, 
             color="black", 
             fill="red", 
             shape=21) +
  facet_grid(ano~.)

# Fig. Balance ----
# Seting the range
rng = range(c((-8300), (18969)))

# Ploting the graphic
ggplot()+
  geom_polygon(data= ec, aes(x=long, 
                             y=lat, 
                             fill = balance, 
                             group=group)) +
  scale_fill_gradientn(colours = c("#d7301F", "white", "#4DA626"),
                       na.value = "white",
                       values = scales::rescale(c(-8300,-1944, 0, 1642, 18969)),
                       breaks = c(-8300,-1944, 0, 1642, 18969),
                       limits=c(floor(rng[1]), 
                                ceiling(rng[2]))) +
  labs(fill = "Balance 
degree
in - out
") +
  theme(legend.key.height= unit(0.75, 'cm'))+
  # geom_point(data=v3, aes(y=lati,
  #                         x=long,
  #                         fill="CSF"),
  #            size=0.4,
  #            colour="black",
  #            shape=21,
  #            fill="pink",
  #            alpha=0.9) +
  facet_grid(cols=vars(ano))+
  xlim(-80.9,-75) +
  coord_fixed()+
  labs(x="Longitude", y="Latitude")

# Testing map -- I do it to copy the CSF legend box in GIMP
f3 <- ggplot()+
  geom_polygon(data= ec, aes(x=long, 
                             y=lat, 
                             fill = balance, 
                             group=group)) +
  scale_fill_gradientn(colours = c("#d7301F", "white", "#4DA626"),
                       na.value = "white",
                       values = scales::rescale(c(-8300,-1944, 0, 1642, 18969)),
                       breaks = c(-8300,-1944, 0, 1642, 18969),
                       limits=c(floor(rng[1]), 
                                ceiling(rng[2]))) +
  labs(fill = "teste para completar") +
  theme(legend.key.height= unit(0.75, 'cm'))+
  geom_point(data=v3, aes(y=lati,
                          x=long,
                          size=NA,
                          colour="CSF",
                          shape="NA"))


tiff(filename = "Fig_6_to_CSF_etiquete_NA.tiff",
     width=9, height=9, units="cm", res = 1000, 
     pointsize = 12, compression = "lzw")
ggarrange(f3)
dev.off()



# Plot degree map with outbreaks ----
# Download map of borders
library(ggsn); library(raster)
# Province limits
ecu <- getData('GADM', country='ECU', level=1)
ecu <- subset(ecu, NAME_1 != "Galápagos")
plot(ecu)

# Ecuador limits
ecu.0 <- getData('GADM', country='ECU', level=0)
ecu.0 <- subset(ecu.0, NAME_1 != "Galápagos")
plot(ecu.0)


quantile(data$all_degree, probs = c(0, 0.2,0.4,0.6,0.8,0.9, 0.95, 0.98,1), na.rm = TRUE)

# 0%      20%      40%      60% 
#   1.0     20.0     80.0    224.4 
# 80%      90%      95%     100% 
#   776.0   2091.8   5539.8 136405.0 

library(scales)
library(ggsn)
library(ggpubr)

# Fig. 6B Study area, movements and cases ----
f2 <- ggplot()+
  geom_polygon(data= ec, aes(x=long, 
                             y=lat, 
                             fill = all_degree, 
                             group=group)) +
  scale_fill_gradientn(colours = c("white", "#A65628"),
                       na.value = "grey60",
                       values = scales::rescale(c(500,5000,50000,100000,136405)),
                       breaks = c(500,5000,50000,100000,136405))+
  geom_path(data=ec3, aes(x=long, y=lat,group=group),
            colour="grey80", size=0.05)+
  geom_path(data=ecu, aes(x=long, y=lat,group=group), #province limits
            colour="black", size=0.03)+
  geom_path(data=ecu.0, aes(x=long, y=lat,group=group), #ecuador limits
            colour="black", size=0.2)+
  geom_point(data=v3, aes(y=lati,
                          x=long,
                          fill="CSF"),
             size=0.15,
             colour="red",
             shape=21,
             fill="red",
             alpha=0.7) +
  xlim(-81,-75) +
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "All
Degree
in+out
")+
  theme_void()+
  north(ec, symbol = 5) +
  ggsn::scalebar(ec, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, 
                 height = 0.01, border.size = 0.07)+
  theme(
    legend.key.height= unit(1.2, 'cm'),
    legend.key.width = unit(0.2, 'cm'))


# Fig. 6AB ----
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")

tiff(filename = "Fig_6_21.tiff",
     width=18, height=9, units="cm", res = 1000, 
     pointsize = 12, compression = "lzw")
ggarrange(f1,f2)
dev.off()


#_--------------------------------------------------
# 3.4 Community Analisis ----
--------------------------------------------------
  # Read the m2 file ----
# Ready with markets in order ----
#final arquive for network
library(epinemo)
library(igraph)
library(dplyr)
# 1 Reading m2----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]

# 2 Creating network of premisses ----
m2 <- m2[m2$operacion.destino != "Faenador",]
# 744787 movements
#751003

m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
#6389239
#6407392

m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(codigo.sitio.origen),
                                   (codigo.sitio.destino)))) 

# Premises
# Confering the same number of premises to create de network
# Year  Premises      New
# 1 2017     66543    
# 2 2018     81994    
# 3 2019    101153    

# New codigo, sitio
# 1 2017    157814
# 2 2018    214972
# 3 2019    273701

#Without movements to slaughterhouses
# Year  Premises      New
# 1 2017     59293    
# 2 2018     74778    
# 3 2019     93192    

# 3 Creting anual networks ----
net <- m2[m2$ano == "2017",]

net <- data.frame(net)
banco <- createUniqueIds(net,
                         from = "sitio.origen2",
                         to= "sitio.destino2")

nodes.da.rede <- banco$correspondence$network.id
grafo <- simplify(graph_from_data_frame(banco$movements[, c("From", "To")], vertices = nodes.da.rede)) 
ecount(grafo) #edges   97928 
vcount(grafo) #nodes  205272

V(grafo)$in_degree <- degree(grafo, mode = "in")
V(grafo)$out_degree <- degree(grafo, mode = "out")
V(grafo)$all_degree <- degree(grafo, mode = "all")
page_rank_igraph <-page.rank(grafo)
V(grafo)$pagerank <- page_rank_igraph$vector

banco$correspondence$page_rank <- V(grafo)$pagerank
banco$correspondence$in_degree <- V(grafo)$in_degree
banco$correspondence$out_degree <- V(grafo)$out_degree
banco$correspondence$all_degree <- V(grafo)$all_degree

summary(banco$correspondence$in_degree)
summary(banco$correspondence$out_degree)
summary(banco$correspondence$all_degree)
summary(banco$correspondence$page_rank)


# 3.4 Community analysis ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
rm(list=ls())
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")
m2 <- m2[,-1]

# 1 Creating network of premisses
m2 <- m2[m2$operacion.destino != "Faenador",]
# 744787 movements
# 751003 movements ok
com <- m2 %>% 
  #filter(ano == 2017) %>%
  #filter(ano == 2018) %>%
  #filter(ano == 2019) %>%
  group_by(numero.certificado, provincia.origen,
           canton.origen, 
           parroquia.origen, 
           origen=paste(provincia.origen, canton.origen, parroquia.origen),
           provincia.destino, canton.destino, 
           parroquia.destino, 
           destino=paste(provincia.destino, canton.destino, parroquia.destino),
           ano) %>%
  summarise(Freq=n(), cantidad=sum(as.numeric(cantidad)))

sum(com$Freq)
# 744787
# 751003

#2017 : 185824
#2018 : 247461
#2019 : 311702

# codigo sitio
# 1 2017    157814
# 2 2018    214972
# 3 2019    273701

# 2 Creating unique ID parroquia ----
origen <- data.frame(com[,2:5])
destino <- data.frame(com[,6:9])
colnames(destino) <- colnames(origen)

data <- rbind(origen, destino)

data <- data %>%
  group_by(provincia=provincia.origen, canton=canton.origen, 
           parroquia=parroquia.origen, origen) %>%
  summarise(cantidad=n())

vigi <- data %>%
  group_by(provincia, canton, parroquia, origen) %>%
  summarise(cantidad = sum(cantidad))

sum(vigi$cantidad , na.rm = TRUE) 
#1502006

#2017 371648
#2018 494522
#2019 623404

### 3 Mapa ---- 
library(rgdal)
library(gdata)
library(sp)

ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# mapa para vigilancia
library(glue)
f{
  # Mapa para vigilancia
  #Provincia
  #atualizado 27.01.2020 banco cadastro 2018-2019 com codigo de sitio
  # fazer colunas comparaveis
  ec3@data$provincia <- ec3@data$DPA_DESPRO
  vigi$p <- vigi$provincia
  
  ec3@data$provincia <- gsub("Ñ","N", ec3@data$provincia)
  ec3@data$provincia <- gsub("Ã","N", ec3@data$provincia)
  
  # Crio os comparaveis
  ec3@data$p <- trim(tolower(paste(ec3@data$provincia)))
  vigi$p <- trim(tolower(paste(vigi$p)))
  
  vigi$p <- gsub("á","a", vigi$p)
  vigi$p <- gsub("ú","u", vigi$p)
  vigi$p <- gsub("é","e", vigi$p)
  vigi$p <- gsub("í","i", vigi$p)
  vigi$p <- gsub("ó","o", vigi$p)
  vigi$p <- gsub("ñ","n", vigi$p)
  
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_p <- tolower(ec3@data$DPA_PROVIN[match(vigi$p, ec3@data$p)])
  
  sum(is.na(as.numeric(vigi$c_p)))
  vigi[is.na(as.numeric(vigi$c_p)), 5]
  # 0
  
  #Canton
  ec3@data$canton <- tolower(ec3@data$DPA_DESCAN)
  vigi$cant <- tolower(vigi$canton)
  
  vigi$cant <- gsub("á","a", vigi$cant)
  vigi$cant <- gsub("ú","u", vigi$cant)
  vigi$cant <- gsub("é","e", vigi$cant)
  vigi$cant <- gsub("í","i", vigi$cant)
  vigi$cant <- gsub("ó","o", vigi$cant)
  
  # Fazer mudancas considerando mapa padrao ouro
  ec3@data$canton <- gsub("ñ","n", ec3@data$canton)
  ec3@data$canton <- gsub("ð","n", ec3@data$canton)
  ec3@data$canton <- gsub("puebloviejo","pueblo viejo", ec3@data$canton)
  
  #tirando o parenteses
  vigi$cant <- trim(gsub("\\(.*","", vigi$cant))
  
  #mudanças para mudar vigi adaptando para ec3@data
  #vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  vigi$cant <- gsub("pelipeo","pelileo", vigi$cant)
  vigi$cant <- gsub("ñ","n", vigi$cant)
  vigi$cant <- gsub("francisco de orellana","orellana", vigi$cant)
  vigi$cant <- gsub("pelileo","san pedro de pelileo", vigi$cant)
  vigi$cant <- gsub("el empalme","empalme", vigi$cant)
  vigi$cant <- gsub("santiago de mendez","santiago", vigi$cant)
  vigi$cant <- gsub("urcuqui","san miguel de urcuqui", vigi$cant)
  vigi$cant <- gsub("marcelino mariduena", "crnel. marcelino mariduena", vigi$cant)
  vigi$cant <- gsub("yaguachi", "san jacinto de yaguachi", vigi$cant) #
  vigi$cant <- gsub("pueblobiejo","pueblo viejo", vigi$cant)
  vigi$cant <- gsub("macas","morona", vigi$cant)
  vigi$cant <- gsub("joya de los sachas","la joya de los sachas", vigi$cant) #
  vigi$cant <- gsub("puyo","pastaza", vigi$cant)
  vigi$cant <- gsub("pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("santiago de santiago de pillaro","santiago de pillaro", vigi$cant)
  vigi$cant <- gsub("rio verde","rioverde", vigi$cant)
  vigi$cant <- gsub("general antonio elizalde","gnral. antonio elizalde", vigi$cant)
  vigi$cant <- gsub("arosemena tola","carlos julio arosemena tola", vigi$cant)
  #vigi$cant <- gsub("banos","banos de agua santa", vigi$cant)
  
  #crio coluna conjunta para comparar
  ec3@data$c <- trim(tolower(paste(ec3@data$provincia,ec3@data$canton)))
  vigi$c <- trim(tolower(paste(vigi$p, vigi$cant)))
  
  #caso especial la concordia cambiandole de provincia
  vigi$c <- gsub("santo domingo de los tsachilas la concordia","esmeraldas la concordia", vigi$c)
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_c <- ec3@data$DPA_CANTON[match(vigi$c, ec3@data$c)]
  
  #numero de catones sem id
  sum(is.na(as.numeric(vigi$c_c)))
  # 0
  
  #cuenta, numero e ordem deles
  sum(is.na(as.numeric(vigi$c_c)))
  vigi[is.na(as.numeric(vigi$c_c)), 8]
  
  cant <-vigi[is.na(as.numeric(vigi$c_c)), 8]
  cant
  
  #Parroquia
  
  #Criacao e transferencia dos valores a novas colunas para comparacao
  ec3@data$parroquia <- ec3@data$DPA_DESPAR
  vigi$par <- tolower(vigi$parroquia)
  
  #Modificando novas colunas por dados comparaveis
  ec3@data$parroquia <- gsub("á","a", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("é","e", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("í","i", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ó","o", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("ú","u", ec3@data$parroquia)
  ec3@data$parroquia <- gsub("Ñ","N", ec3@data$parroquia)
  
  #tirando o parenteses
  ec3@data$parroquia <- trim(gsub("\\(.*","", ec3@data$parroquia))
  vigi$par <- trim(gsub("\\(.*","", vigi$par))
  
  #ec3@data$parroquia <- gsub("ALFREDO BAQUERIZO MORENO (JUJAN)","JUJAN", ec3@data$parroquia)
  vigi$par <- gsub("á","a", vigi$par)
  vigi$par <- gsub("é","e", vigi$par)
  vigi$par <- gsub("í","i", vigi$par)
  vigi$par <- gsub("ó","o", vigi$par)
  vigi$par <- gsub("ú","u", vigi$par)
  vigi$par <- gsub("ñ","n", vigi$par)
  vigi$par <- gsub("ü","u", vigi$par)
  vigi$par <- gsub("crnl.","crnel.", vigi$par)
  
  vigi$par <- gsub("holgupin","holguin", vigi$par)
  vigi$par <- gsub("conrdoncillo","cordoncillo", vigi$par)
  vigi$par <- gsub("curticapa","curtincapa", vigi$par)
  vigi$par <- gsub("general leonidas plaza gutierrez", "gral. leonidas plaza gutierrez",vigi$par)
  #vigi$par <- gsub("guayusa", "san jose de guayusa",vigi$par) #
  #vigi$par <- gsub("puerto francisco de orel", "puerto francisco de orellana",vigi$par) #
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san jose de alluriquin", "alluriquin",vigi$par)
  vigi$par <- gsub("santo domingo", "santo domingo de los colorados",vigi$par)
  vigi$par <- gsub("santo domingo de los colorados de onzole", "santo domingo de onzole",vigi$par)
  #vigi$par <- gsub("guasaganda", "GUASAGANDA (CAB. EN GUASAGANDA CENTRO)",vigi$par)
  #vigi$par <- gsub("simon bolivar", "SIMON BOLIVAR (JULIO MORENO)",vigi$par)
  #vigi$par <- gsub("julio e. moreno", "JULIO E. MORENO (CATANAHUAN GRANDE)",vigi$par)
  #vigi$par <- gsub("san pablo", "SAN PABLO (SAN PABLO DE ATENAS)",vigi$par)
  vigi$par <- gsub("san luis de amenia", "san luis de armenia",vigi$par)
  vigi$par <- gsub("san lorenzo de jipijapa", "jipijapa",vigi$par)
  vigi$par <- gsub("santafe", "santa fe",vigi$par)
  vigi$par <- gsub("san jose de chazo", "san jose del chazo",vigi$par)
  vigi$par <- gsub("cibijies", "cubijies",vigi$par)
  vigi$par <- gsub("crnl. carlos concha torres", "crnel. carlos concha torres",vigi$par)
  vigi$par <- gsub("la lojas", "los lojas",vigi$par)
  vigi$par <- gsub("padre juan batista aguirre", "juan bautista aguirre",vigi$par)
  vigi$par <- gsub("velazco ibarra", "velasco ibarra",vigi$par)
  vigi$par <- gsub("gnral. antonio elizalde", "gral. antonio elizalde",vigi$par)
  vigi$par <- gsub("coronel marcelino mariduenas", "coronel marcelino mariduena",vigi$par)
  vigi$par <- gsub("tarida", "tarifa",vigi$par)
  vigi$par <- gsub("san francisco de natabuela", "san fco. de natabuela",vigi$par)
  vigi$par <- gsub("dr. miguel egas cabezas", "doctor miguel egas cabezas",vigi$par)
  vigi$par <- gsub("san francisco de sigsipamba", "san  fco. de sigsipamba",vigi$par)
  vigi$par <- gsub("chiquiribamba", "chuquiribamba",vigi$par)
  vigi$par <- gsub("bolsapamba", "bolaspamba",vigi$par)
  vigi$par <- gsub("santa susana de chiviaza", "sta susana de chiviaza",vigi$par)
  vigi$par <- gsub("pablo secto", "pablo sexto",vigi$par)
  vigi$par <- gsub("pumipamba", "rumipamba",vigi$par)
  vigi$par <- gsub("pani", "pano",vigi$par)
  vigi$par <- gsub("quinsamola", "quinsaloma",vigi$par)
  vigi$par <- gsub("pelileo grande", "pelileo",vigi$par)
  vigi$par <- gsub("jujan", "alfredo baquerizo moreno",vigi$par)
  vigi$par <- gsub("triunfo dorado", "triunfo-dorado",vigi$par)
  vigi$par <- gsub("chontaduro", "rioverde",vigi$par)
  vigi$par <- gsub("24 de mayo", "sucre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("general leonidas plaza g.", "gral. leonidas plaza gutierrez",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("3 de noviembre", "tres de noviembre",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("julio moreno", "santo domingo de los tsachilas",vigi$par) #no existe 24 de mayo
  vigi$par <- gsub("sta. cecilia", "santa cecilia",vigi$par) #
  #vigi$par <- gsub("el playon de san francis", "el playon de san francisco",vigi$par) #
  #vigi$par <- gsub("banos", "banos de agua santa",vigi$par) #
  vigi$par <- gsub("el guismi", "el guisme",vigi$par) #
  vigi$par <- gsub("yanzatza", "yantzaza",vigi$par) #
  vigi$par <- gsub("nobol", "narcisa de jesus",vigi$par) #
  vigi$par <- gsub("crnel.lorenzo de garaicoa", "crnel. lorenzo de garaicoa",vigi$par) #
  vigi$par <- gsub("general antonio elizalde", "gral. antonio elizalde",vigi$par) #
  
  
  # Criando novas colunas para comparacao
  ec3@data$pp <- trim(tolower(paste(ec3@data$c,ec3@data$parroquia)))
  vigi$pp <- trim(tolower(paste(vigi$c,vigi$par)))
  
  # Transferir dados do mapa cod prov para a base vig
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # Transferindo cantidad vigilancia para o data frame
  ec3@data$cantidad <- vigi$cantidad[match(ec3@data$pp, vigi$pp)]
  
  #numero de parroquias sem id
  sum(is.na(as.numeric(vigi$c_pp)))
  #227
  
  #cuenta, nombre e ordem
  sum(is.na(as.numeric(vigi$c_pp)))
  #229
  
  vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  
  #Transformando as parroquias urbanas atuais em parroquias anteriores a divisao
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca banos de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca cuenca de agua santa", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca bellavista", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el batan", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el sagrario", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca el vecino", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca gil ramirez davalos", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca hermano miguel", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca huaynacapac", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca machangara", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca monay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca octavio cordero", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca palacios", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san blas", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca san sebatian", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca sucre", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca totoracocha", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca yanuncay", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay cuenca canaribamba", "azuay cuenca cuenca",vigi$pp)
  vigi$pp <- gsub("azuay gualaceo daniel cordova", "azuay gualaceo gualaceo",vigi$pp)
  vigi$pp <- gsub("azuay sigsig jima", "azuay sigsig sigsig",vigi$pp)
  vigi$pp <- gsub("azuay ona ona", "azuay ona san felipe de ona",vigi$pp)
  
  vigi$pp <- gsub("bolivar guaranda guanujo", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chavez", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda angel polibio chaves", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabreil ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar guaranda gabriel ignacio veintimilla", "bolivar guaranda guaranda",vigi$pp)
  vigi$pp <- gsub("bolivar las naves las mercedes", "bolivar las naves las naves",vigi$pp)
  
  vigi$pp <- gsub("canar azogues aurelio bayas martinez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues azoguez", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues borrero", "canar azogues azogues",vigi$pp)
  vigi$pp <- gsub("canar azogues san francisco", "canar azogues azogues",vigi$pp)
  
  vigi$pp <- gsub("carchi espejo 27 de septiembre", "carchi espejo el angel",vigi$pp)
  vigi$pp <- gsub("carchi montufar gonzalez suarez", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi montufar san jose", "carchi montufar san gabriel",vigi$pp)
  vigi$pp <- gsub("carchi tulcan gonzalez suarez", "carchi tulcan tulcan",vigi$pp)
  
  vigi$pp <- gsub("chimborazo colta cajabamba", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo colta sicalpa", "chimborazo colta villa la union",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano la matriz", "chimborazo guano guano",vigi$pp)
  vigi$pp <- gsub("chimborazo guano el rosario", "chimborazo guano guano",vigi$pp)
  
  vigi$pp <- gsub("chimborazo riobamba lizarzaburu", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba maldonado", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba velasco", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba veloz", "chimborazo riobamba riobamba",vigi$pp)
  vigi$pp <- gsub("chimborazo riobamba yaruquies", "chimborazo riobamba riobamba",vigi$pp)
  
  vigi$pp <- gsub("cotopaxi la mana el carmen", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga el triunfo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga eloy alfaro", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga ignacio flores", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga juan montalvo", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga la matriz", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi latacunga san buenaventura", "cotopaxi latacunga latacunga",vigi$pp)
  vigi$pp <- gsub("cotopaxi la mana el triunfo", "cotopaxi la mana la mana",vigi$pp)
  
  vigi$pp <- gsub("el oro huaquillas ecuador", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas el paraiso", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas milton reyes", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas union lojana", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro huaquillas hualtaco", "el oro huaquillas huaquillas",vigi$pp)
  vigi$pp <- gsub("el oro las lajas valle hermoso", "el oro las lajas la victoria",vigi$pp)
  vigi$pp <- gsub("el oro machala el cambio", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala la providencia", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala nueve de mayo", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro machala puerto bolivar", "el oro machala machala",vigi$pp)
  vigi$pp <- gsub("el oro pasaje bolivar", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje loma de franco", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje ochoa leon", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pasaje tres cerritos", "el oro pasaje pasaje",vigi$pp)
  vigi$pp <- gsub("el oro pinas la matriz", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas la susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas susaya", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro pinas pinas grande", "el oro pinas pinas",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa jumon", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa nuevo santa rosa", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("el oro santa rosa puerto jeli", "el oro santa rosa santa rosa",vigi$pp)
  vigi$pp <- gsub("esmeraldas eloy alfaro esmeraldas norte", "esmeraldas eloy alfaro santa lucia de las penas",vigi$pp)
  
  vigi$pp <- gsub("esmeraldas esmeraldas luis tello", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas simon torres", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas 5 de agosto", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas esmeraldas bartolome ruiz", "esmeraldas esmeraldas esmeraldas",vigi$pp)
  vigi$pp <- gsub("esmeraldas la concordia las villegas", "esmeraldas la concordia la villegas",vigi$pp)
  
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule la uaurora", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule padre juan bautista aguirre", "guayas daule daule",vigi$pp) #
  vigi$pp <- gsub("guayas duran el recreo", "guayas duran eloy alfaro",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas daule banife", "guayas daule daule",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil pascuales", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil chongon", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil bolivar", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ayacucho", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil carbo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil febres cordero", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil garcia moreno", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil letamendi", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil nueve de octubre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil olmedo", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil rocafuerte", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil sucre", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil tarqui", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil urdaneta", "guayas guayaquil guayaquil",vigi$pp)
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas guayaquil ximena", "guayas guayaquil guayaquil",vigi$pp)
  
  vigi$pp <- gsub("guayas salitre bocana", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre central", "guayas salitre el salitre",vigi$pp)
  vigi$pp <- gsub("guayas salitre grnl. vernaza", "guayas salitre el salitre",vigi$pp)
  
  vigi$pp <- gsub("imbabura antonio ante andrade marin", "imbabura antonio ante atuntaqui",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi sagrario",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra caranqui", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra guayaquil de alpachaca", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra la dolorosa del priorato", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra san francisco", "imbabura ibarra san miguel de ibarra",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo jordan", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura otavalo san luis", "imbabura otavalo otavalo",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi sagrario", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura cotacachi san francisco", "imbabura cotacachi cotacachi",vigi$pp)
  vigi$pp <- gsub("imbabura ibarra sagrario", "imbabura ibarra san miguel de ibarra",vigi$pp)
  
  vigi$pp <- gsub("loja calvas chile", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja calvas san vicente", "loja calvas cariamanga",vigi$pp)
  vigi$pp <- gsub("loja catamayo san jose", "loja catamayo catamayo",vigi$pp)
  vigi$pp <- gsub("loja loja el sagrario", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja san sebastian", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja sucre", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja macara general eloy alfaro", "loja macara macara",vigi$pp)
  vigi$pp <- gsub("loja paltas lourdes", "loja paltas catacocha",vigi$pp)
  vigi$pp <- gsub("loja loja valle", "loja loja loja",vigi$pp)
  vigi$pp <- gsub("loja loja santiago \"san salvador o james\"", "loja loja santiago",vigi$pp)
  
  
  vigi$pp <- gsub("los rios babahoyo clemente baquerizo", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo barreiro", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo dr. camilo ponce", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios babahoyo el salto", "los rios babahoyo babahoyo",vigi$pp)
  vigi$pp <- gsub("los rios buena fe 7 de agosto", "los rios buena fe san jacinto de buena fe",vigi$pp)
  vigi$pp <- gsub("los rios pueblo viejo san juan de iluman", "los rios pueblo viejo puebloviejo",vigi$pp)
  vigi$pp <- gsub("los rios ventanas quinsaloma", "los rios quinsaloma quinsaloma",vigi$pp)
  vigi$pp <- gsub("los rios valencia la esperanza", "los rios valencia valencia",vigi$pp)# no existe parroquia
  vigi$pp <- gsub("los rios valencia la union", "los rios valencia valencia",vigi$pp)#
  vigi$pp <- gsub("los rios valencia vergel", "los rios valencia valencia",vigi$pp)#
  
  vigi$pp <- gsub("los rios quevedo 24 de mayo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo guayacan", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo nicolas infante diaz", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san camilo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo san cristobal", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo sucre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo siete de octubre", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo venus del rio quevedo", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  vigi$pp <- gsub("los rios quevedo viva alfaro", "los rios quevedo quevedo",vigi$pp)
  
  vigi$pp <- gsub("manabi chone santa rita", "manabi chone chone",vigi$pp)
  vigi$pp <- gsub("manabi el carmen 4 de diciembre", "manabi el carmen el carmen",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa dr. miguel moran lucio", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi jipijapa manuel inocencio parrales y guale", "manabi jipijapa jipijapa",vigi$pp)
  vigi$pp <- gsub("manabi manta eloy alfaro", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta tarqui", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta los esteros", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi manta san mateo", "manabi manta manta",vigi$pp)
  vigi$pp <- gsub("manabi montecristi el colorado", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi general eloy alfaro", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi leonidas proano", "manabi montecristi montecristi",vigi$pp)
  vigi$pp <- gsub("manabi montecristi anibal san andres", "manabi montecristi montecristi",vigi$pp) #agregada 14.02.2020 Network_Description.R
  
  vigi$pp <- gsub("manabi portoviejo 12 de marzo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo 18 de octubre", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo andres de vera", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo colon", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo francisco pacheco", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo picoaza", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo san pablo", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi portoviejo simon bolivar", "manabi portoviejo portoviejo",vigi$pp)
  vigi$pp <- gsub("manabi santa ana lodana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi santa ana santa ana de vuelta larga de vuelta larga", "manabi santa ana santa ana de vuelta larga",vigi$pp)
  vigi$pp <- gsub("manabi sucre leonidas plaza gutierrez", "manabi sucre bahia de caraquez",vigi$pp)
  
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago gualaquiza mercedes molina", "morona santiago gualaquiza gualaquiza",vigi$pp)
  vigi$pp <- gsub("morona santiago tiwintza puyo", "morona santiago tiwintza santiago",vigi$pp)
  
  vigi$pp <- gsub("pichincha cayambe ayora", "pichincha cayambe san jose de ayora",vigi$pp)
  vigi$pp <- gsub("pichincha cayambe juan montalvo", "pichincha cayambe cayambe",vigi$pp)
  vigi$pp <- gsub("pichincha quito belisario quevedo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito benalcazar", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito carcelen", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito centro historico", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cotocollao", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chaupicruz", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chilibulo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chillogallo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito chimbacalle", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito cochapamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito comite del pueblo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito guamani", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito el condado", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito eloy alfaro", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito inaquito", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito itchimbia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la concepcion", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ecuatoriana", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la ferroviaria", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la floresta", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la libertad", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la mena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito mariscal sucre", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito ponceano", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito puengasi", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito quitumbe", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito rumipamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san antonio de minas", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san bartolo", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san isidro del inca", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san juan", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito san roque", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito solanda", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito turubamba", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito jipijapa", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito kennedy", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la argelia", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha quito la magdalena", "pichincha quito quito",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san pedro de taboada", "pichincha ruminahui sangolqui",vigi$pp)
  vigi$pp <- gsub("pichincha ruminahui san rafael", "pichincha ruminahui sangolqui",vigi$pp)
  
  vigi$pp <- gsub("santa elena salinas santa rosa", "santa elena salinas salinas",vigi$pp)
  vigi$pp <- gsub("santa elena santa elena ballenita", "santa elena santa elena santa elena",vigi$pp)
  
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chiguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo las mercedes", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo abraham calazacon", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo chimguilpe", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio toachi", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo rio verde", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo zaracay", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo bomboli", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los tsachilas", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo santo domingo de los colorados de los colorados", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  vigi$pp <- gsub("santo domingo de los tsachilas santo domingo nuevo isrrael", "santo domingo de los tsachilas santo domingo santo domingo de los colorados",vigi$pp)
  
  vigi$pp <- gsub("sucumbios lago agrio santa cruz", "sucumbios lago agrio santa cecilia",vigi$pp)
  
  vigi$pp <- gsub("tungurahua ambato atocha - ficoa", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato celiano monge", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi chico", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato huachi loreto", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la merced", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato la peninsula", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato matriz", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato pishilata", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san bartolo de pinllog", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua ambato san francisco", "tungurahua ambato ambato",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pillaro ciudad nueva", "tungurahua santiago de pillaro pillaro",vigi$pp)
  vigi$pp <- gsub("tungurahua santiago de pelileo pelileo grande", "tungurahua santiago de pelileo pelileo",vigi$pp)
  
  vigi$pp <- gsub("zamora chinchipe zamora el limon", "zamora chinchipe zamora zamora",vigi$pp)
  
  #transferencia de codigo do mapa os que forem mach da pro-can-parr
  vigi$c_pp <- ec3@data$DPA_PARROQ[match(vigi$pp, ec3@data$pp)]
  
  # agregando os valores que foram transformados de parroquias urbanas a normais
  vigi2 <- vigi %>%
    group_by(pp) %>%
    summarise(cantidad = sum(cantidad))
  
  #Tranferindo valores agregados
  ec3@data$cantidad <- vigi2$cantidad[match(ec3@data$pp, vigi2$pp)]
  
  # Comparando numeros
  sum(vigi$cantidad, na.rm = TRUE)
  sum(vigi2$cantidad, na.rm = TRUE) -
    sum(ec3@data$cantidad, na.rm = TRUE)
  
  ####ANTERIOR AINDA NAO DELETAR
  sum(is.na(as.numeric(vigi$c_pp)))
  #157
  
  par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
  par
  
  # Animais faltantes no mapa
  sum(vigi$cantidad) - sum(ec3@data$cantidad, na.rm = TRUE)
  
}

# Pasando os codigos de stio origen destino para o banco
com$origin_cod <- vigi$c_pp[match(com$origen, vigi$origen)]
com$destiny_cod <- vigi$c_pp[match(com$destino, vigi$origen)]

# Contagem de quantos ficaram no final
length(unique(com$origin_cod)) #906
length(unique(com$destiny_cod)) #910

a <- c(com$origin_cod,com$destiny_cod)
length(unique(a)) #943  #945

# 2017: 772 e 738
# 2018: 804 e 828
# 2019: 825 e 856

library(epinemo)
library(igraph)
com <- data.frame(com)
banco <- createUniqueIds(com,
                         from = "origin_cod",
                         to= "destiny_cod")

# 4 Analise de comunidade ----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
library(Matrix)
matriz <- sparseMatrix(i=banco$movements$From,j=banco$movements$To, x= banco$movements$Freq,
                       dims = rep(max(banco$correspondence$network.id), 2))

# Page rank e google matriz
pr <- pageRank(matriz)
G <- GoogleMatrix(matriz)

L <- LinkRank(G, pr)
qlrM <- LinkRankModMatrix(L, pr)

# 5 Simulated Annealing ----
#mudando parametros de simulated annealing, temperatura e resfriamento, etc
#fazendo uma particao inicial rapida, para utilizar de inicio na proxima
#Particao inicial de avaliazao
particao <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, cool = 0.95, max_itry = 10*dim(qlrM)[1])
length(unique(particao)) #70

particao <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, cool = 0.80, max_itry = 10*dim(qlrM)[1])

length(unique(particao)) #54

#6 avaliando modularidade (qualidade da divisao) ----
LinkRankMod(L = L, pr = pr, c = particao) #0.344  #0.1619
plot(particao)

# 7 Locking 5 comunities  ----
set.seed(5)
N <- 5
no_nos <- NROW(unique(banco$correspondence$database.id))
inicial <- sample.int(n = N, size = no_nos, replace = TRUE)
particao1 <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, 
                                      cool = 0.95, max_itry = 10*dim(qlrM)[1],
                                      c = inicial)

particao2 <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, 
                                      cool = 0.99, max_itry = 10*dim(qlrM)[1],
                                      c = inicial)

particao1 <- particao2

length(unique(particao1)) #5
LinkRankMod(L = L, pr = pr, c = particao1) #0.3376
plot(particao)

# Temperatura = 1.71e-07 
# Rejeições = 9450 
# Trocas por calor = 1 
# 60% concluído (Faltam 144 ciclos) 
# Estou rodando há 0.034 horas ( 2 minutos) 
# Acho que faltam 0.032 horas ( 2 minutos) 
# 
# Temperatura = 2e-07 
# Rejeições consecutivas = 9450 
# Trocas por calor consecutivas = 0 

#2017-2019: 0.3376
# 5: 0.3502
# 6: 0.371
# 7: 0.362

# 2017: 5, 0.333
# 2018: 5, 0.3434
# 2019: 5 0.353

#2017: 6, 0.3412
#2018: 6, 0.355
#2019: 6, 0.378

# New Teste 15/07/2020
2017-2019: 0.3376

# 8 Taking partition names to banco ----
banco$correspondence$com <- particao1
length(unique(banco$correspondence$com))
banco$correspondence$pr <- pr #pasando o Pagerank
# write.csv(banco$correspondence, file="banco_correspondence.csv")

# Paso comunidades para shape 
ec3@data$com <- banco$correspondence$com[match(ec3@data$DPA_PARROQ, banco$correspondence$database.id)]

table(ec3@data$com)
108+269+164+212+192
# write communities in file 
write.csv(ec3@data, file="Comunidades_ec3.csv")

# eliminando pastaza araujo curarai
ec3@data$com[ec3@data$DPA_PARROQ == "160451"] <- NA
# eliminando Orellana aguarico tiputini
ec3@data$com[ec3@data$DPA_PARROQ == "220254"] <- NA
ec3@data$com[ec3@data$DPA_PARROQ == "210751"] <- NA
#isla puna
ec3@data$com[ec3@data$DPA_PARROQ == "090157"] <- NA


# 9 Reading data and maps to plot communities----
library(dplyr);library(readr);library(rgdal)
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
com <- read_csv(file="Comunidades_ec3.csv")

# eliminando pastaza araujo curarai
com$com[com$DPA_PARROQ == "160451"] <- NA
# eliminando Orellana aguarico tiputini
com$com[com$DPA_PARROQ == "220254"] <- NA
com$com[com$DPA_PARROQ == "210751"] <- NA
#isla puna
com$com[com$DPA_PARROQ == "090157"] <- NA


# Reading map 
ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ec3@data$com <- com$com[match(ec3@data$DPA_PARROQ, com$DPA_PARROQ)]
table(ec3@data$com)

library(leaflet)
# 9 Plotting community map ----
# mypal <- colorFactor(palette = "Set2", na.color = "#ffffff", domain = ec3@data$com)
#Colours for 6 communities
#mypal <- colorFactor(c("#66C2A5","#FC8D62","#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"), na.color = "#ffffff", domain = ec3@data$com)
# #Colours for 5 communities
mypal <- colorFactor(c("#66C2A5","#FC8D62","#FFD92F", "#B3B3B3", "#A6D854"), na.color = "#ffffff", domain = ec3@data$com)

map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #58 Apertura ambiente grafico html
  #addProviderTiles(                                                                                                                                                                                                                                                                                                                                                                                                    "Esri.NatGeoWorldMap") %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  #addProviderTiles("OpenStreetMap.Mapnik") %>%#Apertura open maps
  setView(lat = -1.7, lng = -78.5, zoom = 7) %>% #Localiza o mapa
  addPolygons(data = ec3, stroke = FALSE, smoothFactor = 0.7, fillOpacity = 0.7, #Agrega poligonos e define opacidades
              fillColor = ~ mypal(ec3@data$com), #Prenche cores do shape de acordo a quantidade
              popup = paste("Terr: ", ec3@data$pp, "<br>", #Pop-up dados de territorio
                            "Comunidad: ", ec3@data$com, "<br>")) %>%
  addScaleBar(position = c("bottomright"), 
              options = scaleBarOptions(maxWidth = 200, metric = TRUE, 
                                        imperial = FALSE, updateWhenIdle = TRUE)) %>%
  leaflet::addLegend(position = "bottomright", pal = mypal, values = ec3@data$com,
                     title = "<p>Comunidad</p>", opacity = 0.99)# Lenda e opacidade
map

# 10 Mapa ggplot----
library(ggplot2)
library(ggsn)
ec3@data$id <- rownames(ec3@data)
ec <- fortify(ec3)
ec$com <- ec3@data$com[match(ec$id, ec3@data$id)]

# Mapa comunidades 6 final
cvalues <- c("#66C2A5","#FC8D62","#FFD92F", "#B3B3B3", "#A6D854")

# Download map of borders
library(ggsn); library(raster)
# Province limits
ecu <- getData('GADM', country='ECU', level=1)
ecu <- subset(ecu, NAME_1 != "Galápagos")
plot(ecu)

# Ecuador limits
ecu.0 <- getData('GADM', country='ECU', level=0)
ecu.0 <- subset(ecu.0, NAME_1 != "Galápagos")
plot(ecu.0)

# Quito limits
uio<-rgdal::readOGR(dsn="~/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/",layer="quito")
uio <- spTransform(uio, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# GYE limits
gye<-rgdal::readOGR(dsn="~/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/",layer="guayaquil")
gye <- spTransform(gye, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))




# Fig. 7A Communities ---- 
fig7a <- ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill = as.factor(com)), data = ec)+
  geom_path(data=ec3,aes(x=long, y=lat,group=group), colour="grey60", size=0.03)+
  geom_path(data=ecu, aes(x=long, y=lat,group=group), #province limits
            colour="black", size=0.03)+
  geom_path(data=ecu.0, aes(x=long, y=lat,group=group), #ecuador limits
            colour="black", size=0.2)+
  geom_polygon(data=uio, aes(x=long, y=lat,group=group), #ecuador limits
            colour="red", size=0.1, alpha=0.2, colour="white")+
  geom_polygon(data=gye, aes(x=long, y=lat,group=group), #ecuador limits
               colour="red", size=0.1, alpha=0.2, colour="white")+
  scale_fill_manual(values = cvalues, na.value= "white")+
  xlim(-81,-75) +
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  labs(fill = "Com")+
  theme_void()+
  north(ec, symbol = 5) +
  ggsn::scalebar(ec, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, 
                 height = 0.01, border.size = 0.07)+
  theme(
    legend.key.height= unit(0.9, 'cm'),
    legend.key.width = unit(0.2, 'cm'))+
  geom_text() +
  annotate("text", label="Quito", x=-78.75, y=-0.07, size=2.2)+
  annotate("text", label="Guayaquil", x=-80.40, y=-2.1, size=2)



tiff(filename = "Figure_7a_UIO_2.tiff",
     width=9, height=9, units="cm", res = 1000, 
     pointsize = 12, compression = "lzw")

fig7a
dev.off()



# 11 ChordDiagram ----
# install.packages("circlize")
library(circlize)
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
com <- read_csv(file = "banco.movements.2017-2019.comu.centroids.csv")

chord <- com %>%
  group_by(From, To)%>%
  summarise(Mov=(round(sum(Freq/1000),1)))

sum(chord$Mov)
# 2017:185824
# 2018: 247261
# 2019 : 311702
# 2017: 2019: 744787

chord %>%
  group_by(From)%>%
  summarise(cant=sum(Mov))

chord %>%
  group_by(To)%>%
  summarise(cant=sum(Mov))

#Change environmental cex with par, to make bigger the letters
# Save with 1800 x 1800
par(cex=1.5)

tiff(filename = "Figure_7b.tiff",
     width=9, height=9, units="cm", res = 1000, 
     pointsize = 12, compression = "lzw")

  chordDiagram(x=chord, 
                   # reduce= 1000,
                   transparency = 0.2,
                   grid.col = c("#66C2A5","#FC8D62","#FFD92F", "#B3B3B3", "#A6D854"),
                   annotationTrack = c("grid", "axis"),
                  annotationTrackHeight = mm_h(1))
dev.off()

#returning cext to normal
par(cex=1)

# It is necessary to join both images on EXTERNAL DEVICE

# write.csv(chord, file= "Comunidades.csv")



# 12 Analysis of origin and destiny----
# Reading the ec3data
ec3data %>%
  group_by(com) %>%
  summarise(n= n(), number=sum(as.numeric(cantidad))) %>%
  arrange(desc(n))

# 1 1       182
# 2 2       237
# 3 3       164
# 4 4       178
# 5 5       184
# 6 NA       95

# Communities, from and to 

banco$movements %>%
  group_by(from_com,to_com)%>%
  summarise(movements=sum(Freq))%>%
  ggplot( aes(x=to_com,y=movements)) +
  geom_bar( stat="identity", position = "dodge")+
  geom_text(aes(label=movements), size=3, vjust=-0.4)+
  facet_grid(from_com ~ .)

# Im gonna save the enviroment for community analysis
rm(chord, data, destino, G, L, m1, m2, matriz, net, origen, page_rank_igraph, par,  
   s_simplified, vigi, vigi2, a, inicial, kin, kout, ktotal, N, no_nos, particao, pr, nodes.da.rede)

# Saving a grafo with comunities to plot in gephy



#_--------------------------------------------------
# 3.7 Productivity analysis ----

# Production of meat calc ----
--------------------------------------------------
  library(tidyverse)
# 1 Familiar selfconsuption ----
# Importing cadastral information with codigo de sitio

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
c17 <- read.csv("cad2017.csv", colClasses = "character")
c17$ano <- "2017"
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2018")
c18 <- read.csv("cad2018.csv", colClasses = "character")
c18$ano <- "2018"
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2019")
c19 <- read_csv("cad2019.csv")
c19$ano <- "2019"

##
c19$Cantidad.activos <- as.numeric(c19$Cantidad.activos)
c19$Cantidad.inactivos <- as.numeric(c19$Cantidad.inactivos)

c19$Cantidad.activos[is.na(c19$Cantidad.activos)] <- 0
c19$Cantidad.inactivos[is.na(c19$Cantidad.inactivos)] <- 0

c19$cantidad <- c19$Cantidad.activos + c19$Cantidad.inactivos

sum(c19$cantidad)
c19$ano <- "2019"

##
c19$Cantidad.inactivos <- NULL
c19$Cantidad.activos <- NULL
colnames(c18)
colnames(c19)
colnames(c19) <- colnames(c18)

ca <- rbind(c17,c18,c19)

length(unique(ca$identificacion.propietario))
#259304
# 247190
sum(as.numeric(ca$cantidad))

# Organizing operacion
table(ca$tipo.operacion)

ca$tipo.operacion <- gsub("Expositor", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Reproductor de animales", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Vacunador oficial", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Incubador", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Investigador", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Distribuidor vacuna oficial", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Operador de vacunación", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Operador Traspatio-Comercial", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Importador", "Productor", ca$tipo.operacion)

ca$tipo.operacion <- gsub("Feria de Exposición Animal", "Feria de comercialización animal", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Evento Deportivo -Recreativo Animal", "Feria de comercialización animal", ca$tipo.operacion)

ca$tipo.operacion <- gsub("Comercializador Directo", "Comercializador", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Comercializador directo", "Comercializador", ca$tipo.operacion)

ca$tipo.operacion <- gsub("Cuarentena", "Productor", ca$tipo.operacion)
ca$tipo.operacion <- gsub("Industrializador", "Operador Industrial", ca$tipo.operacion)

unique(ca$tipo.operacion)

table(ca$tipo.operacion)


c4 <- ca %>%
  filter(tipo.operacion != "Faenador") %>%
  filter(tipo.operacion != "Feria de comercialización animal") %>%
  group_by(id_operador=identificacion.propietario, 
           nombre_operador=nombre.propietario,
           nombre_sitio=nombre.sitio,
           codigo.sitio=paste(id_operador,nombre_sitio),
           tipo.operacion,
           provincia, 
           canton, 
           parroquia,
           ano)%>%
  summarise(cantidad = sum(as.numeric(cantidad)))

table(c4$tipo.operacion)

sum(c4$cantidad, na.rm = TRUE)
<#3679026
  
  c4 %>%
  group_by(ano)%>%
  summarize(premises=length(unique(codigo.sitio)))
# number of premises 
1 2017    105542
2 2018    126168
3 2019    120296

library(epinemo)
m2 <- data.frame(m2)
sitios <- createUniqueIds(data = m2, from = 'identificacion.operador.origen',
                          to = 'identificacion.operador.destino')

#2 Procurando os sitios no cadastro e na movimentação ----
msitios <- sitios$correspondence$database.id

table(c4$id_operador %in% msitios)

# FALSE   TRUE 
# 150410 201709

FALSE   TRUE 
161783 190315 

c5 <- c4[!(c4$id_operador %in% msitios),]
#deleting indocumentados
c5 <- c5[c5$id_operador != 1768105720002,]
sum(c5$cantidad, na.rm = TRUE)
#652130
#752052

#3 Number of animals outside movement database ----
c5 %>%
  group_by(ano) %>%
  summarise(s=sum(cantidad, na.rm = TRUE))

1 2017  174945
2 2018  217632
3 2019  259553

1 2017  204144
2 2018  253854
3 2019  294054


#4 Number of premises outside movement database ----
c5 %>%
  group_by(ano) %>%
  summarise(s=length(unique(id_operador)))

1 2017  49412
2 2018  56071
3 2019  55589

#5  Number of animals inside the movement database ----
c6 <- c4[(c4$id_operador %in% msitios),]
#deleting indocumentados
c6 <- c6[c6$id_operador != 1768105720002,]
sum(c6$cantidad, na.rm = TRUE)
#1747299

#number of animals inside movement database
c6 %>%
  group_by(ano) %>%
  summarise(s=sum(cantidad, na.rm = TRUE))

# 1 2017  482529
# 2 2018  696976
# 3 2019  567794
c4 %>%
  group_by(ano) %>%
  summarise(s=sum(cantidad, na.rm = TRUE))


#6  Number of total animals that goes to slaugther ----
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))

# X2017  X2018  X2019
# 1 103052  69743  62646
# 2  65120  70149  91725
# 3 655342 774034 829010
# 4 232141 244774 299586

103052+69743+62646+
  65120+70149+91725+
  655342+774034+829010+
  232141+244774+299586

2017
103052+65120+655342+232141

2018
69743+70149+774034+244774

2019
62646+91725+829010+299586

#1165774

# Industrial
t2 %>%
  group_by(origen, destino)%>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752795-1165774 # 64.74 % from industrial 
(752795*125*0.805)
#75,749.99 TM year industrials - 64.74% of production from industrials

t2 %>%
  group_by(origen, destino)%>%
  filter(destino == "Faenador")%>%
  filter(origen != "Operador Industrial")%>%
  summarise(sum(cantidad))
78480+75665+258834


#Backyard
412979/1165774 # 35.42 % from backyard
(412979*97.5*0.805) #peso menor 97.5 Kg
#32,413,69 TM year traditional farming - 35.42

# Total Backyard and industrial
+32413.69+75749.997
=108163.7


#familiar consuption
#Considering 196.628 anually pigs on the cadastro 
# 2017-2019 that are do not mobilized

(196628*97.5*0.805)/1000
#15432,84 TM year traditional farming

#89016*1.7=151132
# (151132*97.5*0.805) = 118.619,73 MT from considering 1.7 more animals nonsense idelete
# number of animals that are consumed
# on the farm is 1.7 per farm per year 89016*1.7 = 151327 looking at the 
#cadastro we found a mean of 196628 animals per year that were catastrate but not movimented


#7 Total meat prodution ----
75756.63 + 32171.40 + 15432.84
123360.9

75756.63/123360.9
# 61.41% from industrials
(32171.40 + 15432.84)/123360.9
# 38.58% from backyard


#8 Per capita consuption----
75756.63+32171.40+15432.84+3870.03
127230900/17100000
## 7.44


# _----
# Analisis of production and densities in Latin america----
# 1 Loading DB ----
setwd("~/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
library(tidyverse)
fao <- read_csv(file="FAOSTAT.csv")
fao <- read_csv(file="FAOSTAT2.csv")
pop <- read_csv(file = "area_population.csv")
population <- read_csv(file="worldpopulation.csv")

# 2 pigs from official statistics ----
fao <- read_csv(file="FAOSTAT.csv")
fao <- read_csv(file="FAOSTAT2.csv")
pop <- read_csv(file = "area_population.csv")
population <- read_csv(file="worldpopulation.csv")

pig <- 
  fao %>%
  filter(Element == "Stocks") %>%
  group_by(Area, Year) %>%
  summarise(mean_pop=mean(Value)) %>%
  spread(key = "Year",
         value="mean_pop")
p <- 
  fao %>%
  filter(Element == "Stocks") %>%
  group_by(Area) %>%
  summarise(Mean_pigs=mean(Value))

pig$meanpigs <- p$Mean_pigs
pig$km <- pop$land_area[match(pig$Area, pop$Area)]
pig$population <- pop$Population[match(pig$Area, pop$Area)]

# 3 Calculate density of animals ----
pig$density <- pig$meanpigs/pig$km
pig$density2017 <- pig$`2017`/pig$km
pig$density2018 <- pig$`2018`/pig$km
pig$density2019 <- pig$`2019`/pig$km

library(stringr)
pig$Area <- word(pig$Area)

pig2 <- pivot_longer(pig, cols = c(density2017,
                                   density2018,
                                   density2019),
                     names_to = "values")


pig2$values <- gsub("density2017", "2017", pig2$values)
pig2$values <- gsub("density2018", "2018", pig2$values)
pig2$values <- gsub("density2019", "2019", pig2$values)

# Fig. 12.B Density of pigs in Latin America----
density <- ggplot(pig2, aes(Area, value, fill=values))+
  geom_col(position = "dodge")+
  theme_minimal()+
  labs(tag = "B")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_fill_brewer(palette = "Set2")+
  theme(text = element_text(size = 12),
        panel.grid.major.x = element_blank())+
  labs(x=NULL, y="Pigs/km2", fill=NULL)

# 4 population ----
pig2 <- pivot_longer(pig, cols = c(density2017,
                                   density2018,
                                   density2019,
                                   `2017`,
                                   `2018`,
                                   `2019`),
                     names_to = "values")

pig2$values <- gsub("density2017", "2017", pig2$values)
pig2$values <- gsub("density2018", "2018", pig2$values)
pig2$values <- gsub("density2019", "2019", pig2$values)

# Activating Scientific for graphics labels
options(scipen = 1)

pigs <- ggplot(pig2, aes(Area, value, fill=values))+
  geom_col(position = "dodge")+
  geom_hline(yintercept =2200000)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(tag = "A")+
  scale_fill_brewer(palette = "Set2")+
  theme(text = element_text(size = 12),
        legend.key.size = unit(2, "mm"),
        panel.grid.major.x = element_blank())+
  labs(x=NULL, y="Pigs stock", fill=NULL)+
  scale_y_sqrt()
pigs
library(ggpubr)


# Fig 12AB ----
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Exploratory trade network and community detection/")
tiff(filename = "Figure_12.tiff",
     width=14, height=7, units="cm", res = 1200, 
     pointsize = 12, compression = "lzw")

ggarrange(pigs, density, common.legend = TRUE, legend="bottom", ncol=2)

dev.off()

summary(pig$meanpigs[pig$Area != "Brazil"])
summary(pig$density2017)
summary(pig$density2018)
summary(pig$density2019)

write.csv(pig, file="pigs_latin_america.csv")
write.csv(pro, file="produc_latin_america.csv")

