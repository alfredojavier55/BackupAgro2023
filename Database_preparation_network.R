-------------------------------------------------
  # 1 Database Preparation m ----
--------------------------------------------------
install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                   '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())-------------------------------------------------
  # 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
# 1 Database Preparation m ----
--------------------------------------------------
  install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                 '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
  library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                     "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                      "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
  setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
  m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego-------------------------------------------------
  # 1 Database Preparation m ----
--------------------------------------------------
install.packages("devtools")
install.packages("install_github")
install_github(repo='leb-fmvz-usp/epinemo', auth_token =
                   '7055b1fb20190fb0411d0f730d81faccd5968f0e')
library(devtools)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
rm(list = ls())

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                      "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                     "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and catego

m16 <- read.csv("mov2016.csv", colClasses = "character")
m17 <- read.csv("mov2017.csv", colClasses = "character")
m18 <- read.csv("mov2018.csv", colClasses = "character")
m19 <- read.csv("mov2019.csv", colClasses = "character")

#temporal: ----------
m19$Código.Provincia.Origen <- NULL
m19$Código.Provincia.Destino <- NULL
colnames(m19) <- colnames(m16)

#temporal: ----------
m <- rbind(m16,m17,m18,m19)
rm(m16,m17,m18,m19)

# Number of pigs and csmi of the original dataset
m$cantidad <- as.numeric(m$cantidad)
sum(m$cantidad) #10952262
length(unique(m$numero.certificado)) #1230161

# Data excluded from the dataset ----
# 1 Anulados elimination
# Number of eliminated
length(unique(m$numero.certificado[m$estado == "anulado"]))
#11800 canceled

m <- m[(m$estado != "anulado"),]

# 2 indocumentados elimination
# number of eliminated
length(unique(m$numero.certificado[m$identificacion.operador.origen == 1768105720002]))
# 5665
length(unique(m$numero.certificado[m$identificacion.operador.destino == 1768105720002]))
# 3678

#eliminatin indocumentados
m <- m[(m$identificacion.operador.origen != 1768105720002),]
m <- m[(m$identificacion.operador.destino != 1768105720002),]

#Movements by year (premises) ----
m <- m %>%
  mutate(ano = substring(fecha.inicio.vigencia, 7, 10))

#Movements by month (premises)
m <- m%>%
  mutate(mes=substring(fecha.inicio.vigencia,4,5))

#deletin 2016 and 2020
length(unique(m$numero.certificado[m$ano == 2016]))
17799
length(unique(m$numero.certificado[m$ano == 2020]))
155

m <- m[m$ano != 2016, ]
m <- m[m$ano != 2020, ]

# Deleting clandestine slaugtherhouse
sum(length(m$numero.certificado[m$identificacion.operador.destino == 1791782062001]))
1494

m <- m[m$identificacion.operador.destino != 1791782062001, ]

sum(m$cantidad) # (10952262-9906746)/10952262 #9.54% Pigs
length(unique(m$numero.certificado)) # (1230161-1190035)/1230161 #3.3% CSMI


(10952262-9906746)/10952262
1045516 animals diminued
9.5%


(1230161-1190035)/1230161
1230161-1190035 csmi diminued
# 40126

#2016&2020
17799+155
#indocumentados
+5665+3678+1441(from duplicated CSMI because of probles with operations)
# Cancelled
+11800
#Camal clandestino
+1491

# I included the 1441 CSMI duplicated that wede deleted apos m2

17799+155+5665+3678+11800+1491+1441
# 40588
11800/42029 28% canceled
(5665+3678+1491+1441)/42029  29% cadastral
(17799+155)/42029 43% not corresponding to study period

sum(m2$cantidad)

# how many imported pigs?
m %>%
  group_by(sitio.origen, nombre.operador.origen,  ano)%>%
  filter(operacion.origen == "Cuarentena")%>%
  summarize(cerdos=sum(cantidad))



# --------------------------------------------------
# Reorganizing Operation ----
# --------------------------------------------------

table(m$operacion.origen)
unique(m$operacion.origen)

#m$operacion.origen <- gsub("Operador Industrial", "Productor", m$operacion.origen)
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
setwd("~/Dropbox/0.USP/2 Projeto graduação/FAPESP/Dados/vacinacao/2019")
auto <- read.csv("exttras4de4.xls0.csv", colClasses = "character")

auto$Número.Productos.Vacunados <- as.numeric(auto$Número.Productos.Vacunados)

auto_lis <- auto %>%
  group_by(Operador.de.Vacunación)%>%
  summarize(num=sum(as.numeric(Número.Productos.Vacunados)))

# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# write.csv(auto_lis, "autoservicio_lista.csv")

x <- unique(auto$Identificación.Propietario)

m <- m %>%
  mutate(operacion.origen2 = if_else(operacion.origen == "Productor" &
                                       identificacion.operador.origen %in% x,
                                     "Operador Industrial", operacion.origen))
m <- m %>%
  mutate(operacion.destino2 = if_else(operacion.destino == "Productor" &
                                        identificacion.operador.destino %in% x,
                                      "Operador Industrial", operacion.destino))
rm(auto)


# 2 Movement description ----
length(unique(m2$numero.certificado))
1190035

# aggregate without animal type
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
write.csv(m, file="mov2016_2020.csv")
m <- read.csv("mov2016_2020.csv", colClasses = "character")

#Aggregate ----
m2 <- m %>%
  group_by(numero.certificado, provincia.emision, operacion.origen, operacion.origen2, provincia.origen,
           canton.origen, parroquia.origen, sitio.origen, identificacion.operador.origen,
           razon.social.operador.origen, operacion.destino, operacion.destino2, provincia.destino, 
           canton.destino, parroquia.destino, sitio.destino, identificacion.operador.destino, 
           razon.social.operador.destino, fecha.inicio.vigencia, ano, mes) %>%
  summarise(cantidad = sum(as.numeric(cantidad)))


# Creating sitio de origen e destino
m2 <- m2 %>%
  mutate(sitio.origen2 = paste(identificacion.operador.origen, sitio.origen)) %>%
  mutate(sitio.destino2 = paste(identificacion.operador.destino, sitio.destino))

rm(m)


# Finding duplicates Removing CSMI duplicates ----
#Analizing duplicated
length(unique(m2$numero.certificado))
1190035

#table duplicated
table(duplicated(m2$numero.certificado))
#there s 1440 duplicates
duplicated <- m2[duplicated(m2$numero.certificado),]
write.csv(duplicated, file="duplicados.csv")

# how much animals in the duplicated
sum(as.numeric(m2$cantidad[m2$dup == TRUE]))
#8659 animals

dup <- m2 %>% 
  count(numero.certificado)

m2$dupl <- as.character(dup$n[m2$numero.certificado %in% dup$numero.certificado])

#Removing duplicated
m2 <- m2[!duplicated(m2$numero.certificado),]

--------------------------------------------------
  # Reading m2 file----
# fairs are not good
--------------------------------------------------
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
#write arquive to be quicker lines 1190035
write.csv(m2, file="mov2016_2020m2.csv")
m2 <- read.csv("mov2016_2020m2.csv", colClasses = "character")
--------------------------------------------------
  
library(dplyr)
library(ggplot2)
library(stringr)

# Descriptive movements ----
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) # 9898087
#Number of movements 1190035 Number of certificates
length(unique(m2$numero.certificado))

library(tidyverse)
# Total premises involved
m2 %>%
  group_by(Year=ano)%>%
  summarise(Propietarios=(length(unique(c(sitio.origen2, sitio.destino2))))) %>%
  spread(key="Year", value = "Propietarios")


#Source of movilization
m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     50     51     60
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47792  57098  65187

# Destiny of movilization
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20728  31306  44141

ferias <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.origen2, "FERIA"))%>%
  filter(!str_detect(sitio.origen2, "feria"))%>%
  filter(!str_detect(sitio.origen2, "Feria"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE "))%>%
  filter(!str_detect(sitio.origen2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.origen2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.origen2, "SUCUMBIOS PRODUCE"))%>%
  filter(!str_detect(sitio.origen2, "CENTRO DE MERC"))%>%
  filter(!str_detect(sitio.origen2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

no_son_ferias <- c(ferias$identificacion.operador.origen)

# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_origen.csv")

#transforming erroneous markets to farms
m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(operacion.origen2 == "Feria de comercialización animal" &
                                       identificacion.operador.origen %in% no_son_ferias,
                                     "Productor", operacion.origen2))

m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1664   1998   2290
# 2 Feria de comercialización animal     46     49     56
# 3 Operador Industrial                 107    108    117
# 4 Productor                         47795  57103  65189

ferias2 <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                           operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino == "Feria de comercialización animal") %>%
  filter(!str_detect(sitio.destino2, "FERIA"))%>%
  filter(!str_detect(sitio.destino2, "feria"))%>%
  filter(!str_detect(sitio.destino2, "Feria"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE COMER"))%>%
  filter(!str_detect(sitio.destino2, "MERCADO AGRO"))%>%
  filter(!str_detect(sitio.destino2, "SUCUMBIOS PR"))%>%
  filter(!str_detect(sitio.destino2, "REINA DEL CISNE"))%>%
  filter(!str_detect(sitio.destino2, "CENTRO DE MERCAD"))%>%
  filter(!str_detect(sitio.destino2, "EXPO SACHA"))%>%
  filter(!str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Informing the particular to authorities
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write_csv(ferias, "ferias_destino.csv")

no_son_ferias2 <- c(ferias2$identificacion.operador.destino)

m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(operacion.destino2 == "Feria de comercialización animal" &
                                        identificacion.operador.destino %in% no_son_ferias2,
                                      "Productor", operacion.destino2))



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


m2$cantidad <- as.numeric(m2$cantidad)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7954   8119  11420
# 2 Faenador                            159    146    153
# 3 Feria de comercialización animal     49     49     63
# 4 Operador Industrial                  88     98    110
# 5 Productor                         20736  31317  44152



#Looking another ferias qieh problems in the operator name
# Looking for animal markets with changed operation in origin

ferias_no_ident_origen <- m2 %>% group_by(provincia.origen, identificacion.operador.origen, 
                                          operacion.origen, operacion.origen2, sitio.origen2, ano) %>%
  filter(operacion.origen2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.origen2, "FERIA") | str_detect(sitio.origen2, "feria") | str_detect(sitio.origen2, "Feria") | 
           str_detect(sitio.origen2, "MERCADO AGRO") | 
           str_detect(sitio.origen2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.origen2, "CENTRO DE MERC") | str_detect(sitio.origen2, "EXPO SACHA") | 
           str_detect(sitio.origen2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")

# Replacing animal markets found with correct operation feria
ferias_no_ident_origen <- ferias_no_ident_origen %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2 <- m2 %>% 
  mutate(operacion.origen3 = ifelse(sitio.origen2 == "1860000640001 FERIA PORCINA PELILEO" |
                                      sitio.origen2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                      sitio.origen2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA",
                                    "Feria de comercialización animal", operacion.origen2))

m2$operacion.origen2 <- m2$operacion.origen3
m2$operacion.origen3 <- NULL


# Looking for animal markets with changed operation in destiny
ferias_no_identificadas_des <- m2 %>% group_by(provincia.destino, identificacion.operador.destino, 
                                               operacion.destino, operacion.destino2, sitio.destino2, ano) %>%
  filter(operacion.destino2 != "Feria de comercialización animal") %>%
  filter(str_detect(sitio.destino2, "FERIA") | str_detect(sitio.destino2, "feria") | str_detect(sitio.destino2, "Feria") | 
           str_detect(sitio.destino2, "MERCADO AGRO") | 
           str_detect(sitio.destino2, "SUCUMBIOS PRODUCE") | 
           str_detect(sitio.destino2, "CENTRO DE MERC") | str_detect(sitio.destino2, "EXPO SACHA") | 
           str_detect(sitio.destino2, "Asociacion de"))%>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))%>%
  spread(key="ano", value = "CSMI")


# Replacing animal markets found with correct operation feria at destiny
ferias_no_identificadas_des <- ferias_no_identificadas_des %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))

m2 <- m2 %>% 
  mutate(operacion.destino3 = ifelse(sitio.destino2 == "1860000640001 FERIA PORCINA PELILEO" |
                                       sitio.destino2 == "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA" |
                                       sitio.destino2 == "1960000620001 FERIA COMERCIAL GANADERA YANTZAZA" |
                                       sitio.destino2 == "0160048390001 Feria Patamarca" |
                                       sitio.destino2 == "1160000240001 EL PLATEADO FERIA DE GANADO EN PIE(LOJA)" | 
                                       sitio.destino2 == "1160000240001 FERIA COMERCIAL DE GANADO EN PIE EL PLATEADO 2019" |
                                       sitio.destino2 == "0460000210001 FERIA TULCAN" |
                                       sitio.destino2 == "0291502768001 FERIA CALUMA" |
                                       sitio.destino2 == "0560000460001 FERIA COMERCIAL DE ANIMALES MORASPUNGO",
                                     "Feria de comercialización animal", operacion.destino2))



m2$operacion.destino2 <- m2$operacion.destino3
m2$operacion.destino3 <- NULL



m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")


#lista de ferias
ferias_verdaderas_o <- m2 %>% group_by(provincia=provincia.origen, id=identificacion.operador.origen,
                                       operac2=operacion.origen2, sitio= sitio.origen2, ano) %>%
  filter(operacion.origen2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas_d <- m2 %>% group_by(provincia=provincia.destino, id=identificacion.operador.destino, 
                                       operac2=operacion.destino2, sitio=sitio.destino2, ano) %>%
  filter(operacion.destino2 == "Feria de comercialización animal") %>%
  summarise(CSMI=n(), animais=sum(as.numeric(cantidad)))

# %>%
#   spread(key="ano", value = "CSMI")

ferias_verdaderas <- rbind(ferias_verdaderas_o, ferias_verdaderas_d)

ferias_verdaderas <- ferias_verdaderas %>% 
  group_by(provincia, id, operac2, sitio, ano) %>%
  # filter(ano == 2019)%>%
  summarise(cert=sum(CSMI), animais=sum(as.numeric(animais)))%>%
  spread(key="ano", value = "cert")

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2019`)]))
# 68 markets
# 56

length(unique(ferias_verdaderas$sitio[!is.na(ferias_verdaderas$`2017`)]))
#47

# Number of premises by year ----
m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

# Year  Premises
# 1 2017     66534
# 2 2018     81987
# 3 2019    101143

# 101143/66534= 152.02%


# Changing markets with less than 150 animals to producers

fv <- ferias_verdaderas %>% group_by(id, sitio) %>%
  summarise(number=n(), animais=sum(animais)) 

length(unique(fv$id))
#51 ferias
fvd <- fv$id[fv$animais < 150]


m2 <- m2 %>%
  mutate(operacion.origen2 = if_else(identificacion.operador.origen %in% fvd,
                                      "Productor", operacion.origen2))


m2 <- m2 %>%
  mutate(operacion.destino2 = if_else(identificacion.operador.destino %in% fvd,
                                     "Productor", operacion.destino2))


m2$sitio.origen2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.origen2)
m2$sitio.origen2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.origen2)

m2$sitio.destino2 <- gsub("0460000640001 MERCADO AGROGANADERO MONTUFAR", "0460000640001 FERIA (MERCADO AGROGANADERO MONTUFAR)", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060000500001 FERIA DE ANIMALES OTAVALO", "1060000500001 FERIA COMERCIAL DE ANIMALES DE OTAVALO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1060031140001 Feria de Animales La Cruz", "1060031140001 FERIA COMERCIAL DE ANIMALES LA CRUZ - IBARRA", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1790953971001 FERIA COMERCIAL ASOGAN SD", "1790953971001 FERIA COMERCIAL ASOGAN SANTO DOMINGO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("1860000800001 FERIA PORCINOS QUERO", "1860000800001 FERIA SANTIAGO DE QUERO", m2$sitio.destino2)
m2$sitio.destino2 <- gsub("2390017130001 FERIA DE COMERCIALIZACION PROF. JOSE OÑATE RAMOS", "2390017130001 FERIA PEDRO VICENTE MALDONADO", m2$sitio.destino2)

m2$sitio.origen2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.origen2) 
m2$sitio.destino2 <- gsub("0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA - LATACUNGA", "0560000380001 FERIA DE ANIMALES ZUMBALICA - LATACUNGA", m2$sitio.destino2) 

#Checking
m2 %>%
  group_by(Year=ano, operacion.destino2)%>%
  summarise(Propietarios=length(unique(sitio.destino2))) %>%
  spread(key="Year", value = "Propietarios")

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7953   8119  11420
# 2 Faenador                            156    143    151
# 3 Feria de comercialización animal     48     52     56
# 4 Operador Industrial                  88     97    110
# 5 Productor                         20730  31305  44145


m2 %>%
  group_by(Year=ano, operacion.origen2)%>%
  summarise(Propietarios=length(unique(sitio.origen2))) %>%
  spread(key="Year", value = "Propietarios")




t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")


# About collors:
# display.brewer.all(colorblindFriendly = TRUE)
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/


# Results:  ----
# Number of movements
length(unique(m2$numero.certificado))
1190035
# Number of animals
sum(as.numeric(m2$cantidad))
9898087

#write arquive with ordered Markets 
# write.csv(m2, file="mov2017_2019m2_ferias_organizadas.csv")


--------------------------------------------------
  # Fig 1 Tile mean number of movements and animals per year ----
--------------------------------------------------
setwd("~/Dropbox/0.USP/6. 2019 I semestre/Paper sistema de vigilancia")
m2 <- read.csv("mov2017_2019m2_ferias_organizadas.csv", colClasses = "character")
--------------------------------------------------
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
# 9898087

library(tidyverse)
t <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
  summarise(contagem=mean(n())) %>%
  spread(key="ano", value = "contagem")

t <- data.frame(t)

# Movements mean number ----
# Calculating cantidad=mean number of movements by origin and destiny
t$X2017[is.na(t$X2017)] <- 0
t$cantidad <- round(rowMeans(t[, 3:5]), 0)

# Calculating percentages of every mov type
t$por <- t[,6]/sum(t$cantidad)*100
# Ordering porcentages
t[order(t[,6]/sum(t$cantidad)*100), ]

# origen                          destino  X2017  X2018  X2019 cantidad   por
# 4                   Comercializador              Operador Industrial      5     10    121       45  0.00
# 9  Feria de comercialización animal              Operador Industrial      0     19     33       17  0.00
# 19                        Productor              Operador Industrial   6836   6423   1748     5002  0.15
# 1                   Comercializador                  Comercializador   4497   7039   8115     6550  0.20
# 8  Feria de comercialización animal Feria de comercialización animal  12625   9422   7449     9832  0.30
# 5                   Comercializador                        Productor   5236  10895  14981    10371  0.31
# 13              Operador Industrial Feria de comercialización animal  10783  11458  12585    11609  0.35
# 15              Operador Industrial                        Productor  28600  58212  68337    51716  1.57
# 3                   Comercializador Feria de comercialización animal  43563  58497  67483    56514  1.71
# 16                        Productor                  Comercializador  51459  70285  81927    67890  2.06
# 11              Operador Industrial                  Comercializador  89587  65622  62591    72600  2.20
# 7  Feria de comercialización animal                         Faenador  64623  70776  92201    75867  2.30
# 2                   Comercializador                         Faenador 103071  71122  64016    79403  2.41
# 6  Feria de comercialización animal                  Comercializador  99887 104620 117198   107235  3.25
# 20                        Productor                        Productor 110395 157286 190028   152570  4.62
# 10 Feria de comercialización animal                        Productor 150454 236906 314213   233858  7.09
# 17                        Productor                         Faenador 226615 240273 296953   254614  7.72
# 18                        Productor Feria de comercialización animal 371190 473446 580579   475072 14.40
# 12              Operador Industrial                         Faenador 654563 774009 830011   752861 22.82
# 14              Operador Industrial              Operador Industrial 803471 877776 945962   875736 26.54

# % of movements incluiding markets
library(tidyverse)
t %>%
  filter(origen == "Feria de comercialización animal" |
           destino == "Feria de comercialización animal") %>%
  summarise(sum(por))
#57.05 % af all movilizations goes througth markets  

# Numero de animais
t2 <- m2 %>%
  group_by(origen=operacion.origen2, destino=operacion.destino2, ano) %>%
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
t2[order(round(t2[ ,6]/sum(t2[ ,6])*100, 2)), ]


# Number of animals per movement in traditional farms
#this have to be done with t and t2 without order See that!!
t3 <-  cbind(t2[ ,1], t2[ ,2], as.numeric(t2[, 6]/t[ ,6]) )
t3 <- t3[order(as.numeric(t3[ ,3])),]  

t4 <- t3[(t3[,1] != "Operador Industrial"), ] 
t5 <- t4[(t4[,2] != "Operador Industrial"), ] 
mean(as.numeric(t5[ ,3]))
5.9

# Number of animals per movement in industrial
t6 <- t3[(t3[,1] == "Operador Industrial"), ] 
t7 <- t3[(t3[,2] == "Operador Industrial"), ] 
t8 <- rbind(t6,t7)
t8 <- t8[-9,]
mean(as.numeric(t8[ ,3]))
51.5

#number os movilizations to slaughterhouse
t %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#143576

#number os animals to slaughterhouse
t2 %>%
  filter(destino == "Faenador")%>%
  summarise(sum(cantidad))
#1162754


# Production of meat calc ----

# Industrial
t2 %>%
  filter(destino == "Faenador")%>%
  filter(origen == "Operador Industrial")%>%
  summarise(sum(cantidad))

752861/1162754 # 64.74 % from industrial 
(752861*125*0.805)
#75,756.63 TM year industrials - 64.74% of production from industrials

#Backyard
1162754-752861
409893/1162754 # 35.25 % from backyard
(409893*97.5*0.805)
#32,171.4 TM year traditional farming - 35.25

#familiar consuption
(151327*97.5*0.805)
#11,877.3 TM year traditional farming

# number of animals that are consumed on the farm is 1.7 per farm per year
89016*1.7 = 151327

# Total meat prodution
75756.63 + 32171.40 + 11877.3
119805.3


# Per capita consuption
75756.63+32171.40+11877.3+3870.03
123675400/17100000

## 7.23

# Fig.1 Grafic 1 ----

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
t$origen <- gsub("Operador Industrial", "Industrial", t$origen)
t$origen <- gsub("Productor", "Farm", t$origen)

t$destino <- gsub("Comercializador", "Collection", t$destino)
t$destino <- gsub("Feria de comercialización animal", "Market", t$destino)
t$destino <- gsub("Operador Industrial", "Industrial", t$destino)
t$destino <- gsub("Productor", "Farm", t$destino)
t$destino <- gsub("Faenador", "Slaughter", t$destino)

a <-  ggplot(t, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#66C2A5")+
  geom_text(aes(label = t$cantidad))+
  labs(x="Destination", y="Source", fill="Movements") +
  labs(tag = "A")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

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
  geom_text(aes(label = t2$cantidad)) +
  labs(x="Destiny", y="Origin", fill="Number of animals")+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13))  
b  

# English number of animals ----
t2$origen <- gsub("Comercializador", "Collection", t2$origen)
t2$origen <- gsub("Feria de comercialización animal", "Market", t2$origen)
t2$origen <- gsub("Operador Industrial", "Industrial", t2$origen)
t2$origen <- gsub("Productor", "Farm", t2$origen)

t2$destino <- gsub("Comercializador", "Collection", t2$destino)
t2$destino <- gsub("Feria de comercialización animal", "Market", t2$destino)
t2$destino <- gsub("Operador Industrial", "Industrial", t2$destino)
t2$destino <- gsub("Productor", "Farm", t2$destino)
t2$destino <- gsub("Faenador", "Slaughter", t2$destino)
#

b <- ggplot(t2, aes(destino, origen))+
  geom_tile(mapping = aes(fill=cantidad))+
  scale_x_discrete(limits=(c("Farm",
                             "Market",
                             "Collection", 
                             "Industrial",
                             "Slaughter"))) +
  scale_y_discrete(limits=(c("Industrial",
                             "Collection",
                             "Market",
                             "Farm"))) +
  scale_fill_gradient(low = "white",
                      high = "#FC8D62") +
  geom_text(aes(label = t2$cantidad)) +
  labs(tag = "B")+
  labs(x="Destination", y="Source", fill="Animals")+
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 15),
    text = element_text(size = 18))

b  


library(ggpubr)
ggarrange(a,b, ncol = 2)

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"


length(unique(m2$sitio.destino2[m2$operacion.destino2 == "Faenador"]))


#100836/65187 = 154%

# With slaughterhouses
(66181+71731+100836)/3

# Year  Premises
# 1 2017     59297
# 2 2018     74779
# 3 2019     93193

(59297+74779+93193)/3
# 75756



# Number of premises by year and category ----
m_origen <- data.frame(m2$sitio.origen2, m2$operacion.origen2, m2$ano)
m_destino <- data.frame(m2$sitio.destino2, m2$operacion.destino2, m2$ano)
colnames(m_origen) <- c("sitio", "operacion", "ano")
colnames(m_destino) <- c("sitio", "operacion", "ano")
m_origen_destino <- rbind(m_origen, m_destino)

m_origen_destino %>%
  group_by(Year=ano, operacion)%>%
  summarise(Premises=length(unique(paste(sitio, operacion)))) %>%
  spread(key="Year", value=Premises)

89016+121+56+11799+151
8054+112+47+58168+156  


operacion                        `2017` `2018` `2019`
1 Comercializador                    8054   8403  11799
2 Feria de comercialización animal     47     52     56
3 Operador Industrial                 112    115    121
4 Productor                         58165  73274  89016
5 Faenador                            156    143    151

#increment or decrement of these premisses 2017 vs 2019
# Farms 53%
89016/58165
#collection 46%
11799/8054
#industrial 8%
121/112
# Market 18%
56/47
Slaughterhouse -4%
151/156

# Fig. 2. Number of premises (A), number of movements (B) and number of animals expressed by months and years. ----
# Number of active premisses by month/year ----

# Write the file m2 premises and slaugtherhouse ----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write.csv(m2, file="mov2017_2019m2.csv")

write.csv(m2, file="mov2017_2019m2_market.csv") #Last file with the modifications on markets

#Read the m2 file ----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
m2 <- read.csv("mov2017_2019.csv", colClasses = "character")

m2$X <- NULL
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) #6397433 #9905814 #9898087


library(ggplot2)
library(RColorBrewer)

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2017)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano == 2018)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 

m2 %>%
  group_by(Year=ano, mes)%>%
  filter(ano==2019)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2))))) 


# August 2019 vs 2018
2019 august 20129
2018 august 17154

20124/17151  17%

# Dezember 2019 vs 2018
2019 december 21187
2018 december 16900

21187/16900  25%


g1 <-  
  m2 %>%
  group_by(Year=ano, mes)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2, operacion.origen2),
                                     paste(sitio.destino2, operacion.destino2)))))%>%
  ggplot( aes(x=mes, y= Premises, colour=Year, group=Year))+
  geom_point() + geom_line(size=1.5) +
  labs(x = NULL, y ="Active premises", color="Year") +
  labs(tag = "A")+
  theme(text = element_text(size = 18))+
  scale_color_brewer(palette = "Set2")

# Number of swine movements
# Mean movement
m2 %>% 
  group_by(ano) %>%
  summarise(mov=length(unique(numero.certificado)))

m2 %>% 
  group_by(ano, mes) %>%
  summarise(n())

length(unique(m2$numero.certificado))

# graphic Number of swine movements
g2 <- m2 %>%
  group_by(year=ano, mes)%>%
  summarise(movements=length(unique(numero.certificado))) %>%
  ggplot( aes(x=mes, y= movements, color=year, group=year))+
  geom_point()+ geom_line(size=1.5)+
  labs(tag = "B")+
  labs(x = NULL, y ="Movements", color="Year") +
  theme(text = element_text(size = 18))+
  scale_color_brewer(palette = "Set2")


# z <- m2 %>%
#   group_by(year=ano, month=mes) %>%
#   summarise(movements=length(unique(numero.certificado)))
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
# m2 %>%
#   group_by(year=ano,mes)%>%
#   summarise(median(length(unique(numero.certificado))))

# Number of animals
g3 <- m2 %>%
  group_by(year=ano, mes)%>%
  summarise(swine=sum(as.numeric(cantidad))) %>%
  ggplot( aes(x=mes, y= swine, color=year, group=year))+
  geom_point()+ geom_line(size=1.5)+
  labs(tag = "C")+
  labs(x = "Months", y ="Animals", color="Year") +
  theme(text = element_text(size = 18))+
  scale_color_brewer(palette = "Set2")

library(RColorBrewer)
brewer.pal(n=8,name="Set2")

ggarrange(g1, g3, g2, nrow=3, common.legend = TRUE, legend="right")


# Porcentage of emmision by self service
m2%>%
  group_by(ano)%>%
  filter(provincia.emision == "")%>%
  summarise(n=n())%>%
  spread(key = "ano", value = n )

m2%>%
  group_by(ano, provincia.emision)%>%
  filter(provincia.emision == "") %>%
  summarise(n=n())

m2%>%
  group_by(ano)%>%
  summarise(n=n())

297553/314149
380069/394139
471016/481747

mean(0.947,0.964,0.977)



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


# Number of swine
# Mean movement
m2 %>% 
  group_by(ano) %>%
  summarise(animals=sum(as.numeric(cantidad)))

m2 %>% 
  group_by(ano) %>%
  summarise(animals= sum(as.numeric(cantidad))/12)

# ano   animals

# 1 2017  236455 
# 2 2018  275341.
# 3 2019  313044.

# m2 with slaughterhouses ----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
write.csv(m2, file="mov2016_2020_mSlaugther.csv")

# Read the m2 file ----
# Ready with markets in order ----
#arquivo ultimo para rede
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
m2 <- read.csv("mov2016_2020_mSlaugther.csv", colClasses = "character")


# --------------------------------------------------
# 3 Network creation for Fig 3----
# --------------------------------------------------
# Creating the network of slaughterhouses or premises changing s or s----
library(dplyr)
s <- m2[m2$operacion.destino2 == "Faenador",]

#Creatomg network of premisses
s <- m2[m2$operacion.destino2 != "Faenador",]

#Simplify the network for provincia, canton e parroquia 
#and year this is to graphic each network by year

s_simplified <- s %>% 
  filter(ano == 2017) %>%
  #filter(ano == 2018) %>%
  #filter(ano == 2019) %>%
  group_by(provincia.origen, canton.origen, parroquia.origen, origen=paste(provincia.origen, canton.origen, parroquia.origen),
           provincia.destino, canton.destino, parroquia.destino, destino=paste(provincia.destino, canton.destino, parroquia.destino), ano) %>%
  summarise(Freq=n())

sum(s_simplified$Freq)
# 2017 128366
# 2018  146919
#2019 170054

# Premises: 2017:186252, 2018:247785, 2019:312054

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
summary(banco$correspondence$page_rank)

# Creating centroids ----
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
#number of CSMI
#2017:5358   Premises:15798
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
vigi$x <- ec3@data$x[match(vigi$c_pp, ec3@data$DPA_PARROQ)]
vigi$y <- ec3@data$y[match(vigi$c_pp, ec3@data$DPA_PARROQ)]

# Criando os centroides ----
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

# Slaughter network graphic----
#Movements
mov <- data.frame(banco$movements$From, banco$movements$To, banco$movements$Freq)
colnames(mov) <- c("Source", "Target", "Freq")
#Premises
prem <- data.frame(banco$correspondence$network.id, banco$correspondence$y, 
                   banco$correspondence$x, banco$correspondence$all_degree)
colnames(prem) <- c("ID", "latitude", "longitude", "all_degree")

# Plotting the map base
library(maps)
library(geosphere)
library(sp)
library(shape) #Arrows
install.packages("GISTools")
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
# Project Using the same width line between visualization ----
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

#Ploting lines -----
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



#Results over graphic with 6 maps
# for premises
premises <- m2 %>%
  filter(operacion.destino2 != "Faenador") %>%
  group_by(provincia.origen, provincia.destino) %>%
  summarise(movements=n(), cantidad= sum(cantidad))

write.csv(premises, file = "from_to.csv")

#for slaughterhouses
premises_slaug <- m2 %>%
  filter(operacion.destino2 == "Faenador") %>%
  group_by(provincia.origen, provincia.destino) %>%
  summarise(movements=n(), cantidad= sum(cantidad))

write.csv(premises_slaug, file = "from_to_slauh.csv")



premises <- m2 %>%
  filter(operacion.destino2 != "Faenador") %>%
  group_by(provincia.origen2) %>%
  summarise(movements=n(), cantidad= sum(cantidad))

premises_des <- m2 %>%
  filter(operacion.destino2 != "Faenador") %>%
  group_by(provincia.destino) %>%
  summarise(movements=n())



sum(premises$movements)

# Cotopaxi 20.39%
158054/774784
#Santo domigno 11.04%
88361/774784



# 4 Creating final network for analysis ----
# Read the m2 file ----
# Ready with markets in order ----
#final arquive for network
library(epinemo)
library(igraph)

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
m2 <- read.csv("mov2016_2020_mSlaugther.csv", colClasses = "character")

#Creatomg network of premisses
m2 <- m2[m2$operacion.destino2 != "Faenador",]
# 744787 movements

m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad)
#6389239

m2 %>%
  group_by(Year=ano)%>%
  summarise(Premises=length(unique(c(paste(sitio.origen2),
                                     paste(sitio.destino2))))) 

# Premises
# Confering the same number of premises to create de network
# Year  Premises
# 1 2017     66543
# 2 2018     81994
# 3 2019    101153

#Without movements to slaughterhouses
# Year  Premises
# 1 2017     59293
# 2 2018     74778
# 3 2019     93192

# Creting anual networks
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


# 5 Community analysis ----
setwd("/home/alfredo/Network characterization/Network")

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
m2 <- read.csv("mov2016_2020_mSlaugther.csv", colClasses = "character")

# 5.1 Creating network of premisses
m2 <- m2[m2$operacion.destino2 != "Faenador",]
# 744787 movements

com <- m2 %>% 
  #filter(ano == 2017) %>%
  #filter(ano == 2018) %>%
  #filter(ano == 2019) %>%
  group_by(numero.certificado, provincia.origen, canton.origen, parroquia.origen, origen=paste(provincia.origen, canton.origen, parroquia.origen),
           provincia.destino, canton.destino, parroquia.destino, destino=paste(provincia.destino, canton.destino, parroquia.destino), ano) %>%
  summarise(Freq=n(), cantidad=sum(as.numeric(cantidad)))

sum(com$Freq)

# 744787
#2017 : 185824
#2018 : 247461
#2019 : 311702

# Creating unique ID parroquia ----
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
#1489574
#2017 371648
#2018 494522
#2019 623404
### Mapa 
library(rgdal)
library(gdata)

ec3<-readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# mapa para vigilancia
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
length(unique(com$origin_cod)) #893
length(unique(com$destiny_cod)) #909

a <- c(com$origin_cod,com$destiny_cod)
length(unique(a)) #943

# 2017: 772 e 738
# 2018: 804 e 828
# 2019: 825 e 856

library(epinemo)
library(igraph)
com <- data.frame(com)
banco <- createUniqueIds(com,
                         from = "origin_cod",
                         to= "destiny_cod")

# 5.0 Analise de comunidade ----
library(Matrix)
matriz <- sparseMatrix(i=banco$movements$From,j=banco$movements$To, x= banco$movements$Freq,
                       dims = rep(max(banco$correspondence$network.id), 2))
# caregando a matriz
print(matriz)

# Page rank e google matriz
pr <- pageRank(matriz)
G <- GoogleMatrix(matriz)

L <- LinkRank(G, pr)
qlrM <- LinkRankModMatrix(L, pr)

# 5.1 Simulated Annealing 

#mudando parametros de simulated annealing, temperatura e resfriamento, etc
#fazendo uma particao inicial rapida, para utilizar de inicio na proxima
particao <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, cool = 0.95, max_itry = 10*dim(qlrM)[1])
particao <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, cool = 0.80, max_itry = 10*dim(qlrM)[1], plots=TRUE)

length(unique(particao)) #54
#avaliando modularidade (qualidade da divisao)
LinkRankMod(L = L, pr = pr, c = particao) #0.344
plot(particao)

# 5.2 Locking 5 comunities  ----
set.seed(5)
N <- 5
no_nos <- NROW(unique(banco$correspondence$database.id))
inicial <- sample.int(n = N, size = no_nos, replace = TRUE)
particao1 <- linkRankOptimalPartition(qlrM = qlrM, Tc = 1e-2, cool = 0.95, max_itry = 10*dim(qlrM)[1],
                                      c = inicial)
length(unique(particao1)) #5
LinkRankMod(L = L, pr = pr, c = particao1) #0.3465

#2017-2019: 0.37798
# 5: 0.3502
# 6: 0.371
# 7: 0.362

# 2017: 5, 0.333
# 2018: 5, 0.3434
# 2019: 5 0.353

#2017: 6, 0.3412
#2018: 6, 0.355
#2019: 6, 0.378

# 5.3 Taking partition names to banco ----
banco$correspondence$com <- particao1
length(unique(banco$correspondence$com))
banco$correspondence$pr <- pr #pasando o Pagerank
write.csv(banco$correspondence, file="banco_correspondence.csv")

# Paso comunidades para shape 
ec3@data$com <- banco$correspondence$com[match(ec3@data$DPA_PARROQ, banco$correspondence$database.id)]

write.csv(ec3@data, file="Comunidades_ec3.csv")

# eliminando pastaza araujo curarai
ec3@data$com[ec3@data$DPA_PARROQ == "160451"] <- NA
# eliminando Orellana aguarico tiputini
ec3@data$com[ec3@data$DPA_PARROQ == "220254"] <- NA
ec3@data$com[ec3@data$DPA_PARROQ == "210751"] <- NA

library(leaflet)
# 5. 4 Plotting community map ----
# mypal <- colorFactor(palette = "Set2", na.color = "#ffffff", domain = ec3@data$com)
#Colours for 6 communities
#mypal <- colorFactor(c("#66C2A5","#FC8D62","#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"), na.color = "#ffffff", domain = ec3@data$com)
# #Colours for 5 communities
mypal <- colorFactor(c("#66C2A5","#FC8D62","#FFD92F", "#8DA0CB", "#A6D854"), na.color = "#ffffff", domain = ec3@data$com)

map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #58 Apertura ambiente grafico html
  #addProviderTiles("Esri.NatGeoWorldMap") %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%#Apertura open maps
  setView(lat = -1.7, lng = -78.5, zoom = 7) %>% #Localiza o mapa
  addPolygons(data = ec3, stroke = FALSE, smoothFactor = 0.7, fillOpacity = 0.7, #Agrega poligonos e define opacidades
              fillColor = ~ mypal(ec3@data$com), #Prenche cores do shape de acordo a quantidade
              popup = paste("Terr: ", ec3@data$pp, "<br>", #Pop-up dados de territorio
                            "Comunidad: ", ec3@data$com, "<br>")) %>%
  addScaleBar(position = c("bottomright"), 
              options = scaleBarOptions(maxWidth = 200, metric = TRUE, 
                                        imperial = FALSE, updateWhenIdle = TRUE)) %>%
  leaflet::addLegend(position = "bottomright", pal = mypal, values = ec3@data$com,
                     title = "<p>Communities</p>", opacity = 0.99)# Lenda e opacidade
map

# 5.5 Mapa ggplot----
# library(broom)
# library(ggplot2)
# head(ec3@data$DPA_PARROQ)
# map <- tidy(ec3, region = "DPA_PARROQ") 
# map$com <- ec3@data$com[match(map$id, ec3@data$DPA_PARROQ)]
# 
# # Mapa comunidades 6 final
# ggplot()+
#   geom_polygon(aes(x=long, y=lat, group=group, fill = as.factor(com)), data = map)+
#   geom_path(data=ec3,aes(x=long, y=lat,group=group), colour="grey", size=0.03)+
#   scale_fill_viridis_d()+
#   geom_path(data=ec3, aes(x=long, y=lat, group=group), colour="grey60", size=0.1) +
#   xlim(-82,-75) +
#   coord_fixed()+
#   labs(x="Longitude", y="Latitude")+
#   labs(fill = "Communities")
# 
# ec3f <- fortify(ec3)
# 
# ggplot()+
#   geom_polygon(aes(x=long, y=lat, group=group, fill = as.factor(com)), data = map)+
#   geom_path(data=ec3f,aes(x=long, y=lat,group=group), colour="grey", size=0.03)+
#   scale_fill_brewer(palette="Set1")+
#   geom_path(data=ec3, aes(x=long, y=lat, group=group), colour="grey60", size=0.1) +
#   xlim(-81.5,-75) +
#   coord_fixed()+
#   labs(x="Longitude", y="Latitude")+
#   labs(fill = "Communities")
# 
# scale_fill_brewer(palette="Set1")

# 5.6 ChordDiagram ----
library(circlize)

com$origin_cod <- vigi$c_pp[match(com$origen, vigi$origen)]
com$destiny_cod <- vigi$c_pp[match(com$destino, vigi$origen)]

banco$movements$from_com <- banco$correspondence$com[match(banco$movements$origin_cod, banco$correspondence$database.id)]
banco$movements$to_com <- banco$correspondence$com[match(banco$movements$destiny_cod, banco$correspondence$database.id)]

chord <- banco$movements %>%
  group_by(from_com,to_com)%>%
  summarise(Mov=sum(Freq))

sum(chord$Mov)
# 2017:185824
# 2018: 247261
# 2019 : 311702
# 2017: 2019: 744787

#change environmental cex with par, to make bigger the letters
# Save with 1800 x 1800
par(cex=4)
chordDiagram(x=chord, transparency = 0.2, 
             grid.col = c("#66C2A5","#FC8D62","#FFD92F", "#8DA0CB", "#A6D854"))

#returning cext to normal
par(cex=1)

write.csv(chord, file= "Comunidades.csv")

# 5.7 Some analise

ec3@data %>%
  group_by(com) %>%
  summarize(n())

# com `n()` `sum(cantidad)`
# 1     1   152          393445
# 2     2   156          169169
# 3     3   298          514665
# 4     4   204          118798
# 5     5   130          293492
# 6    NA   100              NA

banco$movements %>%
  group_by(from_com)%>%
  summarize(n())

# from_com  `n()`

# 1        1 185717
# 2        2  79517
# 3        3 270294
# 4        4  63923
# 5        5 145336

banco$movements %>%
  group_by(to_com)%>%
  summarize(n(), sum(ca))


ec3@data %>%
  filter(is.na(com)) %>%
  group_by(provincia) %>%
  summarize(n())

# provincia              `n()`
# 1 AZUAY                 16
# 2 CANAR                  2
# 3 EL ORO                 4
# 4 ESMERALDAS            15
# 5 GALAPAGOS              8
# 6 GUAYAS                 4
# 7 LOJA                   3
# 8 LOS RIOS               4
# 9 MANABI                 5
# 10 MORONA SANTIAGO       11
# 11 NAPO                   2
# 12 ORELLANA               8
# 13 PASTAZA                5
# 14 PICHINCHA              1
# 15 SUCUMBIOS              5
# 16 TUNGURAHUA             1
# 17 ZAMORA CHINCHIPE       3
# 18 ZONA NO DELIMITADA     3


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



# 6 Describing network structure ----
setwd("/home/alfredo/Network characterization/Network")
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
m2 <- read.csv("mov2016_2020_mSlaugther.csv", colClasses = "character")

# 5.1 Creating network of premisses
m2 <- m2[m2$operacion.destino2 != "Faenador",]

# 744787 movements

library(tidyverse, epinemo, igraph)
library(Matrix)
# 6.1 Premise level
# Number of premises by year and category ----

# Creating a unique premise code

m2$from <- paste(m2$identificacion.operador.origen, m2$provincia.origen, m2$canton.origen, m2$parroquia.origen, m2$operacion.origen2)
m2$to <- paste(m2$identificacion.operador.destino, m2$provincia.destino, m2$canton.destino, m2$parroquia.destino, m2$operacion.destino2)

m_origen <- data.frame(m2$from, m2$ano, m2$operacion.origen2)
m_destino <- data.frame(m2$to, m2$ano, m2$operacion.destino2)
colnames(m_origen) <- c("sitio", "ano", "operacion")
colnames(m_destino) <- c("sitio", "ano", "operacion")
m_origen_destino <- rbind(m_origen, m_destino)

m_origen_destino %>%
  group_by(Year=ano, operacion)%>%
  summarise(Premises=length(unique(paste(sitio, operacion)))) %>%
  spread(key="Year", value=Premises)
# 
# operacion                        `2017` `2018` `2019`
# 1 Comercializador                    7970   8310  11667
# 2 Feria de comercialización animal     52     51     59
# 3 Operador Industrial                  85     94     98
# 4 Productor                         50304  65619  80552


#Proving that the number of premises are the same 
m2 %>%
  group_by(ano, operacion.origen2) %>%
  summarise(premises=length(unique(from))) %>%
  spread(key="ano", value=premises)

# operacion.origen2                `2017` `2018` `2019`
# 1 Comercializador                    1285   1628   1901
# 2 Feria de comercialización animal     49     48     52
# 3 Operador Industrial                  72     78     82
# 4 Productor                         39083  48220  55261

m2 %>%
  group_by(ano, operacion.destino2) %>%
  summarise(premises=length(unique(to))) %>%
  spread(key="ano", value=premises)

# operacion.destino2               `2017` `2018` `2019`
# 1 Comercializador                    7906   8064  11359
# 2 Feria de comercialización animal     46     45     54
# 3 Operador Industrial                  70     82     92
# 4 Productor                         20426  30968  43717


# Same number of premises: so, Im using sitio.origen2 e sitio.destino2
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



# 6.0 Using the filtered datasets and generating the measures ----
library(epinemo)
library( igraph)

# here we assign m as the dataset that you want to analize with the f
# inside f there is all the analis
m <- m_2
f {
  m <- data.frame(m)
  m$cantidad <- as.numeric(m$cantidad)
  banco <- createUniqueIds(m,
                           from = 'from',
                           to = 'to')
  
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
  
  
  # 6.2 Degree ----
  # Animais ####################
  kin <- colSums(matriz)
  kout <- rowSums(matriz)
  ktotal <- kin + kout
  
  ####
  banco$correspondence$kout <- ktotal
  banco$correspondence[banco$correspondence$kout > 10000,]
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
  
  
  # 6.3 Cluster coefficient ----
  library(igraph)
  cc <- clusteringCoefficient(matriz,directed =T)
  
  # Direccionada e igual a TRUE é uma rede direccionada
  summary(cc)
  
  
  # 6.4 Numero de vizinhos Neighbors ----
  matrizv <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                          dims = rep(max(banco$movements$From, banco$movements$To) ,2))
  
  matriz_nao_direcionada <-matrizv + t(matrizv)
  
  matriz_vizinhos_nao_direcionada <-((matriz_nao_direcionada >0) *1)
  
  ktotalv <- rowSums(matriz_vizinhos_nao_direcionada)
  
  summary(ktotalv)
  
    # 6.5 Betweeness
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
  # 
  # summary(between)
  # 
  # boxplot(between, ylim=c(0,01))
  # #title(main = "Between")
  # 
  #boxplot(between)

  # 6.6  shortest paths
  spath <- mean_distance(grafo)
  
  # Zoom
  #hist(between)
  #hist(between, ylim = c(0, 10))
  
  
  # 6.7 Density ----
  density <-  edge_density(grafo)
  summary(density)
  
  # 6.8 Diameter ----
  diameter <-  diameter(grafo)
  
  
  # 6.9 GWCC
  gwcc <- components(grafo, mode = c("weak"))
  summary(gwcc)
  
  gscc <- components(grafo, mode = c("strong"))
  summary(gscc)
  
  # 6.10 Clossenes 
  #CCentralidade de proximidade
  # ela se preocupa quanto distante o quanto distante o no esta das outros
  
  closeness_in <- closeness(graph =grafo,mode = 'in' )
  # closeness_out <- closeness(graph =grafo,mode = 'out' )
  
  boxplot(closeness_in, closeness_out)
  
  summary(closeness_in)
  summary(closeness_out)
  
  
  
  
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
  #Gigant weak connected conmponent
  gw <- summary(gwcc)
  gw <- gw[2,1]
  gw <- gsub(" ","", gw)
  
  gw1 <- summary(gwcc)
  gw1 <- gw1[1,1]
  gwpor <- as.numeric(gw)/as.numeric(gw1)*100
  
  #Gigant strong connected conmponent
  gs <- summary(gscc)
  gs <- gs[2,1]
  gs <- gsub(" ","", gs)
  
  gs2 <- summary(gscc)
  gs2 <- gs2[1,1]
  gspor <- as.numeric(gs)/as.numeric(gs2)*100
  
}
  r02 <- data.frame(p, m, d, rcc, n, b, cl, de, di, gw, gwpor, gs, gspor)

# for monts
mo <- rbind(r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24,
            r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36)
mo <- t(mo)
colnames(mo) <- rep(c("JAN", "FEV", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),3)

mo <- data.frame(mo)

mo[] <- lapply(mo, as.character)
mo2 <- mo
mo2[] <- lapply(mo, as.numeric)

mo2$mean <- rowMeans(mo2)

write.csv(mo2, file="month3.csv")

# 6.9 Graphics ----

# For years
r2017
r2018
r2019

years <- rbind(r2017, r2018, r2019)
years  <- t(years)
colnames(years) <- c("2017", "2018", "2019")
years <- data.frame(years)
years[] <- lapply(years, as.character)

years$X2017 <- as.character(years$X2017)
years$X2018 <- as.character(years$X2018)
years$X2019 <- as.character(years$X2019)

years[] <- lapply(years, as.numeric)
years$mean <- rowMeans(years)
years$range <- paste("[",min(years[1,]), "-" , max(years[1,]), "]")

write.csv(years, file="years.csv")

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
mo <- read.csv("month3.csv", colClasses = "character")
mo$mean <- NULL
mot <- data.frame(t(mo))

colnames(mot) <- c("p", "m", "d", "rcc", "n", "b", "cl", "de", "di", "gw", "gwpor", "gs", "gspor")
mot <- mot[-1,]
mot[] <- lapply(mot, as.character)
mot[] <- lapply(mot, as.numeric)

mot$mes <- row.names(mot)
mot$month <- 1:36

mot$year <- c(rep("2017", 12),rep("2018", 12),rep("2019", 12))
mot$month2 <- rep(1:12, 3)
mot <- mot[-1,]

# Degree
gd <- ggplot(mot, aes(x=month,y=d))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Degree")+
  theme_linedraw()

# Graphic facet for degree
t <- ggplot(mot, aes(x=month2, y=d, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "A")+
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="All Degree")+
  theme_linedraw() +
  theme(text = element_text(size = 18))+
  facet_grid(rows = vars(year))


# Clustering coefficient
grcc <- ggplot(mot, aes(x=month,y=rcc))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Clustering coeficient")+
  labs(tag = "A")+
  theme_linedraw() + 
  theme(text = element_text(size = 18))

# Graphic facet for CC
u <- ggplot(mot, aes(x=month2, y=rcc, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  labs(tag = "B") +
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="Clustering Coefficient")+
  theme_linedraw() +
  theme(text = element_text(size = 18))+
  facet_grid(rows = vars(year))

# Neighbours
gn <- ggplot(mot, aes(x=month,y=di))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Neighbors")+
  labs(tag = "B")+
  theme_linedraw()  + 
  theme(text = element_text(size = 18))

# Density
gde <- ggplot(mot, aes(x=month,y=de))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Density")+
  labs(tag = "C")+
  theme_linedraw()  + 
  theme(text = element_text(size = 18))

#Diameter
gdi <- ggplot(mot, aes(x=month,y=di))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Diameter")+
  labs(tag = "D")+
  theme_linedraw()  + 
  theme(text = element_text(size = 18))

# Gigant weak component
ggw <- ggplot(mot, aes(x=month,y=gw))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Gigant weak connected component")+
  labs(tag = "E")+
  theme_linedraw() + 
  theme(text = element_text(size = 18))

#Gigant strong component
ggs <- ggplot(mot, aes(x=month,y=gs))+
  geom_point(shape=21, colour="#1F78B4")+
  geom_line(colour="#1F78B4")+
  labs(y = "Values", x ="Month", title="Gigant strong connected component")+
  labs(tag = "F")+
  theme_linedraw() + 
  theme(text = element_text(size = 18))

# Betweeness
be <- ggplot(mot, aes(x=month,y=b))+
  geom_point(shape=21, colour="#E31A1C")+
  geom_line(colour="#E31A1C")+
  labs(y = "Values", x ="Month", title="Betweennes")+
  theme_linedraw() + 
  theme(text = element_text(size = 18))

bef <- ggplot(mot, aes(x=month2, y=b, colour=year, group=year))+
  geom_point(shape=21)+
  geom_line(size=1)+
  guides(colour=FALSE)+
  labs(y = "Values", x ="Month", title="Betweennes")+
  labs(tag = "C")+
  theme_linedraw() +
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
  theme_linedraw() +
  theme(text = element_text(size = 18))+
  facet_grid(rows = vars(year))

  
library(ggpubr)
ggarrange(grcc, gn, gde, gdi, ggw, ggs, ncol=2, nrow=3, common.legend = FALSE, legend="right")

ggarrange(t,u, bef, clf, common.legend = FALSE, legend="right")


# 6.10 Mann Kendal test ----
install.packages("Kendall")
library(Kendall)

# 
# MannKendall(mot$p)
# tau = 0.721, 2-sided pvalue =< 2.22e-16
# 
# MannKendall(mot$n)
# tau = 0.543, 2-sided pvalue =4.7684e-06
# MannKendall(mot$b)
# tau = 0.711, 2-sided pvalue =< 2.22e-16
# MannKendall(mot$di)
# tau = 0.459, 2-sided pvalue =0.00028884
# 
# 
# 
# MannKendall(mot$rcc)
# u = -0.499, 2-sided pvalue =2.9295e-05
# MannKendall(mot$cl)
# tau = -0.734, 2-sided pvalue =5.9472e-10
# MannKendall(mot$de)
# tau = -0.701, 2-sided pvalue =3.4672e-09
# MannKendall(mot$gwpor)
# tau = -0.247, 2-sided pvalue =0.038134
# MannKendall(mot$gspor)
# tau = -0.644, 2-sided pvalue =5.7972e-08
# 
# 
# The Mann-Kendall test show statistical significant (p < 0.001), 
# suggesting the presence of a upward trend in the montly time series for betweenness and diameter
# 
# (p < 0.001) suggesting the presence of a downward trend in the montly time series for Clustering coefficient, 
# Closeness, density, strong connected components and p < 0.05 for the gigant weak connected component.
# 



