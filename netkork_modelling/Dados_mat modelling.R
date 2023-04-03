# Math modelling of CSF in Ecuador
# Alfredo Acosta PhD Epidemiology
# 19.02.2023

library(dplyr); library(tidyr); library(lubridate); library(scales); library(ggplot2)
# --------------------------------------------------
# Catastre, there is not used but, how many farms are there?
# --------------------------------------------------
#1.1.1 Cadastro 2017 ----
# setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
cad17 <- read.csv(file="cad2017.csv", colClasses = "character")

#1.1.2 Numero de predios ----
length(unique(paste(cad17$identificacion.propietario, cad17$nombre.sitio))) #105542
length(unique(cad17$identificacion.propietario)) #103340

#1.1.3 Numero de animais no cadastro ----
sum(as.numeric(cad17$cantidad)) #1204824

# 1.2.1 Cadastro 2018 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2018")

cad18 <- read.csv(file="cad2018.csv", colClasses = "character")
#1.2.2 Numero de predios ----
length(unique(paste(cad18$identificacion.propietario, cad18$nombre.sitio))) #126168
length(unique(cad18$identificacion.propietario)) #123713
#1.2.3 Numero de animais no cadastro ----
sum(as.numeric(cad18$cantidad)) #1311898

# 1.3.1 Cadastro 2019 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2019/antigos/")
cad19 <- read.csv(file="cad2019.csv", colClasses = "character")
#1.3.2 Numero de predios ----
length(unique(paste(cad19$identificacion.propietario, cad19$nombre.sitio))) #105083

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad19$cantidad), na.rm = TRUE) #1504601


# 1.3.1 Cadastro 2020 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2020")
cad20 <- read.csv(file="cad2020.csv", colClasses = "character")
colnames(cad20) <- tolower(iconv(colnames(cad20),  from = 'UTF-8', to = 'ASCII//TRANSLIT'))

#1.3.2 Numero de predios ----
length(unique(paste(cad20$identificacion.propietario, cad20$nombre.sitio))) #104778
length(unique(cad20$identificacion.propietario)) #103188

# Puedo eliminar la cantidad inactivos? 

cad20 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.activos), na.rm = TRUE))

cad20 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.inactivos), na.rm = TRUE))

cad20$cantidad <- cad20$cantidad.activos
cad20$cantidad.inactivos <- NULL
cad20$cantidad.activos <- NULL
colnames(cad20)

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad20$cantidad), na.rm = TRUE) #1107488


# 1.3.1 Cadastro 2021 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2021")
cad21 <- read.csv(file="cad2021.csv", colClasses = "character")
colnames(cad21) <- tolower(iconv(colnames(cad21),  from = 'UTF-8', to = 'ASCII//TRANSLIT'))

#1.3.2 Numero de predios ----
length(unique(paste(cad21$identificacion.propietario, cad21$nombre.sitio))) #104778
length(unique(cad21$identificacion.propietario)) #103188

# Puedo eliminar la cantidad inactivos? 
cad21 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.activos), na.rm = TRUE))

cad21 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.inactivos), na.rm = TRUE))

cad21$cantidad <- cad21$cantidad.activos
cad21$cantidad.inactivos <- NULL
cad21$cantidad.activos <- NULL
colnames(cad21)

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad21$cantidad), na.rm = TRUE) #1107488

# Catastro por anos
cad20$X <- cad20$x
cad20$x <- NULL

cad21$X <- cad21$x
cad21$x <- NULL

# Agregar ano
cad17$ano <- "2017"
cad18$ano <- "2018"
cad19$ano <- "2019"
cad20$ano <- "2020"
cad21$ano <- "2021"

cad <- rbind(cad17,cad18,cad19, cad20, cad21)
# cad <- rbind(cad19, cad20, cad21)

# Número de animales por tipo
# Elimino feria comercial  y faenador

cad %>%
  group_by(tipo.operacion)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

#número de animales por provincia total
cad %>%
  group_by(provincia)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE)) %>% 
  arrange(desc(animales))

# Número de animales en predios con madres
cad %>%
  group_by(ano)%>%
  filter(producto == "Cerda madre")%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

#número de animales totales por ano
cad %>%
  group_by(ano)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))


#número de animales por ano y dosis recibidas (ad animais e doses)
ad <- cad %>%
  group_by(ano)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE),
            doses=animales*1.55)


table(cad$producto)


#cerdas madres y verracos
cv <- cad %>%
  group_by(ano)%>%
  filter(producto == "Verraco" | producto == "Cerda madre")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

#!cerdas madres y verracos
cv$b <- cad %>%
  group_by(ano)%>%
  filter(producto == "Lechona" | 
           producto == "Lechón" |
           producto == "Cerda levante" |
           producto == "Cerdo levante") %>%
  summarise(animales_otros=sum(as.numeric(cantidad), na.rm = TRUE))

cv <- data_frame(cv)

cv %>% mutate(total=animales*2+ b$animales_otros*1.55)

cv %>% mutate(total=animales*2.5+ b$animales_otros*1.8)


str(cv$b$animales_otros)

animais$t <- apply(animais, MARGIN= 2, FUN=sum)
dim(animais)













#número de sitios totales por operación
cad %>%
  group_by(ano,tipo.operacion)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))%>%
  spread(key = "ano", value = "establecimientos")

#número de sitios 
cad %>%
  group_by(ano)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))

table(cad$producto)

# Analisis Indocumentados
cad %>%
  group_by(tipo.operacion, ano)%>%
  filter(identificacion.propietario == 1768105720002)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))%>%
  spread(key = "ano", value = "establecimientos")

cad %>%
  group_by(tipo.operacion, ano)%>%
  filter(identificacion.propietario == 1768105720002)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad)))%>%
  spread(key = "ano", value = "animales")

#Eliminación indocumentados
cad <- cad[cad$identificacion.propietario != 1768105720002, ]

#Eliminar os nombre_sitio eliminar  #
cad <- cad[cad$nombre.sitio != "ELIMINAR", ]

# Establecimientos que tienen cerdas madres
cad %>%
  group_by(ano)%>%
  filter(producto == "Cerda madre")%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))


#número de animales por ano
cad %>%
  group_by(ano)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))


# 3 Vacinação ----
# Home folder ----
library(lubridate); library(tidyverse)

# Transfering higher cadastral number since 2016-2019 population ----
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2017/")
v17 <- read.csv("vac2017m.csv", colClasses = "character")
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2018/")
v18 <- read.csv("vac2018m.csv", colClasses = "character")
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2019/")
v19 <- read.csv("vac2019m.csv", colClasses = "character")
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2020/")
v20 <- read.csv("vac2020m.csv", colClasses = "character")
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2021/")
v21 <- read.csv("vac2021m.csv", colClasses = "character")

colnames(v20)
colnames(v21)
all_equal(v20,v21)

# delete cuvs anulados from 19 and 20
v19 <- v19[v19$Estado != "anulado",]
v20 <- v20[v20$Estado != "anulado",]
v21 <- v21[v21$Estado != "anulado",]

#delete colum estado
v19$Estado <- NULL
v20$Estado <- NULL
v21$Estado <- NULL

# rbind of all CUV
vac <- rbind(v17,v18,v19,v20,v21)

#number of animals
vac$Número.Productos.Vacunados <- as.numeric(vac$Número.Productos.Vacunados)

# number of certificates CUV
length(unique(vac$Número.Certificado)) #857667

# number of vaccined animals
sum(as.numeric(vac$Número.Productos.Vacunados)) #13040325

#Adding year of vaccination
vac$year <- year(dmy(vac$Fecha.Vacunacion))
vac$month <- month(dmy(vac$Fecha.Vacunacion))

yv <- vac %>%
  group_by(year)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))

#Adding year of registry
vac$year_r <- year(dmy(substr(vac$Fecha.Registro, 1, 10)))
vac$month_r <- month(dmy(substr(vac$Fecha.Registro, 1, 10)))

yr <- vac %>%
  group_by(year_r)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))

yv$vac[4:8]-yr$vac

vac %>%
  group_by(Tipo.de.Vacuna, year)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))%>%
  spread(key = "Tipo.de.Vacuna", value="vac")

colnames(vac)
#  Agregando provincia----
vac$pro <- tolower(iconv(vac$Provincia.Sitio, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(vac$pro)
vac$regiao <- vac$pro

vac$regiao <- gsub("azuay", "serra", vac$regiao)
vac$regiao <- gsub("bolivar", "serra", vac$regiao)
vac$regiao <- gsub("canar", "serra", vac$regiao)
vac$regiao <- gsub("carchi", "serra", vac$regiao)
vac$regiao <- gsub("chimborazo", "serra", vac$regiao)
vac$regiao <- gsub("cotopaxi", "serra", vac$regiao)
vac$regiao <- gsub("el oro", "litoral", vac$regiao)
vac$regiao <- gsub("santa elena", "litoral", vac$regiao)
vac$regiao <- gsub("guayas", "litoral", vac$regiao)
vac$regiao <- gsub("imbabura", "serra", vac$regiao)
vac$regiao <- gsub("loja", "serra", vac$regiao)
vac$regiao <- gsub("esmeraldas", "litoral", vac$regiao)
vac$regiao <- gsub("los rios", "litoral", vac$regiao)
vac$regiao <- gsub("manabi", "litoral", vac$regiao)
vac$regiao <- gsub("morona santiago", "amazonia", vac$regiao)
vac$regiao <- gsub("napo", "amazonia", vac$regiao)
vac$regiao <- gsub("orellana", "amazonia", vac$regiao)
vac$regiao <- gsub("pastaza", "amazonia", vac$regiao)
vac$regiao <- gsub("pichincha", "serra", vac$regiao)
vac$regiao <- gsub("santo domingo de los tsachilas", "litoral", vac$regiao)
vac$regiao <- gsub("sucumbios", "amazonia", vac$regiao)
vac$regiao <- gsub("tungurahua", "serra", vac$regiao)
vac$regiao <- gsub("zamora chinchipe", "amazonia", vac$regiao)
table(vac$regiao)

colnames(vac)

vac %>%
  filter(year=="2019")%>%
  summarise(vacunados=length(unique(
    paste(Identificación.Propietario,Nombre.Sitio))))


# Proporcao de vacinacao
vac %>%
  group_by(regiao, year)%>%
  # filter(year=="2017")%>%
  summarise(vacunados=length(unique(
    paste(Identificación.Propietario,Nombre.Sitio))))

# Numero de animais vaccinados
vac %>%
  # group_by(regiao,year)%>%
  filter(year=="2019")%>%
  summarise(sum(Número.Productos.Vacunados))

# Proporcao por categoria de risco
vac %>%
  group_by(regiao,year)%>%
  filter(year=="2017")%>%
  summarise(nvac=sum(Número.Productos.Vacunados),
            por=nvac/1811077)
   regiao    year `sum(Número.Productos.Vacunados)`
1 amazonia  2017                             60637  0.03
2 litoral   2017                           1102811  0.609
3 serra     2017                            647629  0.358

colnames(vac)

vac %>% 
  group_by(floor_date(dmy(Fecha.Vacunacion), unit = "month")) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados))

v <- vac %>% 
  group_by(floor_date(dmy(Fecha.Vacunacion), unit = "month")) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados))

summary(v$vacunados)

#
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month")) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  geom_line(aes(mes, mean(vacunados)), colour="grey")

vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# Por provincia
# Pichincha
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "Pichincha") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# Por provincia
# carchi
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "Carchi") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# imbabura
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "Imbabura") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# Cotopaxi
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "Cotopaxi") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# El Oro
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "El Oro") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)

# Santo Domingo
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "Santo Domingo de los Tsáchilas") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)+
  theme_minimal()


# todas las provincias
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna, Provincia.Sitio) %>% 
  filter(Tipo.de.Vacuna == "Normal(Permanente)") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  facet_wrap(~Provincia.Sitio, nrow=4)

vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna, Provincia.Sitio) %>% 
  filter(Tipo.de.Vacuna == "Autoservicio") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  facet_wrap(~Provincia.Sitio, nrow=4)


vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_path(aes(mes, vacunados, color=Tipo.de.Vacuna))
  

library(tidyverse); library(ggplot2); library(lubridate)
# 4 Banco datos certificados ----
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2022/")
cr <- read_csv("Certificados.PPC_17-22.csv", col_names = FALSE, trim_ws = FALSE)

colnames(cr) <- cr[1,]
colnames(cr)[3] <- "numero.certificado"
colnames(cr)[5] <- "fecha.registro"
colnames(cr)[6] <- "fecha.vacunacion"
colnames(cr)[7]
colnames(cr)
cr <- data.frame(cr)
cr <- cr[-1,]

str(cr)
colnames(cr)
#number of animals
cr$numero.productos.vacunados <- as.numeric(cr$CONTAR)
cr$CONTAR <- NULL

# number of certificates CUV
length(unique(cr$numero.certificado)) #857667 # 1032827

# number of vaccined animals
sum(cr$numero.productos.vacunados) #13040325 # 15975181

#Adding year of vaccination
cr$year <- year(dmy(cr$fecha.vacunacion))
cr$month <- month(dmy(cr$fecha.vacunacion))

yv <- cr %>%
  group_by(year)%>%
  summarise(vac=sum(numero.productos.vacunados, na.rm = TRUE))

#Adding year of registry
cr$year_r <- year(dmy(substr(cr$fecha.registro, 1, 10)))
cr$month_r <- month(dmy(substr(cr$fecha.registro, 1, 10)))

yr <- vac %>%
  group_by(year_r)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))

# Pregunta, porque la fecha de registro esta hasta el 2021 y la fecha de vacunacion esta hasta el 2022

#Cuantos CSMI tenemos de diferencia
table(cr$numero.certificado[cr$year != "2022"] %in% vac$Número.Certificado)
# FALSE   TRUE 
# 47702 857696 

# Vacunación por reporte de aretes
cr %>% 
  group_by(mes=floor_date(dmy(fecha.vacunacion), unit = "month")) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(numero.productos.vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  geom_line(aes(mes, mean(vacunados)), colour="grey")

# Analisis de cobertura ----
cobertura <- cr %>% 
  group_by(mes=floor_date(dmy(fecha.vacunacion), unit = "month")) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(numero.productos.vacunados))

summary(cobertura$vacunados)

cobertura$por <- round(cobertura$vacunados/max(cobertura$vacunados), 2)*100
summary(cobertura$por)


ggplot(cobertura, aes(mes, por))+
  geom_point()
 
setwd("~/Dropbox/0.USP/7.Publicações/Mathematical modelling CSFV/Analise/")
write.csv(cobertura, file = "cobertura_vac_2017_21.csv")


<- cr %>% 
  group_by(mes=floor_date(dmy(fecha.vacunacion), unit = "month")) %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(numero.productos.vacunados))



# El Oro
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna) %>% 
  filter(Provincia.Sitio == "El Oro") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  # facet_grid(rows = Tipo.de.Vacuna)
  facet_wrap(~Tipo.de.Vacuna, nrow=4)


# todas las provincias
vac %>% 
  group_by(mes=floor_date(dmy(Fecha.Vacunacion), unit = "month"), Tipo.de.Vacuna, Provincia.Sitio) %>% 
  filter(Tipo.de.Vacuna == "Normal(Permanente)") %>% 
  filter(year > 2015) %>% 
  summarise(vacunados=sum(Número.Productos.Vacunados)) %>% 
  ggplot()+
  geom_line(aes(mes, vacunados))+
  facet_wrap(~Provincia.Sitio, nrow=4)



# 4 Banco datos aretes ----
setwd("~/Backup/USP/Projeto fapesp/Dados/vacinacao/2022/")
cr <- read_csv(file="Aretes.PPC_17-19.csv", col_names = FALSE, trim_ws = FALSE)


a <- read_csv("Aretes.PPC_17-19.csv", col_names = FALSE)

colnames(a) <- a[1,]
colnames(a)[2] <- "identificador.producto"
colnames(a)[4] <- "fecha.vacunacion"
a <- a[-1,] 
a <- a[-1,] 
a <- a[-1,] 
a <- data.frame(a)
str(a)


#number of animals
a$numero.vacunados <- as.numeric(a$CONTAR)
a$CONTAR <- NULL

table(is.na(a$identificador.producto))
# number of vaccined animals
sum(a$numero.vacunados, na.rm=TRUE) #13040325 # 15975181 #7324155

#Adding year of vaccination
a$year <- year(dmy(a$fecha.vacunacion))
a$month <- month(dmy(a$fecha.vacunacion))

yv <- a %>%
  group_by(year)%>%
  summarise(vac=sum(numero.vacunados, na.rm = TRUE))


revacu <- a %>% 
  group_by(year, Producto, identificador.producto) %>% 
  summarise(numero.registros=n(),
            vacunados=sum(numero.vacunados),
            revacunados=table(duplicated(identificador.producto)))


revacu <- a %>% 
  group_by(year, Producto, identificador.producto) %>% 
  filter(year== "2018") %>% 
    summarise(numero.registros=n(),
            vacunados=sum(numero.vacunados),
            revacunados=table(duplicated(identificador.producto)))



revacu <- a %>% 
  group_by(year, Producto) %>% 
  filter(year== "2018") %>% 
  summarise(numero.registros=n(),
            vacunados=sum(numero.vacunados),
            revacunados=table(duplicated(identificador.producto)))




table(duplicated(a$identificador.producto[a$year == "2019"]))

table()
dupli

a$identificador.producto




















# 6 Fiscalizacção vacinação ----
library(tidyverse)
library(lubridate)
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/fiscalizacion/codigo_sitio")
fv1 <- readxl::read_excel("fiscalizacion2016.xlsx")
fv2 <- readxl::read_excel("fiscalizacion2017.xlsx")
fv3 <- readxl::read_excel("fiscalizacion2018.xlsx")
fv4 <- readxl::read_excel("fiscalizacion2019.xlsx")

fv <- rbind(fv2,fv3,fv4)
fv <-fv %>% mutate(ano=substring(fecha_fiscalizacion, 1,4))


#8626 fiscalizaçoes
length(unique(fv$codigo_sitio)) #6213 predios fiscalizados

fv %>% 
  group_by(ano)%>%
  summarise(n=n(),
            predios_fiscalizados = length(unique(codigo_sitio)),
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))

#   ano       n predios_fiscalizados   pos   neg
# 2 2016   2316                 2125  2183   133
# 3 2017   9516                 7374  8646   870
# 4 2018   8584                 6184  7676   908
# 5 2019  10120                 7136  8660  1460

fv %>% 
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion))
  )%>%
  filter(ano=="2019")%>%
  summarise(fiscalizaçoes=n(),
            predios_fiscalizados = length(unique(codigo_sitio)),
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))



fv %>% 
  group_by(ano,estado_fiscalizacion) %>%
  summarise(fisc=n(), n_predios=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=n_predios)

# ano   negativo positivo
# 2 2016       127     2013
# 3 2017       719     6824
# 4 2018       721     5645
# 5 2019      1141     6236

# Replacing every year manually
fv %>%
  group_by(ano,estado_fiscalizacion) %>%
  filter(ano=="2019")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano=="2019"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

#    ano   negativo positivo
# 1 2017         7       38
# 2 2018         8       38
# 3 2019         8       37

fv %>%
  group_by(ano,
           estado_fiscalizacion, 
           month=month(ymd(fecha_fiscalizacion))
           ) %>%
  filter(ano == "2017")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

#    ano   month negativo positivo
# 1  2017      1       NA        3
# 2  2017      2       NA        2
# 3  2017      3       NA        6
# 4  2017      4       NA        5
# 5  2017      5       NA        5
# 6  2017      6        3        5
# 7  2017      7        1        9
# 8  2017      8        2        3
# 9  2017      9        1        1
# 10 2017     10        1        6
# 11 2017     11       NA        4
# 12 2017     12       NA        3


# Identificando os casos positivos a PPC
# ano   negativo  positivo
# 2017        0          10
# 2018        6          20
# 2019        1           7

fv %>%
  group_by(ano,estado_fiscalizacion, 
           month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2017")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


fv %>%
  group_by(ano,estado_fiscalizacion, 
           # month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2018")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2018" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


fv %>%
  group_by(ano,estado_fiscalizacion, 
           # month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2019")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2019" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


# Fiscalizacion olhar o numero de visitas que tecnicos fazem
fv %>% 
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           provincia,usuario_responsable)%>%
  # filter(ano=="2017")%>%
  summarise(n=n()/20,
            predios_fiscalizados = length(unique(codigo_sitio))/20,
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))%>%
  arrange(desc(predios_fiscalizados))

# Ruben dario mar 419 fiscalizaciones ano 2019



# Codigo para olhar se as fiscalizacoes tem relacao com as notificacoes
# Crio um codigo que filtre os predios pelo nome da cedula, e ano, fazendo
# relacao com a data da fiscalizacao e a data da notificacao, se eles estiverem
# 30 dias antes o depois da notificacao entendemos que tem relacao.
# incluimos tambem um filtro para saber se gerou um caso de PSC ou nao
v2017 <- fv %>%
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           identificacion_propietario,
           estado_fiscalizacion, 
  ) %>%
  filter(ano == "2018")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2018" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

v2017$fecha_fisc <- fv$fecha_fiscalizacion[match(v2017$identificacion_propietario,
                                                 fv$identificacion_propietario)]

v2017$fecha_fisc <- ymd(v2017$fecha_fisc)
v2017$fecha_noti <- v2$f_notificación[match(v2017$identificacion_propietario,
                                            v2$identificador_operador)]
v2017$fecha_noti <- dmy(v2017$fecha_noti)
v2017$dias_not2fisc <- v2017$fecha_fisc - v2017$fecha_noti 

v17 <-v2017
v18 <-v2017
v19 <-v2017


# Conferir com o arquivo v19 os dias de diferenca -/+ 30



# 4 Movimentação ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
mov18 <- readxl::read_excel("movilizacion2019.xlsx")
length(unique(mov18$numero_certificado))# 2017:321482 ; 2018:401189 2019:489337 ;movimentações
sum(as.numeric(mov18$cantidad)) #2017:3005958; 2018:3472296; 2019:3941454

mov18 <- data.frame(mov18)
banco <- createUniqueIds(mov18,
                         from = "codigo_sitio_origen",
                         to= "codigo_sitio_destino")



library(epinemo); library(tidyverse)

# 3 Predios que movimentaram ----
length(unique(banco$correspondence$network.id)) #2017:66787 ; 2018:82253; 2019:101547

#5 Fiscalização de movimentacao
# Ficalización movimentação
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/fiscalizacion/codigo_sitio")
fm <- read.csv("fiscalizacao2016-2019.csv", colClasses = "character")

# setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/fiscalización 09.2016-12.2018")
# fm18 <- read.csv(file = "fiscalmov2016-2018.csv", colClasses = "character")
# fm18 <- fm18 %>%
#   mutate(ano = substring(fecha.fiscalizacion, 1, 4))

#fm18 <-fm18[fm18$ano == "2018",]

#fm18$sitio <- paste(fm18$identificacion.propietario, fm18$sitio.origen)
#200075 fiscalizacoes de movimentação
table(fm$tipo_usuario)

# numero de fiscalizacoes
fm %>%
  group_by(ano) %>%
  summarise(n())

# ano    `n()`
# 1 2016    1602
# 2 2017  146771
# 3 2018  202176
# 4 2019  247315

fm %>%
  group_by(ano, tipo_usuario) %>%
  summarise(fiscalizadores=length(unique(identificacion_responsable_fiscalizacion)))


fm %>%
  select(ano, codigo_sitio_origen, tipo_usuario)%>%
  group_by(ano) %>%
  summarise(predios=length(unique(codigo_sitio_origen)))

#   ano   predios
# 1 2016      370
# 2 2017    32605
# 3 2018    40992
# 4 2019    49717

fm %>% group_by(ano, resultado, tipo_usuario) %>%
  filter(ano != "2016")%>%
  #filter(tipo_usuario == "usuario externo")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key = tipo_usuario, value=fisc)

# 1 2017  negativo   3611
# 2 2017  positivo  31611
# 3 2018  negativo   4947
# 4 2018  positivo  39543
# 5 2019  negativo   4756
# 6 2019  positivo  48581

31611/(31611+3611)

table(fm$accion_correctiva)


# Fiscalizacoes cada mes
# Para calcular sensibilidad na movimentaçao 7.04.22----

library(lubridate)

# Anuais
fm %>%
  select(ano, resultado,
         codigo_sitio_origen, 
         tipo_usuario, 
         fecha_fiscalizacion,
         accion_correctiva)%>%
  group_by(ano, tipo_usuario) %>%
  filter(ano == 2017)%>%
  # filter(resultado=="negativo")%>%
  filter(accion_correctiva=="inactivar emision de certificado")%>%
  summarise(predios=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=predios)


fm %>%
  select(ano, resultado,
         codigo_sitio_origen, 
         tipo_usuario, 
         fecha_fiscalizacion,
         accion_correctiva)%>%
  group_by(ano, month(ymd(fecha_fiscalizacion)), tipo_usuario) %>%
  filter(ano == 2017)%>%
  filter(accion_correctiva=="inactivar emision de certificado")%>%
  summarise(predios=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=predios)


fm %>%
  select(ano, codigo_sitio_origen, tipo_usuario, fecha_fiscalizacion)%>%
  group_by(ano, month(ymd(fecha_fiscalizacion))) %>%
  filter(ano == 2017)%>%
  summarise(predios=length(unique(codigo_sitio_origen)))


# Fiscalizações por interno ou externo predios
library(tidyr)
fm %>% group_by(ano, tipo_usuario, resultado) %>%
  filter(ano != "2016")%>%
  filter(tipo_usuario == "usuario externo")%>%
  # filter(tipo_usuario == "usuario interno")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=resultado, value=fisc)

# ano   tipo_usuario       negativo positivo
# 1 2017  usuario externo     1656  + 25353
# 2 2018  usuario externo     2620    32383
# 3 2019  usuario externo     2111    41666 

# ano   tipo_usuario       negativo positivo
# 1 2017  usuario interno     2259     9741
# 2 2018  usuario interno     2609    10955
# 3 2019  usuario interno     2946    10284

# 2017
# externo 
(1656 + 25353)/((2259 +  9741)+(1656 + 25353))
#0.6923 de fiscalizacoes efetuadas pelos externos

#internos
(2259+  9741)/((2259+  9741)+(1656+ 25353))

(1656)/(1656+ 25353)
# 0.0613 Externo ficalizacao negativa

#interno 
2259/(2259+  9741)
#0.188 Interno fiscalizacao negativa





# Numero de fiscalizações inactivação de usuario
# amostra de vigilancia por movimentacao
#mudar ano e usuario para reporte
fm %>%
  group_by(ano,
           month(ymd(fecha_fiscalizacion)),
           tipo_usuario, resultado, accion_correctiva) %>%
  filter(ano == "2019")%>%
  filter(accion_correctiva == "inactivar emision de certificado")%>%
  filter(tipo_usuario == "usuario externo")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=resultado, value=fisc)






# Fiscalização de movimentação que termina em notificação
fm %>% select(identificacion_propietario,ano, codigo_sitio_origen, tipo_usuario, resultado, 
              accion_correctiva)%>%
  group_by(ano,tipo_usuario, resultado) %>%
  filter(resultado == "negativo") %>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v2$identificador_operador)%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)

# ano   resultado                `externo` `interno`
# 1 2016  negativo                 NA             1
# 2 2017  negativo                 33             7
# 3 2018  negativo                 30            15
# 4 2019  negativo                 33             8


month(ymd(fecha_fiscalizacion))

# Fiscalização de movimentação que termina em notificação
fm %>% select(identificacion_propietario,
              ano, 
              codigo_sitio_origen, 
              tipo_usuario, 
              resultado, 
              accion_correctiva,
              fecha_fiscalizacion)%>%
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           tipo_usuario 
  ) %>%
  filter(resultado == "negativo") %>%
  filter(ano == 2017)%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017"])%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)





# Fiscalização que terminou em notificação e fez um caso positivo
fm %>% select(identificacion_propietario,ano, codigo_sitio_origen, tipo_usuario, resultado, 
              accion_correctiva)%>%
  group_by(ano,tipo_usuario, resultado) %>%
  filter(resultado == "negativo") %>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v3$identificador_operador)%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)

# ano   resultado `usuario externo` `usuario interno`
# 1 2017  negativo            5                NA
# 2 2018  negativo            4                 1
# 3 2019  negativo            3                 3

# Para identificar as fiscalizações que terminaram como caso positivo
table(fm$identificacion_propietario %in% v3$identificador_operador)
# 0 zero


# ppc em 2 CSMI usuario interno

#quantas fiscalizações se encontran nos eventos sanitarios
table(fm18$identificacion.propietario %in% v2$cedula)
# FALSE  TRUE 
# 199388  687

fm18 %>% group_by(tipo.usuario, resultado, accion.correctiva) %>%
  #filter(resultado == "negativo")%>%
  filter(identificacion.propietario %in% v2$cedula) %>%
  summarise(fisc=n())%>%
  spread(key=resultado, value=fisc)

fm18 %>% group_by(tipo.usuario, resultado, accion.correctiva) %>%
  filter(identificacion.propietario %in% v2$cedula) %>%
  summarise(fisc=n())%>%
  spread(key=resultado, value=fisc)






table(fis18$resultado)
#9066 negativo

table(fis18$accion.correctiva)
# Anular certificado 3981
# Inactivar emision 5085




#olhando o cadastro quantos codigos de sitio existem
# Banco Cadastro 2019-sept ----
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")
c <- readxl::read_excel("catastroProdCom2019-09-18.xlsx")
length(unique(c$codigo.sitio)) #207735


# Adding region
# creating the regions columns
c$pro <- tolower(iconv(c$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(c$pro)
c$regiao <- c$pro

c$regiao <- gsub("azuay", "serra", c$regiao)
c$regiao <- gsub("bolivar", "serra", c$regiao)
c$regiao <- gsub("canar", "serra", c$regiao)
c$regiao <- gsub("carchi", "serra", c$regiao)
c$regiao <- gsub("chimborazo", "serra", c$regiao)
c$regiao <- gsub("cotopaxi", "serra", c$regiao)
c$regiao <- gsub("el oro", "litoral", c$regiao)
c$regiao <- gsub("santa elena", "litoral", c$regiao)
c$regiao <- gsub("guayas", "litoral", c$regiao)
c$regiao <- gsub("imbabura", "serra", c$regiao)
c$regiao <- gsub("loja", "serra", c$regiao)
c$regiao <- gsub("esmeraldas", "litoral", c$regiao)
c$regiao <- gsub("los rios", "litoral", c$regiao)
c$regiao <- gsub("manabi", "litoral", c$regiao)
c$regiao <- gsub("morona santiago", "amazonia", c$regiao)
c$regiao <- gsub("napo", "amazonia", c$regiao)
c$regiao <- gsub("orellana", "amazonia", c$regiao)
c$regiao <- gsub("pastaza", "amazonia", c$regiao)
c$regiao <- gsub("pichincha", "serra", c$regiao)
c$regiao <- gsub("santo domingo de los tsachilas", "litoral", c$regiao)
c$regiao <- gsub("sucumbios", "amazonia", c$regiao)
c$regiao <- gsub("tungurahua", "serra", c$regiao)
c$regiao <- gsub("zamora chinchipe", "amazonia", c$regiao)

c$regiao <- gsub("amazonia", "3amazonic", c$regiao)
c$regiao <- gsub("litoral", "3coastal", c$regiao)
c$regiao <- gsub("serra", "1highlands", c$regiao)
table(c$regiao)
table(c$regiao)
c$
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
cad17 <- read.csv(file="cad2017.csv", colClasses = "character")
colnames(cad17)

cad17$pro <- tolower(iconv(cad17$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad17$pro)
cad17$pro <- tolower(iconv(cad17$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad17$pro)
cad17$regiao <- cad17$pro

cad17$regiao <- gsub("azuay", "serra", cad17$regiao)
cad17$regiao <- gsub("bolivar", "serra", cad17$regiao)
cad17$regiao <- gsub("canar", "serra", cad17$regiao)
cad17$regiao <- gsub("carchi", "serra", cad17$regiao)
cad17$regiao <- gsub("chimborazo", "serra", cad17$regiao)
cad17$regiao <- gsub("cotopaxi", "serra", cad17$regiao)
cad17$regiao <- gsub("el oro", "litoral", cad17$regiao)
cad17$regiao <- gsub("santa elena", "litoral", cad17$regiao)
cad17$regiao <- gsub("guayas", "litoral", cad17$regiao)
cad17$regiao <- gsub("imbabura", "serra", cad17$regiao)
cad17$regiao <- gsub("loja", "serra", cad17$regiao)
cad17$regiao <- gsub("esmeraldas", "litoral", cad17$regiao)
cad17$regiao <- gsub("los rios", "litoral", cad17$regiao)
cad17$regiao <- gsub("manabi", "litoral", cad17$regiao)
cad17$regiao <- gsub("morona santiago", "amazonia", cad17$regiao)
cad17$regiao <- gsub("napo", "amazonia", cad17$regiao)
cad17$regiao <- gsub("orellana", "amazonia", cad17$regiao)
cad17$regiao <- gsub("pastaza", "amazonia", cad17$regiao)
cad17$regiao <- gsub("pichincha", "serra", cad17$regiao)
cad17$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad17$regiao)
cad17$regiao <- gsub("sucumbios", "amazonia", cad17$regiao)
cad17$regiao <- gsub("tungurahua", "serra", cad17$regiao)
cad17$regiao <- gsub("zamora chinchipe", "amazonia", cad17$regiao)

cad17$regiao <- gsub("amazonia", "3amazonic", cad17$regiao)
cad17$regiao <- gsub("litoral", "3coastal", cad17$regiao)
cad17$regiao <- gsub("serra", "1highlands", cad17$regiao)
table(cad17$regiao)
table(cad17$regiao)

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2018")
cad18 <- read.csv(file="cad2018.csv", colClasses = "character")
length(unique(paste(cad18$identificacion.propietario, cad18$nombre.sitio))) #126168
sum(as.numeric(cad18$cantidad)) #2354109

cad18$pro <- tolower(iconv(cad18$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad18$pro)
cad18$regiao <- cad18$pro

cad18$regiao <- gsub("azuay", "serra", cad18$regiao)
cad18$regiao <- gsub("bolivar", "serra", cad18$regiao)
cad18$regiao <- gsub("canar", "serra", cad18$regiao)
cad18$regiao <- gsub("carchi", "serra", cad18$regiao)
cad18$regiao <- gsub("chimborazo", "serra", cad18$regiao)
cad18$regiao <- gsub("cotopaxi", "serra", cad18$regiao)
cad18$regiao <- gsub("el oro", "litoral", cad18$regiao)
cad18$regiao <- gsub("santa elena", "litoral", cad18$regiao)
cad18$regiao <- gsub("guayas", "litoral", cad18$regiao)
cad18$regiao <- gsub("imbabura", "serra", cad18$regiao)
cad18$regiao <- gsub("loja", "serra", cad18$regiao)
cad18$regiao <- gsub("esmeraldas", "litoral", cad18$regiao)
cad18$regiao <- gsub("los rios", "litoral", cad18$regiao)
cad18$regiao <- gsub("manabi", "litoral", cad18$regiao)
cad18$regiao <- gsub("morona santiago", "amazonia", cad18$regiao)
cad18$regiao <- gsub("napo", "amazonia", cad18$regiao)
cad18$regiao <- gsub("orellana", "amazonia", cad18$regiao)
cad18$regiao <- gsub("pastaza", "amazonia", cad18$regiao)
cad18$regiao <- gsub("pichincha", "serra", cad18$regiao)
cad18$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad18$regiao)
cad18$regiao <- gsub("sucumbios", "amazonia", cad18$regiao)
cad18$regiao <- gsub("tungurahua", "serra", cad18$regiao)
cad18$regiao <- gsub("zamora chinchipe", "amazonia", cad18$regiao)

cad18$regiao <- gsub("amazonia", "3amazonic", cad18$regiao)
cad18$regiao <- gsub("litoral", "3coastal", cad18$regiao)
cad18$regiao <- gsub("serra", "1highlands", cad18$regiao)
table(cad18$regiao)
table(cad18$regiao)


setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2019/antigos/")
cad19 <- read.csv(file="cad2019.csv", colClasses = "character")

cad19$pro <- tolower(iconv(cad19$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad19$pro)
cad19$regiao <- cad19$pro

cad19$regiao <- gsub("azuay", "serra", cad19$regiao)
cad19$regiao <- gsub("bolivar", "serra", cad19$regiao)
cad19$regiao <- gsub("canar", "serra", cad19$regiao)
cad19$regiao <- gsub("carchi", "serra", cad19$regiao)
cad19$regiao <- gsub("chimborazo", "serra", cad19$regiao)
cad19$regiao <- gsub("cotopaxi", "serra", cad19$regiao)
cad19$regiao <- gsub("el oro", "litoral", cad19$regiao)
cad19$regiao <- gsub("santa elena", "litoral", cad19$regiao)
cad19$regiao <- gsub("guayas", "litoral", cad19$regiao)
cad19$regiao <- gsub("imbabura", "serra", cad19$regiao)
cad19$regiao <- gsub("loja", "serra", cad19$regiao)
cad19$regiao <- gsub("esmeraldas", "litoral", cad19$regiao)
cad19$regiao <- gsub("los rios", "litoral", cad19$regiao)
cad19$regiao <- gsub("manabi", "litoral", cad19$regiao)
cad19$regiao <- gsub("morona santiago", "amazonia", cad19$regiao)
cad19$regiao <- gsub("napo", "amazonia", cad19$regiao)
cad19$regiao <- gsub("orellana", "amazonia", cad19$regiao)
cad19$regiao <- gsub("pastaza", "amazonia", cad19$regiao)
cad19$regiao <- gsub("pichincha", "serra", cad19$regiao)
cad19$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad19$regiao)
cad19$regiao <- gsub("sucumbios", "amazonia", cad19$regiao)
cad19$regiao <- gsub("tungurahua", "serra", cad19$regiao)
cad19$regiao <- gsub("zamora chinchipe", "amazonia", cad19$regiao)

cad19$regiao <- gsub("amazonia", "3amazonic", cad19$regiao)
cad19$regiao <- gsub("litoral", "3coastal", cad19$regiao)
cad19$regiao <- gsub("serra", "1highlands", cad19$regiao)
table(cad19$regiao)

table(cad19$identificacion.propietario %in% c$identificacion.propietario)

# Relação entre propriedades e proprietarios
length(unique(c$codigo.sitio))-length(unique(c$identificacion.propietario))
207735/202302
1.026 mais propriedades que predios

#cuantos proprietarios se encontraram registrados em cualquer ano              
proprietarios <- length(unique(c(c$identificacion.propietario,
                cad17$identificacion.propietario,
                cad18$identificacion.propietario,
                cad19$identificacion.propietario)))

# 258997 propietarios. Asumo que cada proprietario tem 1.026 propriedades
# propriedades <- 258997*1.026
# 264176.9

propriedades/407475
propriedades/342421
0.648 considerando as propriedades do censo 2000

#vou fazer comparacoes com o maior número de propriedades ja cadastradas
#Maior numero de sitios do cadastro histórico ----
cadastro_total <-length(unique(c(c$codigo.sitio))) #2018
length(unique(cad17$identificacion.propietario))

a <- length(unique(c$identificacion.propietario))*1.026
207735
207561.9

# 2017
b <- length(unique(cad17$identificacion.propietario))
b <- length(unique(cad17$nombre.sitio))
b <- length(unique(paste(cad17$identificacion.propietario, cad17$nombre.sitio)))
cad17$codigo.sitio <- paste(cad17$identificacion.propietario, cad17$nombre.sitio)

length(unique(cad17$codigo.sitio))
cad17 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/105542)

#   regiao         n `n/105542`
# 1 1highlands 82539     0.782 
# 2 3amazonic   5081     0.0481
# 3 3coastal   17922     0.170 

# 2018
d <- length(unique(cad18$identificacion.propietario))
123713
# 2019
e <- length(unique(cad19$identificacion.propietario))
103296

cad18$codigo.sitio <- paste(cad18$identificacion.propietario, cad18$nombre.sitio)

length(unique(cad18$codigo.sitio))

cad18 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/126168)

# regiao         n `n/126168`
# 1 1highlands 97394     0.772 
  # 2 3amazonic   6665     0.0528
# 3 3coastal   22114     0.175 


cad19$codigo.sitio <- paste(cad19$identificacion.propietario, cad19$nombre.sitio)
length(unique(cad19$codigo.sitio))

cad19 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/105083)

# regiao         n `n/105083`
# 1 1highlands 78588     0.748 
# 2 3amazonic   5524     0.0526
# 3 3coastal   20973     0.200 



cadastro_anual <- c(b,d,e)
proporcao_cadastro <- cadastro_anual/a 

mean(proporcao_cadastro)
sd(proporcao_cadastro)



# comparacção existencias animais ----
sum(as.numeric(cad17$cantidad))
1204824
sum(as.numeric(cad18$cantidad))
1311989
sum(as.numeric(cad19$Cantidad.activos), na.rm = TRUE) + sum(as.numeric(cad19$Cantidad.inactivos), na.rm = TRUE)

# Cadastro (2017,2018,2019)
ESPAC (2017,2018,2019)
1115473
1283338
1162685

Agrocalidad (2017,2018,2019) Considering mataderos and ferias
1204824
1311989
1504601

withouth mataderos e ferias
1097469
1207607
1111668

Agrocalidad/ESPAC (2017,2018,2019)
1204824/1115473  8.01%
1311989/1283338  2.23%
1504601/1162685  29.41%

# Withouth mataderos
1-1097469/1115473  -1.6%
1-1207607/1283338  -5.9%
1-1111668/1162685  -4.4%


length(unique(paste(cad19$identificacion.propietario, 
                    cad19$nombre.sitio)))




# Comparar com codigo

# Notificacoes de ppc que tiveram diagnostico
v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(ano,amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

#     ano amostrado notifi surtos amostras
# 1  2015 0              1      0        0
# 2  2015 1            218     82     1572
# 3  2016 1            127     30      844
# 4  2017 1            160     39     1000
# 5  2018 1            178     60     1212
# 6  2019 1            160     35      961

v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

library(tidyverse)
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado, detalle_diagnóstico)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

length(unique(v2$orden[v2$ano >2014 & v2$ano <2020],))
932+486

v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

#Porcentagem de amostragem
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)))%>%
  spread(key="amostrado", value="notifi")

# % de amostragem nas notificacoes (desestimacao de casos)
1  2017   116   /175  0.6628
2  2018    92   /214  0.4299
3  2019   105   /168  0.625

# 2017:
122/(175+116) #notificaciones 
c <-v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  filter(amostrado == 0)%>%
  group_by( amostrado, patología)%>%
  summarise(notifi=length(unique(orden)))




# Riscos ajustados
library(RSurveillance)
# Vigilancia ativa
# Lavagen
round(adj.risk(c(1,17.61), c(0.167,0.833)),2)
# 0.07 1.19

adj.risk(c(1,18), c(0.167,0.833))
# [1] 0.06596 1.18726

#vaccination coverage
sum(0.815, 0.185)
round(adj.risk(c(1,1.92), c(0.815, 0.185)),4)
# 0.8546 1.6407

#age
sum(0.815, 0.185)
round(adj.risk(c(1,1.57,1.45), c(0.131, 0.816, 0.053)),3)
# [1] 0.672 1.054 0.974

1	0,131
1,57	0,816
1,45	0,053



