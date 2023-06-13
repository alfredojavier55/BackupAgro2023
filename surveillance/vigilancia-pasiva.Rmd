---
title: "Vigilancia_notificaciones_brotes"
author: "Alfredo Acosta"
date: "24/02/2023"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Notificaciones y brotes Vigilancia SIZSE
# V 0.01 
# Projecto Estimacion de la sensibilidad del sistema de vigilancia epidemiologica PPC
# Autor Alfredo Acosta PhD
# alfredoacosta@alumni.usp.br
# EPIDAT
# University of Sao Paulo
# Preventive veterinary medicine department
# https://github.com/alfredojavier55/Preparing_agro_data

```{r}
library(dplyr);library(lubridate);library(ggplot2); library(scales)

```


# Juntar archivos vigilancia (join-vig) ----

```{r}
# setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados/2021")
setwd("~/Dropbox/1.Epidat/Consultoria Agro/vigilancia-2023/")

# Colnames correctos ----
vge0 <- read.csv("Rep_General_EventosSan0.csv", colClasses = "character",encoding = "UTF-8")
vr0 <- read.csv("Rep_ResultadosGEV0.csv", colClasses = "character",encoding = "UTF-8")
vc0 <- read.csv("Rep_CierreGEV0.csv", colClasses = "character",encoding = "UTF-8")

# Archivos de general ----
vge1 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2014.xls")
vge2 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2015.xls")
vge3 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2016.xls")
vge4 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2017.xls")
vge5 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2018.xls")
vge6 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2019.xls")
vge7 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2020.xls")
vge8 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2021.xls")

vge <- rbind(vge1, vge2, vge3, vge4, vge5, vge6, vge7, vge8)
# vge1 <- readxl::read_excel("VEPG02_REP_GENERAL_BASICO 2015.xls")
# colnames(vge) <- vge1[1,]

# colnames(vge0)
# colnames(vge)

# Reordenar y revisar bien Cristian ----
vge2 <- vge[,c(1,5:7,9,10,4,8,8,8,11:18,
              20:24,3,3,27)]
colnames(vge2)
# por algo agregaron las columnas y las deberian usar
# vge <- vge[,c(1,5:7,9,10,4,8,8,8,11:18,
#               20:24,3,3,27,  2,19,25,26)]
# 
# colnames(vge)[27] <- "Alerta"

colnames(vge2) <- colnames(vge0)

vge2 <- cbind(vge2, vge[,c(2,19,25,26)])

# Archivos de resultados ----
vr1 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2014.xls")
vr2 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2015.xls")
vr3 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2016.xls")
vr4 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2017.xls")
vr5 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2018.xls")
vr6 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2019.xls")
vr7 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2020.xls")
vr8 <- readxl::read_excel("VEPG02_REP_RESULTADOS 2021.xls")

vr <- rbind(vr1, vr2, vr3, vr4, vr5, vr6, vr7, vr8)

# colnames(vr)
# colnames(vr2)

vr2 <- vr[,c(1,5:7,9,10,4,8,8,8,11:18,20,21,3,3,27,28,29,33,32,34:39,41,43,44,42)]

colnames(vr2) <- colnames(vr0)
colnames(vr2)

vr2 <- cbind(vr2, vr[,c(2,19,22,23,24,25,26,30,31,40,45,46)])

# Archivos de cierre ----
vc1 <- readxl::read_excel("VEPG02_REP_CIERRE 2014.xls")
vc2 <- readxl::read_excel("VEPG02_REP_CIERRE 2015.xls")
vc3 <- readxl::read_excel("VEPG02_REP_CIERRE 2016.xls")
vc4 <- readxl::read_excel("VEPG02_REP_CIERRE 2017.xls")
vc5 <- readxl::read_excel("VEPG02_REP_CIERRE 2018.xls")
vc6 <- readxl::read_excel("VEPG02_REP_CIERRE 2019.xls")
vc7 <- readxl::read_excel("VEPG02_REP_CIERRE 2020.xls")
vc8 <- readxl::read_excel("VEPG02_REP_CIERRE 2021.xls")

vc <- rbind(vc1, vc2, vc3, vc4, vc5, vc6, vc7, vc8)
# vc1 <- readxl::read_excel("VEPG02_REP_CIERRE 2014.xls")
# colnames(vc) <- vc1[1,]

# colnames(vc0)
# colnames(vc)

# Reordenar ----
vc2 <- vc[,c(1,7:9,11,12,4,10,10,10,13:20,
            22,23,3,3,5,36,26:35)]

colnames(vc2) <- colnames(vc0)

vc2 <- cbind(vc2, vc[,c(2,6,21,24,25)])

vge2$canton <- vge2$cantón
vge2$cantón <- NULL

#Banco Resultados de eventos
vr2$cant_muestras <- as.numeric(vr2$cant_muestras)
vr2$positivos <- as.numeric(vr2$positivos)
vr2$negativos <- as.numeric(vr2$negativos)
vr2$reactivos <- as.numeric(vr2$reactivos)
# vr2$indeterminados <- as.numeric(vr2$indeterminados)
vr2$canton <- vr2$cantón
vr2$cantón <- NULL

# Banco Cierre de eventos
vc2$existentes <- as.numeric(vc2$existentes)
vc2$enfermos <- as.numeric(vc2$enfermos)
vc2$muertos <- as.numeric(vc2$muertos)
vc2$sacrificad <- as.numeric(vc2$sacrificad)
vc2$canton <- vc2$cantón
vc2$cantón <- NULL

# -- Vigilancia general ----
v0 <- full_join(vc2, vr2)
v1 <- full_join(v0,vge2)

table(v1$especie)
# 
# rm(vge0, vr0, vc0)
# rm(vc1,vc3,vc4,vc5,vc6,vc7,vc8)
# rm(vge1,vge3,vge4,vge5,vge6,vge7,vge8)
# rm(vr1,vr2,vr3,vr4,vr5,vr6,vr7,vr8)
# rm(vc2, vge2, vr2, vge)

colnames(v1)

```
```{r}
# Anonimizando el documento
# cambiando cedulas

length(unique(v1$cedula))
v1$cedula <- as.character(as.numeric(v1$cedula)+234-98987)
length(unique(v1$cedula))
length(unique(v1$propietario))

# Cambiando propietarios
v1$propietario <- gsub("A","II", v1$propietario)
v1$propietario <- gsub("E","C(", v1$propietario)
v1$propietario <- gsub("I","J%%J", v1$propietario)
v1$propietario <- gsub("O","P", v1$propietario)
v1$propietario <- gsub("U","P*P", v1$propietario)
v1$propietario <- gsub("I","JJ", v1$propietario)
v1$propietario <- gsub("E","P", v1$propietario)
v1$propietario <- gsub("L","PP", v1$propietario)
length(unique(v1$propietario))
length(unique(v1$predio))
v1$predio <- gsub("A","II", v1$predio)
v1$predio <- gsub("E","C(", v1$predio)
v1$predio <- gsub("I","J%%J", v1$predio)
v1$predio <- gsub("O","P", v1$predio)
v1$predio <- gsub("U","P*P", v1$predio)
v1$predio <- gsub("I","JJ", v1$predio)
v1$predio <- gsub("E","P", v1$predio)
v1$predio <- gsub("L","PP", v1$predio)
length(unique(v1$predio))

```

```{r}
# save(v1, file = "eventos.RDATA")

```

```{r}
# Leyendo 
# cat("\014")
# load("eventos.RDATA")
```


## Ejemplo todas las especies

```{r}
v2 <- v1 %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  # filter(detalle_diagnóstico == "PESTE PORCINA CLÁSICA")%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, 
           zona,
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           detalle_diagnóstico,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), reactivo=sum(reactivos))

# Agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

```

## Total de eventos
```{r, "Numero total de eventos"}
length(unique(v2$orden))
```

```{r}
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")

```

## Total de notificaciones por ano

```{r}
# Numero de ordenes
v2 %>%
  group_by(ano)%>%
  # filter(ano >2020)%>%
  summarise(notifi=length(unique(orden)))

```

## Número de notificaciones por especie

```{r}
# Numero de notificaciones
v2 %>%
  # group_by(month)%>%
  group_by(especie_f)%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  summarise(notifi_geral=length(unique(orden)))%>%
  arrange(desc(notifi_geral))

```
  

## Número de notificaciones por especie alta notificacion

```{r}
# Número de notificaciones por especie alta notificacion
 v2 %>%
  # group_by(month)%>%
  group_by(ano, especie_f)%>%
  filter(ano <2020)%>%
  filter(ano >2013)%>%
  filter(especie_f=="BOVINOS" | especie_f == "PORCINOS"
         | especie_f == "OVINOS" | especie_f == "EQUINOS" | especie_f == "ABEJAS")%>%
  summarise(notifi_geral=length(unique(orden)))%>%
  arrange(desc(notifi_geral))%>%
ggplot()+
  geom_col(aes(ano,notifi_geral), colour="#377EB8")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 200)+
  facet_grid(rows = vars(especie_f))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general",
       x="Año")+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

## Número de notificaciones por especie notificacion intermedia
```{r}

 
 v2 %>%
   # group_by(month)%>%
   group_by(ano, especie_f)%>%
   filter(ano <2020)%>%
   filter(ano >2013)%>%
   filter(especie_f =="CAPRINOS" | especie_f == "OVINOS"
        | especie_f == "CUYES" | especie_f == "AVES"
        | especie_f == "PERROS" | especie_f == "CAMELIDOS" | especie_f == "GATOS")%>%
   summarise(notifi_geral=length(unique(orden)))%>%
   arrange(desc(notifi_geral))%>%
   ggplot()+
   geom_col(aes(ano,notifi_geral), colour="#377EB8")+
   geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 10)+
   facet_grid(rows = vars(especie_f))+
   # scale_y_continuous(breaks= pretty_breaks())+
   labs(y="Notificaciones vigilancia general",
        x="Año")+
   theme_minimal() +
   theme(text = element_text(size = 14))

```

## Número de notificaciones por especie baja notificacion
 
```{r}
 
 v2 %>%
   # group_by(month)%>%
   group_by(ano, especie_f)%>%
   filter(ano <2020)%>%
   filter(ano >2013)%>%
   filter(especie_f =="CERVIDOS" | especie_f == "CONEJOS"
          | especie_f == "MURCIELAGOS" | especie_f == "NO APLICA" | especie_f == "BUBALINOS")%>%
   summarise(notifi_geral=length(unique(orden)))%>%
   arrange(desc(notifi_geral))%>%
   ggplot()+
   geom_col(aes(ano,notifi_geral))+
   geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 1)+
   facet_grid(rows = vars(especie_f))+
   # scale_y_continuous(breaks= pretty_breaks())+
   labs(y="Notificaciones vigilancia general",
        x="Año")+
   theme_minimal() +
   theme(text = element_text(size = 10))
 
```

## Número de notificaciones por especie alta notificacion (2014-2021)
 
```{r}
 v2 %>%
  # group_by(month)%>%
  group_by(ano, especie_f)%>%
  # filter(ano <2020)%>%
  filter(ano >2013)%>%
  filter(especie_f=="BOVINOS" | especie_f == "PORCINOS"
         | especie_f == "OVINOS" | especie_f == "EQUINOS" | especie_f == "ABEJAS")%>%
  summarise(notifi_geral=length(unique(orden)))%>%
  arrange(desc(notifi_geral))%>%
ggplot()+
  geom_col(aes(ano,notifi_geral), colour="#4DAF4A")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 200)+
  facet_grid(rows = vars(especie_f))+
  labs(y="Numero",
       title="Notificaciones vigilancia general por especies 2014-2021",
       x="Año")+
  theme_minimal() +
  theme(text = element_text(size = 12))
```

## Número de notificaciones por especie notificacion intermedia

```{r}
 
 v2 %>%
   # group_by(month)%>%
   group_by(ano, especie_f)%>%
   filter(ano <2020)%>%
   filter(ano >2013)%>%
   filter(especie_f =="CAPRINOS" | especie_f == "OVINOS"
        | especie_f == "CUYES" | especie_f == "AVES"
        | especie_f == "PERROS" | especie_f == "CAMELIDOS" | especie_f == "GATOS")%>%
   summarise(notifi_geral=length(unique(orden)))%>%
   arrange(desc(notifi_geral))%>%
   ggplot()+
   geom_col(aes(ano,notifi_geral), colour="#4DAF4A")+
   geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 10)+
   facet_grid(rows = vars(especie_f))+
   labs(y="Numero",
       title="Notificaciones vigilancia general por especies 2014-2021",
       x="Año")+
    theme_minimal() +
   theme(text = element_text(size = 6))

```

## Número de notificaciones por especie baja notificacion
 
```{r}
 
 v2 %>%
   # group_by(month)%>%
   group_by(ano, especie_f)%>%
   # filter(ano <2020)%>%
   filter(ano >2013)%>%
   filter(especie_f =="CERVIDOS" | especie_f == "CONEJOS"
          | especie_f == "MURCIELAGOS" | especie_f == "NO APLICA" | especie_f == "BUBALINOS")%>%
   summarise(notifi_geral=length(unique(orden)))%>%
   arrange(desc(notifi_geral))%>%
   ggplot()+
   geom_col(aes(ano,notifi_geral),colour="#4DAF4A")+
   geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 1)+
   facet_grid(rows = vars(especie_f))+
   labs(y="Numero",
       title="Notificaciones vigilancia general por especies 2014-2021",
       x="Año")+
   theme_minimal() +
   theme(text = element_text(size = 10))
 
```

## Numero de notificaciones por sindrome presuntivo


```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month,síndrome_presuntivo, especie_f)%>%
  # filter(ano <2020)%>%
  filter(ano >2013)%>%
  filter(especie_f=="BOVINOS" | especie_f == "PORCINOS"
         | especie_f == "OVINOS" | especie_f == "EQUINOS" | especie_f == "ABEJAS")%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral,fill=síndrome_presuntivo))+
  facet_grid(rows = vars(especie_f))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y=NULL,
       title="Notificaciones por sindrome presuntivo",
       x=NULL)+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

## Ejemplo bovinos --
## Numero de notificaciones por sindrome presuntivo

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month,síndrome_presuntivo, especie_f)%>%
  # filter(ano <2020)%>%
  filter(ano >2013)%>%
  filter(especie_f=="BOVINOS")%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral,fill=síndrome_presuntivo))+
  facet_grid(rows = vars(especie_f))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y=NULL,
       title="Notificaciones por sindrome presuntivo",
       x=NULL)+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

## Ejemplo bovinos --
# Numero de notificaciones por sindrome presuntivo, 2021
Mostrando primeras 40 lineas

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month, ano,patología, especie_f)%>%
  filter(ano >2020)%>%
  # filter(ano >2013)%>%
  filter(especie_f=="BOVINOS")%>%
  summarise(notifi_geral=length(unique(orden)))%>%
  arrange(desc(notifi_geral))%>%
  head(.,40)%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral, fill=patología))+
  facet_grid(rows = vars(ano))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y=NULL,
       title="Notificaciones por sindrome presuntivo",
       x=NULL)+
  theme_minimal() +
  theme(text = element_text(size = 8))

```

## --Vigilancia por especie --
## PORCINOS PPC ----
Vigilancia General

La vigilancia general no se centra en una enfermedad en particular, pero puede usarse para detectar cualquier enfermedad o patógeno. Por ejemplo, el sistema de notificación de enfermedades de los agricultores es un sistema de vigilancia general, ya que se puede notificar cualquier enfermedad. Sin embargo, no todas las enfermedades se notificarán con la misma fiabilidad. 

Es más probable que los agricultores notifiquen enfermedades que muestran signos claros y tienen un impacto significativo (por ejemplo, muchos animales están infectados o la enfermedad provoca la muerte, como la septicemia hemorrágica) que las enfermedades que muestran pocos signos o que no lo hacen. resultar en un impacto económico inmediato (como el causado por infecciones parasitarias intestinales).

Algunas pruebas de laboratorio, como la histopatología, permiten la detección de muchas enfermedades diferentes, en lugar de una sola enfermedad.

Una característica importante de la vigilancia general es que no solo puede detectar enfermedades conocidas de interés, sino que también puede detectar enfermedades endémicas nuevas, emergentes, exóticas o desconocidas. En otras palabras, no es necesario estar buscando una enfermedad específica para encontrarla.
(Cameron, 2014).

```{r}
# Filtrando porcinos
v2 <- v1 %>%
  filter(especie == "PORCINOS") 

v2 <- v2 %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  # filter(detalle_diagnóstico == "PESTE PORCINA CLÁSICA")%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados), reactivo=sum(reactivos))

# Agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

# Borrar bovinos
v2 <- v2[v2$orden != "1",] #0700860679 loja paltas casamba
v2 <- v2[v2$orden != "2382",] 
v2 <- v2[v2$orden != "1096",] 
v2 <- v2[v2$orden != "4114",] 
v2 <- v2[v2$orden != "6270",] 
```


# Numero total de eventos

```{r}
length(unique(v2$orden))
#1758 com atualizacao 10/10/2021 todas as patologias
#1863 com atualizacao 10/12/2021 todas as patologias

# Nmero de notificaciones generales por especie

# Asignando columna de fechas para graficos
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")

```


## Numero de notificaciones

```{r}
v2 %>%
  group_by(ano)%>%
  # filter(ano >2020)%>%
  summarise(notifi=length(unique(orden)))

```

## Número de muestras procesadas

sum(as.numeric(v2$total_muestras), na.rm=TRUE)

## Grafico número de notificaciones generales de porcinos

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral), fill="#377EB8")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general porcinos",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Notificaciones por patologia


```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month, patología)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral, fill=patología))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general porcinos",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 8))

```

## Número de notificaciones por sindrome presuntivo


```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month,síndrome_presuntivo)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral,fill=síndrome_presuntivo))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general porcinos",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

## -- Vigilancia dirigida ---
## El término vigilancia dirigida se puede utilizar refiriéndose a la vigilancia dirigida a una enfermedad específica  (Cameron, 2014).

La distinción entre vigilancia general y dirigida depende del sistema de detección de enfermedades que se utilice. La vigilancia dirigida se basa en el uso de pruebas que pueden dar una respuesta sí / no para una enfermedad específica. Ejemplos incluyen:

• reacción en cadena de la polimerasa (PCR)

• ensayo inmunoabsorbente ligado a enzimas (ELISA)

• Agar gel de inmunodifusión (AGID).

Necesito llegar a los mismos resultados sin tener la columna de DIAGNOSTICO DEFINITIVO en el sizse. 
Filtro la especie porcinos, luego la prueba solicitada diferente a Anticuerpo, luego el detalle diagnostico, luego en prueba solicitada unicamente los que inicien con PESTE PORCINA.... (aqui hay antigeno, PCR qPCR y PCR out), al final asigno una nueva columna que indica cada linea como caso o notificacion.


```{r}
#criando o v2 novo com as notificações extra
# Linkage dos reportes
library(stringr)

v0 <- full_join(vc2, vr2)
v1 <- full_join(v0,vge2)

# table(v1$especie)
# table(v1$prueba_solicitada)

# Filtro porcinos
v2 <- v1 %>%
  filter(especie == "PORCINOS") 

#Filtro vigilancia especifica ppc. 
# Todas aquellas notificaciones en las cuales
# se requirio prueba para PPC
v2 <- v2 %>%
  filter(prueba_solicitada != "PESTE PORCINA CLÁSICA (AC.)")

###
v2 <- v2 %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  filter(detalle_diagnóstico == "PESTE PORCINA CLÁSICA")%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden, prueba_solicitada,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  filter(str_detect(prueba_solicitada, "PESTE PORCINA"))%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados), reactivo=sum(reactivos))%>%
  mutate(definitivo=ifelse(pos>=1,"caso", "notifi"))


length(unique(v2$orden))
table(v2$definitivo)

plot(v2$existente)
hist(log(v2$existente))
table(v2$existente)
summary(v2$existente)

#agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

# Borrar bovinos
v2 <- v2[v2$orden != "1",] #0700860679 loja paltas casamba
v2 <- v2[v2$orden != "2382",] 
v2 <- v2[v2$orden != "1096",] 
v2 <- v2[v2$orden != "4114",] 
v2 <- v2[v2$orden != "6270",] 

# Asignando columna de fechas para graficos
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")

```

## Numero total de eventos

```{r}
length(unique(v2$orden))
#1089 com atualizacao 10/12/2021 todas as patologias

```

## Numero de notificaciones y casos confirmados


```{r}
library(tidyr)
v2 %>%
  group_by(ano, definitivo)%>%
  summarise(numero=n())%>%
  spread(value="numero", key="definitivo")

```

## Graficos notificaciones

## Notificaciones agregadas por mes

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral), fill="#984EA3")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia especifica PPC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```


## Notificaciones agregadas por anos

```{r}
# notificaciones por ano
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(ano,notifi_geral), fill="#984EA3")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 7)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia dirigida PPC",
       x="Anos")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Notificaciones y casos confirmados

```{r}
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(ano,notifi_geral), fill="#377EB8")+
  geom_col(aes(ano,casos), fill="#984EA3")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 7)+
  geom_text(aes(ano, casos, label=(casos)), nudge_y = 6)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones y casos vigilancia PPC",
       x="Anos")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Numero de notificaciones y casos

```{r}
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notificacion_vigilancia_dirigida=length(unique(orden)),
            casos=sum(definitivo == 'caso'))
```

## Casos por meses

```{r}
# Casos por meses
v2 %>%
  group_by(month)%>%
  # group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(month,casos), fill="#984EA3")+
  geom_text(aes(month, casos, label=(casos)), nudge_y = 3)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PPC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Casos por meses historico

```{r}
# Casos por meses
v2 %>%
  group_by(Month)%>%
  # group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2013)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,casos), fill="#984EA3")+
  geom_text(aes(Month, casos, label=(casos)), nudge_y = 3)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PPC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Número de afectados (sacrificados, muertos) por meses historico

```{r}
# Casos por meses

v2 %>%
  group_by(Month)%>%
  filter(definitivo == "caso") %>% 
  # group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2013)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            afectados=sum(afetados))%>%
  ggplot()+
  geom_col(aes(Month,afectados), fill="#984EA3")+
  # geom_text(aes(Month, afectados, label=(afectados)), nudge_y = 3)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PPC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 10))

```
## Número de animales muestreados y brotes por semana epidemiológica

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, week)%>%
  filter(ano > 2019) %>%
    summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#377EB8")+
  # geom_col(aes(week, Casos), fill="#984EA3") +
  # geom_point(aes(week, brotes*30), size=0.2)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /30, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2021)",
       y="Total de animales muestreados por semana")+
  theme_minimal()
  
```

## Número de animales muestreados y brotes por semana epidemiológica

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, week)%>%
  filter(ano > 2020) %>%
    summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#377EB8", alpha=0.5)+
  geom_col(aes(week, Casos), fill="black", alpha=0.5) +
  geom_col(aes(week, (Casos*95)/5), fill="red", alpha=0.5) +
  geom_point(aes(week, brotes*30), size=0.2)+
  geom_point(aes(week, (((brotes*10)*95)/5)), size=1)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /10, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2021)",
       y="Total muestreados semana simulando Se real")+
  theme_minimal()
  
```

```{r eval=FALSE, include=FALSE}
afectados <- v2 %>%
  group_by(Month)%>%
  filter(definitivo == "caso") %>% 
  # group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2013)%>%
  # filter(ano >2016)%>%
  summarise(afectados=sum(afetados))

# write.csv(afectados, file = "afectados.2014-2021.csv")

```


## --Síndrome respiratorio reproductivo porcino
## --PRRS ----
## -- Vigilancia dirigida ---
## El término vigilancia dirigida se puede utilizar refiriéndose a la vigilancia dirigida a una enfermedad específica  (Cameron, 2014).
La distinción entre vigilancia general y dirigida depende del sistema de detección de enfermedades que se utilice. La vigilancia dirigida se basa en el uso de pruebas que pueden dar una respuesta sí / no para una enfermedad específica. Ejemplos incluyen:

• reacción en cadena de la polimerasa (PCR)
• ensayo inmunoabsorbente ligado a enzimas (ELISA)

Necesito llegar a los mismos resultados sin tener la columna de DIAGNOSTICO DEFINITIVO en el sizse. 
Filtro la especie porcinos, luego la prueba solicitada diferente a Anticuerpo, luego el detalle diagnostico, luego en prueba solicitada unicamente los que inicien con PESTE PORCINA.... (aqui hay antigeno, PCR qPCR y PCR out), al final asigno una nueva columna que indica cada linea como caso o notificacion.


```{r}
#criando o v2 novo com as notificações extra
# Linkage dos reportes
library(stringr)

v0 <- full_join(vc2, vr2)
v1 <- full_join(v0,vge2)

# table(v1$especie)
# table(v1$prueba_solicitada)

# Filtro porcinos
v2 <- v1 %>%
  filter(especie == "PORCINOS") 

#Filtro vigilancia especifica PRRS. 
# Todas aquellas notificaciones en las cuales el detalle diagnóstico fue PRRS
###
v2 <- v2 %>%
  filter(detalle_diagnóstico == "SÍNDROME DISGENÉSICO Y RESPIRATORIO PORCINO")%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden, prueba_solicitada,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio, t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados), reactivo=sum(reactivos))%>%
  mutate(definitivo=ifelse(pos>=1,"caso", "notifi"))


length(unique(v2$orden))
table(v2$definitivo)

plot(v2$existente)
hist(log(v2$existente))
table(v2$existente)
summary(v2$existente)

#agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

# Asignando columna de fechas para graficos
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")

```

## Numero total de eventos

```{r}
length(unique(v2$orden))
#64 com atualizacao 10/12/2021 todas as patologias

```

## Numero de notificaciones y casos confirmados


```{r}
library(tidyr)
v2 %>%
  group_by(ano, definitivo)%>%
  summarise(numero=n())%>%
  spread(value="numero", key="definitivo")

```

## Graficos notificaciones

## Notificaciones agregadas por mes

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral), fill="#f98e09")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notif vigilancia especifica PRRS",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```


## Notificaciones agregadas por anos

```{r}
# notificaciones por ano
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(ano,notifi_geral), fill="#f98e09")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 1)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia dirigida PRRS",
       x="Anos")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Notificaciones y casos confirmados

```{r}
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(ano,notifi_geral), fill="#fcffa4")+
  geom_col(aes(ano,casos), fill="#f98e09")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 1)+
  geom_text(aes(ano, casos, label=(casos)), nudge_y = 0.5)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones y casos vigilancia PRRS",
       x="Anos")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## numero de notificaciones y casos

```{r}
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notificacion_vigilancia_dirigida=length(unique(orden)),
            casos=sum(definitivo == 'caso'))
```

## Casos por meses

```{r}
# Casos por meses
v2 %>%
  group_by(month)%>%
  # group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(month,casos), fill="#f98e09")+
  geom_text(aes(month, casos, label=(casos)), nudge_y = 0.2)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PRRS",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Casos por meses historico

```{r}
# Casos por meses
v2 %>%
  group_by(Month)%>%
  # group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2013)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,casos), fill="#f98e09")+
  geom_text(aes(Month, casos, label=(casos)), nudge_y = 0.2) +
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PRRS",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Número de afectados (sacrificados, muertos) por meses historico

```{r}
# Casos por meses

v2 %>%
  group_by(Month)%>%
  filter(definitivo == "caso") %>% 
  # group_by(ano)%>%
  # group_by(week)%>%
  # filter(ano > 2013)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            afectados=sum(afetados))%>%
  ggplot()+
  geom_col(aes(Month,afectados), fill="#f98e09")+
  # geom_text(aes(Month, afectados, label=(afectados)), nudge_y = 3)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos PRRS (número de animales afectados)",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

# Grafico del número de animales muestreados
```{r}
library(tidyr)

v2 %>%
  group_by(orden, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  ggplot()+
  geom_boxplot(aes(month, Total_muestras, group=factor(month))) + scale_color_brewer(palette="PiYG")+
  labs(x="meses",
       y="Total de muestras",
       fill=NULL)+
  theme_minimal()


```

# Eventos sanitarios por mes distribuidos en caso y no caso
```{r}
# refazer 
# v2 %>%
#   group_by(Month, definitivo)%>%
#   summarise(nu=n()) %>%
#   ggplot()+
#   geom_col(aes(Month, nu, fill=definitivo))+
#     labs(y="Número de eventos sanitarios",
#        x="",
#        fill="") +
#   theme_minimal() +
#   theme(text = element_text(size = 14))+
#   scale_fill_brewer(palette="PuOr", direction = -1)

```


## Número de animales muestreados y brotes por semana epidemiológica

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, week)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#fcffa4")+
  geom_col(aes(week, Casos), fill="#f98e09") +
  geom_point(aes(week, brotes*55), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /55, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2014-2021)",
       y="Total de animales muestreados por semana")+
  theme_minimal()
  
```
## Número de animales muestreados y brotes por semana epidemiológica y año
```{r}

library(tidyr)
v2 %>%
  group_by(ano,definitivo, week)%>%
  filter(ano > 2014) %>% 
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
    geom_col(aes(week, Total_muestras), fill="#fcffa4")+
    geom_col(aes(week, Casos), fill="#f98e09") +
    geom_point(aes(week, brotes*55), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /55, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica",
       y="Total de animales muestreados por semana")+
  theme_minimal()+
  facet_grid(rows = vars(ano))
  
```

## Número de animales muestreados y brotes por mes

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, Month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(Month, Total_muestras), fill="#fcffa4")+
  geom_col(aes(Month, Casos), fill="#f98e09") +
  geom_point(aes(Month, brotes*55), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /55, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2014-2021)",
       y="Total de animales muestreados por mes")+
  theme_minimal()
  
```



##Prevalencia media por meses
```{r}
library(tidyr)

v2 %>%
  group_by(ano, Month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>% 
  ggplot()+
  geom_boxplot(aes(ano, Prevalencia, group=Month),fill="#f98e09")
```



## --Vigilancia por especie --
## LENGUA AZUL ----
Vigilancia General para Lengua azul

La vigilancia general no se centra en una enfermedad en particular, pero puede usarse para detectar cualquier enfermedad o patógeno. Por ejemplo, el sistema de notificación de enfermedades de los agricultores es un sistema de vigilancia general, ya que se puede notificar cualquier enfermedad. Sin embargo, no todas las enfermedades se notificarán con la misma fiabilidad. 

Es más probable que los agricultores notifiquen enfermedades que muestran signos claros y tienen un impacto significativo (por ejemplo, muchos animales están infectados o la enfermedad provoca la muerte, como la septicemia hemorrágica) que las enfermedades que muestran pocos signos o que no lo hacen, o pueden no resultar en un impacto económico inmediato (como el causado por infecciones parasitarias intestinales).

Algunas pruebas de laboratorio, como la histopatología, permiten la detección de muchas enfermedades diferentes, en lugar de una sola enfermedad.

Una característica importante de la vigilancia general es que no solo puede detectar enfermedades conocidas de interés, sino que también puede detectar enfermedades endémicas nuevas, emergentes, exóticas o desconocidas. En otras palabras, no es necesario estar buscando una enfermedad específica para encontrarla.
(Cameron, 2014).
Este es el caso en lengua azul, que ha sido detectada al ser diferencial de otras enfermedades vesiculares sin estar buscándola.

```{r}
# Filtrando LENGUA AZUL
# v2 <- v1 %>%
#   filter(especie == "BOVINOS") 

v2 <- v1 %>%
  filter(detalle_diagnóstico == "LENGUA AZUL")%>%
    # filter(stringr::str_detect(prueba_solicitada, "LENGUA"))%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
 especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), reactivo=sum(reactivos))

# Agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

```

## Considerando el síndrome presuntivo vesiculares cuales fueron los diagnósticos realizados?
```{r}
v1 %>% 
  filter(síndrome_presuntivo == "VESICULARES") %>% 
  group_by(detalle_diagnóstico) %>% 
  summarise(numero=length(unique(orden)))
```


# Numero total de eventos

```{r}
length(unique(v2$orden))
#1758 com atualizacao 10/10/2021 todas as patologias
#1863 com atualizacao 10/12/2021 todas as patologias

# Nmero de notificaciones generales por especie

# Asignando columna de fechas para graficos
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")

```


## Numero de notificaciones por año

```{r}
v2 %>%
  group_by(ano)%>%
  # filter(ano >2020)%>%
  summarise(notifi=length(unique(orden)))

```

## Numero de notificaciones por especie

```{r}
v2 %>%
  group_by(ano, especie)%>%
  # filter(ano >2020)%>%
  summarise(notifi=length(unique(orden)))

```

## Grafico número de notificaciones generales

```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral), fill="#377EB8")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general BTV",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Notificaciones por patologia


```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral, color=notifi_geral))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general BTV",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 8))

```

## Número de notificaciones por sindrome presuntivo


```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month,síndrome_presuntivo)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral,fill=síndrome_presuntivo))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia general BTV",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 10))

```

## -- Vigilancia dirigida ---
## El término vigilancia dirigida se puede utilizar refiriéndose a la vigilancia dirigida a una enfermedad específica  (Cameron, 2014).

La distinción entre vigilancia general y dirigida depende del sistema de detección de enfermedades que se utilice. La vigilancia dirigida se basa en el uso de pruebas que pueden dar una respuesta sí / no para una enfermedad específica. Ejemplos incluyen:

• reacción en cadena de la polimerasa (PCR)

• ensayo inmunoabsorbente ligado a enzimas (ELISAc) en el caso de Lengua azul.

Existe columna de DIAGNOSTICO DEFINITIVO en el sizse?. 


```{r}
#criando o v2 novo com as notificações extra
# Linkage dos reportes
library(stringr)

#Filtro vigilancia especifica BTV. 
# Todas aquellas notificaciones en las cuales
# se requirio prueba para PPC
# v2 <- v1 %>%
#    filter(stringr::str_detect(prueba_solicitada, "LENGUA"))

###
v2 <- v1 %>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, coord_x, coord_y, predio,t_explotación, notificador, f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden, prueba_solicitada,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  filter(str_detect(prueba_solicitada, "LENGUA"))%>%
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta)%>%
  summarise(existente=sum(existentes, muertos, sacrificad), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados), reactivo=sum(reactivos))%>%
  mutate(definitivo=ifelse(pos>=1,"caso", ".no caso"))


length(unique(v2$orden))
table(v2$definitivo)

plot(v2$existente)
hist(log(v2$existente))
table(v2$existente)
summary(v2$existente)

#agregando ano
v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))
v2$week <- week(dmy(v2$f_1er_enfermo))

# Asignando columna de fechas para graficos
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$Week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$Month <- floor_date(v2$f_1er_enfermo, "month")


```

## Numero total de eventos

```{r}
length(unique(v2$orden))
#380 com atualizacao 10/12/2021 todas as patologias

```
## Numero total de propriedades

```{r}
length(unique(paste(v2$cedula,v2$predio)))
#380 com atualizacao 10/12/2021 todas as patologias

```

## Numero de notificaciones y casos confirmados

```{r}
library(tidyr)
v2 %>%
  group_by(ano)%>%
  summarise(Notificaciones=length(unique(orden)),
            Casos=sum(definitivo == "2 caso"),
            porcentaje_positivos=round((Casos/Notificaciones),2))

```

## Numero de notificaciones y casos confirmados por propriedad

```{r}
library(tidyr)
v2 %>%
  group_by(ano)%>%
  summarise(Notificaciones=length(unique(paste(cedula, predio))),
            Casos=sum(definitivo == "2 caso"),
            prev=round((Casos/Notificaciones),2))

```


## Graficos notificaciones

## notificaciones agregadas por años

```{r}
# Casos por meses

v2 %>%
  group_by(month, definitivo)%>%
  summarise(nu=n()) %>%
  ggplot()+
  geom_col(aes(month, nu, fill=definitivo))+
    labs(y="Número de eventos sanitarios",
       x="",
       fill="")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  scale_fill_brewer(palette="Blues")
```
```{r}
v2 %>%
  # group_by(month)%>%
  group_by(Month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,notifi_geral), fill="#4287f5")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia BTV",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```


## Notificaciones agregadas por anos

```{r}
# notificaciones por ano
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(ano,notifi_geral), fill="#4287f5")+
  geom_text(aes(ano, notifi_geral, label=(notifi_geral)), nudge_y = 7)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificaciones vigilancia pasiva vesiculares",
       x="Anos")+
  theme_minimal() +
  theme(text = element_text(size = 14))

```

## Notificaciones y casos confirmados

```{r}
v2 %>%
  group_by(ano, definitivo)%>%
  summarise(nu=n()) %>%
  ggplot()+
  geom_col(aes(ano, nu, fill=definitivo))+
      labs(y="Número de eventos sanitarios",
       x="",
       fill="")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  scale_fill_brewer(palette="Blues")


```
# Eventos sanitarios por mes distribuidos en caso y no caso
```{r}
v2 %>%
  group_by(Month, definitivo)%>%
  summarise(nu=n()) %>%
  ggplot()+
  geom_col(aes(Month, nu, fill=definitivo))+
    labs(y="Número de eventos sanitarios",
       x="",
       fill="")+
  theme_minimal() +
  theme(text = element_text(size = 14))+
  scale_fill_brewer(palette="Blues")

```



## numero de notificaciones y casos

```{r}
v2 %>%
  # group_by(Month)%>%
  group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(Vigilancia_vesiculares=length(unique(orden)),
            BTV=sum(definitivo == 'caso'))
```

## Casos por meses

```{r}
# Casos por meses
v2 %>%
  group_by(Month)%>%
  # group_by(ano)%>%
  # group_by(week)%>%
  filter(ano > 2014)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            casos=sum(definitivo == 'caso'))%>%
  ggplot()+
  geom_col(aes(Month,casos), fill="#4287f5")+
  # geom_text(aes(Month, casos, label=(casos)), nudge_y = 3)+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos BTV",
       x="Meses")+
  theme_minimal()

```
```{r}
v2 %>%
  group_by(month)%>%
    summarise(no_casos=sum(definitivo == "no caso"),
            casos=sum(definitivo == 'caso'))
```


# Enfermedad
## Prevalencia (número de animales positivos/número de animales testados)
   
```{r}
library(tidyr)

v2 %>%
  group_by(ano)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3)))

```

# Grafico del número de animales muestreados
```{r}
library(tidyr)

v2 %>%
  group_by(orden, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  ggplot()+
  geom_boxplot(aes(month, Total_muestras, group=factor(month), fill="blue"))+
  scale_fill_brewer(palette="BuPu")+
  labs(x="meses",
       y="Total de muestras",
       fill=NULL)+
  theme_minimal()


```

```{r}
library(tidyr)
v2 %>%
  group_by(ano, month, definitivo)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  arrange(month)
  
```




```{r}
library(tidyr)
v2 %>%
  group_by(ano, definitivo, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  ggplot()+
  geom_boxplot(aes(month, Total_muestras, group=factor(month), fill=definitivo))+
   facet_grid(rows=vars(definitivo), scales = "free_y")+
  labs(fill="",
       x="Meses",
       y="Total de muestras por año")+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()
```


```{r}
library(tidyr)
v2 %>%
  group_by(orden, definitivo, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  ggplot()+
  geom_boxplot(aes(month, Total_muestras, group=factor(month), fill=definitivo))+
   facet_grid(rows=vars(definitivo), scales = "free_y")+
  labs(fill="",
       x="Meses",
       y="Total de muestras por predio")+
  scale_fill_brewer(palette="BuPu")+
  theme_minimal()
```

```{r}
library(tidyr)
v2 %>%
  group_by(ano, definitivo, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>%
  ggplot()+
  geom_col(aes(month, Total_muestras, group=factor(month), fill=definitivo))+
   facet_grid(rows=vars(definitivo), scales = "free_y")+
  labs(fill="",
       x="Meses",
       y="Total de muestras")+
  scale_fill_brewer(palette="Blues")+
  theme_minimal()
```

```{r}
library(RColorBrewer)
display.brewer.all()

display.brewer.pal(n=3, name = 'Blues')
brewer.pal(n=3, name = 'Blues')
```


```{r}
library(tidyr)
v2 %>%
  group_by(definitivo, month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras)) %>%
  ggplot()+
  geom_col(aes(month, Total_muestras, group=factor(month)), fill="#DEEBF7")+
  geom_col(aes(month, Casos, group=factor(month)), fill="#9ECAE1")+
  labs(fill="",
       x="Meses",
       y="Total de animales muestreados por mes")+
  theme_minimal()
```

```{r}
library(tidyr)
v2 %>%
  group_by(definitivo, week)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#DEEBF7")+
  geom_col(aes(week, Casos), fill="#9ECAE1") +
  labs(fill="",
       x="Semana epidemiologica",
       y="Total de animales muestreados por semana")+
  theme_minimal()
```


```{r}
v2 %>%
  group_by(definitivo, week)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso'))
```

## Número de animales muestreados y brotes por semana epidemiológica

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, week)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#DEEBF7")+
  geom_col(aes(week, Casos), fill="#9ECAE1") +
  geom_point(aes(week, brotes*20), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /20, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2014-2021)",
       y="Total de animales muestreados por semana")+
  theme_minimal()
  
```
## Número de animales muestreados y brotes por semana epidemiológica y año
```{r}

library(tidyr)
v2 %>%
  group_by(ano,definitivo, week)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(week, Total_muestras), fill="#DEEBF7")+
  geom_col(aes(week, Casos), fill="#9ECAE1") +
  geom_point(aes(week, brotes*20), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /20, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica",
       y="Total de animales muestreados por semana")+
  theme_minimal()+
  facet_grid(rows = vars(ano))
  
```

## Número de animales muestreados y brotes por mes

```{r}

library(tidyr)
v2 %>%
  group_by(definitivo, Month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            brotes=sum(definitivo == 'caso')) %>%
  mutate(brotes=ifelse(brotes==0,NA,brotes)) %>% 
  ggplot()+
  geom_col(aes(Month, Total_muestras), fill="#DEEBF7")+
  geom_col(aes(Month, Casos), fill="#9ECAE1") +
  geom_point(aes(Month, brotes*20), size=0.4)+
    scale_y_continuous(
                sec.axis = sec_axis(trans = ~. /20, name="Número de brotes")) + 
  
    labs(fill="",
       x="Semana epidemiologica (2014-2021)",
       y="Total de animales muestreados por mes")+
  theme_minimal()
  
```



##Prevalencia media por meses
```{r}
library(tidyr)

v2 %>%
  group_by(ano, Month)%>%
  summarise(Casos=sum(pos),
            Total_muestras=sum(total_muestras),
            Prevalencia=100*(round((Casos/Total_muestras),3))) %>% 
  ggplot()+
  geom_boxplot(aes(ano, Prevalencia, group=Month),fill="#4287f5")
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.
```{r}

```

