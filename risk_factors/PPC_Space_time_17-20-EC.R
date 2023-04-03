#-------------------------------------------------------
#      Risk factors analysis of CSF in Ecuador
#      Bayesian Analysis
#      Alfredo Acosta
#      Preparing the data
#      12.05.2021
#      24.06.2022
#-------------------------------------------------------
# Libraries ----
library(spdep);library(INLA);library(rgdal);library(ggplot2);library(INLAOutputs)
library(ggsn);library(dplyr);library(SpatialEpi);library(sp)
library(raster);library(brinla);library(imputeTS); library(viridis())

# devtools::install_github("julianfaraway/brinla")
# INLA is not on cran https://www.r-inla.org/download-install
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# inla.upgrade() 
# Home folder ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/Code space-time")

setwd()
cat("\014")
load("datos.RData")

# 1 Reading the RData datos ----
colnames(datos)

# Variable dictionary
# [1] "DPA_PARROQ"     # Unique name of the parish polygons
# [2] "populationa"    # Number of animals
# [3] "casesa"         # Number of animals in the Cases (premises) 
# [4] "populationp"    # Number of premises
# [5] "casesp"         # Number of observed cases (premises)
# [6] "year"           # Year 
# [7] "doses_vac"# Number of pigs vaccinated 
# [8] "vacinators"     # Number of vaccinators 

#Please change dsn to the working directory !!!!

# Reading the map (islands and parish from amazon exluded 1006) ----
ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/10.2020 II sem/FLI/case-control/bayesian/",layer="ec3.1006")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Linear interpolation and centralization of data ----
## Preparing the data
datos$pop_a <- datos$populationa
datos$pop_p <- datos$populationp

datos$pop_a[datos$pop_a == 0] <- NA
datos$pop_p[datos$pop_p == 0] <- NA

statsNA(datos$pop_a)
statsNA(datos$pop_p)

ggplot_na_distribution(datos$pop_p)
ggplot_na_distribution(datos$pop_a)

#Ordering the file by parish code DPA and year ----
datos <- datos[order(datos$DPA_PARROQ, datos$year),]

# Linear interpolation of population of animals and premises
set.seed(555)
datos$pop_a_na <- round(na_interpolation(datos$pop_a),0)
datos$pop_p_na <- round(na_interpolation(datos$pop_p),0)

datos$pop_a <- NULL
datos$pop_p <- NULL

summary(datos$populationa)
summary(datos$pop_a_na)
sum(datos$populationa, na.rm = TRUE)
sum(datos$pop_a_na) #4,760,819
sum(datos$pop_p_na) #467,607

datos%>%
  group_by(year)%>%
  summarise(premises=sum(populationp),
            adj_premises=sum(pop_p_na))

#imputed data make difference to the data? No
t.test(datos$populationp, datos$pop_p_na) #0.554
sum(datos$pop_p_na) #467607
sum(datos$populationp) #452741

# Animal Population adjusted by doses_vac ----
# Animals received idealy two doses per year, used to correct population 
datos %>%
  group_by(year) %>%
  summarize(dosis.per.animal=mean(doses_vac/pop_a_na))
# 2017: 2.73, 2018: 1.68. 2019: 2.34, 2020: 7.94

datos %>%
  group_by(year) %>%
  # filter(DPA_PARROQ != "071256")%>% #effect of the parish vitoria El Oro
  summarize(dosis.per.animal=mean(doses_vac/pop_a_na))

datos %>%
  summarize(dosis.per.animal=mean(doses_vac/pop_a_na))
#3.67

# Coverage of vaccination ----
datos$coverage_vac <- round(as.numeric((datos$doses_vac/2)/datos$pop_a_na),2)
summary(datos$coverage_vac)

datos%>%
  dplyr::select(DPA_PARROQ, casesp, populationa, year, doses_vac, coverage_vac)%>%
  group_by(year, DPA_PARROQ, coverage_vac)%>%
  arrange(desc(coverage_vac))
  
#Correcting the population according to vacinated pigs ----
#We consider that every animal in a year should receive two vaccine doses
# If the population is below the number of vaccinated/2 we keep the last
datos <- mutate(datos, 
                pop_a_vac_adj = 
                  ifelse(pop_a_na < doses_vac/2,
                         round((doses_vac/2),0), 
                         pop_a_na))
datos %>%
  group_by(year) %>%
  summarize(dosis.per.animal=
              mean(doses_vac/pop_a_vac_adj, na.rm=TRUE))
#0.99, 1.19, 1.42,1.12

summary(datos$populationa)
summary(datos$pop_a_na)
summary(datos$pop_a_vac_adj)

t.test(datos$pop_a_na, datos$pop_a_vac_adj) #are they different?? yes
#0.00032
datos %>% 
  group_by(year)%>%
  summarize(original_population= sum(pop_a_na),
            pop_adj_by_vac = sum(pop_a_vac_adj))

datos$coverage_vac_adj <- round(as.numeric((datos$doses_vac/2)/datos$pop_a_vac_adj),2)
summary(datos$coverage_vac_adj)

# Comparing coverage of vaccination with adjusted----
summary(datos$coverage_vac)
summary(datos$coverage_vac_adj)

hist(datos$coverage_vac)
hist(datos$coverage_vac_adj)

# Statistical significant difference not
t.test(datos$coverage_vac,
       datos$coverage_vac_adj)


# Parishes with 0 vaccinated animals ----
datos %>%
  group_by(year)%>%
  filter(doses_vac =="0")%>%
  summarise(n(), sum(populationa))

# Scaling of covariates population (animals, premises) ----
#vaccine and vaccinators 
datos$pop_a_s <- scale(datos$pop_a_na)
datos$pop_p_s <- scale(datos$pop_p_na)
datos$doses_vac_s <- scale(datos$doses_vac)
datos$vacin_s <- scale(datos$vacinators)
datos$coverage_vac_adj_s <- scale(datos$coverage_vac_adj)
datos$coverage_vac_s <- scale(datos$coverage_vac)
datos$pop_a_vac_adj_s <- scale(datos$pop_a_vac_adj)

#-------------------------------------------------------
# Calculate the expected for premises ----
# na_interpolated
datos$exp_pop_p_na <- expected(datos$pop_p_na, 
                               datos$casesp, 
                               n.strata = 1)
sum(datos$exp_pop_p_na) #146
sum(datos$casesp) #146
summary(datos$exp_pop_p_na)
hist(datos$exp_pop_p_na)

#-------------------------------------------------------
# Creating the map ----
# Creating id to the data file
ec3@data$id <- rownames(ec3@data)
map <- ec3

# keeping only with the data on the map
datos <- datos[datos$DPA_PARROQ %in% ec3@data$DPA_PARROQ == TRUE,]
length(unique(ec3@data$DPA_PARROQ))
length(unique(datos$DPA_PARROQ))

# Copying the map id to the data
datos$id <- map$id[match(datos$DPA_PARROQ, map$DPA_PARROQ)]
datos$DPA_DESPRO <- ec3@data$DPA_DESPRO[match(datos$DPA_PARROQ, ec3@data$DPA_PARROQ)]
datos$DPA_DESPAR <- ec3@data$DPA_DESPAR[match(datos$DPA_PARROQ, ec3@data$DPA_PARROQ)]

# Format colums form dataset ----
datos$timeformatted <- as.character(datos$year)
datos$time <- paste(datos$year,"-01-01", sep = "")
datos$id <- as.numeric(datos$id)
datos$time <- as.Date(datos$time, format = "%Y-%m-%d")
datos$timeformatted <- as.character(datos$timeformatted)

# order the colums ...
datos <- datos[order(datos$id, datos$year),]
datos$idtime <- rep(1:4, length(rownames(ec3@data)))

#--------------------------------------------------
# Preparing the map
datos.map <- datos[datos$year == "2017",]
datos.map <- data.frame(datos.map)
rownames(datos.map) <- datos.map$id
map <- ec3
map <- SpatialPolygonsDataFrame(map, datos.map, match.ID = TRUE)

# Calculate de area of the poligons ----
map@data$area <- raster::area(map) /1000000
datos$Area <- map@data$area[match(datos$DPA_PARROQ, map@data$DPA_PARROQ)]

# Animals vaccinated per square kilometer ----
datos$doses_vac_km2 <- datos$doses_vac/datos$Area
# scaling
datos$doses_vac_km2_s <-scale(datos$doses_vac_km2)

# Descriptive analysis ----
# Analizing correlation of variables 
library(PerformanceAnalytics)
colnames(datos)
chart.Correlation(datos[,c(29,13,8,9,12,10,7)])
# doses_vac_km2
# coverage_vac_adj
# doses_vac
# pop_a_na
# pop_a_vac_adj
# vacin_s
cor(datos[,c(29,13,8,9,12,10,7)])

# Centrality measures for variables in the model
library(sjmisc)
#Descriptive of variables used on the model
library(dplyr)
datos %>%
  group_by(year) %>%
  dplyr::select(Area, 
                pop_p_na,
                casesp,
                doses_vac_km2, 
                pop_a_vac_adj,
                coverage_vac_adj,
                vacinators
              ) %>%
  descr(show= c("mean", "sd", "md", "range"))

# vaccinated population
datos %>%
  group_by(year) %>%
  summarize(vac.coverage=mean(coverage_vac_adj),
            vac.applied.doses=sum(doses_vac),
            pigs=sum(pop_a_vac_adj),
            percentage=vac.applied.doses/pigs/2)

datos %>%
  summarize(vac.coverage=mean(coverage_vac_adj),
            average.annual.applied.doses=sum(doses_vac)/4,
            pigs=sum(pop_a_vac_adj)/4)

# Population of premises
datos %>%
  group_by(year)%>%
  summarise(premises=mean(pop_p_na),
            premises_total=sum(pop_p_na),
            premises_original=sum(populationp),
            observed=sum(casesp),
            expected=sum(exp_pop_p_na))%>%
  arrange(desc(premises))

datos %>%
    summarise(premises=mean(pop_p_na)/4,
            premises_total=sum(pop_p_na)/4)
            
#-------------------------------------------------------
#Create adjacency matrix ----
sf::sf_use_s2(FALSE) # I turned off the spherical geometry 24.06.22
map.nb <- poly2nb(map)
head(map.nb) #To see the adyacent premises

### UNLISTING THE ADJACENCY ORDER
# unlist <- data.frame(sapply(map.nb,"[[",1))
# unlist$s <- sapply(map.nb,"[[",2)
###
#Convert the adjacency matrix into a file in the INLA format ----
nb2INLA("map.adj", map.nb)
g = inla.read.graph(filename="map.adj")
str(g)

# Organize data to the model ----
# The id 0 (Zero) cause problems, so in this part I will create
# a 1:1006 id to use in the regression
# I will create on the map and match to the data DESPAR

#id2 is the name of the polygons ID, take care with this
#the map.nb use the order of the polygons from 1 to 1006
#this creates various confussions and lot of time gone
#the name that match de ids is the id not the id2

#saving the id2 for reference of constructing the data  
datos$id2 <- datos$id

#Create on the map the consecutive number id used by inla ----
map@data$id2 <- map@data$id
map@data$id <- seq(1:length(rownames(map@data)))

# Coping the id to datos considering the DPA_PAR
datos$id <- map@data$id[match(datos$DPA_PARROQ, map@data$DPA_PARROQ)]

# Correcting zonas no delimitadas
# El piedrero
datos$DPA_DESPRO[datos$DPA_PARROQ == "900451"] = "GUAYAS"
# Manga del cura
datos$DPA_DESPRO[datos$DPA_PARROQ == "900351"] = "MANABI"
#Las golondrinas
datos$DPA_DESPRO[datos$DPA_PARROQ == "900151"] = "IMBABURA"

# creating the regions columns
datos$regiao <- datos$DPA_DESPRO

datos$regiao <- gsub("AZUAY", "SERRA", datos$regiao)
datos$regiao <- gsub("BOLIVAR", "SERRA", datos$regiao)
datos$regiao <- gsub("CAÑAR", "SERRA", datos$regiao)
datos$regiao <- gsub("CARCHI", "SERRA", datos$regiao)
datos$regiao <- gsub("CHIMBORAZO", "SERRA", datos$regiao)
datos$regiao <- gsub("COTOPAXI", "SERRA", datos$regiao)
datos$regiao <- gsub("EL ORO", "LITORAL", datos$regiao)
datos$regiao <- gsub("SANTA ELENA", "LITORAL", datos$regiao)
datos$regiao <- gsub("GUAYAS", "LITORAL", datos$regiao)
datos$regiao <- gsub("IMBABURA", "SERRA", datos$regiao)
datos$regiao <- gsub("LOJA", "SERRA", datos$regiao)
datos$regiao <- gsub("ESMERALDAS", "LITORAL", datos$regiao)
datos$regiao <- gsub("LOS RIOS", "LITORAL", datos$regiao)
datos$regiao <- gsub("MANABI", "LITORAL", datos$regiao)
datos$regiao <- gsub("MORONA SANTIAGO", "AMAZONIA", datos$regiao)
datos$regiao <- gsub("NAPO", "AMAZONIA", datos$regiao)
datos$regiao <- gsub("ORELLANA", "AMAZONIA", datos$regiao)
datos$regiao <- gsub("PASTAZA", "AMAZONIA", datos$regiao)
datos$regiao <- gsub("PICHINCHA", "SERRA", datos$regiao)
datos$regiao <- gsub("SANTO DOMINGO DE LOS TSACHILAS", "LITORAL", datos$regiao)
datos$regiao <- gsub("SUCUMBIOS", "AMAZONIA", datos$regiao)
datos$regiao <- gsub("TUNGURAHUA", "SERRA", datos$regiao)
datos$regiao <- gsub("ZAMORA CHINCHIPE", "AMAZONIA", datos$regiao)

datos$regiao <- gsub("AMAZONIA", "3Amazonic", datos$regiao)
datos$regiao <- gsub("LITORAL", "3Coastal", datos$regiao)
datos$regiao <- gsub("SERRA", "1Highlands", datos$regiao)
table(datos$regiao, datos$casesp)
table(datos$regiao)

# Changing reference to test
# datos$regiao <- gsub("Amazonic", "1Amazonic", datos$regiao)
# datos$regiao <- gsub("Coastal", "2Coastal", datos$regiao)
# datos$regiao <- gsub("Highlands", "3Highlands", datos$regiao)
# table(datos$regiao)


#Formula spatio-temporal ----
# Bernardinelli  
# Besag-York-Mollié (BYM) Conditional autoregresive distribution CAR
# model iid Independent and identically distributed Gaussian random effect.  

# constr=TRUE, ie constraining each realisation of the spatial model to
# sum to zero

# The option adjust.for.con.comp adjust the model if the graph has more than one connected
# compoment, and this adjustment can be disabled setting this option to FALSE. This means that
# constr=TRUE is interpreted as a sum-to-zero constraint on each connected component and the
# rankdef parameter is set accordingly

# # Problem with package Matrix()
# remove.packages("Matrix")
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.3-2.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# the matrix 1.3-4 package have some problem
# I instaled the 1.3-2 to solve the problem.


##################################################

#Indices in datos id and idtime ----
#idAreaInteger in index in map
datos1 <- datos

#create indices random effects area ----
datos1$ID.area <- datos1$id
datos1$ID.area1 <- datos1$id

#create indices random effects time
datos1$ID.time <- datos1$idtime
datos1$ID.time1 <- datos1$idtime

# Priors
prec_bym2 <- list(phi = list(prior = "pc", param = c(.5, 2/3)),
                  prec = list(prior = "pc.prec", param = c(.3/.31, .01)))

#Formula spatio-temporal ----
# Bernardinelli  
# Besag-York-Mollié (BYM) Conditional autoregresive distribution CAR
# model iid Independent and identically distributed Gaussian random effect.  
# Model 0  ----
# Model 1  ----
formula = casesp ~ 1 +
  f(ID.area,
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1,
    ID.time,
    model="iid",
    constr=TRUE) +
  ID.time

res1  =  inla(formula,
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE),
              quantiles=c(0.025,0.975), verbose = FALSE)

# Analysis m1
datos$Risk1 <-res1$summary.fitted.values[,"mean"]
summary(res1)
bri.fixed.plot(res1)
summary(as.numeric(datos$Risk1))
datos %>% ## Average RR by year
  group_by(year)%>%
  summarize(mean=mean(Risk1), 
            med=median(Risk1),
            min=min(Risk1),
            max=max(Risk1))

FixedEffects(res1) #RR


# # Model 2  ----
formula = casesp ~ 1 +
  f(ID.area,
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1,
    ID.time,
    model="iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s+
  coverage_vac_s

res2  =  inla(formula,
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE),
              quantiles=c(0.025,0.975), verbose = FALSE)

# Analysis m2
datos$Risk2 <- res2$summary.fitted.values[,"mean"]
summary(res2)
bri.fixed.plot(res2)
summary(as.numeric(datos$Risk2))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk2),
            med=median(Risk2),
            min=min(Risk2),
            max=max(Risk2))
FixedEffects(res2) # RR

# # Model 3  ----
formula = casesp ~ 1 +
  f(ID.area,
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1,
    ID.time,
    model="iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s +
  pop_a_vac_adj_s

res3  =  inla(formula,
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE),
              quantiles=c(0.025,0.975))

# Analysis m3
datos$Risk3 <-res3$summary.fitted.values[,"mean"]
summary(res3)
bri.fixed.plot(res3)
summary(as.numeric(datos$Risk3))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk3),
            med=median(Risk3),
            min=min(Risk3),
            max=max(Risk3))
FixedEffects(res3) #RR correlation 37%
cor(datos$doses_vac_km2_s, datos$pop_a_vac_adj_s)

# # Model 4  ----
formula = casesp ~ 1 +
  f(ID.area,
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1,
    ID.time,
    model="iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s +
  pop_a_vac_adj_s +
  vacin_s

res4  =  inla(formula,
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE),
              quantiles=c(0.025,0.975))

# Analysis m4
datos$Risk4 <-res4$summary.fitted.values[,"mean"]
summary(res4)
bri.fixed.plot(res4)
summary(as.numeric(datos$Risk4))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk4),
            med=median(Risk4),
            min=min(Risk4),
            max=max(Risk4))
FixedEffects(res4) #RR

# Model 5  ----
formula = casesp ~ 1 +
  f(ID.area, 
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1, 
    ID.time, 
    model="iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s +
  pop_a_s
  
res5  =  inla(formula, 
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE), 
              quantiles=c(0.025,0.975))

#Analisys m5
datos$Risk5 <-res5$summary.fitted.values[,"mean"]
summary(res5)  
bri.fixed.plot(res5)
summary(as.numeric(datos$Risk5))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk5),
            med=median(Risk5),
            min=min(Risk5),
            max=max(Risk5))
FixedEffects(res5) #RR

# Model 6  ----
formula = casesp ~ 1 +
  f(ID.area, 
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1, 
    ID.time, 
    model = "iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s +
  pop_a_vac_adj_s +
  regiao

res6  =  inla(formula, 
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE), 
              quantiles=c(0.025,0.975))

#Analisys m6
datos$Risk6 <-res6$summary.fitted.values[,"mean"]
summary(res6)  
bri.fixed.plot(res6)
summary(as.numeric(datos$Risk6))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk6),
            med=median(Risk6),
            min=min(Risk6),
            max=max(Risk6))
FixedEffects(res6) #RR

# Model 7  ----
formula = casesp ~ 1 +
  f(ID.area, 
    model="bym2",
    hyper = prec_bym2,
    graph=g,
    scale.model = TRUE,
    constr = TRUE,
    adjust.for.con.comp=TRUE) +
  f(ID.area1, 
    ID.time, 
    model = "iid",
    constr=TRUE) +
  ID.time +
  doses_vac_km2_s +
  pop_a_s + #without adjustment
  regiao

res7  =  inla(formula, 
              family="poisson",
              data=datos1,
              E=exp_pop_p_na,
              control.predictor=list(compute=TRUE),
              control.compute=list(dic=TRUE), 
              quantiles=c(0.025,0.975))

#Analisys m7
datos$Risk7 <-res7$summary.fitted.values[,"mean"]
summary(res7)  
bri.fixed.plot(res7)
summary(as.numeric(datos$Risk7))
datos %>%
  group_by(time)%>%
  summarize(mean=mean(Risk7),
            med=median(Risk7),
            min=min(Risk7),
            max=max(Risk7))
FixedEffects(res7) #RR

# Model Analysis ----
# library(devtools)
# install_github('oswaldosantos/INLAOutputs')
library(INLAOutputs)

# Calculate DIC
DIC(res1, res2, res3, res4, res5, res6, res7)

#Fixed effects (exp coefs >> RR)
FixedEffects(res6)

# ?
RandomEffectsExcess(res6,res7)

#Explained variance
ExplainedVariance(res6)

#comparign random effects (I dont know what they mean)
CompareRandomEfectSD(res6,res7, percentage = TRUE)

# Priority Index ----
pr_idx <- PriorityIndex(res6, effect = "fitted", cutoff = 1, rescale_by = "year")

datos <- datos %>%
  mutate(pr_index = pr_idx)

library(tidyverse)
datos %>%
  group_by(PRO, year)%>%
  summarise(pi=mean(as.numeric(pr_index)))%>%
  arrange(desc(pi))


#< Fig. Posterior distributions ----
rmf = res6$marginals.fixed
cf = data.frame(do.call(rbind, rmf))
cf$parameter = rep(names(rmf), times = sapply(rmf, nrow))

colnames(cf)

cf$parameter <- gsub("doses_vac_km2_s", "Doses vac km2", cf$parameter)
cf$parameter <- gsub("pop_a_vac_adj_s", "Population of pigs", cf$parameter)
cf$parameter <- gsub("regiao3Amazonic", "Amazonic Region", cf$parameter)
cf$parameter <- gsub("regiao3Coastal", "Coastal Region", cf$parameter)


#Fig. Posterior distributions ----
rr <- ggplot(cf, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~parameter, 
          scales = "free", ncol=2) +
  geom_vline(xintercept = 0, 
             size=0.8,
             color=c("#541352FF","#10a53dFF","#10a53dFF",
                     "#10a53dFF","#10a53dFF","#541352FF")) + 
  labs(tag = "a", x = NULL, y ="Density")+
  theme_light()+
  theme(strip.background = element_blank(),
        strip.text = element_text(color="Black"))+
  theme(text = element_text(size = 8))

rr
library(viridis)
# Fig Risk boxplot ----          
rrb <-
  datos %>%
  group_by(year) %>%
  summarize(Risk = Risk6) %>%
  ggplot(aes(year, Risk, fill= Risk)) +
  geom_boxplot(outlier.colour="#10a53dFF",
               outlier.size=0.05) +
  coord_trans(y="log10")+
  scale_fill_viridis()+
  theme_light()+
  annotate("text",x = "2017", y = 5.60, label = "3.63", size=2.5)+
  annotate("text",x = "2018", y = 3.87, label = "2.48", size=2.5)+
  annotate("text",x = "2019", y = 2.72, label = "1.75", size=2.5)+
  annotate("text",x = "2020", y = 1.96, label = "1.26", size=2.5)+
  labs(tag = "b", x = NULL, y ="Relative Risk (log scale)")+
  theme(text = element_text(size = 7))+
  scale_y_continuous(breaks = c(0.5, 1,5,10,25,50,75))+
  theme(panel.grid.minor.y = element_blank())

rrb

#< Fig. 13  Density and boxplot of risk model results ----

tiff(filename = "Fig.14 Model results.tiff",
     width=9, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggpubr::ggarrange(rr,rrb, ncol = 1, heights = c(1, 0.7))

dev.off()



-------------------------------------------------------
# Ploting the maps ----
library(rgdal)
library(ggplot2)
library(ggsn)
library(dplyr)

# renaming the datos to results 
results <- datos
#--------------------------------------------------
# The map used on the analysis do not have the amazon parishes
# I used an complete shape to plot complete ecuador
# using with a map with all the provinces, fortify and join with the data

#Please change the working directory in dsn
ec<-rgdal::readOGR(dsn="~/Dropbox/0.USP/10.2020 II sem/FLI/case-control/bayesian/",layer="nxparroquias")
ec <- spTransform(ec, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ec <- subset(ec, DPA_DESPRO != "GALAPAGOS")
ec$id <- rownames(ec@data)
map1 <- fortify(ec)
map1$DPA_PARROQ <- ec@data$DPA_PARROQ[match(map1$id, ec@data$id)]
results <- datos
map0 <- left_join(map1, results, by="DPA_PARROQ")
m2017 <- subset(map0, is.na(timeformatted))
m2018 <- m2017
m2019 <- m2017
m2020 <- m2017
m2017$year <- "2017"
m2017$timeformatted <- "2017"
m2018$year <- "2018"
m2018$timeformatted <- "2018"
m2019$year <- "2019"
m2019$timeformatted <- "2019"
m2020$year <- "2020"
m2020$timeformatted <- "2020"
map0 <- subset(map0, !is.na(timeformatted))
map0 <- rbind(map0, m2017, m2018, m2019, m2020)
# Map organized!!!

# Ecuador complete map ----

# Casesp replacing the 0 by NA
map0$casesp[map0$casesp == 0] <- NA

# doses vaccine by sqr km
map0$doses_vac_km2[map0$doses_vac_km2 == 0] <- NA

# percentage of vaccination
map0$coverage_vac_adj[map0$coverage_vac_adj == 0] <- NA

rm(map, map1, ec, ec3, m2017, m2018, m2019, m2020, datos.map, datos1,g,map.nb, results, prec_bym2, res6)

# Entire map ecuador
#<-Fig. 7 Observed cases ----
tiff(filename = "Fig.7 observed cases.tiff", width=18, height=6,
     units="cm", res=1200,compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=casesp)) +
  geom_path(aes(x=long, y=lat, group=group), 
            colour="black", size=0.01) +
  facet_wrap(vars(timeformatted), ncol = 4) +
  # scale_fill_viridis_c(direction = -1, option =  "D", na.value = "gray90")+
  # scale_fill_continuous(low = "#fff7ec", high = "#7F0000", na.value="gray99") +
  scale_fill_continuous(low = "#31a354", high = "#440154FF", na.value="gray99") +
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill=("    Cases 
    of CSF
")) +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")
dev.off()

# < Fig. 8 Expected cases ----
quantile(datos$exp_pop_p_na, prob=c(0.75,0.994,0.9984,0.9995,0.9996,1), na.rm=TRUE)

tiff(filename = "Fig.8 expected cases.tiff", width=18, height=6,
     units="cm", res=1200,compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=exp_pop_p_na)) +
  geom_path(aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  facet_wrap(vars(timeformatted), ncol = 4) +
  # scale_fill_continuous(low = "white", high = "#7F0000", na.value="white") +
  scale_fill_continuous(low = "white", high = "#31a354", na.value="gray99") +
  theme(strip.background = element_blank(), title = NULL)+
  labs(fill= "Expected
cases
") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")
dev.off()

#< Fig. 9 Doses Km2 ----
round(quantile(map0$doses_vac_km2, prob=c(0.05,.5,.75,.95,.95,1), na.rm=TRUE),3)
summary(map0$doses_vac_km2)

tiff(filename = "Fig.9 doses km2.tiff", width=18, height=6,
     units="cm", res=1200,compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=doses_vac_km2)) +
  geom_path(aes(x=long, y=lat, group=group), 
            colour="black", size=0.01) +
  facet_wrap(vars(timeformatted), ncol = 4) +
  scale_fill_continuous(low = "#e5f5e0", high = "#31a354", na.value="gray99",
                        limits=c(0.079, 68.37), oob = scales::squish) +
  theme(strip.background = element_blank())+
  labs(fill= "  Doses
  km2

") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing.x=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")
dev.off()

#< Fig.10 Vaccination coverage ----
quantile(map0$coverage_vac_adj , prob=c(0.05,0.15,0.25,0.5,0.75,0.8,0.95,1), na.rm=TRUE)
summary(map0$coverage_vac_adj)

tiff(filename = "Fig.10 vac Coverage.tiff", width=18, height=6,
     units="cm", res=1200,compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=coverage_vac_adj)) +
  geom_path(aes(x=long, y=lat, group=group), 
            colour="black", size=0.01) +
  facet_wrap(vars(timeformatted), ncol = 4) +
  # scale_fill_viridis_c(option = "D", na.value = "white",
  #                      limits=c(0.01,1), oob = scales::squish)+
  # scale_fill_continuous(low = "#fff7ec", high = "#7F0000", na.value="white",
  #                       limits=c(0.37, 0.95), oob = scales::squish) +
  scale_fill_continuous(low = "#e5f5e0", high = "#31a354", na.value="gray99",
                        limits=c(0.15, 0.95), oob = scales::squish) +
  theme(strip.background = element_blank())+
  labs(fill= "Vaccin.
coverage

") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing.x=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")
dev.off()
-------------------------------------------------------
# Risk maps Ecuador (parish 1032)
library(scales)
library(viridis)

# I will plot every risk map, to do it I use a continuous fill, 
# with the oob especification. Worked nicely. 
# I am using limits max and min with the scale fill viridis c.
# I am using the top limit of the fill the 4th quintil limit (80%) to plot
# I am wondering if I should change to a scale_fill_continuous max and min

#< Fig.11 final model (m6) ----
quantile(datos$Risk6, prob=c(0,0.05,0.25,0.5,0.75,0.9,0.95,0.96,1), na.rm=TRUE)

tiff(filename = "Fig.9 CSF RR Bayesian m6.tiff", width=18, height=6,
     units="cm", res=1200, compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=Risk6)) +
  geom_path(aes(x=long, y=lat, group=group),
            colour="black", size=0.01) +
  facet_wrap(vars(timeformatted), ncol = 4) +
    # Fist option:
    # scale_fill_viridis_c(option = "D", na.value = "white",
    #                      limits=c(0.039,11.45), oob = scales::squish)+
    # Second option: 
    # scale_fill_viridis_c(na.value="white",
    #                    breaks = round(as.numeric(quantile(datos$Risk6,
    #                                                       prob=c(0.25,0.5,0.75,0.95), na.rm=TRUE)),1),
    #                    limits= round(as.numeric(quantile(datos$Risk6,
    #                                                      prob=c(0,.96), na.rm=TRUE)),1), oob = scales::squish)+
  # Third and last option>
  # scale_fill_continuous(low = "#fff7ec", high = "#7F0000", na.value="white",
  #                       limits=c(0.034,10.7), oob = scales::squish) +
  scale_fill_continuous(low = "#e5f5e0", high = "#31a354", na.value="gray99",
                        limits=c(0.34, 10.7), oob = scales::squish) +
  # scale_fill_continuous(low = "#541352FF", high = "#fde725FF", na.value="white") +
  # scale_fill_continuous(low = "#440154ff", high = "#fde725FF", na.value="white") + #two more contrasting purple and yelow
  # scale_fill_continuous(low = "#2a788eff", high = "#fde725FF", na.value="white") +
  theme(strip.background = element_blank())+
  labs(fill= " Relative
 Risk

") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10),
        panel.spacing.x=unit(-0.5, 'cm'))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=8))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")

dev.off()

# < Fig. 12 Priority index ----
quantile(map0$pr_index, prob=c(0,0.25,0.5,0.75,1), na.rm=TRUE)

tiff(filename = "Fig.14 Priority_Index.tiff", width=19, height=6,
     units="cm", res=1200, compression = "lzw", pointsize = 12)

ggplot(map0, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=pr_index)) +
  geom_path(aes(x=long, y=lat, group=group),
            colour="black", size=0.001) +
  facet_wrap(vars(timeformatted), ncol = 4) +
  scale_fill_continuous(low = "#e5f5e0", high = "#31a354", na.value="gray99",
                        limits=c(0.34, 10.7), oob = scales::squish) +
  
    
  theme(strip.background = element_blank())+
  labs(fill= "Priority
Index
") +
  xlim(-81.1,-75.1)+
  theme(text = element_text(size = 10))+
  theme(legend.key.width = unit(0.15, 'cm'),
        legend.title=element_text(size=0))+
  blank()+
  north(map0, symbol = 3) +
  ggsn::scalebar(map0, dist = 100, dist_unit = "km",transform = TRUE, 
                 model = "WGS84", st.size = 2, st.dist = 0.05, 
                 anchor = c(x = -76, y = -4.5),
                 height = 0.02, border.size = 0.09,
                 facet.var = "timeformatted",
                 facet.lev = "2017")

dev.off()
