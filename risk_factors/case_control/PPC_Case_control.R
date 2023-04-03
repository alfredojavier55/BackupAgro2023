-------------------------
# FLI internship case control study CSF ----
# 16/11/2020 # Insel Reims 
# Final update 10/03/2021 Writting final paper version
# Update 08/03/2022 Update with Oswaldo commentaries
-------------------------
library(tidyverse); library(lubridate); library(epiDisplay)
library(dplyr);library(sjmisc); library(ResourceSelection)
library(modEvA); library(lubridate)

# Please change to you location folder
# Location of the project ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/Code case-control")
v2 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")

# Mutating information ----

# Highest notification months ----
# Modifying dates ----
# Day of notification
v2$f_1er_enfermo <- ymd(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$month <- floor_date(v2$f_1er_enfermo, "month")

v2$Month <- month(v2$month)

# Results 
# 3.1 Descriptive ----
# --- --- --- --- --- --- --- --- 
# Number of cases and controls ----
table(v2$fcaso)
# case    control
# 338     916

# Percentage of herd types in the dataset ----
table(v2$ftof)
table(v2$ftof)/1254
months_not <- v2 %>%  
  group_by(month, Type=fcaso) %>%
  filter(Type == "case")%>%
  summarise(Number_of_events=n()) %>%
  arrange(desc(Number_of_events))


#< Fig 1. Spatial representation of study area and outbreaks----
library(rgdal);library(gdata);library(sp);library(ggsn);library(ggmap)

# Import the base map of ecuador ---
# Download map looking at the bordes of colombia and Peru
ecu2 <- get_stamenmap(bbox = c(left = - 81.4, 
                               bottom = -5.1, 
                               right = -75, 
                               top = 1.6),
                      zoom = 8, 
                      maptype = "terrain-background")

# get world borders
world <- map_data("world")

# plot cases as points
v2$lon <- as.numeric(v2$lon)
v2$lat <- as.numeric(v2$lat)

# Final figure circles and triangles
g <- 
  ggmap(ecu2) +  
  geom_path(data=world, aes(x=long, y=lat, group=group),
            size=0.1, color="black") +
  geom_point(data=v2, aes(x=lon, y=lat, shape=fcaso, fill=fcaso),
             alpha=0.8, size=1.5, stroke=0.2)+
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values=c("red", "#FDE725FF"))+
  annotate("text", x = -76.4, y = 0.9, 
           label = "Colombia", size=3)+
  annotate("text", x = -76, y = -3.5, 
           label = "Peru", size=3)+
  labs(x = "Longitude",
       y ="Latitude",
       colour="")+
  facet_wrap(~ano, ncol=4)+
  theme_void()+
  guides(colour = guide_legend(override.aes = list(size=20)))+
  theme(legend.position = c(0.9,0.2),
        legend.text=element_text(size=16),
        # legend.key.size = unit(2, "cm"),
        legend.title = element_blank())

# Add scale bar
g2 <- g +
  blank()+
  ggsn::scalebar(x.min = -78, x.max = -76,
                 y.min = -4.2, y.max = -5,
                 dist = 100, dist_unit = "km", 
                 st.size = 1.9, 
                 st.bottom = FALSE,
                 height = 0.1,
                 st.dist = 0.3,
                 border.size = 0.1,
                 model = "WGS84", transform=TRUE)

# Add north symbol and save final graphic
tiff(filename = "Fig.1 Cases and controls_spatial_300.tiff",
     width=19, height=10, units="cm",
     compression = "lzw", pointsize = 12, res = 300)
g2

dev.off()

a <- table(v2$ftof)
a[4]

#< Fig.2 Distribution of events ----
# Eventos discriminated by case and control, cat
tiff(filename = "Fig.2 Distribution of events.tiff",
     width=19, height=7, units="cm", res=600,
     compression = "lzw", pointsize = 12)

v2 %>%  mutate(ftof = (dplyr::recode(ftof,
                              '1|>310 Industrial' = "a| Industrial (>310)")))%>%
  mutate(ftof = (dplyr::recode(ftof,
                        "2|21-207 Commercial" = "b| Commercial (21–207)")))%>%
  mutate(ftof = (dplyr::recode(ftof,
                        "3|5-22 Familiar" = "c| Familiar (5–22)")))%>%
  mutate(ftof = (dplyr::recode(ftof,
                        "4|1-4 Backyard" = "d| Backyard (1–4)")))%>%
  group_by(month, ftof, Type=fcaso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(month, Number_of_events, fill=ftof))+
  facet_grid(rows = vars(Type), scales = "free_y")+
  labs(y="Number of events", 
       x="Date of start of the event (month)",
       fill="Herd type:")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.y =element_text(size=12),
        axis.text.x =element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=10),
        legend.key.size = unit(4, "mm"))+
  scale_fill_viridis_d(direction=-1)

dev.off()

# Number of premises
premises <- v2 %>%
  group_by(ano,caso,paste(v2$identificador_operador, v2$parroquia))%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  spread(key=ano , value=n)

premises$total <- rowSums(premises[,c(3:9)], na.rm = TRUE)
# 21 events happened on the same premise 
# 21/1228 1.71%

v2 %>% group_by(ano, cat)%>%
  summarise(cases=sum(caso == 1)) %>%
  spread(key="cat", value= "cases")

length(unique(paste(v2$identificador_operador, v2$parroquia)))
table(is.na(v2$codigo.sitio2))

# Table 2 Descriptive measures on continuous variables ----
v2$d_notifi <- as.numeric(v2$d_notifi)
v2$populacao <- as.numeric(v2$populacao)
v2$age_months <- as.numeric(v2$age_months)

#Case heards
v2 %>% dplyr::select(d_notifi, populacao, age_months, caso) %>%
  filter(caso == "1") %>%
  descr()

v2 %>% dplyr::select(d_notifi, populacao, age_months, caso) %>%
  filter(caso == "1") %>%
  summary()

# Control heards
v2 %>% dplyr::select(d_notifi, populacao, age_months, caso) %>%
  filter(caso == "0") %>%
  descr()

v2 %>% dplyr::select(d_notifi, populacao, age_months, caso) %>%
  filter(caso == "0") %>%
  summary()

# Fig Case and controls by epidemiological week ----
v2$Week <- as.numeric(v2$semana)

tiff(filename = "Fig.3 Distribution of cases and controls by Epiweek9.tiff",
     width=9, height=7, units="cm", res = 600, 
     pointsize = 12, compression = "lzw")

v2 %>% group_by(Week, fcaso)%>%
  summarise(N=n())%>%
  ggplot() +
  geom_col(aes(Week, N, fill=fcaso))+
  # geom_point(aes(Week, N, fill=fcaso), size=0.0001,col="#FDE725FF")+
  facet_grid(rows=vars(fcaso), scales = "free_y")+
  labs(y="Number of events", x="Epidemiological week")+
  labs(fill="")+
  theme_light()+
  theme(text = element_text(size = 14),
        axis.text.y =element_text(size=14),
        axis.text.x =element_text(size=14),
        legend.position = "")+
  scale_fill_manual(values = c("#440154FF", "#21908CFF"))
  scale_fill_viridis_d(direction = -1)

dev.off()


# boxplots graphic by epidemiological week
v2 %>% group_by(ano,Week, fcaso)%>%
  summarise(N=n())%>%
  ggplot() +
  geom_boxplot(aes(Week, N, group = Week, fill= fcaso, outlier.size=0.01))+
  # geom_point(aes(Week, N, fill=fcaso), size=0.0001,col="#FDE725FF")+
  facet_grid(rows=vars(fcaso), scales = "free_y")+
  labs(y="Annual distribution of events 2014-2020", x="Epidemiological week")+
  labs(fill="")+
  theme_minimal()+
  theme(text = element_text(size = 16),
        axis.text.y =element_text(size=14),
        axis.text.x =element_text(size=14),
        legend.position = "")+
scale_fill_manual(values = c("#756bb1", "#21908CFF"))


#< Fig. 3 Monthly boxplot graphic---- 
  tiff(filename = "Fig.3.1 Montly Distribution of cases and controls.tiff",
       width=9, height=7, units="cm", res = 600, 
       pointsize = 12, compression = "lzw")

v2 %>% group_by(ano,Month,fcaso)%>%
  summarise(N=n())%>%
  ggplot() +
  geom_boxplot(aes(factor(Month), N, group = factor(Month), fill= fcaso),
               outlier.size=0.5,
               outlier.colour="#10a53dFF")+
  # geom_point(aes(Week, N, fill=fcaso), size=0.0001,col="#FDE725FF")+
  facet_grid(rows=vars(fcaso), scales = "free_y")+
  labs(y="Annual distribution of events", x="Month")+
  labs(fill="")+
  theme_minimal()+
  theme(text = element_text(size = 11),
        axis.text.y =element_text(size=12),
        axis.text.x =element_text(size=12),
        legend.position = "")+
  scale_fill_manual(values = c("#756bb1", "#21908CFF"))
  dev.off()

#Case heards
v2 %>% group_by(ano, Month, fcaso)%>%
  filter(fcaso == "case") %>%
  summarise(N=n())%>%
  summary()

case.herds <- v2 %>% group_by(ano, Month, fcaso)%>%
  filter(fcaso == "case") %>%
  summarise(N=n())
  
sd(case.herds$N)

v2 %>% group_by(ano, Month, fcaso)%>%
  filter(fcaso == "control") %>%
  summarise(N=n())%>%
  summary()

control.herds <- v2 %>% group_by(ano, Month, fcaso)%>%
  filter(fcaso == "control") %>%
  summarise(N=n())

sd(control.herds$N)


#to analize by year
v2 %>% group_by(Week, fcaso, ano)%>%
  summarise(N=n())%>%
  ggplot() +
  geom_col(aes(Week, N, fill=fcaso))+
  geom_point(aes(Week, N, fill=fcaso), size=0.1)+
  facet_grid(cols=vars(fcaso), rows = vars(ano))+
  labs(y="Number of events", x="Epidemiological week")+
  labs(fill="")

# Proportion cases/control
v2 %>% group_by(ano)%>%
  summarise(cases=sum(caso == 1), controls=sum(caso ==0),
            cases.controls=controls/cases)

# ano   cases controls cases.controls
# 1 2014     79       76          0.962
# 2 2015     82      247          3.01 
# 3 2016     30      112          3.73 
# 4 2017     39      134          3.44 
# 5 2018     60      151          2.52 
# 6 2019     35      132          3.77 
# 7 2020     13       64          4.92 

# ----Variables----
# > Univariable analysis ----
# Logistic regression----
library(stats); library(epiDisplay)
v2 <- v2[order(v2$orden),]
# 0 Dependent variable Case ----
v2$fcaso <- factor(v2$caso, levels = c(0,1), labels = c("control", "case"))
table(v2$fcaso)
# plot(v2$fcaso)
fit <- glm(v2$fcaso ~ 1, family = binomial(logit))
# plot(fit)

#1 Vaccine auto declaration -fvacuno----
v2$fvacuno <- as.factor(v2$fvacuno)
tabela <- xtabs(~ v2$fvacuno + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.vacuno <- glm(v2$fcaso ~ v2$fvacuno, family = binomial(logit))
summary(fit.vacuno)
cc(v2$fcaso, v2$fvacuno)
cbind(exp(coef(fit.vacuno)), exp(confint(fit.vacuno)))
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fvacuno) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fvacuno, Number_of_events, fill=fcaso))

#2 Swill feed -fdes_lav----
v2$fdes_lav <- as.factor(v2$des_lav2)

# changing swill NO to 6 
v2$fdes_lav[v2$orden == "-33"] <- "NO"
v2$fdes_lav[v2$orden == "-91"] <- "NO"
v2$fdes_lav[v2$orden == "14"] <- "NO"
v2$fdes_lav[v2$orden == "16"] <- "NO"
v2$fdes_lav[v2$orden == "2813"] <- "NO"
v2$fdes_lav[v2$orden == "901"] <- "NO"

tabela <- xtabs(~ v2$fdes_lav + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.desper <- glm(v2$fcaso ~ v2$fdes_lav, family = binomial(logit))
summary(fit.desper)
# plot(fit.desper)
cbind(exp(coef(fit.desper)), exp(confint(fit.desper)))
cc(v2$fcaso, v2$fdes_lav)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fdes_lav) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fdes_lav, Number_of_events, fill=fcaso))

#3 Notification time - fd_notifi----
#library(lubridate)
v2$fd_notifi <- as.factor(v2$fd_notifi)
table(v2$fd_notifi)

tabela <- xtabs(~ v2$fd_notifi + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.d_notifi <- glm(v2$fcaso ~ v2$fd_notifi, family = binomial(logit))
summary(fit.d_notifi)
cc(v2$fcaso, v2$fd_notifi)
exp(coef(fit.d_notifi))
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fd_notifi) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fd_notifi, Number_of_events, fill=fcaso))

#4 Animal entry -fingreso----
v2$fingreso <- as.factor(v2$fingreso)
tabela <- xtabs(~ v2$fingreso + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.ingreso <- glm(v2$fcaso ~ v2$fingreso, family = binomial(logit))
summary(fit.ingreso)
cc(v2$fcaso, v2$fingreso)
exp(coef(fit.ingreso))
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fingreso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fingreso, Number_of_events, fill=fcaso))

#5 Vaccine registry - fvac----
v2$fvac <- factor(v2$fvac)
table(v2$fvac)
tabela <- xtabs(~ v2$fvac + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.vacuna.registro <- glm(v2$fcaso ~ v2$fvac, family = binomial(logit))

cc(v2$fcaso, v2$fvac)
summary(fit.vacuna.registro)
exp(coef(fit.vacuna.registro))
cc(v2$fcaso, v2$fvac)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fvac) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fvac, Number_of_events, fill=fcaso))

#6 Region -fregiao----
# re referencing
table(v2$regiao)
v2$fregiao <- v2$regiao
table(v2$fregiao)

v2$fregiao <- gsub("Highlands", "1Highlands", v2$fregiao)
v2$fregiao <- gsub("Coastal", "2Coastal", v2$fregiao)
v2$fregiao <- gsub("Amazonic", "3Amazonic", v2$fregiao)

table(v2$fregiao)

v2$fregion <- as.factor(v2$fregiao)
tabela <- xtabs(~ v2$fregion + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela,1),2)

fit.regiao <- glm(v2$fcaso ~ v2$fregion, family = binomial(logit))
summary(fit.regiao)
exp(coef(fit.regiao))
exp(confint(fit.regiao))
cc(v2$fcaso, v2$fregiao)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fregion) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fregion, Number_of_events, fill=fcaso))

#7 Year -fyear----
v2$fyear <- as.factor(v2$fyear)
tabela <- xtabs(~ v2$fyear + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela,1),2)

fit.year <- glm(v2$fcaso ~ v2$fyear, family = binomial(logit))
summary(fit.year)
exp(coef(fit.year))
cc(v2$fcaso, v2$fyear)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fyear) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fyear, Number_of_events, fill=fcaso))

#8 Age in months - fage ----
v2$fage <- as.factor(v2$fage)
tabela <- xtabs(~ v2$fage + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.idade <- glm(v2$fcaso ~ v2$fage, family = binomial(logit))
summary(fit.idade)
exp(coef(fit.idade))
round(exp(confint((fit.idade))),2)
cc(v2$fcaso, v2$fage)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fage) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fage, Number_of_events, fill=fcaso))

#9 Control program - fcp ----
v2$fcp <- as.factor(v2$fcp)

tabela <- xtabs(~ v2$fcp + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.cp <- glm(v2$fcaso ~ v2$fcp, family = binomial(logit))
summary(fit.cp)
cc(v2$fcaso, v2$fcp)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fcp) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fcp, Number_of_events, fill=fcaso))

#10  Notificator -fnotificador----
v2$fnotificador <- as.factor(v2$fnotificador)

tabela <- xtabs(~ v2$fnotificador+ v2$fcaso)
addmargins(tabela)
round(prop.table(tabela,1),2)

fit.notificador <- glm(v2$fcaso ~ v2$fnotificador, family = binomial(logit))
summary(fit.notificador)
exp(coef(fit.notificador))
cc(v2$fcaso, v2$fnotificador)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fnotificador) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fnotificador, Number_of_events, fill=fcaso))

#11 Farm size -fsize----
v2$fsize <- as.factor(v2$fsize)
tabela <- xtabs(~ v2$fsize + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela,1),2)

fit.s <- glm(v2$fcaso ~ v2$fsize, family= binomial(logit))
summary(fit.s)
cc(v2$fcaso, v2$fsize)
exp(fit.s$coefficients)
chisq.test(tabela, correct = F)

v2 %>% group_by(fsize, fcaso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fsize, Number_of_events, fill=fcaso))

#12 Comunity -fcom----
v2$fcom <- as.factor(v2$fcom)
tabela <- xtabs(~ v2$fcom + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.com <- glm(v2$fcaso ~ v2$fcom, family = binomial(logit))
summary(fit.com)
exp(coef(fit.com))
cc(v2$fcaso, v2$fcom)
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fcom) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fcom, Number_of_events, fill=fcaso))

#13 Breed -fraza----
# The reference is the white animals
v2$fraza <- factor(v2$fraza)

tabela <- xtabs(~ v2$fraza + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.raza <- glm(v2$fcaso ~ v2$fraza, family = binomial(logit))
cc(v2$fcaso, v2$fraza)
summary(fit.raza)
exp(coef(fit.raza))
chisq.test(tabela, correct = F)

v2 %>% group_by(fcaso, fraza) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fraza, Number_of_events, fill=fcaso))

# 14 Other species ----
table(v2$foe)
tabela <- xtabs(~ v2$foe + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela, 1),2)

fit.oe <- glm(v2$fcaso ~ v2$foe, family = binomial(logit))
exp(fit.oe$coefficients)
summary(fit.oe)
cc(v2$fcaso, v2$foe)
chisq.test(tabela, correct = F)

v2 %>% group_by(fotras.esp, fcaso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(fotras.esp, Number_of_events, fill=fcaso))

# 15 Type of farm -tof ----
# Production system of the farm
v2$ftof <- as.factor(v2$ftof)
table(v2$ftof)

tabela <- xtabs(~ v2$ftof + v2$fcaso)
addmargins(tabela)
round(prop.table(tabela), 2)

fit.tof <- glm(v2$fcaso ~ v2$ftof, family = binomial(logit))
summary(fit.tof)
cc(v2$fcaso, v2$ftof)
chisq.test(tabela, correct = F)
table(v2$fcaso)
v2 %>% group_by(fcaso, ftof) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(ftof, Number_of_events, fill=fcaso))


# > Multivariavel analysis ----
# * Final model 
# Multi Model in order----
m1 <- glm(v2$fcaso ~ v2$fdes_lav, family = binomial(logit))
summary(m1)
anova(fit, m1, test = "Chi")
round(cbind(exp(coef(m1)), exp(confint(m1))),2)

m2 <- update(m1, .~. + v2$fd_notifi)
summary(m2)
anova(m1, m2, test = "Chi")
round(cbind(exp(coef(m2)), exp(confint(m2))),2)

m3 <- update(m2, .~. + v2$fingreso)
summary(m3)
anova(m2, m3, test = "Chi")
round(cbind(exp(coef(m3)), exp(confint(m3))),2)

m4 <- update(m3, .~. + v2$fvac)
summary(m4)
anova(m3, m4, test = "Chi")
round(cbind(exp(coef(m4)), exp(confint(m4))), 2)

m5 <- update(m4, .~. + v2$fregion)
summary(m5)
anova(m4, m5, test = "Chi")
round(cbind(exp(coef(m5)), exp(confint(m5))),2)

m6 <- update(m5, .~. + v2$fregion:v2$fd_notifi)
summary(m6)
anova(m5, m6, test = "Chi")
round(cbind(exp(coef(m6)), exp(confint(m6))),2)

library(modelsummary)
msummary(m6, exponentiate = TRUE,stars = c('*' = .1, '**' = .05, '***' = .01))

library(fixest)
library(AER)
library(ResourceSelection)
library(modEvA)
coeftest(m6, vcov = vcovHC(m6))
exp(coeftest(m6, vcov = vcovHC(m6)))

# Testing VIF Colinearidade
car::vif(m6)

#Testing fit of the model
hoslem.test(m6$y, fitted(m6)) #0.817% good fit
plotGLM(obs = m6$y, pred = m6$fitted.values) #r2:0.163, d2=0.15
AUC(model = m6) #0.746

########---------
# Testing interactions
# There was a modification on the value of OR superior to 10%
# when entering notification delay and animal entry

# No significative and lost fit
m7 <- update(m6, .~. + v2$fregion:v2$fd_notifi)
m7 <- update(m6, .~. + v2$fvacuno)
summary(m7)
cbind(round(exp(coef(m7)),2), round(exp(confint(m7)),2))
anova(m6, m7, test = "Chi")#  0.0074 **

m8 <- update(m7, .~. + v2$fvacuno:v2$fdes_lav)
summary(m8)
cbind(round(exp(coef(m8)),2), round(exp(confint(m8)),2))
anova(m6, m7, test = "Chi")#  0.0074 **

car::vif(m7)

m7 <- update(m6, .~. + v2$fdes_lav:v2$fd_notifi)
summary(m7)
cbind(round(exp(coef(m7)),2), round(exp(confint(m7)),2))
anova(m6, m7, test = "Chi")#  0.0074 **

m8 <- update(m6, .~. + v2$fdes_lav:v2$fingreso)
summary(m8)
cbind(exp(coef(m8)), exp(confint(m8)))
anova(m6, m8, test = "Chi") 

m9 <- update(m6, .~. + v2$fsize)
summary(m9)
cbind(round(exp(coef(m9)),3), round(exp(confint(m9)),3))
anova(m6, m9, test = "Chi")

m10 <- update(m9, .~. + v2$fdes_lav:v2$fsize)
summary(m10)
cbind(round(exp(coef(m10)),2), round(exp(confint(m10)),3))
anova(m6, m9, test = "Chi")

car::vif(m10)

# Melhor modelo agora é o m4, oas outras variaveis foram tiradas
# porque 
m6 <- m4

# GOF test ----
# Hosmer and Lemeshow goodness of fit (GOF) test ----
library(ResourceSelection)
hoslem.test(m6$y, fitted(m6)) #0.78
# install.packages("modEvA", repos = "http://R-Forge.R-project.org")

plotGLM(obs = m6$y, pred = m6$fitted.values)

#< Fig.4 Coefficients estimates----
# https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html
# install.packages("sjPlot")
library(sjmisc)
library(sjlabelled)
library(tidyverse)
# creating a data frame with de information
#I should use recode to change the variables in spanish

# create data frame for fitting model
df <- data.frame(
  y = to_factor(v2$fcaso),
  'Swill feeding' = to_factor(v2$fdes_lav),
  'Notif time' = to_factor(v2$fd_notifi),
  'Animal entry' = to_factor(v2$fingreso),
  'CSF Vaccine' = v2$fvac
)

df <- df %>%
  mutate(Swill.feeding=(dplyr::recode(Swill.feeding,"SI" = "Yes")))%>%
  mutate(Animal.entry=(dplyr::recode(Animal.entry,"SI" = "Yes")))%>%
  mutate(CSF.Vaccine=(dplyr::recode(CSF.Vaccine,"2no" = "No")))%>%
  # mutate(Region=(dplyr::recode_factor(Region, "1Highlands" = "Highlands", "2Coastal"="Coastal", "3Amazonic"= "Amazon")))%>%
  mutate(Notif.time=(dplyr::recode(Notif.time,"b|> 7 days" = ">7 days")))%>%
  # mutate(Age.of.pig=(dplyr::recode(Age.of.pig,"b| 3-6"="3-6 months","c|>=7"="≥7 months")))
table(df$Region)  

# fit model
m66 <- glm(y ~., data = df, family = binomial(link = "logit"))
library(sjPlot)

pm <- plot_model(m66,
            axis.title = "Odds Ratio",
            auto.label = FALSE, 
            sort.est = NULL,
            show.values = TRUE,
            value.offset = 0.4,
            axis.lim = c(.8, 30),
            # vline.color = "#10a53dFF",
            vline.color = "#55c667ff",
            # colors = "viridis",
            colors = c("#414487ff","#440154ff","#55c667ff","#55c667ff","#55c667ff","#55c667ff"),
            dot.size = 1,
            value.size = 3,
            jitter = 3,
           )
pm

tiff(filename = "_Fig.4 coefficient estimates_highlands_ref.tiff",
     width=9, height=6, units="cm", 
     compression = "lzw", pointsize = 12, res=600)
pm +
  theme_light()+
  annotate(geom="text", label="20", x=0.68, y=18, color="grey30", size=3.3)+
  theme(text=element_text(size=12))

dev.off()

Dsquared(model = m6) #0.132

#  Model Plot
plot(m6)

# Residuals plot 
# Read about the residual analysis
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4885900/pdf/atm-04-10-195.pdf
tiff(filename = "Fig. Residuals.tiff",
     width=9, height=7, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

plot(m6$residuals, type="p", cex=0.25, ylim=c(-10,15), ylab= "Residuals")
abline(h=(5), col="red")
abline(h=(-5), col="red")

dev.off()

hist(m6$residuals)

qqnorm(residuals(m6))
qqline(residuals(m6))

res <- resid(m6)

plot(fitted(m6), res)

#add a horizontal line at 0 
abline(0,0)

qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

plot(density(res))

library(car)
car::residualPlots(m6)

tiff(filename = "Fig. Influence_index.tiff",
     width=10, height=18, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

car::influenceIndexPlot(m6, cex=0.01, main=NULL)
dev.off()
plot

# Outlier Test bonferroni
car::outlierTest(m6)

#<  Fig. 5 ROC curve with ggplot ----
library(pROC)

# modEVA package
modEvA::AUC(model = m6)

v2$predic <- predict(m6)

roc <- roc(v2$fcaso, v2$predic) #creating an roc object
r <- ggroc(roc, legacy.axes = TRUE, colour="#55C667FF", size=1) #generating the roc curve

# Saving with tiff
tiff(filename = "Fig.5 ROC curve.tiff",
     width=9, height=5, units="cm", res=600,
     compression = "lzw", pointsize = 12)
r+
  ggtitle("ROC curve") + 
  geom_segment(aes(x = 0, xend=1, y = 0, yend=1), linetype="dashed", color="grey",)+ #creating the diagonal line
  theme(text = element_text(size = 8))+
  theme_light()+
  ylab("True positive rate \n (sensitivity)") +
  xlab("False positive rate \n(1-specificity)")+
  annotate("text", x = 0.8, y = 0.2, 
         label = "AUC=0.746", size=3.5)
  
dev.off()

#--------------------------------------------------
#For matched 1254 cases and controls model 6
# Graficos ----
predicted.data <- data.frame(
  probabilidade = m6$fitted.values,
  Case_Prob = m6$y)

predicted.data <- predicted.data[
  order(predicted.data$Case_Prob,
        decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

head(predicted.data)

cpo <- ggplot(data=predicted.data, aes(x=rank, y=probabilidade)) +
  geom_point(aes(x=rank, y=Case_Prob), stroke=0.01)+ 
  geom_point(aes(color=Case_Prob), alpha=0.9, shape=1, stroke=0.5) +
  theme(text = element_text(size = 10))+
  xlab("Observed") +
  ylab("Case probability")+
  labs(tag = "a",
       color="Pred.Prob")+
  scale_color_viridis_c()+
  theme_linedraw()
cpo

#< Fig 6 predicted probability ----
#Probabilidade da observacao individual de ter sido parto ces?rea 
predicted.data <- data.frame(
  probabilidade.caso=m6$fitted.values,
  p.caso=m6$y)

v2$predic <- predicted.data$probabilidade.caso

predicted.data <- predicted.data[
  order(predicted.data$probabilidade.caso, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

head(predicted.data)

cp <- ggplot(data=predicted.data, aes(x=rank, y=probabilidade.caso)) +
  geom_point(aes(x=rank, y=probabilidade.caso), stroke=0.05)+ 
  geom_point(aes(color=probabilidade.caso), alpha=0.9, shape=1, stroke=0.5) +
  theme(text = element_text(size = 10))+
  xlab("Predicted") +
  ylab("Case probability") +
  labs(color = "Pred.
Prob")+
  scale_color_viridis_c()+
  labs(tag = "b")+
  theme_linedraw()

cp

#< Fig 4 a,b ---- Predicted probability
tiff(filename = "Fig.6 predict probability.tiff",
     width=9, height=12, units="cm", res=600,
     compression = "lzw", pointsize = 12)

ggpubr::ggarrange(cpo, cp, ncol = 1,
                  common.legend = TRUE,
                  legend = "right")
dev.off()

v2$predic <- as.numeric(v2$predic)
sum(v2$predic)

length(unique(v2$DPA_PARROQ))

1040-441

# Test Corrections
library(car)
plot(m6)
residualPlots(m6)
influenceIndexPlot(m6, cex=0.01, main=NULL)

#Influence circles plots
influencePlot(m6,col="red", scale=10)

# Outlier Test
car::outlierTest(m6)

#<  Fig. 5 ROC curve with ggplot ----
library(pROC)
# install.packages("pscl")
pscl::pR2(m6)["McFadden"]

# Install.packages("caret") 
# Variable importante for regression
caret::varImp(m6)

car::vif(m6)

