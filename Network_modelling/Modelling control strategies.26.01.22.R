# Modelling control strategies against Classical Swine Fever: influence of traders and
# markets using static and temporal networks in Ecuador
# Alfredo Acosta, PhD Candidate
# Nicolas Cespedes Cardenas, PhD
# Cristian Imbacuan, DVM
# Harmut H.K. Lentz, PhD
# Klaas Dietze, PhD
# Marcos Amaku, PhD
# Alexandra Burbano, MVD
# Vitor S.P Gonçalves, PhD
# Fernando Ferreira, PhD

library(Matrix); library(epinemo);library(dplyr); library(ggplot2);library(scales)

# 1 Describing network structure ----
# setwd("/home/alfredo/Network characterization/Network")
# setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network")
# m3 <- read.csv("mov2016_2020_mSlaugther.csv", colClasses = "character")

# # This file include the correction on markets
# m2 <- read.csv("mov2017_2019m2_market.csv", colClasses = "character")

# 2 Loading data ----

# Confirming this arquive 27.05.2021 
#The one used with the paper
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Paper_Network")
# setwd('/home/alfredo/Dados/Paper_Network/')
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")

# https://drive.google.com/file/d/1BNmaOfSJ2ce2KRYUjAAKQ-kijAvqz6Bs/view?usp=sharing
m2 <- m2[,-1]
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) #9904714

# 3 Creating network of premisses ----
m2 <- m2[m2$operacion.destino != "Faenador",]
sum(m2$cantidad) #6,407.392
table(m2$ano)
# Using just 2019 for the simulation ----
# To use only in the simulation
m2019 <- m2[m2$ano == "2019",]
m2019 <- data.frame(m2019)
m2019$cantidad <- as.numeric(m2019$cantidad)
sum(m2019$cantidad) #2475853 #deleting the faenador
m2 <- m2019

# 2839400 2017
# 3306494 2018
# 3758820 2019

# Using the entire dataset for descriptive ----
m2 <- data.frame(m2)

# Use m2019 for simulation and m2 total for other analysis
# 4 Create the banco (unique Id)----
banco <- createUniqueIds(m2,
                         from = 'codigo.sitio.origen',
                         to = 'codigo.sitio.destino')

banco$movements$cantidad <- as.numeric(banco$movements$cantidad)

length(unique(banco$correspondence$database.id))


banco$correspondence %>%
  group_by(ano)%>%
  summarise(fazendas=length(unique(codi)))



# Creating the adjacency matrix ----
#  Cria uma matriz onde estao os uns mas guarda os ceros
#  Acrescentando o numero de animais, aqui so trabalharemos
# com o numero de Atestados X=1 significa isso
# matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
#                        x=banco$movements$cantidad,
#                        dims = rep(max(banco$movements$From, banco$movements$To) ,2))
#Degree ktotal
matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                       x=1,
                       dims = rep(max(banco$movements$From, banco$movements$To) ,2))
# Degree ----
#  ####################
kin <- colSums(matriz)
kout <- rowSums(matriz)
ktotal <- kin + kout


# Degree to correspondence ----
banco$correspondence$ktotal <- ktotal
banco$correspondence$kin <- kin
banco$correspondence$kout <- kout  


# 4.1 # Weighted degree by number of pigs 
matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                       x=banco$movements$cantidad,
                       dims = rep(max(banco$movements$From, banco$movements$To) ,2))
kin <- colSums(matriz)
kout <- rowSums(matriz)
ktotal_animais <- kin + kout


banco$correspondence$ktotal_animais <- ktotal_animais

pr <- pageRank(matriz)
banco$correspondence$page_r <- pr  

summary(pr)
pareto(matrizv)


#< Fig. 1 Premises maps premises and density ----
# 17 Ploting the network ----

# looking cadastral location of premises ordered on the correspondence banc

origen <- data.frame(banco$movements$codigo.sitio.origen,
                     banco$movements$provincia.origen, 
                     banco$movements$canton.origen,
                     banco$movements$parroquia.origen,
                     banco$movements$ano)

destino <- data.frame(banco$movements$codigo.sitio.destino,
                      banco$movements$provincia.destino,
                      banco$movements$canton.destino,
                      banco$movements$parroquia.destino,
                      banco$movements$ano)

colnames(origen) <- c("codigo.sitio", "provincia", "canton", "parroquia", "ano")
colnames(destino) <- c("codigo.sitio", "provincia", "canton", "parroquia","ano")

od <- rbind(origen,destino)

map_premises <- od %>%
  group_by(codigo.sitio, provincia, canton,parroquia) %>%
  summarize(n())

library(dplyr)
vigi <- map_premises %>%
  group_by(provincia,
           canton,
           parroquia) %>%
  summarise(cantidad = n())

sum(vigi$cantidad) #93707 #165648  #2019=1000998

#passing throug map to agregate in the spatial map
library(rgdal)
library(gdata)
library(sp)

ec3<-rgdal::readOGR(dsn="~/Dropbox/0.USP/5. 2018 II semestre/1 Biologia de sistemas/SHP",layer="nxparroquias")
ec3 <- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# Available on the drive link
ec3 <- subset(ec3, DPA_DESPRO != "GALAPAGOS")
ec3@data$area <- raster::area(ec3) /1000000

library(glue)
# Versao guia adptada para cadastro
f{
  # Mapa para vigilancia
  #Provincia
  #atualizado 27.01.2020 banco cadastro 2018-2019 com codigo de sitio
  # fazer colunas comparaveis
  library(raster)
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

ec3@data$id <- rownames(ec3@data)
ec3@data$density <- ec3@data$cantidad/ec3@data$area
density <- data.frame(ec3@data$density)
colnames(density) <- "density"
density <- mutate(density, density = ifelse(density <=1,1,density))
density <- round(density$density,0)
summary(density)
ec3@data$density <- density
summary(ec3@data$density)

# deleting pastaza araujo curarai

ec3@data$cantidad[ec3@data$id =="294"] <- NA
ec3@data$density[ec3@data$id =="294"] <- NA




# three years 2017-2019
# Using the mean of the three years study period
ec3@data$cantidad_mean <- ec3@data$cantidad/3
sum(ec3@data$cantidad_mean, na.rm = TRUE)
#55215.33

cantidad <- data.frame(ec3@data$cantidad_mean)
colnames(cantidad)[1] <- "cantidad"
cantidad <- mutate(cantidad, cantidad2 = 
                     ifelse(cantidad <=1,1, cantidad))
cantidad$cantidad3 <- round(cantidad$cantidad2,0)
sum(cantidad$cantidad3, na.rm = TRUE) #55236
ec3@data$cantidad3 <- cantidad$cantidad3

# density
ec3@data$density2 <- ec3@data$cantidad3/ec3@data$area
density <- data.frame(ec3@data$density2)
colnames(density)[1] <- "density"
density <- mutate(density, density2 = 
                    ifelse(density <=1,1, density))
density$density3 <- round(density$density2,0)
sum(density$density3, na.rm = TRUE) #55236

ec3@data$density3 <- density$density3

# Summary statistics for 2017-2019 and average of year
summary(ec3@data$density)
summary(ec3@data$density3)
hist(ec3@data$density3)

summary(ec3@data$cantidad)
summary(ec3@data$cantidad3)
table(ec3@data$cantidad3)
hist(ec3@data$cantidad3, freq=FALSE)
rug(ec3@data$cantidad3)

data <- data.frame(ec3@data)
library(scales)

# < Fig 1.1 Histogram of premises ----
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

tiff(filename = "Fig.1.1 Histogram number of premises.tiff",
     width=3.5, height=3, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggplot(data, aes(cantidad3))+
  geom_histogram(col="black", fill="grey", size=0.1)+
  geom_rug(size=0.1)+
  theme_minimal()+
  labs(x="Number of premises", y= "Frequency")+
  theme(text = element_text(size = 8))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())


dev.off()

hist(data$cantidad3)

# < Fig 1.2 Histogram of density ----

tiff(filename = "Fig.1.2 Histogram density of premises.tiff",
     width=3.5, height=3, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggplot(data, aes(density3))+
  geom_histogram(col="black", fill="grey", size=0.1)+
  geom_rug(size=0.1)+
  theme_minimal()+
  #scale_x_continuous(trans = log2_trans())
  labs(x="Density of premises", y= "Frequency")+
  theme(text = element_text(size = 8))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

dev.off()

summary(ec3@data$area)

quantile(ec3@data$cantidad3, prob=c(0.1,0.25,0.5,0.75,0.9,0.95,0.995,1), na.rm=TRUE)

# 10%      25%      50%      75%      90% 
# 1.000    3.000   11.000   38.000  117.000 
# 95%    99.5%     100% 
# 246.350 1144.135 2689.000

quantile(ec3@data$density3, prob=c(0.1,0.25,0.5,0.75,0.9,0.95,0.995,1), na.rm=TRUE)


# After selecting the whole dataset 2017-2019 or the mean
map <- fortify(ec3)
map <- left_join(map, ec3@data, by="id")

# Fig. 1 premises and densities mean of 2017-2019 ----
library(ggsn)
# Map of density

densitym <-
  ggplot(map, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=density3)) +
  geom_path(aes(x=long, y=lat, group=group), colour="grey60", size=0.04)+
  scale_fill_viridis_c(option = "D", na.value = "white", direction = -1,
                       breaks = c(1,2,5,15,30),
                       values = scales::rescale(c(1,2,5,15,30))) +
  theme_minimal()+
  theme(legend.key.height= unit(1.3, 'cm'),
        legend.key.width = unit(0.25, 'cm')) +
  labs(tag = "b")+
  labs(x="Longitude", y= "Latitude", fill= "Density") +
  theme(text = element_text(size = 9))+
  blank()+
  north(map, symbol = 3) +
  ggsn::scalebar(map, dist = 100, dist_unit = "km", transform = TRUE, 
                 st.size = 2, height = 0.01, border.size = 0.07,
                 model = "WGS84", location = "bottomleft")

# Map of premises
premisesm <-
  ggplot(map, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=cantidad3)) +
  geom_path(aes(x=long, y=lat, group=group), colour="grey60", size=0.04)+
  scale_fill_viridis_c(option = "C", na.value = "white", direction = -1,
                       breaks = c(38,117,246,1144,2689),
                       values = scales::rescale(c(38,117,246,1144,2689))) +
  theme_minimal()+
  theme(legend.key.height= unit(1.3, 'cm'),
        legend.key.width = unit(0.25, 'cm'))+
  theme(text = element_text(size = 9))+
  labs(tag = "a")+
  labs(x="Longitude", y= "Latitude", fill= "Premises") +
  blank()+
  north(map, symbol = 3) +
  ggsn::scalebar(map, dist = 100, dist_unit = "km", transform = TRUE,
                 st.size = 2, height = 0.01, border.size = 0.07,
                 model = "WGS84", location = "bottomleft")

# Joining the two maps
tiff(filename = "Fig.1 Density and premises.tiff",
     width=19, height=8, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggpubr::ggarrange(premisesm, densitym, ncol = 2)  

dev.off()



#< Fig. 2 Descriptive number of movements and animals ----
# 78% of the total area
sum(ec3@data$area[is.na(ec3@data$cantidad)== FALSE])/sum(ec3@data$area)
table(is.na(ec3@data$cantidad))
# 87% of ecuador parishes
1-(88/(944+88))
# 91%


summary(ec3@data$density3)
table(ec3@data$density)
quantile(ec3@data$density, prob=c(0.1,0.8,0.85,0.9, 0.95,1), na.rm=TRUE)

summary(ec3@data$cantidad)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.0     5.0    18.0   104.1    63.0  4659.0     132 

table(ec3@data$cantidad)
quantile(ec3@data$cantidad, prob=c(0.1,0.2,0.5,0.8,0.85,0.9, 0.95,1), na.rm=TRUE)
1,10,20,40,80,100,200,500,4659

# Descriptive network
table(m2$operacion.origen)
table(m2$operacion.destino)

# Total
m2 %>%
  summarize(n=n(), animals=sum(cantidad))

# By years
m2 %>%
  group_by(ano) %>%
  summarize(n=n(), animals=sum(cantidad))

m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Collector",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial Farm",
                                   "Productor" = "Backyard-commercial Farm")) %>%
  group_by(operacion.origen) %>%
  summarize(n=n(), animals=sum(cantidad))%>%
  summarize(porcentage.mov=n/sum(n), percentage.ani=animals/(sum(animals)) )


operacion.origen              n animals
1 Backyard-commercial Farm 428162 2100520
2 Collector                 32717  223211
3 Industrial Farm           32192 3036011
4 Market                   257932 1047650



porcentage.mov percentage.ani
1         0.570          0.328 
2         0.0436         0.0348
3         0.0429         0.474 
4         0.343          0.164 

# Fig. 2 Outgoing animals and movements ----
a <- m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  group_by(operacion.origen, ano) %>%
  summarize(n=n(), animals=sum(cantidad)) %>%
  ggplot(aes(ano, animals, fill=operacion.origen)) +
  labs(tag = "b")+
  geom_col()+
  labs(x=NULL,
       y="Number of animals",
       fill="Type of 
premises")+
  theme_linedraw()+
  theme(text = element_text(size = 9))+
  scale_y_continuous(labels = scales::scientific)

a
b <- m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  group_by(operacion.origen, ano) %>%
  summarize(n=as.numeric(n()), animals=sum(cantidad)) %>%
  ggplot(aes(ano, n, fill=operacion.origen)) +
  geom_col()+
  labs(tag = "a")+
  labs(x=NULL,
       y="Number of movements",
       fill="Type of
premises") +
  theme_linedraw()+
  scale_color_binned(palette = "Set2")+
  theme(text = element_text(size = 9))

library(ggpubr)
ggarrange(b, a, common.legend = TRUE, legend="right", ncol=2)


# Comments by reviewers 20.01.22----

# Mean/media/ pigs per movement
# considering the year
# SM1 movements to slaughter
m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  group_by(operacion.origen, ano) %>%
  summarize(Mov=n(),
            Pigs=sum(cantidad), 
            mean_pigs=round(mean(cantidad),2),
            median_pigs=median(cantidad),
            n_t=length(unique(codigo.sitio.origen)),
            Pigs/3497322,
            Mov/439988)
  SM2
  
# Between farms (using database of mov between premises)
m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  group_by(operacion.origen,ano) %>%
  summarize(Mov=n(),
            Pigs=sum(cantidad), 
            median_pigs=median(cantidad),
            mean_pigs=round(mean(cantidad),2),
            n_t=length(unique(codigo.sitio.origen)),
            Pigs/6407392,
            Mov/751003)


  # Between all network
m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  # group_by(operacion.origen,ano) %>%
  summarize(Mov=n(),
            Pigs=sum(cantidad), 
            median_pigs=median(cantidad),
            mean_pigs=round(mean(cantidad),2),
            n_t=length(unique(codigo.sitio.origen)),
            Pigs/9904714,
            Mov/1190991)



# Without the year but lookik at the number of movementsthat each type of farm make every year. (not completed)
m2 %>%
  mutate(operacion.origen = recode(operacion.origen, 
                                   "Comercializador" = "Trader",
                                   "Feria de comercialización animal" = "Market",
                                   "Operador Industrial" = "Industrial",
                                   "Productor" = "Farm")) %>%
  # group_by(operacion.origen) %>%
  summarize(Mov=n(),
            Pigs=sum(cantidad), 
            mean_pigs=mean(cantidad),
            median_pigs=median(cantidad),
            mean_mov=mean(length(unique(codigo.sitio.origen))),
            mov_per_type=Mov/mean_mov,
            med_mov=median(length(unique(codigo.sitio.origen))),
            mov_per_type=Mov/med_mov,)

# Joining the two maps
# To visualize the image change elemten_text(size=12)
# To save as tiff change to size 9

setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

tiff(filename = "Fig.2 Number of movements and animals.tiff",
     width=9, height=8, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggarrange(b, a, common.legend = TRUE, legend="bottom", ncol=2)

dev.off()



# Aditional code

decile %>%
  mutate(operacion = recode(operacion, 
                            "Comercializador" = "Collector",
                            "Feria de comercialización animal" = "Market",
                            "Operador Industrial" = "Industrial Farm",
                            "Productor" = "Backyard-commercial Farm")) %>%
  filter(operacion =="Market") %>%
  ggplot(aes(quintile, page_r, fill=operacion), col="grey") +
  geom_boxplot(width=0.3)+
  theme_linedraw()+
  labs(x = "Degree (ingoing + outgoing movements)",
       y ="PageRank [log scale]",
       size=12, fill="Type of premise") +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(size=16, color="grey30"),
        axis.text.y = element_text(size=16, color="grey30"),
        legend.position = "none")+
  scale_y_continuous(trans = log2_trans())+
  geom_text_repel(aes(x=quintile, y=page_r,
                      label=sitio2), 
                  direction="y",
                  size=6, 
                  force=2,
                  color="grey30",
                  nudge_x = 0.5,
                  segment.size = 0.3,
                  segment.colour = "grey80",
                  box.padding = unit(0.5, "lines"))
a <- c(1,2,3)
class(a)


#< Fig. 3 cumulative distribution function and pareto ----
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

# Calculating the cumulative distribution fucntion 
occur = (table(ktotal))
occur = occur/sum(occur)
p = occur/sum(occur)
y = rev(cumsum(rev(p)))
x = as.numeric(names(table(ktotal)))
plot(x, y, log="xy", type="l")

plot(log(x), log(y))
xy <- data.frame(x,y)

cdf <- ggplot(xy, aes(x,y))+
  geom_point(size = 0.5, colour="#FC8D62")+
  scale_x_log10()+
  scale_y_log10()+
  theme_linedraw()+
  labs(x="Degree",
       y="CDF")+
  theme(text = element_text(size = 9))
cdf

# to save as tiff element_size(=9), to sisualize 12

tiff(filename = "Fig.3 CDF.tiff",
     width=9, height=8, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

cdf
dev.off()





#  Media de amigos ----
statfp <- friendshipParadox(matrizv)
statfp
pareto()

# pareto rule ----
# with a weighted martiz
A <-  matriz

legin = "in"
legout = "out" 

require(ggplot2)
kwin <- colSums(A)
kwout <- rowSums(A)
skin <- sort(kwin, decreasing = TRUE)
skout <- sort(kwout, decreasing = TRUE)
cumin <- cumsum(skin)
cumout <- cumsum(skout)
pcumin <- cumin/(cumin[length(cumin)])
pcumout <- cumout/(cumout[length(cumout)])
hin <- (1:length(pcumin))/(length(pcumin))
hout <- (1:length(pcumout))/(length(pcumout))
hstack <- append(hin, hout)
pcstack <- append(pcumin, pcumout)
legend <- c(rep(legin, length(hin)), rep(legout, length(hout)))
df <- data.frame(hstack, pcstack, legend)

# Figura com legenda por fora
fig <- ggplot(df, aes(x = hstack * 100, y = pcstack * 100, 
                      group = legend, linetype = legend))+
  geom_line(size = 1.1, colour="#FC8D62") + 
  theme(legend.position = c(0.8, 0.2), legend.title = element_blank()) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  theme_linedraw()+
  theme(legend.title=element_blank())+
  labs(tag = "A")+
  labs(x = "Major traders (%)",
       y ="Mobilized animals (%)", fill=NULL)+
  theme(text = element_text(size = 15))

# Plot with legen inside
fig <- ggplot(df, aes(x = hstack * 100, y = pcstack * 100, 
                      group = legend, linetype = legend)) +
  geom_line(size = 1.1,colour="#FC8D62") + 
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  labs(tag = "B")+
  labs(x = "Major traders (%)",
       y ="Mobilized animals (%)", fill=NULL)+
  theme_minimal()+
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank())+
  theme(text = element_text(size = 15))

fig  


# Plotting two images CDF and Pareto
library(ggpubr)
ggarrange(cdf,fig, font.label = list(size = 16))


# 4.2 matriz de vizinhos ----
matrizv <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                        dims = rep(max(banco$movements$From, banco$movements$To) ,2))

library(igraph)

# 4.5 Creating a grafo with igraph ----
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


plot(log(kin)~log(kout))

hist(log(ktotal), breaks=30)


#6 Copy sitio, operacion ----
# Transferindo nome do sitio e operacão para o correpondence
banco$correspondence$operaciono <- banco$movements$operacion.origen[match(
  banco$correspondence$database.id, banco$movements$codigo.sitio.origen)]
banco$correspondence$operaciond <- banco$movements$operacion.destino[match(
  banco$correspondence$database.id, banco$movements$codigo.sitio.destino)]
banco$correspondence$sitio <- banco$movements$sitio.origen[match(
  banco$correspondence$database.id, banco$movements$codigo.sitio.origen)]
banco$correspondence$prov <- banco$movements$provincia.origen[match(
  banco$correspondence$database.id, banco$movements$codigo.sitio.origen)]

#Copy to correspondence the operacion
operacion <- mutate(banco$correspondence, operacion= ifelse(is.na(operaciono), operaciond, operaciono))
banco$correspondence$operacion <- operacion$operacion






# Calculate the mean of the centrality measures to plot the boxplots of premises and markets fig 2 and 3
banco$correspondence$ktotalmean <- round(banco$correspondence$ktotal/3,3)
summary(banco$correspondence$ktotalmean)
table(banco$correspondence$ktotalmean)

banco$correspondence$ktotal_animaismean <- round(banco$correspondence$ktotal_animais/3,3)
summary(banco$correspondence$ktotal_animaismean)
table(banco$correspondence$ktotal_animaismean)


# Plot degree distributions by animal heard by typo of premise
# Boxplots of degree (number of outgoing or ingoing movements)
# for each decile category of herd size (number of animals bougth or sold).

# 7 Plot  Mean of three years ----
#Using separate banks

#< Fig 4 boxplot of Premises ----
# 8.1 ----
# cut deciles of premises
db <- banco$correspondence
dbpre <- db[db$operacion != "Feria de comercialización animal",]
dbpre <- dbpre[dbpre$database.id != "0591733168001.0503",]

summary(dbpre$ktotalmean)

# Better distribution
cp <- quantile(dbpre$ktotalmean, 
               prob=c(0,0.7,0.90, 0.995,0.999,1))
cp
dbpre$quintile <- cut(dbpre$ktotalmean, cp, include.lowest=TRUE)
table(dbpre$quintile)

dbpre$quintile <- cut(dbpre$ktotalmean, cp, include.lowest=TRUE,
                      labels=c("0.33 - 1","1.33 - 2.67","3 - 36.7",
                               "38 - 105.6", "107 - 1372"))

table(dbpre$quintile)

# premises database to plot
decile <- dbpre

# 8.2  Stats & save the file ----

# <  Table 2 Network centrality values and characteristics of premises ----
d2 <- 
  dbpre %>%
  group_by(quintile)%>%
  summarise(premises=n(), 
            mean_d=round(mean(ktotalmean),2),
            percent=round(premises/nrow(dbpre),2),
            med_d=round(median(ktotalmean),2),
            meanw_d=round(mean(ktotal_animaismean),2), 
            max=round(max(ktotal_animaismean),2),
            medianw_d=round(median(ktotal_animaismean),2), 
            w25=round(quantile(ktotal_animaismean,prob=c(0.25)),2)/2,
            w75=round(quantile(ktotal_animaismean,prob=c(0.75)),2)/2,)

d2

# <  Table 3 Network centrality values and characteristics of premises ----
d <- 
  dbpre %>%
  mutate(operacion = recode(operacion, 
                            "Comercializador" = "2Trader",
                            "Operador Industrial" = "3Industrial",
                            "Productor" = "1Farm")) %>%
  group_by(quintile,operacion)%>%
  summarise(premises=n(), 
            percent=round(premises/nrow(dbpre),3),
            mean_d=round(mean(ktotalmean),2),
            med_d=round(median(ktotalmean),2),
            meanw_d=round(mean(ktotal_animaismean),2), 
            max=round(max(ktotal_animaismean),2),
            medianw_d=round(median(ktotal_animaismean),2), 
            w25=round(quantile(ktotal_animaismean,prob=c(0.25)),2)/2,
            w75=round(quantile(ktotal_animaismean,prob=c(0.75)),2)/2,)
d


insdustrial.list <- unique(banco$correspondence$database.id[banco$correspondence$operacion == "Operador Industrial"])
summary(banco$correspondence$ktotal_animais[banco$correspondence$operacion == "Operador Industrial"])
107483/124976
17480/124976
13/124976

41/166
45/166
80/166

setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")
# write.csv(d2, file="tabledbpremises.csv")
# write.csv(d, file="tabledbpremisesbytype.csv")

library(scales)
fig.4 <-
  decile %>%
  mutate(operacion = recode(operacion, 
                            "Comercializador" = "Trader",
                            "Feria de comercialización animal" = "Market",
                            "Operador Industrial" = "Industrial",
                            "Productor" = "Farm")) %>%
  ggplot(aes(quintile, ktotal_animaismean, fill=operacion)) +
  geom_boxplot(
    outlier.size = 0.4,
    outlier.colour = "#66C2A5",
  )+
  theme_linedraw()+
  labs(x = "Degree (Annual mean)",
       y ="Weighted degree (pigs) [log scale]",
       size=12,
       fill="Type of premise") +
  labs(color ='') +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size=14, color = "grey30"),
        axis.text.y = element_text(size=14, color= "grey30"),
        legend.position = c(0.2, 0.8))+
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(trans = log2_trans())+
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  annotate("text",x = 1, y = 1000, label = "75th")+
  annotate("text",x = 2, y = 4050, label = "90th")+
  annotate("text",x = 3, y = 27000, label = "99.5th")+
  annotate("text",x = 4, y = 82000, label = "99.9th")+
  annotate("text",x = 5, y = 980000, label = "100th")+
  annotate("label",x = 1, y = 400, label = "124,976")+
  annotate("label",x = 2, y = 2000, label = "24,159")+
  annotate("label",x = 3, y = 13000, label = "15,638")+
  annotate("label",x = 4, y = 40000, label = "654")+
  annotate("label",x = 5, y = 480000, label = "166")

# save tiff
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")


tiff(filename = "Fig.4 boxplots type of premises2.tiff",
     width=14, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

fig.4

dev.off()



# 7.1 ---- 
# < Fig.5 boxplot markets    ----
# Using the market database
db <- banco$correspondence
dbmarket <- db[db$operacion == "Feria de comercialización animal",]

# Two markets repeted are deleted
# Agregating the ktotalmean and ktotal animals mean pillaro
dbmarket$ktotalmean[dbmarket$network.id == "162518"] <- "5334"
dbmarket$ktotal_animaismean[dbmarket$network.id == "162518"] <- "12123"
# Deleting centro pillaro
dbmarket <- dbmarket[dbmarket$network.id != "162517",]

# Agregating the ktotalmean and ktotal animals mean pillaro
dbmarket$ktotalmean[dbmarket$network.id == "91998"] <- "3767"
dbmarket$ktotal_animaismean[dbmarket$network.id == "91998"] <- "15551"
# Deleting Otavalo little
dbmarket <- dbmarket[dbmarket$network.id != "91999",]

dbmarket$ktotalmean <- as.numeric(dbmarket$ktotalmean)
dbmarket$ktotal_animaismean <- as.numeric(dbmarket$ktotal_animaismean)

cm <- quantile(dbmarket$ktotalmean, prob=c(0,0.2,0.4,0.6,0.8,1))
cm
dbmarket$quintile <- cut(dbmarket$ktotalmean, cm, include.lowest=TRUE,
                         labels=c("13 - 167","224 - 713","720 - 3,650","3767 - 5,518","5,990 - 29,407"))

# dbmarket$quintile <- cut(dbmarket$ktotalmean, cm, include.lowest=TRUE)

table(dbmarket$quintile)

# 7.2 Stats Average degree by quintile QU----
dbmarket %>%
  group_by(quintile)%>%
  summarise(n(), mean(ktotalmean))
# how mani timesbigger are the first to the fifth quintile 
# 13628/84
# 156

dbmarket %>%
  group_by(quintile)%>%
  summarise(n(), mean(ktotal_animaismean))
59684/395

# Preparing the table of supplementary materials
dbmarket$Provincia <- banco$movements$provincia.origen[match(dbmarket$database.id,banco$movements$codigo.sitio.origen)]
dbmarket$Canton <- banco$movements$canton.origen[match(dbmarket$database.id,banco$movements$codigo.sitio.origen)]
dbmarket$Parroquia <- banco$movements$parroquia.origen[match(dbmarket$database.id,banco$movements$codigo.sitio.origen)]

# Cleaning the names of the markets
dbmarket$sitio2 <- dbmarket$sitio
dbmarket$sitio2 <- toupper(dbmarket$sitio2)

dbmarket$sitio2 <- iconv(dbmarket$sitio2, from = 'UTF-8', to = 'ASCII//TRANSLIT')

dbmarket$sitio2 <- gsub("FERIA ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("DE GANADO ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("\\(", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("\\)", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("COMERCIAL ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("DE ANIMALES ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("DE COMERCIALIZACION ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("MERCADO AGROGANADERO ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("MUNICIPAL DE ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("EN PIE ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("PORCINOS ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("PORCINA ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("DE CHANCHOS ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("GANADERA ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("GANADERA ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("DE ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("MERCADEO ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("MAYORES ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("- IBARRA", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("MEGASA - ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub(" ASOPROGASMIC", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("EL CARMEN", "EC", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("EL PLATEADO 2019", "PLATEADO", dbmarket$sitio2)
dbmarket$sitio2 <- gsub(" SALCEDO", "SALCEDO", dbmarket$sitio2)
dbmarket$sitio2 <- gsub(" SAN LUCAS", "SAN LUCAS", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("EL PORVENIR ", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("SAN MIGUEL", "SM", dbmarket$sitio2)
dbmarket$sitio2 <- gsub(" PRODUCE", "", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("SANTIAGO QUERO", "QUERO", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("PEDRO VICENTE MALDONADO", "P MALDONADO", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("ASOGAN SD", "ASOGANSD", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("ASOGAN EC", "ASOGANEC", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("REINA DEL CISNE", "R DEL CISNE", dbmarket$sitio2)
dbmarket$sitio2 <- gsub("JULIO ANDRADE", "J ANDRADE", dbmarket$sitio2)

dbmarket$sitio2 <- tolower(dbmarket$sitio2)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
dbmarket$sitio2 <- firstup(dbmarket$sitio2)

dbmarket$sitio2

decile <- dbmarket

dbmarket %>%
  group_by(quintile) %>%
  summarise(Mean_Animal_degree=mean(ktotal_animaismean),
            Mean_Movement_Degree=mean(ktotalmean))
#   quintile            Mean_Animal_degree Mean_Movement_Degree
#   <fct>                            <dbl>                <dbl>
# 1 [13.3,167]                        395.                 83.6
# 2 (167,713]                        1795.                456. 
# 3 (713,3.65e+03]                   7774.               1948. 
# 4 (3.65e+03,5.52e+03]             20221.               4610. 
# 5 (5.52e+03,2.94e+04]             59684.              13628. 


# 7.3 Save data frame with markets ----
setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")
write.csv(dbmarket, file="tablemarkets.csv")


# Fig. SM suplementary  Markets by page rank ----
library(ggrepel); library(scales)

mark <- decile %>%
  mutate(operacion = recode(operacion, 
                            "Comercializador" = "Collector",
                            "Feria de comercialización animal" = "Market",
                            "Operador Industrial" = "Industrial Farm",
                            "Productor" = "Backyard-commercial Farm")) %>%
  filter(operacion =="Market") %>%
  ggplot(aes(quintile, as.numeric(ktotal_animaismean), fill=operacion)) +
  geom_boxplot(width=0.3,
               outlier.size = 0.7,
               outlier.colour = "#66C2A5")+
  theme_linedraw()+
  labs(x = "Degree (Annual mean)",
       y ="Weighted Degree (pigs) [log scale]", size=12, fill="Type of premise") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size=9, color="grey30"),
        axis.text.y = element_text(size=10, color="grey30"),
        legend.position = "none")+
  scale_y_continuous(trans = log2_trans(), labels = comma)+
  scale_color_brewer(palette = "Set2")+
  geom_text_repel(aes(x=quintile, y=ktotal_animaismean,
                      label=sitio2), 
                  direction="y",
                  size=3.0, 
                  force=2,
                  color="grey20",
                  nudge_x = 0.5,
                  segment.size = 0.3,
                  segment.colour = "grey80",
                  box.padding = unit(0.5, "lines"))+
  annotate("label",x = 1, y = 3000, label = "Q1")+
  annotate("label",x = 2, y = 8000, label = "Q2")+
  annotate("label",x = 3, y = 32000, label = "Q3")+
  annotate("label",x = 4, y = 95000, label = "Q4")+
  annotate("label",x = 5, y = 160000, label = "Q5")+
  annotate("text",x = 6, y = 200000, label = "", color="white")+
  theme( # remove the vertical grid lines
    #panel.grid.major.x = element_blank() ,
    # panel.grid.minor.x = element_blank() ,
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

  

# Save tiff
tiff(filename = "Fig.5 boxplots markets2.tiff",
     width=14, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)
mark

dev.off()


# < Fig SM1 Plot Page Rank markets ----
markp <- decile %>%
  mutate(operacion = recode(operacion, 
                            "Comercializador" = "Collector",
                            "Feria de comercialización animal" = "Market",
                            "Operador Industrial" = "Industrial Farm",
                            "Productor" = "Backyard-commercial Farm")) %>%
  filter(operacion =="Market") %>%
  ggplot(aes(quintile, page_rank, fill=operacion)) +
  geom_boxplot(width=0.3,
               outlier.size = 0.4,
               outlier.colour = "#66C2A5")+
  theme_linedraw()+
  labs(x = "Degree Annual mean",
       y ="Page rank", size=12, fill="Type of premise") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size=9, color="grey30"),
        axis.text.y = element_text(size=10, color="grey30"),
        legend.position = "none")+
  scale_color_brewer(palette = "Set2")+
  annotate("label",x = 1, y = 0.04, label = "Q1")+
  annotate("label",x = 2, y = 0.04, label = "Q2")+
  annotate("label",x = 3, y = 0.04, label = "Q3")+
  annotate("label",x = 4, y = 0.04, label = "Q4")+
  annotate("label",x = 5, y = 0.04, label = "Q5")+
  annotate("text",x = 6, y = 0.04, label = "", color="white")+
  geom_text_repel(aes(x=quintile, y=page_rank,
                      label=sitio2), 
                  direction="y",
                  size=3.5, 
                  force=2,
                  color="grey10",
                  nudge_x = 0.5,
                  segment.size = 0.3,
                  segment.colour = "grey80",
                  box.padding = unit(0.5, "lines"))+
theme( # remove the vertical grid lines
  panel.grid.minor.y = element_blank())


# Save tiff
tiff(filename = "Fig.5 boxplots markets_pr.tiff",
     width=14, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)
markp

dev.off()



# Plot degree distributions by animal heard by typoe of premise
# Boxplots of degree (number of outgoing or ingoing movements)
# for each decile category of herd size (number of animals bougth or sold).

# Plot degree herd size ----
boxplot(banco$correspondence$ktotal)

pareto(matriz)

library(RColorBrewer)
brewer.pal(n = 8, name = "Set2")
[1] "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"
palette(brewer.pal(n = 8, name = "Set2"))


# Fig. 6 Market locations ----
# 20 Plotting markets ----
library(rgdal)
library(gdata)
library(sp)
ec1<-rgdal::readOGR(dsn="~/Dropbox/0.USP/1 Projeto/SHP",layer="nxprovincias")
ec1 <- spTransform(ec1, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ec1 <- subset(ec1, DPA_DESPRO != "GALAPAGOS")

setwd("~/Dropbox/0.USP/9. 2020 I sem/Projeto/Paper Ecuador swine network/Analise_codigo_sitio")

library(readr)
market <- read_csv(file = "market_location.csv")
# marke <- read_csv(file = "markets.details.csv")
#market$q <- as.factor(marke$Quintile[match(market$nombre, marke$`Market name`)])
market <- market[market$Ferias_2019 == TRUE,]
market$Q <- as.factor(market$q)
table(market$Q)
# Import the base map of ecuador ----
library(ggmap)
# exact ecuador map
# ecu <- get_stamenmap(bbox = c(left = - 81.2, 
#                               bottom = -5.05, 
#                               right = -75.2, 
#                               top = 1.5),
#                      zoom = 8, 
#                      maptype = "terrain-background")
# 
# map looking at the bordes of colombia and Peru
ecu2 <- get_stamenmap(bbox = c(left = - 81.4, 
                               bottom = -5.1, 
                               right = -75, 
                               top = 1.7),
                      zoom = 8, 
                      maptype = "terrain-background")


plot(ecu2)


# Map with province boundaries
ggmap(ecu2) +  
  geom_path(data=ec1, aes(x=long, y=lat, group=group), colour="black", size=0.04)+
  geom_point(data=market, aes(x=x, y=y, group = q,
                              shape=q, color=q, size=q))+
  scale_shape_manual(values=c(15,16,17,18,15))+
  scale_size_manual(values=c(2,3,3,3,3))+  
  scale_color_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB", "#E5C494", "#A6D854"))+
  labs(x="Longitude", y= "Latitude", fill= NULL)


# get world borders
world <- map_data("world")

# Map with markets
library(ggsn)
gg <- ggmap(ecu2) +  
  geom_path(data=world, aes(x=long, y=lat, group=group), size=0.3) +
  geom_point(data=market, aes(x=x, y=y, group = Q,
                              color=Q, size=Q, 
                              shape=Q, fill=Q), alpha=0.8) +
  scale_shape_manual(values=c(21,21,21,21,21)) +
  # scale_shape_manual(values=c(21,22,24,15,25)) +
  scale_size_manual(values=c(4,4,6,6,8)) +
  # scale_size_manual(values=c(2,2,3,3,4)) +
  scale_color_manual(values=c("black", "black", "black", "black", "black")) +
  # scale_fill_brewer(palette = "YlOrRd") +
  scale_fill_brewer(palette = "Set1") +
  # scale_fill_viridis_d(option = "A") +
  annotate("label", x = -77, y = 1.3, 
           label = "Colombia", size=8)+
  annotate("label", x = -77, y = -4, 
           label = "Peru", size=8)+
  labs(x = "Longitude",
       y ="Latitude")+
  theme(text = element_text(size = 20))

gg



geom_text_repel(aes(x=quintile, y=ktotal_animaismean,
                    label=sitio2), 
                direction="y",
                size=6, 
                force=2,
                color="grey30",
                nudge_x = 0.5,
                segment.size = 0.3,
                segment.colour = "grey80",
                box.padding = unit(0.5, "lines"))




# Last map
#< Fig. 6 last map market locations ----

library(ggrepel)

# creating another colum with the markets of the fifth quintile
market$nombre5q <- mutate(market, market5 = ifelse(q==5, nombre, NA))

mk <- ggmap(ecu2) +  
  geom_path(data=world, aes(x=long, y=lat, group=group), size=0.3) +
  geom_point(data=market, aes(x=x, y=y, group = Q,
                              color=Q, size=Q, 
                              shape=Q, fill=Q), alpha=0.9) +
  scale_shape_manual(values=c(21,21,21,21,21)) +
  scale_size_manual(values=c(1,2,3,3.5,4)) +
  scale_color_manual(values=c("black", "black", "black", "black", "black")) +
  scale_fill_brewer(palette = "Set2") +
  annotate("text", x = -76.5, y = 1.3, 
           label = "Colombia", size=6)+
  annotate("text", x = -77, y = -4, 
           label = "Peru", size=6)+
  labs(x = NULL,
       y = NULL)+
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title=element_text(size=14))+
  geom_text_repel(data= market[c(4,42,38,3, 22, 40,23,24,18),],
                  aes(x=x, y=y,                                                                  label=nombre),
                  size=4, 
                  force=6,
                  color="black",
                  nudge_y = -0.18,
                  nudge_x = -0.04,
                  segment.size = 0.5,
                  segment.colour = "grey30",
                  box.padding = unit(1.2, "lines"))

# Save Tiff
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

tiff(filename = "Fig.6 markets location.tiff",
     width=14, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)
mk+
  north2(mk, symbol = 3, x=0.15) +
  ggsn::scalebar(x.min = -79, x.max = -75,
                 y.min = -4.5, y.max = -2, st.bottom = TRUE,
                 dist = 100, dist_unit = "km",
                 model = "WGS84",transform = TRUE )

dev.off()
################################ ----




# < Fig. 7 Power law distribution ----
plot(ktotal)
hist(log(ktotal), freq = FALSE, breaks = 40)

# Plot
ggplot(banco$correspondence, aes(x=ktotal, y=..density..))+
  geom_histogram(bins=25, color="grey")+
  scale_x_log10()


# 10 Modelling ----
# Funcao vizinhos aleatorios
vizinhos.aleatorios <-function(matriz,compra=F) {
  require(Matrix)
  if (compra!=T) matrizv <- t(matrizv)
  if (class(matrizv) != 'dgCMatrix')
    matrizv <- as(matrizv, 'dgCMatrix')
  tmp <- summary(matrizv)
  tmp <-tmp[ which(tmp$x!=0), ]
  tmp$r <- unlist(sapply(rle(tmp$j)$lengths,
                         FUN=function(b) {sample(c(1,rep(0,b-1)))} ) )
  tmp <-tmp[tmp$r==1,]
  va <- rep(0, ncol(matriz))
  va[tmp$j] <-(tmp$i)
  return(va)
}

# 10 looking at the surveillance files v2 ----
#looking for infection time min and max

setwd("~/Dropbox/0.USP/1 Projeto")
v2 <- read.csv("casos_vigilancia-2014-2018-lat-long.csv",
               colClasses = "character")

v2$f_1er_enfermo <- as.Date(v2$f_1er_enfermo, "%d/%m/%Y")
v2$f_notificación <- as.Date(v2$f_notificación, "%d/%m/%Y")
v2$f_cierre_orden <- as.Date(v2$f_cierre_orden, "%d/%m/%Y")

#10.1 Tempo de recuperado ou inmune ----
# O periodo de incubação do virus é de 4-15 dias, se uma fazenda se infectar 
# o tempo para apresentar sintomas seriam eses 4-15 dias. Esos dias a fazenda
# Enquanto a fazenda não apresentar doençã, sería inmune.
v2$rec <- v2$f_notificación - v2$f_1er_enfermo
v2$rec <- as.numeric(v2$rec)
hist(v2$rec)
rug(v2$rec)
summary(v2$rec)
plot(v2$rec)
abline(h=9.387, col="red")
boxplot(v2$rec)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.000   7.000   9.387  13.000  70.000 

# 10.2 Calculando o tempo de um nó infetado (dados) ----
v2$po <- v2$f_cierre_orden - v2$f_1er_enfermo
v2$po <- as.numeric(v2$po)
hist(v2$po)
rug(v2$po)
boxplot(v2$po)
summary(v2$po)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   42.00   52.50   57.52   71.25  193.00 

boxplot(v2$rec, v2$po)
v2$
v2$po2 <- v2$f_cierre_orden - v2$f_1er_enfermo
v2$po2 <- as.numeric(v2$po2)
summary(v2$po2)

v2 %>% 
  group_by(ano)%>%
  summarise(n(), casos=sum(as.numeric(caso)))

# 11 SIMULACAO Controle ----
library(Matrix); library(epinemo);library(dplyr); library(ggplot2);library(scales)

setwd('/home/alfredo/Dados/Paper_Network/')
m2 <- read.csv("movimentos_db_mark_sla.csv", colClasses = "character")

m2 <- m2[,-1]
m2$cantidad <- as.numeric(m2$cantidad)
sum(m2$cantidad) #9904714

# 3 Creating network of premisses ----
m2 <- m2[m2$operacion.destino != "Faenador",]
sum(m2$cantidad) #6,407.392

# Using just 2019 for the simulation ----
# To use only in the simulation
m2019 <- m2[m2$ano == "2019",]
m2019 <- data.frame(m2019)
m2019$cantidad <- as.numeric(m2019$cantidad)
sum(m2019$cantidad) #2475853

# Rede de 2019 (filtrando)
m2 <- data.frame(m2019)
rm(m2019)
sum(m2$cantidad)

# Use m2019 for simulation and m2 total for other analysis
# 4 Create the banco (unique Id)----
banco <- createUniqueIds(m2,
                         from = 'codigo.sitio.origen',
                         to = 'codigo.sitio.destino')
banco$movements$cantidad <- as.numeric(banco$movements$cantidad)

#Degree ktotal
matriz <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                       x=1,
                       dims = rep(max(banco$movements$From, banco$movements$To) ,2))
# Degree ----
kin <- colSums(matriz)
kout <- rowSums(matriz)
ktotal <- kin + kout
# Degree to correspondence ----
banco$correspondence$ktotal <- ktotal

# 4.2 matriz de vizinhos ----
matrizv <- sparseMatrix(i =banco$movements$From,j=banco$movements$To,
                        dims = rep(max(banco$movements$From, banco$movements$To) ,2))

nrow(banco$correspondence) #93707
# Pensando que sao 5% de nos infetados
# 93707*0.05
# 4685
39/(93376*0.005)

# Number of nodes under control ----
93707*0.0225 = 2108
93707*0.031 = 2904
93707*0.038= 3560
93707*0.054= 4685
93707*0.25 = 23426
93707*0.01= 9370

# counting the number of markets commer
codigo_sitio_ferias <- unique(m2$codigo.sitio.origen[m2$operacion.origen == "Feria de comercialización animal"])
codigo_sitio_traders_origen <- unique(m2$codigo.sitio.origen[m2$operacion.origen == "Comercializador"])
codigo_sitio_traders_destino <- unique(m2$codigo.sitio.destino[m2$operacion.destino == "Comercializador"])

codigo_sitio_traders <- unique(c(codigo_sitio_traders_origen, codigo_sitio_traders_destino))

All premises-traders-markets
93707-11436-53
82218

82218*0.01 = 822
82218*0.05 = 4110
82218*0.005 = 411

# Testing scenarios(1.1)
# 1.25		First scenario low prevalence
# 2.5		Second scenario intermediate prevalence
# 3.75		Third scenario
# 5		Fourth scenario

93707*0.01 #937
93707*0.0125 #1171
93707*0.025 #2343
93707*0.0375 #3560
93707*0.05 #4685

# 11.1   loading parameters ----
# pareto(matrizv) #execute L66
set.seed(5555)
numero_infectados <-822 #1%
numero_infectados <-4110 #1%
numero_infectados <-411 #0.5%
# numero_infectados <- 1171 #low prevalence 1.25
# numero_infectados <-2342 #2.5
# numero_infectados <-2717 #3.75
# numero_infectados <-4685 #5

probabilidade <-0.048
tImin <-42
tImax <-71
tRmin <-30
tRmax <-180
tSim <-365*3

I <- rep(x=0,times= nrow(matrizv))
I[1:numero_infectados] <-1
I <- sample(I)
R <- rep(x=0,times= nrow(matrizv))
Control <- rep(x=0,times=nrow(matrizv))

# 11.2 Simulation----
simulacao <- simulationSIRS(A =matrizv, pspread = probabilidade, tSim = tSim,
                            I = I, tImin = tImin, tImax = tImax,
                            R =R, tRmin =tRmin, tRmax =tRmax,
                            Control = Control)

# 11.3 saving results of simulation none ----
# install.packages("lubridate")
library(lubridate)
Iw <- data.frame(colMeans(simulacao[[1]]>0))
colnames(Iw)[1] <- "none"
# 5years
# Iw$days <- seq(ymd('2019-01-01'),ymd('2023-12-31'), by = 'days')
# #2 years
# Iw$days <- seq(ymd('2019-01-01'),ymd('2020-12-31'), by = 'days')

#3 years
Iw$days <- seq(ymd('2019-01-01'),ymd('2021-12-31'), by = 'days')
summary(Iw$none)

# install.packages("sjmisc")
library(sjmisc);library(dplyr);library(lubridate)
########################
#12 Simulacao de controle ----
########################
# Parâmetros de controle
# Data of supervisions 2019
# 49717/93707 53.05% ficalizacion 2019
# 2946/93707 3.14% ficalizacion en transito
# 2111/93707 2.25% ficalização em matadouro e feria

c.probabilidade <-0.048
c.timin <-42
c.timax <-71
c.trmin <-30
c.trmax <-180
c.tsim <-365

# Seleção dos nós infectados no início
# evitando o sorteio de nós sob controle
# c.numero_infectados <-937 # 1%
# c.numero_infectados <-2343 #prevalencia 2.5
# c.numero_infectados <-4685 # 5
c.numero_infectados <-822 # 1%
c.numero_infectados <-4110
c.numero_infectados <-411
# c.numero_infectados <-2717 # 2.9

#12.1 changing the percentage and then make the simulation 
# Seleção aleatória de nós sob controle
c.prop.cont <-0.054 # proporcao de nos sob controle
c.prop.cont <-0.10 # proporcao de nos sob controle
c.prop.cont <-0.15 # proporcao de nos sob controle
c.prop.cont <-0.25 # proporcao de nos sob controle

c.nos.controle <- sample.int(n=nrow(matrizv),size=c.prop.cont* nrow(matrizv))
c.control <- rep(x=0,times=nrow(matrizv))
c.control[c.nos.controle] <-1

c.I <- rep(x=0,times= nrow(matrizv))
c.I[sample(x=(1:nrow(matrizv))[-c.nos.controle],size=c.numero_infectados)] <-
  trunc(runif(c.numero_infectados,min=c.timin,max=c.timax))

# Número de recuperados na condição inicial (0, nesse exemplo)
c.R <- rep(x=0,times= nrow(matrizv))

# Simulacao
c.sim <- simulationSIRS(A =matrizv, pspread =c.probabilidade, tSim =c.tsim,
                        I =c.I,tImin =c.timin,tImax =c.timax,
                        R =c.R,tRmin =c.trmin,tRmax =c.trmax,
                        Control =c.control)

# Temporal
# Saving vector with 5% control----
Iw$r.05 <- colMeans(c.sim[[1]]>0)

# # Saving vector with 10% control
Iw$r.10 <- colMeans(c.sim[[1]]>0)

# # Saving vector with 15% control
Iw$r.15 <- colMeans(c.sim[[1]]>0)

# # Saving vector with 25% control
Iw$r.25 <- colMeans(c.sim[[1]]>0)

#############################
# 13 Controle dirigido de nos ----
#13.1 Ferias do primero quintil----
c.nos.controle <- c(78533,37590,37589,10542,50389,78549,21531,15590,53433,59095)

#@13.2 Saquisili market ----
c.nos.controle <- c(37589) #saquisili market

#13.3 Asogansd market ----
c.nos.controle <- c(78533) #santo domingo market

#13.4 All markets ----
codigo_sitio_ferias <- unique(m2$codigo.sitio.origen[m2$operacion.origen == "Feria de comercialización animal"])

c.nos.controle <- banco$correspondence$network.id[match(
  codigo_sitio_ferias, banco$correspondence$database.id)]  
id.ferias <-c.nos.controle

# 13.5 Top 10 collectors ----
c.nos.controle <- c(18196,41897,52423,53764,76329,18648,27009,53528,42835,43053,12017)
collectors <- c(18196,41897,52423,53764,76329,18648,27009,53528,42835,43053,12017)

#5 117 traders by each province
banco$correspondence$provincia.origen <- banco$movements$provincia.origen[match(banco$correspondence$database.id, banco$movements$codigo.sitio.origen)]
prov <- data.frame(banco$correspondence)

control.traders <- prov %>%
  filter(database.id %in% codigo_sitio_traders) %>%
  group_by(provincia.origen)%>%
  top_n(5,ktotal)%>%
  arrange(provincia.origen, desc(ktotal))
c.nos.controle <- control.traders$network.id
traders <- control.traders$network.id

# 13.6 top 100 k total province ----
banco$correspondence$provincia.origen <- banco$movements$provincia.origen[match(banco$correspondence$database.id, banco$movements$codigo.sitio.origen)]
prov <- data.frame(banco$correspondence)

control.prov <- prov %>%
  filter(!network.id %in% id.ferias) %>%
  filter(!network.id %in% collectors) %>%
  filter(!is.na(provincia.origen))%>%
  group_by(provincia.origen)%>%
  top_n(100,ktotal)%>%
  arrange(provincia.origen, desc(ktotal))

c.nos.controle <- control.prov$network.id

# 13.8 Best strategy mixture
# 50 in each province+markets_quintil+117 traders
control.prov <- prov %>%
  filter(!network.id %in% id.ferias) %>%
  filter(!network.id %in% collectors) %>%
  filter(!is.na(provincia.origen))%>%
  group_by(provincia.origen)%>%
  top_n(50,ktotal)%>%
  arrange(provincia.origen, desc(ktotal))

c.nos.controle <- control.prov$network.id

c.nos.controle <- c(c.nos.controle,
                    collectors,
                    id.ferias)

c.nos.controle <- unique(c.nos.controle) #1321


#############################
# 14 Simulation ----
c.I <- rep(x=0,times= nrow(matrizv))
c.I[sample(x=(1:nrow(matrizv))[-c.nos.controle],size=c.numero_infectados)] <-
  trunc(runif(c.numero_infectados,min=c.timin,max=c.timax))

# Número de recuperados na condição inicial (0, nesse exemplo)
c.R <- rep(x=0,times= nrow(matrizv))

# Simulacao
c.sim <- simulationSIRS(A =matrizv,pspread =c.probabilidade,tSim =c.tsim,
                        I =c.I,tImin =c.timin,tImax =c.timax,
                        R =c.R,tRmin =c.trmin,tRmax =c.trmax,
                        Control =c.control)

# Saving the results to plot ----
# 1 Directed control from 10 markets do primeiro quintil ----
Iw$d.markets.quintil <- colMeans(c.sim[[1]]>0)

#2 Directed control of 1 market saquisili----
Iw$d.single_market_saquisili <- colMeans(c.sim[[1]]>0)

#3 Directed control of 1 market santo domingo ----
Iw$d.single_market_asogan <- colMeans(c.sim[[1]]>0)

#4 Directed control of 53 markets ----
Iw$d.markets <- colMeans(c.sim[[1]]>0)

#5 Direct control of 10 collectors ----
Iw$d.collectors <- colMeans(c.sim[[1]]>0)

#5 Direct control of 117 (5xProvince) collectors ----
Iw$d.117.collectors <- colMeans(c.sim[[1]]>0)

# Direct control of 100 premise of every political administration
Iw$d.prov.control <- colMeans(c.sim[[1]]>0)
summary(Iw$d.prov.control)

# Directed control mix 117 colectors+markets_Quintil+100_farms_each_province
Iw$d.mix <- colMeans(c.sim[[1]]>0)
summary(Iw$d.mix)

Iw$d.mix2 <- colMeans(c.sim[[1]]>0)

# Plotting results ----
library(ggpubr);library(ggplot2);library(sjmisc);library(dplyr)

# Summary ----
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

z1 <- 
  Iw %>%
  # group_by(year(days))%>%
  descr(show = c("mean", "sd","range","md"))
z1

write.csv(data.frame(z1), file="SR_prev_0.5.csv")


# Grahics ----

#< Fig.7a Random control----

library(tidyr)
a <- Iw %>%
  gather(key, value, -days) %>%
  filter(key == "none" | key == "r.05" | key == "r.10" | key == "r.25") %>%
  mutate(key=recode(key,"none" = "None"))%>%
  mutate(key=recode(key,"r.05" = "r 05%"))%>%
  mutate(key=recode(key,"r.10" = "r 10%"))%>%
  mutate(key=recode(key,"r.25" = "r 25%"))%>%
  ggplot(aes(days,value)) +
  geom_line(aes(color=(key)), size=0.8) +
  labs(tag = "a")+
  theme_linedraw()+
  labs(x=NULL,
       y ="Prevalence", size=12) +
  labs(color ='') +
  theme(text = element_text(size = 10),
        legend.key.size = unit(2, "mm"))+
  scale_color_brewer(palette = "Set2")
a
#< Fig.7a Random control boxplots----

b <- Iw %>%
  gather(key, value, -days) %>%
  filter(key == "none" | key == "r.05" | key == "r.10" | key == "r.25") %>%
  mutate(key=recode(key,"none" = "None"))%>%
  mutate(key=recode(key,"r.05" = "r 05%"))%>%
  mutate(key=recode(key,"r.10" = "r 10%"))%>%
  mutate(key=recode(key,"r.25" = "r 25%"))%>%
  ggplot(aes(days,value)) +
  geom_boxplot(aes(color=(key)), size=0.6,
               outlier.size = 0.001,
               outlier.color = "gray") +
  labs(tag = "b")+
  theme_linedraw()+
  ylim(0,0.25)+
  labs(x = NULL, y ="Prevalence", size=12) +
  labs(color ='') +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_line(size=.1, color="white"), 
        panel.grid.major.x = element_line(size=.1, color="white"))+ 
  theme(text = element_text(size = 10),
        legend.key.size = unit(4, "mm"))+
  scale_color_brewer(palette = "Set2")
b
library(ggpubr)

#show graphic ----  
ggarrange(a, b, common.legend = TRUE, legend="bottom", 
          ncol=1, heights = c(1.5,1))

# Save Tiff
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

tiff(filename = "Fig.7 RS_p0.5.tiff",
     width=9, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggarrange(a, b, common.legend = TRUE, legend="bottom", 
          ncol=1, heights = c(1.5,1))

dev.off()

#< Fig.8a targeted modelling control ----
c <- Iw %>%
  gather(key, value, -days) %>%
  filter(key == "none" |
           key == "d.markets.quintil" |
           key == "d.single_market_asogan" |
           key == "d.collectors" | 
           key == "d.prov.control" |
           key == "d.mix2" |
           key == "d.mix") %>%
  
  mutate(key=recode(key,"none" = "None"))%>%
  mutate(key=recode(key,"d.markets.quintil" = "Markets-Q5 (10)"))%>%
  mutate(key=recode(key,"d.prov.control" = "Prem-GL (2,300)"))%>%
  mutate(key=recode(key,"d.collectors" = "Traders (10)"))%>%
  mutate(key=recode(key,"d.single_market_asogan" = "Market Asogan (1)"))%>%
  mutate(key=recode(key,"d.mix2" = "Prem-MT (1,305)"))%>%
  mutate(key=recode(key,"d.mix" = "Prem-MT (1,321)"))%>%
  ggplot(aes(days,value)) +
  geom_line(aes(color=(key)), size=0.5) +
  labs(tag = "a")+
  theme_linedraw()+
  labs(x = NULL, y ="Prevalence", size=12) +
  # ylim(0,0.31)+
  labs(color ='') +
  theme(text = element_text(size = 10),
        legend.key.size = unit(2, "mm"))+
  scale_color_brewer(palette = "Set2")
c

#< Fig.8b targeted modelling control boxplots ----
d  <- Iw %>%
  gather(key, value, -days) %>%
  filter(key == "none" |
           key == "d.markets.quintil" |
           key == "d.single_market_asogan" |
           key == "d.collectors" | 
           key == "d.prov.control" |
           key == "d.mix") %>%
  mutate(key=recode(key,"none" = "None"))%>%
  mutate(key=recode(key,"d.markets.quintil" = "Markets-Q5 (10)"))%>%
  mutate(key=recode(key,"d.prov.control" = "Prem-GL (2,300)"))%>%
  mutate(key=recode(key,"d.collectors" = "Traders (10)"))%>%
  mutate(key=recode(key,"d.single_market_asogan" = "Market Asogan (1)"))%>%
  mutate(key=recode(key,"d.mix" = "PremMT (1,241)"))%>%
  
  ggplot(aes(days,value)) +
  geom_boxplot(aes(color=(key)), size=0.6,
               outlier.size = 0.000001,
               outlier.color = "gray70") +
  labs(tag = "b")+
  ylim(0,0.17)+
  theme_linedraw()+
  labs(x = NULL, y ="Prevalence", size=14) +
  labs(color ='') +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_line(size=.1, color="white"), 
        panel.grid.major.x = element_line(size=.1, color="white"),
        legend.text = element_text(size=10),
        legend.key.size = unit(15, "mm"))+
  scale_color_brewer(palette = "Set2")

#show graph----
ggarrange(c, d, common.legend = TRUE, legend="bottom", 
          ncol=1, heights = c(1.5,1))

#Save tiff----
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")

tiff(filename = "Fig.8 TS_p0.5.tiff",
     width=9, height=12, units="cm", 
     compression = "lzw", pointsize = 12, res=600)

ggarrange(c, d, common.legend = TRUE, legend="bottom", 
          ncol=1, heights = c(1.5,1))

dev.off()


# Saving file
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/")
# write.csv(Iw, file="Iw09.02.21_p5.0.csv")


# Descriptive analysis of simulation results ----
# Calculate min max epidemic curve ----
# I did looking at the table

summary(Iw$none)

max(Iw$none)


max(Iw$none[Iw$days])
max(Iw$none[as.character(Iw$days)])

min(Iw$none[Iw$days])
Iw$days[max(Iw$none)]
min(Iw$none[Iw$days])


max(Iw$`Market (Saquisili)`[Iw$days])
min(Iw$`Market (Saquisili)`[Iw$days])

lowest day
mean(c(275,283,278,272,270,272,257))

# Describing the summary of the prevalence to link with boxplot
summary(Iw$none)
IQR(Iw$none)

library(sjmisc)
#Descriptive of variables used on the model
library(dplyr)
descrip <- descr(Iw)
# write.csv(descrip, file="descriptive simulation.csv")


#16  Number of nodes in the three years ----
# look for od
origen <- data.frame(banco$movements$codigo.sitio.origen,
                     banco$movements$provincia.origen, 
                     banco$movements$canton.origen,
                     banco$movements$parroquia.origen,
                     banco$movements$ano)

destino <- data.frame(banco$movements$codigo.sitio.destino,
                     banco$movements$provincia.destino,
                     banco$movements$canton.destino,
                     banco$movements$parroquia.destino,
                     banco$movements$ano)

colnames(origen) <- c("codigo.sitio", "provincia", "canton", "parroquia", "ano")
colnames(destino) <- c("codigo.sitio", "provincia", "canton", "parroquia","ano")

od <- rbind(origen,destino)

#how many times per year a premise appears
od2 <- od %>%
  group_by(codigo.sitio,ano) %>%
  summarize(n())

#Grouping with ones repeat 3 times, 2 and 1 time
od3 <- od2 %>%
  group_by(codigo.sitio) %>%
  summarize(n=n())

table(od3$n)
# 125633+35437+17607 # total premises
First year 
125633+35437+17607

(35437+17607)/(125633+35437+17607)

(17607)/(125633+35437+17607)


od3$sitio <- banco$correspondence$sitio[match(od3$codigo.sitio,
                                              banco$correspondence$database.id)]

od3$opera <- banco$correspondence$operacion[match(od3$codigo.sitio,
                                              banco$correspondence$database.id)]

table(od3$n, od3$opera)


od3 <- od3 %>%
  group_by(opera,n) %>%
  summarise(n())

od3 %>%
  group_by(n) %>%
  summarise(n())

1     1 125633
2     2  35437
3     3  17607

125633+35437+17607



# write.csv(od3,file="premises_fidelity.csv")




# other way to visualize
library(tidyr)
premises <- od %>%
  group_by(codigo.sitio, ano) %>%
  summarize(n=length(unique(ano)))%>%
  spread(value = "n", key="ano")



  



  


# Model parameters
#5% of prevalence ----
library(tidyverse)
v2 %>%
  group_by(caso,ano)%>%
  summarize(n=n(),existente=sum(as.numeric(existente)),
            #muestras=sum(as.numeric(total_muestras)),
            pos=sum(as.numeric(pos)),
            casos=sum(as.numeric(caso)),
            afec=sum(as.numeric(afetados)),
            #pos/muestras,
            pos/existente,
            afec/existente)

(149+215+132)/(11132+10583+4268)

215/10583
(132/4268)

v2 %>%
   filter(ano=="2019")%>%
   group_by(caso,ano)%>%
   summarise(n(), pop=sum(as.numeric(populacao)),
      positives=sum(as.numeric(pos)),
      sick=sum(as.numeric(enfermo3)),
      death=sum(as.numeric(mortos3)),
      culled=sum(as.numeric(sacrificados3)),
      cadastal_last_visit=sum(as.numeric(existente3)),
      prevalence2=(sick+death+culled)/(pop+10157))

25/167
caso  ano   `n()`   pop positives  sick death culled
0     2019    132 10157         0   608   500      2
1     2019     35  1019       132   128   250     57

# 1019-128-250-57=584 VIVOS NA ULTIMA VISITA
sick+death+culled/total population of cases and controls
(128+250+57)/(10157+1019)
# Aparent prevalence 
0.0389




v2$prev2 <- as.numeric(as.numeric(v2$afetados)/as.numeric(v2$existente))
summary(v2$prev[v2$pos != 0])
summary(v2$prev[v2$ano == 2017])
summary(v2$prev[v2$ano == 2018])
summary(v2$prev[v2$ano == 2019])

summary(v2$prev)


summary(v2$prev2[v2$ano == 2019])


boxplot(v2$prev[v2$ano == 2019])
sum(0.033,0.069,0.074)/3

summary(v2$prev[v2$ano >= 2017 | v2$ano<=2019])

boxplot(v2$prev[v2$ano == 2017 | v2$ano==2018 | v2$ano==2019], ylim=c(0,0.2))

ativa <- c(0.02,
0.00,
0.47,
0.03,
0.01,
0.00)

ativa
summary(ativa)
boxplot(ativa)

# True prevalence
ap <- 0.0389
se <- 0.95
sp <- 0.99
tp <- (ap+sp-1)/(se+sp-1)
tp


# See the pdf about apparent prevalence on the commenetaries folder
Model{
  for(i in 1:1){
  y[i] ~ dbin(ap[i],n[i])
  ap[i] <- se*pi+(1-sp)*(1-pi)
  }
  se ~ dbeta(8, 8)
  sp ~ dbeta(9999, 1)
  pi ~ dbeta(1, 1)
  pvn <- sp*(1-pi)/((1-se)*pi+sp*(1-pi))
  pvp <- se*pi/(se*pi+(1-sp)*(1-pi))
  OneMinusPVN <- 1-pvn
  }-sp)*(1-pi)}se ~ dbeta(8, 8)sp ~ dbe
list(y=c(39),n=c(11270))


################################
#sURVEILLANCE FILE v2 ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/Code case-control/")
v2 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")
library(tidyverse)

v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  filter(fingreso == "SI")%>%
  select(orden)%>%
  summarise(n())
# 7 answered that the animals enter the farm

v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  filter(nacido == NA)%>%
  summarise(n())
# all answered that didnt been born on the farm

v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  filter(fvac == "1si")%>%
  summarise(n())
table(v2$fvac, v2$ano, v2$caso)


v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  filter(fvacuno == "SI")%>%
  summarise(n())
# selfdeclaration of vaccination 27 of 35


# how may are backyard

table(v2$ano, v2$caso, v2$cat)
# 35 of 35 were backayard (farm in the paper denomination)

v2 %>%
  group_by(cat)%>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  summarise(n())

# Poopulation of animals

population_out_2019 <- v2 %>%
  group_by(existente3)%>%
  filter(ano=="2019")%>%
  filter(caso=="1")%>%
  summarise(n())%>%
  arrange(desc(as.numeric(existente3)))

# Population on outbreaks
pp <-v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")
summary(as.numeric(pp$populacao))



# only 7 of the 35 outbreaks indicate that did take animals to the farm.
--------------------------------------------------

#Read simulation information----
setwd("~/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/Scenarios static network/Other simulation results/")
Iw1 <- read.csv(file="Iw29.01.22.csv")
Iw2 <- read.csv(file="Iw09.02.21_p2.5.csv")
Iw5 <- read.csv(file="Iw09.02.21_p5.0.csv")

plot(Iw1$none)  
summary(Iw1$none)  
summary(Iw2$none)  
summary(Iw5$none)  

Iw1$ac <- cumsum(Iw1$none)

plot(Iw1$ac)

# old code ----

acu <- data.frame(Iw1$days,Iw1$none, Iw2$none, Iw5$none)

acu$c1 <- cumsum(acu$Iw1.none)
acu$c2 <- cumsum(acu$Iw2.none)
acu$c5 <- cumsum(acu$Iw5.none)
acu$d <- seq(1:1096)


  ggplot(acu, aes(d, c1)) +
  geom_line()+
    geom_line(aes(d,c2))+
    geom_line(aes(d,c5))

  
  
  
  labs(tag = "a")+
  theme_linedraw()+
  labs(x = NULL, y ="Prevalence", size=12) +
  # ylim(0,0.31)+
  labs(color ='') +
  theme(text = element_text(size = 10),
        legend.key.size = unit(2, "mm"))+
  scale_color_brewer(palette = "Set2")
c







library(ggsn)
library(viridis)
ggplot(map, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=density)) +
  geom_path(aes(x=long, y=lat, group=group), colour="grey60", size=0.04)+
  #scale_fill_viridis_c(option = "B", na.value = "white", direction = 1) +
  # scale_fill_viridis_c(option = "C", na.value = "white", direction = 1,
  #                      breaks = c(1,5, 20, 40, 50)) +
  # scale_fill_gradientn(colours = c("yellow", "green", "purple", "red"),
  #                      values = scales::rescale(c(1, 2, 10, 25, 50)))
  scale_fill_viridis_c(option = "D", na.value = "white", direction = -1,
                       breaks = c(1, 4, 25, 55),
                       values = scales::rescale(c(1, 4, 25, 55)))

# Fig. 1 premises and densities mean of 2017-2019 (total)

densitym <-
  ggplot(map, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=density)) +
  geom_path(aes(x=long, y=lat, group=group), colour="grey60", size=0.04)+
  scale_fill_viridis_c(option = "D", na.value = "white", direction = -1,
                       breaks = c(1, 2, 5, 10, 20, 40, 55, 89),
                       values = scales::rescale(c(1, 2, 5, 10, 20, 40, 55, 89))) +
  theme(legend.key.height= unit(2.5, 'cm')) +
  labs(x="Longitude", y= "Latitude", fill= "Density of
Premises (Km2)") +
  theme(text = element_text(size = 18))+
  north(map, symbol = 2) +
  ggsn::scalebar(map, dist = 100, dist_unit = "km", transform = TRUE, 
                 st.size = 4, height = 0.01, border.size = 0.07,
                 model = "WGS84")

summary(vigi$cantidad)

# Map of premises
premisesm <-
  ggplot(map, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=cantidad)) +
  geom_path(aes(x=long, y=lat, group=group), colour="grey60", size=0.04)+
  scale_fill_viridis_c(option = "C", na.value = "white", direction = -1,
                       breaks = c(200,500,1000, 2000, 4000, 6000, 8068),
                       values = scales::rescale(c(200,500, 1000,2000,4000,6000, 8068))) +
  theme(legend.key.height= unit(2.5, 'cm'))+
  theme(text = element_text(size = 18))+
  labs(x="Longitude", y= "Latitude", fill= "Number of
Premises") +
  north(map, symbol = 2) +
  ggsn::scalebar(map, dist = 100, dist_unit = "km", transform = TRUE,
                 st.size = 4, height = 0.01, border.size = 0.07,
                 model = "WGS84")

ggpubr::ggarrange(premisesm, densitym, ncol = 2)  



# Descriptive of outbreaks
# inclussion on case information ----
# Filtering the new population

v2 %>%
  filter(ano=="2019")%>%
  group_by(caso,ano)%>%
  summarise(n(), pop=sum(as.numeric(populacao)),
            positives=sum(as.numeric(pos)),
            sick=sum(as.numeric(enfermo3)),
            death=sum(as.numeric(mortos3)),
            culled=sum(as.numeric(sacrificados3)),
            cadastal=sum(as.numeric(existente3)),
            prevalence2=(positives)/pop)

10157+1019
132+129+250

b <- v2 %>%
  filter(ano=="2019")%>%
  filter(caso=="1")

# coping all degree and weigthed degree from m2 2019 ()
# looking lost codigo de sitio
b$identificador <- trimws(b$identificador_operador)
# $pronaca and novapork
b$codigo.sitio2[b$identificador_operador == "1790319857"] <- "1790319857001.1711"
b$codigo.sitio2[b$identificador_operador == "0400872305"] <- "0400872305001.2302"

b$codigo.sitio2 <- trimws(b$codigo.sitio2)

b$d <- banco$correspondence$ktotal[match(b$codigo.sitio2,
                                         banco$correspondence$database.id)]

b$wd <- banco$correspondence$ktotal_animais[match(b$codigo.sitio2,
                                                  banco$correspondence$database.id)]

# Percentage of notifications that were not on the official registry of movements
table(is.na(b$d))
# FALSE  TRUE 
# 48   119 
119/167

# 72% of notifications are not in the movement database
# Percentage of cases that were not on the official registry of movements
table(is.na(b$d[b$caso=="1"]))
# FALSE  TRUE 
# 10    25 
25/35

#71.42 of confirmed cases were not on the official registry of movements

# Only 1 case is in the official registry of vaccine
# 4 not vaccinated
table(b$vac_2019[b$caso =="1"])

table(b$vac_2019)

table(b$d)
table(b$wd)

table(b$d)  
table(b$d, b$wd)  
table(b$populacao)
summary(b$populacao)

summary(b$d)
summary(b$wd)

table(b$size_premises)
b$d[is.na(b$d)] <- "1"
b$d[b$d=="0"] <- "1"

# Percentages of outbreaks considering 
120/167
43/167
4/167

1-0.7185
b$d <- as.numeric(b$d)
table(b$all.degree)  
summary(b$all.degree)

b$degree.percentiles <- cut(b$d, c(0,3,36,104,1372))
table(b$degree.percentiles)

# (0,3]         (3,36]       (36,104] 
# 140             26              0 
# (104,1.37e+03] 
# 1 

136/167 #75th
81%
140/167 
# 83.8% in the 90th 
26/167
# 15.6% in the 99.5th
1/167
table(b$degree.percentiles[b$caso =="1"])

# (0,1]          (1,3]         (3,36] 
# 30              0              5 
# (36,104] (104,1.37e+03] 
# 0              0 

30/35
# 85.72% of cases on the 75th
5/35
#14.29% on the 99.5th

(128+250+57)/11176
(128+250+57)/1019
132/(10157+1019)
132/1019
500/3.89


# < 21 Temporal Graphs in ggplot ----
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Modelling control strategies for CSF/Temporal/Lenz/TemporalNetworkAccessibility/")
setwd("/home/alfredo/Dropbox/0.USP/7.Publicações/Modelling control strategies against CSF: traders and markets static and temporal networks/Temporal/Lenz/TemporalNetworkAccessibility/")

library(rjson); library(readr); library(ggplot2)
c <- fromJSON(file="file.json")
h <- read_csv(file="h1719.csv")
h <- data.frame(h)
colnames(h) <- c("date", "h")
c1 <- c(966,966,966,966)
c <- c(c1,c)
temp <- data.frame(c,h)
plot(temp$date, temp$c)
plot(temp$h, temp$X0)

summary(temp$h)
summary(temp$c)
ratio <- max(temp$h)/max(temp$c)

ggplot(temp, aes(date, h))+
  geom_line()

ggplot(temp, aes(date, c))+
  geom_line()
  
ggplot(temp, aes(x=date))+
  geom_line(aes(y=h), col="red")+
  geom_line(aes(y=c^2.17))+
  theme_linedraw()+
  labs(x="Time (days)")+
  theme(text = element_text(size = 16))+
  scale_y_continuous(
    name = "Shortest paths",
    sec.axis = sec_axis(~.-1000, 
                        name="Cumulative paths"))


ggplot(temp, aes(x=date))+
  geom_line(aes(y=h), col="red")+
  geom_line(aes(y=c^2.15))+
  theme_linedraw()+
  labs(x="Time (days)")+
  theme(text = element_text(size = 16))+
  scale_y_continuous(
    name = "Shortest paths",
    sec.axis =~.-1000)


library(RColorBrewer)
display.brewer.pal(n = 8, name = 'Set2')
# Hexadecimal color specification 
brewer.pal(n = 8, name = "Set2")



#individual years
library(rjson); library(readr); library(ggplot2)
h <- read_csv(file="h17.csv")
c <- read_csv(file="c17.csv")
temp <- data.frame(c,h)
temp$date <- 1:nrow(temp)


colnames(temp) <- c("c", "h", "date")
plot(temp$date, temp$c)
plot(temp$h, temp$X0)

summary(temp$h)
summary(temp$c)
hist(temp$h)
rug(temp$h)

ggplot(temp, aes(date, h))+
  geom_line()

ggplot(temp, aes(date, c))+
  geom_line()

c <- ggplot(temp, aes(x=date))+
  geom_line(aes(y=h), col="#66C2A5")+
  geom_line(aes(y=c*ratio))+
  theme_linedraw()+
  labs(x="Time (days)",
       tag="c")+
  theme(text = element_text(size = 16))+
  scale_y_continuous(
    name = "Shortest paths",
    sec.axis = sec_axis(~., 
                        name="Cumulative paths"))

# 2017
temp$w <- c(rep(1:52,each=7))

# 2018
temp$w <- c(rep(1:52,each=7),53)

# 2019
temp$w <- c(rep(1:52,each=7),53)

library(tidyverse)
temp %>%
  group_by(w)%>%
  summarise(h2=mean(h))%>%
  arrange(desc(h2))


ggpubr::ggarrange(a,b,c, ncol=1)


234=234
365+262=627
262+(365*2)=992
(234+262+205)/3

median(temp$h)

