{
  # >> Versao guia ---- pass the map 26.09.2022----

# agregate in the spatial map and get the DPA_PARROQ 
ec3 <-rgdal::readOGR(dsn="~/Dropbox/1.Epidat/Consultoria ASPE/Informes dinámicos/cvmppc/",layer="nxparroquias")
ec3 <<- spTransform(ec3, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

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

print(sum(is.na(as.numeric(vigi$c_p))))
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
vigi$pp <- gsub("pichincha quito san blas", "pichincha quito quito",vigi$pp)
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
print("cantidad a redistribuir")
print(sum(vigi$cantidad, na.rm = TRUE))
print("Diferencia entre datos y mapa")
print(sum(vigi2$cantidad, na.rm = TRUE) -
        sum(ec3@data$cantidad, na.rm = TRUE))

####ANTERIOR AINDA NAO DELETAR
print('Cantidad de polígonos faltantes')
print(sum(is.na(as.numeric(vigi$c_pp))))
#157

par <- vigi[is.na(as.numeric(vigi$c_pp)), 11]
print("Nombre de parroquias faltantes")
print(par)

print("cantidad faltante en el mapa")
print(sum(vigi$cantidad) - sum(ec3@data$cantidad, na.rm = TRUE)) }