# Transforming coordinates utm2lonlat.R
# Alfredo Acosta PhD
# alfredojavier55                                       
# 15.04.2023

library(raster); library(rgdal);library(sp)

# Code to transform UTM to Lon-Lat ----
# 1 Reading the information and organizing ----
setwd("~/Dropbox/1.Epidat/Consultoria ASPE/Informes dinámicos/utm2lonlat")
c <- read_excel("VEPG02_REP_GENERAL_BASICO 2022.xls")

colnames(c)

# Subseting the coordinates from the archives ----
c$zona <- substr(c$`UTM WGS-84`,1,4)
table(c$zona)
c$x <-substr(c$`UTM WGS-84`, 7,12)
c$y <-substr(c$`UTM WGS-84`, 16,22)
c$y <-gsub(")", "", c$y)

plot(c$x, c$y)
colnames(c)

# 2 17S Coordinates selecction and projection  ----
c17S <- c[,c(29,30)]
colnames(c17S)
c17S <- data.frame(c17S)
c17S$x <- as.numeric(c17S$x)
c17S$y <- as.numeric(c17S$y)
class(c17S)

# 2.1 Creating a spatial points with the assumed projection ----
c17S_sp <- SpatialPoints(c17S, proj4string=CRS("+proj=utm +zone=17 +south +datum=WGS84 +units=m")) 
c17S_coord <- data.frame(c17S_sp@coords)

ggplot(c17S_coord, aes(y=y, x=x))+
  geom_point(size=3, color="red")

# 2.2 Transforming to lat long ----
c17S_lon_lat <- spTransform(c17S_sp, CRS("+proj=longlat +datum=WGS84"))
plot(c17S_lon_lat)

# 2.3 Saving the lon lat coordinates ----
c17S$lon <- c17S_lon_lat@coords[,1]
c17S$lat <- c17S_lon_lat@coords[,2]

# Plot a map with the known coordinates
# Plot single points
ggplot(c17S, aes(y=lat, x=lon))+
  geom_point(size=3, color="red")

# Plot on the map
# 2.4 Using manabi coordinates to see the location on green ----
setwd("~/Dropbox/0.Postdoc/Publications/Bluetongue/")
v2 <- read.csv(file = "manabi.lon.lat.csv")

m <- ggplot(v2, aes(x=lon, y=lat))+
  geom_point(size=0.1)

m +
  geom_point(data=c17S, aes(x=lon,y=lat ),
             size=1, color="green")

# 2.5 Filter coordinates eliminating erroneous coord ----
c17S_2 <- c17S %>%
  filter(lon >-82)%>%
  filter(lon < -77)%>%
  filter(lat <2)%>%
  filter(lat >-7)

# Transfering the new coordinates to c ----
c$lon17S <- c17S_2$lon[match(c$x, c17S_2$x)]
c$lat17S <- c17S_2$lat[match(c$y, c17S_2$y)]
colnames(c)

# Map of provinces ----
ec2<-rgdal::readOGR(dsn="~/Dropbox/0.USP/1 Projeto/SHP/", layer="nxprovincias")
ec2 <- spTransform(ec2, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ec <- fortify(ec2)

# plot map of provinces
m +
  geom_point(data=c, aes(x=lon17S,y=lat17S),
             size=2, color="green")+
  geom_text(data=c, aes(x=lon17S,y=lat17S, label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group))+
  xlim(-81,-75)

# 2.6 Selecting the coordinates outsite country boundaries ----
c2 <- c[is.na(c$lat17S) == TRUE,]
table(c2$zona)

# filtering colums
c3 <- data.frame(c2$`# ORDEN`, c2$x, c2$y, c2$zona, c2$PROVINCIA)
table(c3$c2.zona)

# 3 18S Coordinates selection and projection ----
colnames(c3)
c4 <- c3[,c(2,3)]
colnames(c4) <- c("x","y")
c18S <- c4
c18S <- data.frame(c18S)
c18S$x <- as.numeric(c18S$x)
c18S$y <- as.numeric(c18S$y)

# Creating a spatial points with the known projection to see
c18S_sp <- SpatialPoints(c18S, proj4string=CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m")) 

c18S_coord <- data.frame(c18S_sp@coords)

ggplot(c18S_coord, aes(y=y, x=x))+
  geom_point(size=3, color="red")

# Transforming to lat long
c18S_lon_lat <- spTransform(c18S_sp, CRS("+proj=longlat +datum=WGS84"))
plot(c18S_lon_lat)

# saving the lon lat coordinates
c18S$lon <- c18S_lon_lat@coords[,1]
c18S$lat <- c18S_lon_lat@coords[,2]

# Plot a map with the known coordinates
# Plot single points
ggplot(c18S, aes(y=lat, x=lon))+
  geom_point(size=3, color="red")

# Plot on the map
m +
  geom_point(data=c18S, aes(x=lon,y=lat ),
             size=1, color="green")

# Filter coordinates 
c18S_2 <- c18S %>%
  filter(lon >-82)%>%
  filter(lon < -75)%>%
  filter(lat <2)%>%
  filter(lat >-7)

# Transfering the new coordinates to c
c$lon18S <- c18S_2$lon[match(c$x, c18S_2$x)]
c$lat18S <- c18S_2$lat[match(c$y, c18S_2$y)]

#plot map of provinces
m +
  geom_point(data=c, aes(x=lon18S, y=lat18S), 
             size=5, color="green") +
  geom_text(data=c, aes(x=lon18S,y=lat18S, label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group))+
  xlim(-81,-75)

# Deleting coordinates that are incorrect
# Example
# c4$lon18S[c4$v3.orden == "1220"] <- NA

# 3 Looking the other outside the boundaries after 17s and 18s----
# Cases without coordinates
c5 <- c[is.na(c$lat17S) == TRUE & is.na(c$lat18S) == TRUE,]
table(c5$zona)
# 17 M 17 N 18 M 18 N 
#    6   29    4    8 

# filtering colums
c6 <- data.frame(c5$`# ORDEN`, c5$x, c5$y, c5$zona, c5$PROVINCIA)
table(c6$c5.zona)
colnames(c6)

# 4 18N Coordinates selection and projection ----
c7 <- c6[,c(2,3)]
colnames(c7) <- c("x","y")
c18N <- c7
c18N <- data.frame(c18N)
c18N$x <- as.numeric(c18N$x)
c18N$y <- as.numeric(c18N$y)

# creating a spatial points with the known projection to see
c18N_sp <- SpatialPoints(c18N, proj4string=CRS("+proj=utm +zone=18 +north +datum=WGS84 +units=m")) 

c18N_coord <- data.frame(c18N_sp@coords)

ggplot(c18N_coord, aes(y=y, x=x))+
  geom_point(size=3, color="red")

# Transforming to lat long
c18N_lon_lat <- spTransform(c18N_sp, CRS("+proj=longlat +datum=WGS84"))
plot(c18N_lon_lat)

# saving the lon lat coordinates
c18N$lon <- c18N_lon_lat@coords[,1]
c18N$lat <- c18N_lon_lat@coords[,2]

# Plot a map with the known coordinates
# Plot single points
ggplot(c18N, aes(y=lat, x=lon))+
  geom_point(size=3, color="red")

# Plot on the map
m +
  geom_point(data=c18N, aes(x=lon,y=lat ),
             size=1, color="green")

# Filter coordinates 
c18N_2 <- c18N %>%
  filter(lon >-82)%>% #86
  filter(lon < -77)%>% #75
  filter(lat <5)%>% #2 
  filter(lat >-7)

# Transfering the new coordinates to c ----
c$lon18N <- c18N_2$lon[match(c$x, c18N_2$x)]
c$lat18N <- c18N_2$lat[match(c$y, c18N_2$y)]

#plot map of provinces
m +
  geom_point(data=c, aes(x=lon18N,y=lat18N),
             size=3, color="green")+
  geom_text(data=c, aes(x=lon18N,y=lat18N, label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group))+
  xlim(-81,-75)

# Deleting incorrect coordinates ----
# Example 
# c4$lat18N[c4$v3.orden == "298"] <- NA


# 5 17N Looking the other outside the boundaries after 17s and 18s and 18N----
# Cases without coordinates 17N
c8 <- c[is.na(c$lat17S) == TRUE & is.na(c$lat18S) == TRUE & is.na(c$lon18N) == TRUE,]
table(c8$zona)
# 17 M 17 N 18 M 18 N 
#   25  100   25  11 

# filtering colums
c9 <- data.frame(c8$`# ORDEN`, c8$x, c8$y, c8$zona, c8$PROVINCIA)

# all
c10 <- c9[,c(2,3)]
colnames(c10) <- c("x","y")
c17N <- c10
c17N <- data.frame(c17N)
c17N$x <- as.numeric(c17N$x)
c17N$y <- as.numeric(c17N$y)

# creating a spatial points with the known projection to see
c17N_sp <- SpatialPoints(c17N, proj4string=CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m")) 

c17N_coord <- data.frame(c17N_sp@coords)

ggplot(c17N_coord, aes(y=y, x=x))+
  geom_point(size=3, color="red")

# Transforming to lat long
c17N_lon_lat <- spTransform(c17N_sp, CRS("+proj=longlat +datum=WGS84"))
plot(c17N_lon_lat)

# saving the lon lat coordinates
c17N$lon <- c17N_lon_lat@coords[,1]
c17N$lat <- c17N_lon_lat@coords[,2]

# Plot a map with the known coordinates
# Plot single points
ggplot(c17N, aes(y=lat, x=lon))+
  geom_point(size=3, color="red")

# Plot on the map
m +
  geom_point(data=c17N, aes(x=lon,y=lat ),
             size=1, color="green")

# Filter coordinates 
c17N_2 <- c17N %>%
  filter(lon >-82)%>%
  filter(lon < -77)%>%
  filter(lat <2)%>%
  filter(lat >-7)

# Transfering the new coordinates to c
c$lon17N <- c17N_2$lon[match(c$x, c17N_2$x)]
c$lat17N <- c17N_2$lat[match(c$y, c17N_2$y)]

#plot map of provinces
m +
  geom_point(data=c, aes(x=lon17N,y=lat17N),
             size=5, color="green")+
  geom_text(data=c, aes(x=lon17N,y=lat17N, label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group))+
  xlim(-81,-75)


# 6 Looking the other outside the boundaries after 17s and 18s and 18N and 17N----
c11 <- c[is.na(c$lat17S) == TRUE & is.na(c$lat18S) == TRUE &
           is.na(c$lon18N) == TRUE & is.na(c$lon17N) == TRUE,]

table(c11$zona)
# 17 M 17 N 18 M 18 N 
#   21   29    25    10 

# colapsing lat long ----
library(tidyverse)
c$lon <- str_replace_all(paste(c$lon17S,c$lon18S, c$lon18N, c$lon17N),"NA","")
c$lon <- str_replace_all(c$lon," ","")

c$lat <- str_replace_all(paste(c$lat17S,c$lat18S, c$lat18N, c$lat17N),"NA","")
c$lat <- str_replace_all(c$lat," ","")

c$lon[c$lon==""] <- NA
c$lat[c$lat==""] <- NA

c <- data.frame(c)

# how many coordinates have not been trasnformed ----
## Should be changed manually
table(is.na(c$lat))
colnames(c)

class(c$lon)

# 7 Looking how are the suspicious events distributed ----
m +
  geom_point(data=c, aes(x=as.numeric(lon), 
                         y=as.numeric(lat),
                         colour=ESPECIE),
             size=0.05, alpha=0.4)+
  scale_colour_viridis_d()+
  # geom_text(data=c, aes(x=as.numeric(lon),y=as.numeric(lat), label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group), size=0.1)+
  xlim(-81,-75)+
  theme_minimal()

# 8 Doing it again ... ----
# filtering colums of the ones that are not transformed ----
c13 <- data.frame(c12$X..ORDEN, c12$x, c12$y, c12$zona, c12$PROVINCIA)

# How many per zone and provincia
c13 %>% 
  group_by(c12.zona, c12.PROVINCIA) %>% 
  summarise(n())

# all
c14 <- c13[,c(2,3)]
colnames(c14) <- c("x","y")
c17x <- c14
c17x <- data.frame(c17x)
c17x$x <- as.numeric(c17x$x)
c17x$y <- as.numeric(c17x$y)

# creating a spatial points with the known projection to see
c17x_sp <- SpatialPoints(c17x, proj4string=CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m")) 

c17x_coord <- data.frame(c17x_sp@coords)

ggplot(c17x_coord, aes(y=y, x=x))+
  geom_point(size=3, color="red")

# Transforming to lat long
c17x_lon_lat <- spTransform(c17x_sp, CRS("+proj=longlat +datum=WGS84"))
plot(c17x_lon_lat)

# saving the lon lat coordinates
c17x$lon <- c17x_lon_lat@coords[,1]
c17x$lat <- c17x_lon_lat@coords[,2]

# Plot a map with the known coordinates
# Plot single points
ggplot(c17x, aes(y=lat, x=lon))+
  geom_point(size=3, color="red")

# Plot on the map
m +
  geom_point(data=c17x, aes(x=lon,y=lat ),
             size=1, color="green")

# Filter coordinates 
c17x_2 <- c17x %>%
  filter(lon >-86)%>%  #82
  filter(lon < -75)%>%
  filter(lat <2)%>%
  filter(lat >-7)

# Transfering the new coordinates to c
c$lon17x <- c17x_2$lon[match(c$x, c17x_2$x)]
c$lat17x <- c17x_2$lat[match(c$y, c17x_2$y)]

#plot map of provinces
m +
  geom_point(data=c, aes(x=lon17x,y=lat17x),
             size=5, color="green")+
  geom_text(data=c, aes(x=lon17x,y=lat17x, label=PROVINCIA), size=2)+
  geom_path(data=ec, aes(x=long, y=lat, group=group))+
  xlim(-88,-72)










# Coping long lat from c from transformar coordenadas utm to lat lon
# Ready to transform all the coordinates to v2
# v2$lat <- c4$lat[match(v2$orden, c$orden)]
# v2$lon <- c4$lon[match(v2$orden, c$orden)]
# 
# v2 <- v2 %>% mutate(lon = ifelse(is.na(lon_c), lon_c4, lon_c))
# v2 <- v2 %>% mutate(lat = ifelse(is.na(lat_c), lat_c4, lat_c))
# 
# 
# 9 Saving database of transformed coordinates ----
# setwd("~/Dropbox/0.Postdoc/Publications/Bluetongue/")
# write.csv(v2, file = "coordinates.csv")
