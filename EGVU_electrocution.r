##########################################################################
# ASSESSING OVERLAP OF EGYPTIAN VULTURES WITH ELECTRICITY INFRASTRUCTURE IN AFRICA
# original script written by Steffen Oppel
# October 2018
# to guide fieldwork in Africa
##########################################################################

### modified on 16 March to curtail to project areas

# Load necessary library
library(readxl)
library(maptools)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(data.table)
library(lubridate)
library(rworldmap) # library rworldmap provides different types of global maps, e.g:
data(countriesLow)
#devtools::install_github("tidyverse/ggplot2")
require(ggplot2)
library(ggmap)
library(RANN)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM DATABASE AND ELIMINATE CRAP DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## only need to re-run after changes to database
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\R_imports\\RODBC_telemetry_input.r")), wait = FALSE, invisible = FALSE)
try(setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\R_imports"), silent=T)
#try(setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Raw_Data\\R_imports"), silent=T)
load("RODBC_EGVU_telemetry_input.RData")
setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA")
#setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA")


dim(tracks)
tracks<- tracks %>%
	filter(!LocID %in% c(68850, 35863, 38421, 40832, 3238)) %>%						### manually enter non-sensical GPS locations to be excluded
	mutate(Time=format(Time, format="%H:%M:%S")) %>%
	mutate(DateTime=ymd_hms(paste(Date,Time))) %>%
	mutate(Month=month(DateTime)) %>%
	mutate(loc_quality=ifelse(loc_type %in% c("GPS","gps"),"3",loc_quality))%>%
	filter(loc_quality %in% c("2","3")) %>%
	filter(duplicate==0) %>%
	arrange(Bird_ID,DateTime) %>%
	select(Transmitter_ID,Bird_ID,DateTime,Month,long,lat)
dim(tracks)
head(tracks)
str(tracks)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMOVE LOCATIONS OF DEAD BIRDS OR UNDEPLOYED TRANSMITTERS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (a in unique(tracks$Bird_ID)){										## this function does not work for transmitter 61952, because it was deployed twice
startd<-birds$Tag_date[match(a,birds$Name)]
tracks<-tracks[!(tracks$Bird_ID==a & as.Date(tracks$DateTime)<startd),]
if(birds$Status[birds$Name==a]=="Dead"){
endd<-birds$Stop_date[match(a,birds$Name)]
tracks<-tracks[!(tracks$Bird_ID==a & as.Date(tracks$DateTime)>endd),]}
}
dim(tracks)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ IN OUR PROJECT AREAS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data")
proj_area<-st_read("PROJECT_AREA.kml")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA OF WIND TURBINES AND POWER PYLONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### DATA PROVIDED BY RSPB FROM OPEN STREET MAP ######

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures")
#setwd("A:\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures")
pylons<-read_excel("EGVU_power_lines.xlsx", sheet="Pylons")
turbines<-read_excel("EGVU_power_lines.xlsx", sheet="WindTurbines")

head(pylons)
pylons<- pylons %>% filter(POINT_Y<20) %>%
	select(power,POINT_X,POINT_Y)
names(pylons)<-c("structure","long","lat")
pylons$structure<-"Electricity structure"
head(turbines)
turbines<- turbines %>% filter(POINT_Y<20) %>%
	select(generator_,POINT_X,POINT_Y)
names(turbines)<-c("structure","long","lat")
turbines$structure<-"Wind turbine"
structures<-rbind(pylons,turbines)



###### DATA PROVIDED BY SCF FROM NIGER AND CHAD ######

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger")
shapefiles<-list.files(pattern=".shp")

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\33KV")
shapefiles33KV<-list.files(pattern=".shp")

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\20KV")
shapefiles20KV<-list.files(pattern=".shp")



### files are in different formats and require custom treatment


### FILE TYPE 1 ###
setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger")
for (f in 1:2){
  nc <- st_read(shapefiles[f])
  if(f==1){NER_power1<-nc
  NER_power1<-st_transform(NER_power1, 4326) #### convert CRS to latlong WGS84 with EPSG 4326
  NER_power1<- NER_power1 %>% select(tident,ident,Latitude,Longitude,geometry)
  }else{
    
    ## check that CRS are identical
    if(st_crs(nc)!=st_crs(NER_power1)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
    nc<- nc %>% select(tident,ident,Latitude,Longitude,geometry)
    NER_power1<-do.call(rbind, list(NER_power1,nc))}
}

NER_power1<-st_zm(NER_power1, drop = TRUE, what = "ZM")				### remove the Z dimension




### FILE TYPE 2 ###

for (f in c(3,5)){
  nc <- st_read(shapefiles[f])
  if(f==3){NER_power2<-nc
  NER_power2<-st_transform(NER_power2, 4326) #### convert CRS to latlong WGS84 with EPSG 4326
  NER_power2<- NER_power2 %>% select(ID,Tension,Conducteur,AVAP,TypeI,BranchesI,LongitudeI,LatitudeI,geometry)
  }else{
    
    ## check that CRS are identical
    if(st_crs(nc)!=st_crs(NER_power2)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
    nc<- nc %>% select(ID,Tension,Conducteur,AVAP,TypeI,BranchesI,LongitudeI,LatitudeI,geometry)
    NER_power2<-do.call(rbind, list(NER_power2,nc))}
}


setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\20KV")
nc <- st_read(shapefiles20KV[1])
if(st_crs(nc)!=st_crs(NER_power2)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
nc<- nc  %>% select(ID,Tension,Conduct,AVAP,TypeI,BranchesI,LongitudeI,LatitudeI,geometry)
names(nc)<-names(NER_power2)
NER_power2<-do.call(rbind, list(NER_power2,nc))

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\33KV")
nc <- st_read(shapefiles33KV[1])
if(st_crs(nc)!=st_crs(NER_power2)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
nc<- nc  %>% select(ID,Tension,Conduct,AVAP,TypeI,BranchesI,LongitudeI,LatitudeI,geometry)
names(nc)<-names(NER_power2)
NER_power2<-do.call(rbind, list(NER_power2,nc))


head(NER_power2)
NER_power2<-st_zm(NER_power2, drop = TRUE, what = "ZM")				### remove the Z dimension






### FILE TYPE 3 ###

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\20KV")
for (f in c(3:7)){
  nc <- st_read(shapefiles20KV[f])
  if(f==3){NER_power3<-nc
  NER_power3<-st_transform(NER_power3, 4326) #### convert CRS to latlong WGS84 with EPSG 4326
  NER_power3<- NER_power3  %>% select(Name,descriptio,geometry)
  }else{
    
    ## check that CRS are identical
    if(st_crs(nc)!=st_crs(NER_power3)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
    nc<- nc  %>% select(Name,descriptio,geometry)
    NER_power3<-do.call(rbind, list(NER_power3,nc))}
}

setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Raw_Data\\NewLIFE_DATA\\A3_structures\\Niger\\33KV")
for (f in c(3:5)){
  nc <- st_read(shapefiles33KV[f])
    ## check that CRS are identical
    if(st_crs(nc)!=st_crs(NER_power3)){nc<-st_transform(nc, 4326)} ## convert CRS to latlong WGS84 with EPSG 4326 if not already in that CRS
    nc<- nc  %>% select(Name,descriptio,geometry)
    NER_power3<-do.call(rbind, list(NER_power3,nc))
}


head(NER_power3)
NER_power3<-st_zm(NER_power3, drop = TRUE, what = "ZM")				### remove the Z dimension





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE MAP OF PROJECT AREAS WITH POWERLINES AND EGVU locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wintracks<- tracks %>% filter(lat<20) %>%
  arrange(Bird_ID,DateTime)

### START WITH THE BASEMAP AND THE STUDY AREAS POLYGON
ggplot() + ## geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) +
  
  ### ADD POLYGON FOR STUDY AREA AND SPECIFY MAP EXTENT
  geom_sf(data=proj_area, color = "darkred", lwd=1.5, fill=NA) +
  
  ### ADD POWER LINES FROM NCF
  geom_sf(data=NER_power1, color = "green", lwd=1, fill=NA) +
  geom_sf(data=NER_power2, color = "green", lwd=1, fill=NA) +
  geom_sf(data=NER_power3, color = "green", lwd=1, fill=NA) +
  coord_sf(ylim = c(12,16.1),  xlim = c(4,12.1))+
  
  ### ADD PYLON LOCATIONS FROM OSM
  geom_point(data=structures, aes(x=long, y=lat), color="blue",size=1.2) +
  
  ### ADD VULTURE LOCATIONS
  geom_point(data=wintracks, aes(x=long, y=lat),colour="black", size=0.8)+
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20),
        legend.background = element_rect(),
        legend.title = element_text(size=16),
        legend.key = element_blank(),
        legend.text=element_text(size=12),
        
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black")) +
  ylab("Longitude") +
  xlab("Latitude") 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE NUMBER OF EGVU LOCATIONS WITHIN A GIVEN DISTANCE OF A LINE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:\\STEFFEN\\RSPB\\Bulgaria\\Analysis\\Migration\\EGVUelectrocution")

### COMBINE FEATURES WITH DIFFERENT INFORMATION INTO SINGLE BASIC LINESTRING FEATURE 

NER_power1<- NER_power1 %>%
  mutate(Tension=330) %>%
  select(tident,Tension,geometry)

NER_power2<- NER_power2 %>%
  mutate(Tension=ifelse(Tension==0,33,Tension), ID=as.character(ID)) %>%
  select(ID,Tension,geometry)

NER_power3<- NER_power3 %>%
  mutate(Tension=33) %>%
  select(Name,Tension,geometry)

names(NER_power1)<-names(NER_power3)<-names(NER_power2)
NER_power<-do.call(rbind, list(NER_power1,NER_power2,NER_power3))



### CREATE SIMPLE FEATURES OBJECT OF VULTURE LOCATIONS
head(tracks)
tracks<- tracks %>% filter(!is.na(long)) %>% filter (!is.na(lat))
EV_sf <- st_as_sf(tracks, coords = c("long", "lat"), crs = 4326, agr = "constant")
EV_sf[1:3,]

### CREATE A WIDE BUFFER AROUND EACH LINE (10 km)
LINEBUFF<-st_buffer(NER_power, dist=0.1, nQuadSegs = 30, endCapStyle = "ROUND")

### OVERLAY VULTURE LOCATIONS IN EACH LINEBUFFER
## the st_join function requires 'sf' objects, not 'sfc' objects, so we need to transform first
## this gives a line for every point in a polygon, hence the number of lines is larger tjhan the input point feature because some points are in multiple polygons
OVERLAP <- st_join(EV_sf, st_as_sf(LINEBUFF), join = st_intersects)
head(OVERLAP)
str(OVERLAP)
dim(tracks)


### COUNT NUMBER OF VULTURE LOCATIONS IN EACH LINEBUFFER
MONTHLY_SUMMARY<-OVERLAP %>% filter (!is.na(ID)) %>% mutate(count=1) %>%
  group_by(ID, Month) %>%
  summarise(n_locs=sum(count),n_ind=length(unique(Bird_ID))) %>%
  st_set_geometry(NULL) %>%   ### this removes the geometry feature class and returns a data.frame
  arrange(desc(n_locs),desc(n_ind))
fwrite(MONTHLY_SUMMARY,"Niger_PowerLines_EGVU_abundance_by_month.csv")



TOTAL_SUMMARY<-OVERLAP %>% filter (!is.na(ID)) %>% mutate(count=1) %>%
  group_by(ID) %>%
  summarise(n_locs=sum(count),n_ind=length(unique(Bird_ID))) %>%
  st_set_geometry(NULL) %>%   ### this removes the geometry feature class and returns a data.frame
  arrange(desc(n_locs),desc(n_ind))


### COMBINE THE SUMMARY WITH THE ORIGINAL DATA FOR PLOTTING
NER_power_sum<-merge(NER_power,TOTAL_SUMMARY, by="ID", all.x=T)
fwrite(NER_power_sum %>% st_set_geometry(NULL),"Niger_PowerLines_EGVU_abundance.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE MAP OF PROJECT AREAS WITH POWERLINES AND EGVU locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pdf("EGVU_electrocution_risk_NIGER.pdf", width=9, height=6)
#postscript("EGVUmigration_Balkan.eps", width=9, height=6)
#jpeg("Figure1.jpg", width=9, height=6, units="in", res=600, quality=100)


### START WITH THE BASEMAP AND THE STUDY AREAS POLYGON
ggplot() + ## geom_polygon(data = basemap, aes(x=long, y = lat, group = group)) +
  
  ### ADD POLYGON FOR STUDY AREA AND SPECIFY MAP EXTENT
  geom_sf(data=proj_area, color = "cadetblue1", lwd=1.5, fill=NA) +
  
  ### ADD VULTURE LOCATIONS
  geom_point(data=wintracks, aes(x=long, y=lat),colour="darkgrey", size=0.1)+
  
  ### ADD POWER LINES FROM SCF
  geom_sf(data=NER_power_sum, aes(color = n_locs), lwd=1.5, fill=NA) +
  scale_colour_gradient(name = 'EGVU locations \nwithin 10 km', low="darkolivegreen4", high="red", guide = "colourbar", limits=c(0, 500))+ 
  guides(fill=guide_legend(title="EGVU locations \nwithin 10 km"))+
  coord_sf(ylim = c(11.8,15.9),  xlim = c(5,12.1))+
  
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20),
        legend.background = element_rect(),
        legend.title = element_text(size=16),
        legend.key = element_blank(),
        legend.text=element_text(size=12),
        
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black")) +
  ylab("Longitude") +
  xlab("Latitude") 



dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT OUTPUT TO INTERACTIVE POWERLINE FILE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### these two lines crash R for unknown reasons
#st_write(NER_power_sum, dsn="NIGER_powerlines_EGVU_abundance.shp", driver="ESRI")
#st_write(NER_power_sum, "NIGER_powerlines_EGVU_abundance.kml", layer="NIGER_powerlines_EGVU_abundance", driver="KML")

## CONVERT TO SPATIAL LINES DATAFRAME AND EXPORT
output<-as(NER_power_sum,"Spatial")
writeOGR(output, dsn = '.', layer = 'NIGER_powerlines_EGVU_abundance', driver = "ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(output, dsn = 'NIGER_powerlines_EGVU_abundance.kml', layer = 'NIGER_powerlines_EGVU_abundance', driver = "KML")


