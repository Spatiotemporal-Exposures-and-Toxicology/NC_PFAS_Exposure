# Explore and join PFAS data
library(data.table)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(janitor)
library(sf)
library(cartography)
library(nhdplusTools)


#### Read in the PFAS Data ####
PFAS <- sf::read_sf(paste0(my.path,"input/PFAST-Summary-Round1.csv"))


#### Read in the GIS Data from NCOneMap ####

##Public Water Supply Sources ##
Public.Water.Supply.Sources <- sf::read_sf("input/GIS/Public_Water_Supply_Water_Sources/Public_Water_Supply_Water_Sources.shp")

## Surface_Water_Intakes ##
Surface.Water.Intakes <- sf::read_sf("input/GIS/Surface_Water_Intakes/Surface_Water_Intakes.shp")

## Current Public Water Systems ## 
Current.Public.Water.Systems <- sf::read_sf(paste0(my.path.GIS,"Type_A_Current_Public_Water_Systems_(2004)/Type_A_Current_Public_Water_Systems_(2004).shp"))

## Surface Water Supply Watersheds ## 
Surface.Watersheds <- sf::read_sf(paste0(my.path.GIS,"NC_Surface_Water_Supply_Watersheds/dwq_wsws_20141126.shp"))

## EPA Federal Registerd Facilities in NC
FRS <- sf::read_sf(paste0(my.path.GIS,"state_single_nc/STATE_SINGLE_NC.csv"))
FRS.Water <- subset(FRS,SITE_TYPE_NAME == "WATER SYSTEM")

# The StationID in the PFAS data is comparable to the  SFDW
# (i.e. Safe Drinking Water) ID in the the field PGM_SYS_ACRNMS
# Use stringr to extract and create a new ID
pattern <- "SFDW:\\s*(.*?)\\s*,"
 FRS.Water %<>% mutate("SFDW.ID" = str_match(FRS.Water$PGM_SYS_ACRNMS,pattern = pattern)[,2])
 
#### Transform and/or calculate consistent spatial coordinates ####
# The public water supply sources uses the NC State Plane coordinate system
# we will use this as the common projected coordinate system
# We will also calculate lat/lon coordiantes

 
PWS.crs <- st_crs(Public.Water.Supply.Sources)

PWS.coords.proj <- Public.Water.Supply.Sources %>% st_coordinates() %>% as_tibble()
PWS.coords.geo <- Public.Water.Supply.Sources %>% 
                  st_transform(crs = 4269) %>% 
                  st_coordinates() %>% as_tibble() %>% 
                  rename("Longitude" = "X","Latitude" = "Y")

Public.Water.Supply.Sources <- cbind(Public.Water.Supply.Sources,PWS.coords.proj,PWS.coords.geo)

# FRS.Water.coords.proj <- FRS.Water %>% st_coordinates()

 #### Fix unmatched ID ####
# Prior to Joining we have to fix some wrong IDs in the PFAS data
# StationID and EPA_srcID of the unmatched PFAS data
# We need to correct these to the correct public water source ID (EPA_srcID)
# We'll do this by hand
# [1,] "NC0100010-2" "39012"
# [2,] "NC0161015"   "18601"
# [3,] "NC0286020-2" "65263"
# [4,] "NC0363025-2" "39297"
# [5,] "NC0407030"   "31427"
# [6,] "NC0424020"   "32565"
# [7,] "NC0431010"   "33929"
# [8,] "NC0433015"   "34137"
# [9,] "NC0437020"   "34248"
# [10,] "NC0446015"   "34565"
# [11,] "NC0452010"   "34686"
# [12,] "NC0465020"   "35340"
# [13,] "NC0465025"   "35349"
# [14,] "NC0466040"   "35860"
# [15,] "NC6059015-1" "71963"


# The better join is the federal registered system ID (EPA_srcID) because it 
# is specific to the intake point. The pws_id or station_id is good, but encompasses multiple points
# so it is a many to one join. We will use it when the EPA_srcID fails 

# NC0100010-2 --> simple mismatch, change 39012 to 39013
PFAS$EPA_srcID[PFAS$EPA_srcID== 39012] = 39013
# NC0161015  -->  Multiple wells with the IDs  different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 18601] = 39013
# NC0286020-2 --> NC0286020 is a match to the town. Change the EPA_srcID to match
PFAS$EPA_srcID[PFAS$EPA_srcID== 65263] = 39251
# NC0363025-2 --> simple mismatch in EPA_srcID (also remove the -2 in station ID)
PFAS$EPA_srcID[PFAS$EPA_srcID== 39297] = 39296
# NC0407030  31427 --> Can join using the water system name in FRS (BATH WATER SYSTEM) Has lat/lon

# "NC0424020"   "32565" --> --> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 32565] = 32566
# "NC0431010"   "33929" --> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 33929] = 33932
# NC0433015"   "34137"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 34137] = 34139
# "NC0437020"   "34248"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 34248] = 34250
# "NC0446015"   "34565"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 34565] = 34566

# "NC0452010"   "34686"
# This is the town of Maysville - it does not match to the public water supply or FRS
# It is also not mapped on the SDWIS or FRS website. It is however, near Camp Lejune 
# and has many his PFAS, so we need to resolve it 

# "NC0465020"   "35340"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 35340] = 35339

#"NC0465025"   "35349"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 35349] = 35350

# "NC0466040"   "35860"--> Multiple wells with the IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 35860] = 35861

# "NC6059015-1" "71963"-->  IDs just slightly different. 
#                       Change it to the one that looks to be the primary source
PFAS$EPA_srcID[PFAS$EPA_srcID== 71963] = 74912

#### Join PFAS with Public Water Supply Water Intakes (point) ####
PFAS$EPA_srcID <- as.numeric(PFAS$EPA_srcID)
PFAS.Water.Supply <- left_join(PFAS,Public.Water.Supply.Sources,by = c("EPA_srcID"= "federal_id"))
idx.NA <- is.na(PFAS.Water.Supply$source_typ)

PFAS.Water.Supply <- PFAS.Water.Supply[!idx.NA,]
#### Join PFAS with Public Water Supply Water Distribution areas (polygon) ####

# PFAS$wasyid <- str_sub(PFAS$station_id,3,9)
# PFAS.PWS<- right_join(PFAS,Current.Public.Water.Systems,by = c("wasyid"))
# idx.NA <- is.na(PFAS.PWS$futcip)
# PFAS.PWS.na <- PFAS.PWS[idx.NA,]

#### Exploratory Analysis  with Joined Data ####

#1) Percent Non-Detect by chemical 
#2) Percent Non-Detect Total
#3) Percent Non-Detect by site and by chemical 
#4) Map the non-detect percentage
#5) Map the concentration of the highest 2 or 3 detected

# Concert PFAS.Water.Supply to a spatial.points.dataframe

PFAS.Water.Supply$Concentration <- as.numeric(PFAS.Water.Supply$Conc_ppt)
PD.total <- PFAS.Water.Supply %>% 
  summarize("Total_NonDetect" = sum(is.na(Concentration))/length(Concentration))

PD.chem <- PFAS.Water.Supply %>% 
  group_by(Analyte_Name) %>%
  summarize("Chem_NonDetect" = sum(is.na(Concentration))/length(Concentration))

PD.site <- PFAS.Water.Supply %>% 
  group_by(EPA_srcID) %>%
  summarize("Site_Detect" = sum(!is.na(Concentration))/length(Concentration)*100)


NC.Watershed <- sf::read_sf(paste0(my.path.GIS,"8-Digit_HUC_Subbasins/HUC_8.shp"))


Major.Watershed <- NC.Watershed %>% 
  group_by(DWQ_Basin) %>%
  summarise() %>%
  ungroup() %>% st_as_sf()

PFAS.PWS.stat <-   left_join(PFAS.Water.Supply,PD.site,by = "EPA_srcID") %>% 
  group_by(EPA_srcID) %>% summarize_all(mean) 



PFAS.sf <- st_as_sf(x = PFAS.PWS.stat, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
        st_transform(st_crs(NC.Watershed))


plot(st_geometry(NC.Watershed), col = "grey90", border = "grey")
plot(st_geometry(PFAS.sf[PFAS.sf$Site_Detect==0,]),add = TRUE)
# plot population
propSymbolsLayer(
  x = PFAS.sf, 
  var = "Site_Detect", 
  inches = 0.2, 
  col = "#E84923",
  legend.pos = "topleft",  
  legend.title.txt = "Percent Detected "
)

# layout
layoutLayer(title = "Percent Detected PFAS/PFOA by Public Water System",
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "bottomleft")


## Map the watersheds

plot(st_geometry(Major.Watershed), col = "#e4e9de", border = "darkseagreen4", 
     bg = "lightblue1", lwd = 0.5)

typoLayer(
  x = Major.Watershed, 
  var = "DWQ_Basin",
  border = "white", 
  lwd = 0.5,
  legend.pos = "n", 
  add = TRUE
)

# plot labels
labelLayer(
  x = Major.Watershed, 
  txt = "DWQ_Basin", 
  col= "black", 
  cex = 0.7, 
  font = 4,
  halo = TRUE, 
  bg = "white", 
  r = 0.1, 
  overlap = FALSE, 
  show.lines = FALSE
)
# map layout
layoutLayer(
  title = "NC Major Watersheds", 
  sources = "Sources: NC Department of Water Quality",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE, 
  theme = "taupe.pal"
) 


# HUC 8 plots
plot(st_geometry(NC.Watershed), col = "grey90", border = "grey")
# plot population
typoLayer(
  x = NC.Watershed, 
  var = "SUBBASIN_1",
  border = "white", 
  lwd = 0.5,
  legend.pos = "n", 
  add = TRUE
)

# layout
layoutLayer(
  title = "NC HUC 8 Watersheds", 
  sources = "Sources: NC Department of Water Quality",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE, 
  theme = "taupe.pal"
) 

# 12-digit watersheds

NC.HUC12 <- get_huc12(Major.Watershed)
plot(st_geometry(NC.HUC12), col = "grey90", border = "grey")
# plot population
typoLayer(
  x = NC.HUC12, 
  var = "huc12",
  border = "white", 
  lwd = 0.5,
  legend.pos = "n", 
  add = TRUE
)

# layout
layoutLayer(
  title = "NC HUC 12 Watersheds", 
  sources = "Sources: NC Department of Water Quality",  
  author = paste0("cartography ", packageVersion("cartography")), 
  frame = FALSE,
  north = TRUE, 
  tabtitle = TRUE, 
  theme = "multi.pal"
) 

