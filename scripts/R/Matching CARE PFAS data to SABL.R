# CARE Top 10% PFOS and PFOA by SABL Map
# This code matches geocoded CARE 2, CARE 3, and CARE LA participant addresses to water system
# boundaries from the SABL, and then maps participants and SABLs, showing which participants
# have top 10% of PFOS and PFOA levels.


# load libraries
library("writexl")
library("haven")
library("tigris")
library("readxl")
library ("xlsx")
library("tidyverse")
library("dplyr")
library("sf")
library("tmap")


## link CARE geocoded data with PFAS lab results
#read in CARE geocoded addresses excel file (these addresses were geocoded in ArcGIS)
careLA_geo <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_LA", range = cell_cols("X:AO"))
careLA_geo <- careLA_geo[ ,c(1:4,18)]

care2_geo <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_2", range = cell_cols("X:AM"))
care2_geo <- care2_geo[ ,c(1:4,16)]

care3_geo <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_3", cell_cols("X:AM"))
care3_geo <- care3_geo[ ,c(1:4,16)]

#read in CARE PFAS lab data (n may differ slightly from above if participant did not give blood)
care2_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-2/Final_SAS/care2_pfas_20200121.sas7bdat')
care2_lab <- care2_lab[ ,-c(8)]

care3_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-3/Final_SAS/care3_pfas_20210428.sas7bdat')
care3_lab <- care3_lab[ ,-c(8:9)]

careLA_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE/Final_SAS/care_allpfas_20191209_ka.sas7bdat')
careLA_lab <- careLA_lab[ ,-c(8:10)]

#join geocoded data and lab data, then stack dataframes
careLA_pfas <- left_join(careLA_geo, careLA_lab, by=c('Kit Code'='Patient_ID'))
careLA_pfas$study <- "careLA"

care2_pfas <- left_join(care2_geo, care2_lab, by=c('Kit Code'='Patient_ID'))
care2_pfas$study <- "care2"

care3_pfas <- left_join(care3_geo, care3_lab, by=c('Kit Code'='Patient_ID'))
care3_pfas$study <- "care3"

care_pfas <- rbind(careLA_pfas, care2_pfas, care3_pfas)

#create new variables denoting top 10% of PFOS and PFOA
care_pfas <- care_pfas %>% mutate(PFOS_Top10 =
                    case_when(PFOS_num >= quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9) ~ "Top 10%", 
                              PFOS_num < quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9) ~ "Lower"))
care_pfas <- care_pfas %>% mutate(PFOA_Top10 =
                    case_when(PFOA_num >= quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9) ~ "Top 10%", 
                              PFOA_num < quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9) ~ "Lower"))


## Load water system shapefile and create map
#load water system area boundaries (SABL = system area boundaries layer)
SABL_sf <- st_read("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE-2-3_PFAS_Biomarker/SABL_Public_10.5.22/SABL_Public_221005.shp")

qtm(SABL_sf)

#convert CARE dataframe into spatial data (make sure the crs (coordinate reference system) is correct!)
care_pfas_sf <- st_as_sf(care_pfas, coords= c(x="X",y="Y"), crs=4326)

#if need be convert the coordinate system / projection so it is consistent for the spatial join
SABL_crs <- st_crs(SABL_sf)
care_pfas_sf1 <- st_transform(care_pfas_sf, crs = SABL_crs)

#spatially join CARE data with SABL
care_pfas_j <- st_join(care_pfas_sf1, SABL_sf, join = st_within)

#get CA border from tigris package
state <- tigris::states()
CA <- state[state$NAME == "California", ]
CA <- st_transform(CA, crs = SABL_crs)

# map of CA boundary, SABL, and CARE PFOS and PFOA top 10%
#order dataframe so top 10% appear first in each map
care_pfas_j <- care_pfas_j %>% arrange(PFOS_Top10)
PFOS_Top10 <- tm_shape(CA) + tm_borders(lwd = 1) + tm_shape(SABL_sf) +  tm_fill() + 
  tm_shape(care_pfas_j) + tm_bubbles(col = "PFOS_Top10", size = 0.06, pal = c("skyblue", "red"))

care_pfas_j <- care_pfas_j %>% arrange(PFOA_Top10)
PFOA_Top10 <- tm_shape(CA) + tm_borders(lwd = 1) + tm_shape(SABL_sf) +  tm_fill() + 
  tm_shape(care_pfas_j) + tm_bubbles(col = "PFOA_Top10", size = 0.06, pal = c("skyblue", "purple"))

tmap_save(PFOS_Top10, "PFOS_Top10.png", height = 7) 
tmap_save(PFOA_Top10, "PFOA_Top10.png", height = 7) 

#save data as excel file
write_xlsx(care_pfas_j, "CARE_PFAS_SABL.xlsx")

