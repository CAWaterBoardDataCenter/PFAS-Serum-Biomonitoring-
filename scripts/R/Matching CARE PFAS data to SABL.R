# CARE PFAS data linkage to SABL, Drinking water PFAS
# This code spatially joins geocoded CARE 2, CARE 3, and CARE LA participant geocoded addresses to water system
# boundaries from the SABL, downloads UCMR3 and Waterboard drinking water data, and links CARE participant data 
# to drinking water PFAS data


# load libraries
library("writexl")
library("haven")
library("tigris")
library("readxl")
library ("xlsx")
library("vroom")
library("dplyr")
library("tidyr")
library("tidyverse")
library("sf")
library("tmap")


###### CARE data ######

### link CARE geocoded data with PFAS lab results
# read in CARE geocoded addresses excel file (these addresses were geocoded in ArcGIS)
careLA_geo0 <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_LA", range = cell_cols("X:AO"))
careLA_geo <- careLA_geo0[ ,c(1:4,11,18)]

care2_geo0 <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_2", range = cell_cols("X:AM"))
care2_geo <- care2_geo0[ ,c(1:4,11,16)]

care3_geo0 <- read_excel("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_3", cell_cols("X:AM"))
care3_geo <- care3_geo0[ ,c(1:4,11,16)]

# read in CARE PFAS lab data (n may differ slightly from above if participant did not give blood)
care2_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-2/Final_SAS/care2_pfas_20200121.sas7bdat')
care2_lab <- care2_lab[ ,-c(8)]

care3_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-3/Final_SAS/care3_pfas_20210428.sas7bdat')
care3_lab <- care3_lab[ ,-c(8:9)]

careLA_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE/Final_SAS/care_allpfas_20191209_ka.sas7bdat')
careLA_lab <- careLA_lab[ ,-c(8:10)]

# join geocoded data and lab data (right join to exclude patients who did not give blood)
careLA_pfas <- right_join(careLA_geo, careLA_lab, by=c('Kit Code'='Patient_ID'))
careLA_pfas$study <- "careLA"

care2_pfas <- right_join(care2_geo, care2_lab, by=c('Kit Code'='Patient_ID'))
care2_pfas$study <- "care2"

care3_pfas <- right_join(care3_geo, care3_lab, by=c('Kit Code'='Patient_ID'))
care3_pfas$study <- "care3"

# stack data frames
care_pfas <- rbind(careLA_pfas, care2_pfas, care3_pfas)

# (removed this) keep only geocoded matches and ties
# care_pfas <- subset(care_pfas, Status %in% c('M', 'T'))

# create new variables denoting top 10% of PFOS and PFOA
care_pfas <- care_pfas %>% mutate(PFOS_Top10 =
                    case_when(PFOS_num >= quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9)
                               & Status != "U" ~ "Top 10%", 
                              PFOS_num < quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9)
                               & Status != "U" ~ "Lower"))
care_pfas <- care_pfas %>% mutate(PFOA_Top10 =
                    case_when(PFOA_num >= quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9)
                              & Status != "U" ~ "Top 10%", 
                              PFOA_num < quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9)
                              & Status != "U" ~ "Lower"))


### Load water system shapefile and create map
# load water System Area Boundaries Layer (SABL)
SABL_sf <- st_read("J:/BiomCA/BC_GIS/CARE-2-3_GIS/CARE-2-3_PFAS_Biomarker/SABL_Public_10.5.22/SABL_Public_221005.shp")

# quick check - quick thematic map
qtm(SABL_sf)

# convert CARE dataframe into spatial data (make sure the crs (coordinate reference system) is correct!)
care_pfas_sf <- st_as_sf(care_pfas, coords= c(x="X",y="Y"), crs=4326)

# if need be convert the coordinate system / projection so it is consistent for the spatial join
SABL_crs <- st_crs(SABL_sf)
care_pfas_sf1 <- st_transform(care_pfas_sf, crs = SABL_crs)

# spatially join CARE data with SABL
care_pfas_j <- st_join(care_pfas_sf1, SABL_sf, join = st_within)

# create new variable 'overlap' that shows when participants matched to multiple water systems
# (fromLast = TRUE indicates that "duplication should be considered from the reverse side". 
# The two logical vectors are combined using | since a TRUE in at least one of them indicates a duplicated value.)
care_pfas_j$overlap0 <- duplicated(care_pfas_j$Sample_ID) | duplicated(care_pfas_j$Sample_ID, fromLast = TRUE)
care_pfas_j <- care_pfas_j %>% mutate(overlap =
                                    case_when(is.na(SABL_PWSID) ~ "NA",
                                              overlap0 == "TRUE" ~ "1", 
                                              overlap0 == "FALSE" ~ "0")) %>% 
                                subset(select = -c(overlap0) )

# save data as excel file
write_xlsx(care_pfas_j, "CARE_PFAS_SABL.xlsx")
# for some reason csv variable names shift in csv
# write.csv(care_pfas_j, "CARE_PFAS_SABL1.csv", row.names = FALSE)


###### UCMR3 DATA ######

# create tempfil object
temp1 <- tempfile()

# download UCMR3 zip file https://www.epa.gov/sites/default/files/2017-02/ucmr-3-occurrence-data-by-state.zip
# filename UCMR3_All_Tribes_AK_LA
download.file("https://www.epa.gov/sites/default/files/2017-02/ucmr-3-occurrence-data-by-state.zip", temp1)
unzip(temp1, 
      exdir = "ucmr3")

# subset to CA
ucmr3_allstates <- vroom("ucmr3/UCMR3_All_Tribes_AK_LA.txt")
ucmr3_all <- subset(ucmr3_allstates, State == "CA")

#make list of PFAS and filter
unique(ucmr3_all$Contaminant) %>% sort()

pfaslist1 <- c("PFBS",
               "PFHpA",
               "PFHxS",
               "PFNA",
               "PFOA",
               "PFOS")

#filter for PFAS data and delete the rest
ucmr3 <- ucmr3_all %>% 
  filter(ucmr3_all$Contaminant %in% pfaslist1)

#assign zero for results below LOD (for now)
ucmr3$AnalyticalResultValue1 <- case_when(ucmr3$AnalyticalResultsSign == "<" ~ 0,
                                          ucmr3$AnalyticalResultsSign == "=" ~ ucmr3$AnalyticalResultValue)

# remove temp files
rm(temp1, ucmr3_allstates, ucmr3_avg_test)

# take average of each separate PFAS by PWSID (includes non-detects assigned 0)
ucmr3_avg <- ucmr3 %>% group_by(PWSID, Contaminant) %>% summarize(ucmr3_pwsid_mean = mean(AnalyticalResultValue1))
print.data.frame(ucmr3_avg)

# transpose contaminant variable to wide in order to list by PWSID
ucmr3_avg1 <- ucmr3_avg %>%
  pivot_wider(names_from = Contaminant, values_from = ucmr3_pwsid_mean)

# sum the averages of each separate PFAS to get total PFAS
ucmr3_avg1$U_TotalPFAS <- ucmr3_avg1$PFBS + ucmr3_avg1$PFHpA + ucmr3_avg1$PFHxS + 
  ucmr3_avg1$PFNA + ucmr3_avg1$PFOA + ucmr3_avg1$PFOS

# rename UCMR3 PFAS variables
ucmr3_avg1 <- ucmr3_avg1 %>% rename("U_PFBS"="PFBS", 
                                    "U_PFHpA"="PFHpA", 
                                    "U_PFHxS"="PFHxS", 
                                    "U_PFNA"="PFNA", 
                                    "U_PFOA"="PFOA", 
                                    "U_PFOS"="PFOS")

# join UCMR3 to CARE SABL data
care_ucmr3 <- left_join(care_pfas_j, ucmr3_avg1, by=c('SABL_PWSID'='PWSID'))

# create indicator variable for whether or not the water system was tested for PFAS in ucmr3
care_ucmr3 <- care_ucmr3 %>% mutate(ucmr3 =
                                      case_when(is.na(SABL_PWSID) ~ "NA",
                                                is.na(U_TotalPFAS) ~ "0",
                                                TRUE ~ "1"))

# write excel file
# for some reason column names shift when saving as csv
#write.csv(care_ucmr3, "CARE_UCMR3_jointest.csv", row.names = FALSE)
write_xlsx(care_ucmr3, "CARE_PFAS_UCMR3.xlsx")


###### WATERBOARD SDWIS FILES ######

# sdwis original files are very large.  download may take some time.
# tab files seem to work better for download than csv
# SDWIS https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/20190101-to-present.csv

# 2011-2014
sdwis1 <- vroom("https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/20110101-20141231.tab")

# 2015-2018
sdwis2 <- vroom("https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/20150101-20181231.tab")

# 2019-present
sdwis3 <- vroom("https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/20190101-present.tab")

#write.csv(temp2, "SDWIS/SDWIS3.csv", row.names = FALSE)
#rm(temp2)

#stack data frames, 
sdwis <- rbind(sdwis1, sdwis2, sdwis3)

#make list of PFAS and filter
unique(sdwis$"Analyte Name") %>% sort()

pfaslist <- c("PERFLUOROBUTANESULFONIC ACID (PFBS)",
              "PERFLUOROCTANE SULFONIC ACID (PFOS)",
              "PERFLUOROCTANOIC ACID (PFOA)",
              "PERFLUORODECANOIC ACID (PFDA)",
              "PERFLUORODODECANOIC ACID (PFDOA)",
              "PERFLUOROHEPTANOIC ACID (PFHPA)",
              "PERFLUOROHEXANE SULFONIC ACID (PFHXS)",
              "PERFLUOROHEXANOIC ACID (PFHXA)",
              "PERFLUORONONANOIC ACID (PFNA)",
              "PERFLUOROTETRADECANOIC ACID (PFTA)",
              "PERFLUOROTRIDECANOIC ACID (PFTRDA)",
              "PERFLUOROUNDECANOIC ACID (PFUNA)")

#filter for PFAS data and delete the rest
sdwis_pfas <- sdwis %>% 
  filter(sdwis$"Analyte Name" %in% pfaslist)

#remove no longer needed
rm(sdwis, sdwis1, sdwis2, sdwis3)

## Write PFAS data to csv ##
write.csv(sdwis_pfas, "sdwis_pfas_12.21.22.csv", row.names = FALSE)
#write_xlsx(sdwis_pfas, "sdwis_pfas.xlsx")



