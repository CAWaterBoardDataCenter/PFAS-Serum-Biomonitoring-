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
# read in CARE geocoded addresses excel files (these addresses were geocoded in ArcGIS)
# Status "U" removed from CareLA, Care2, and Care3 because these were regeocoded and added in care_regeocoded
careLA_geo <- read_excel("J:/BiomCA/BC_GIS/CARE_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_LA", range = cell_cols("X:AO"))
careLA_geo <- careLA_geo[careLA_geo$Status != "U",c(1:4,11,18)]
careLA_geo$study <- "careLA"

care2_geo <- read_excel("J:/BiomCA/BC_GIS/CARE_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_2", range = cell_cols("X:AM"))
care2_geo <- care2_geo[care2_geo$Status != "U",c(1:4,11,16)]
care2_geo$study <- "care2"

care3_geo <- read_excel("J:/BiomCA/BC_GIS/CARE_GIS/CARE_PFAS_address/CARE_Geocoded_10.20.22_fromArcGIS.xlsx", sheet = "CARE_3", cell_cols("X:AM"))
care3_geo <- care3_geo[care3_geo$Status != "U",c(1:4,11,16)]
care3_geo$study <- "care3"

care_regeocoded <- read_excel("J:/BiomCA/BC_GIS/CARE_GIS/CARE_PFAS_address/CARE_unmatched_regeocoded_1.6.22.xlsx", sheet = "forR")

# stack geocoded address data
care_geo <- rbind(careLA_geo, care2_geo, care3_geo, care_regeocoded)

# read in CARE PFAS lab data (n may differ slightly from above if participant did not give blood)
care2_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-2/Final_SAS/care2_pfas_20200121.sas7bdat')
care2_lab <- care2_lab[ ,-c(8)]
care2_lab$Analysis_Date <- NA
care2_lab <- care2_lab %>% relocate(Analysis_Date, .after="DateReceived")

care3_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE-3/Final_SAS/care3_pfas_20210428.sas7bdat')
care3_lab <- care3_lab[ ,-c(9)]

careLA_lab <- read_sas('J:/BiomCA/Archive_LAB_DATA_restore/CARE/Final_SAS/care_allpfas_20191209_ka.sas7bdat')
careLA_lab <- careLA_lab[ ,-c(8:9)]
careLA_lab <- careLA_lab %>% rename("Analysis_Date"="AnalysisDate")

# stack lab data
care_lab <- rbind(care2_lab, care3_lab, careLA_lab)

# join geocoded data and lab data (right join to exclude patients who did not give blood)
care_pfas <- right_join(care_geo, care_lab, by=c('Kit Code'='Patient_ID'))

# rename Kit Code to Patient_ID
care_pfas <- care_pfas %>% rename("Patient_ID"="Kit Code")

# (removed this) keep only geocoded matches and ties
# care_pfas <- subset(care_pfas, Status %in% c('M', 'T'))

# create new variables denoting top 10% of PFOS and PFOA
# quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9)
# quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9)

care_pfas <- care_pfas %>% mutate(PFOS_Top10 =
                    case_when(PFOS_num >= quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9)
                               & Status != "U" ~ ">= 6.38", 
                              PFOS_num < quantile(care_pfas$PFOS_num, na.rm=TRUE, probs = 0.9)
                               & Status != "U" ~ "< 6.38"))
care_pfas <- care_pfas %>% mutate(PFOA_Top10 =
                    case_when(PFOA_num >= quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9)
                              & Status != "U" ~ ">= 2.36", 
                              PFOA_num < quantile(care_pfas$PFOA_num, na.rm=TRUE, probs = 0.9)
                              & Status != "U" ~ "< 2.36"))

# move top 10% variable
care_pfas <- care_pfas %>% relocate(PFOS_Top10, .after="Analysis_Date")
care_pfas <- care_pfas %>% relocate(PFOA_Top10, .after="Analysis_Date")

# save data as excel file
write_xlsx(care_pfas, "care_pfas.xlsx")


### Load water system shapefile and create map
# load water System Area Boundaries Layer (SABL)
SABL_sf <- st_read("J:/BiomCA/BC_GIS/CARE_GIS/CARE_PFAS_Biomarker/SABL_Public_10.5.22/SABL_Public_221005.shp")

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
                                    case_when(is.na(SABL_PWSID) ~ as.character(NA),
                                              overlap0 == "TRUE" ~ "1", 
                                              overlap0 == "FALSE" ~ "0")) %>% 
                                subset(select = -c(overlap0) )

# save data as excel file
write_xlsx(care_pfas_j, "care_pfas_sabl.xlsx")
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

# new variable for abbreviated PFAS names (for joining to CARE data)
ucmr3$Contaminant <- case_when(ucmr3$Contaminant == "PFBS" ~ "u_PFBS",
                                ucmr3$Contaminant == "PFHpA" ~ "u_PFHpA",
                                ucmr3$Contaminant == "PFHxS" ~ "u_PFHxS",
                                ucmr3$Contaminant == "PFNA" ~ "u_PFNA",
                                ucmr3$Contaminant == "PFOA" ~ "u_PFOA",
                                ucmr3$Contaminant == "PFOS" ~ "u_PFOS")

#assign zero for results below LOD (for now)
ucmr3$AnalyticalResultValue1 <- case_when(ucmr3$AnalyticalResultsSign == "<" ~ 0,
                                          ucmr3$AnalyticalResultsSign == "=" ~ ucmr3$AnalyticalResultValue)

# new indicator variable for detects (for some reason summarize function below did not work with Y/N variable)
ucmr3$detect <- case_when(ucmr3$AnalyticalResultsSign == "<" ~ 0,
                            ucmr3$AnalyticalResultsSign == "=" ~ 1)

# remove temp files
rm(temp1, ucmr3_allstates)

# take average of each separate PFAS by PWSID (includes non-detects assigned 0)
ucmr3_avg <- ucmr3 %>% group_by(PWSID, Contaminant) %>% 
              summarize(count = n(),
                        detect = sum(detect, na.rm=TRUE),
                        mean = mean(AnalyticalResultValue1, na.rm=TRUE))
# print.data.frame(ucmr3_avg)

# transpose contaminant variable to wide in order to list by PWSID
ucmr3_avg1 <- ucmr3_avg %>%
  pivot_wider(names_from = Contaminant, values_from = c(count, detect, mean))

# sum the averages of each separate PFAS to get total PFAS
ucmr3_avg1$u_TotalPFAS <- ucmr3_avg1$mean_u_PFBS + ucmr3_avg1$mean_u_PFHpA + ucmr3_avg1$mean_u_PFHxS + 
  ucmr3_avg1$mean_u_PFNA + ucmr3_avg1$mean_u_PFOA + ucmr3_avg1$mean_u_PFOS

# join UCMR3 to CARE SABL data
care_ucmr3 <- left_join(care_pfas_j, ucmr3_avg1, by=c('SABL_PWSID'='PWSID'))

# create indicator variable for whether or not the water system was tested for PFAS in ucmr3
care_ucmr3 <- care_ucmr3 %>% mutate(ucmr3 =
                                      case_when(is.na(SABL_PWSID) ~ as.character(NA),
                                                is.na(u_TotalPFAS) ~ "0",
                                                TRUE ~ "1"))

# create indicator variable for at least one detect from ucmr3 data
care_ucmr3 <- care_ucmr3 %>% mutate(u_detect =
                                      case_when((detect_u_PFBS > 0 |
                                                   detect_u_PFHpA > 0 |
                                                   detect_u_PFHxS > 0 |
                                                   detect_u_PFNA > 0 |
                                                   detect_u_PFOA > 0 |
                                                   detect_u_PFOS > 0) ~ '1',
                                                (detect_u_PFBS == 0 &
                                                   detect_u_PFHpA == 0 &
                                                   detect_u_PFHxS == 0 &
                                                   detect_u_PFNA == 0 &
                                                   detect_u_PFOA == 0 &
                                                   detect_u_PFOS == 0) ~ '0'))

# write excel file
# for some reason column names shift when saving as csv
#write.csv(care_ucmr3, "CARE_UCMR3_jointest.csv", row.names = FALSE)
write_xlsx(care_ucmr3, "care_pfas_ucmr3.xlsx")


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

# new variable for abbreviated PFAS names (for joining to CARE data)
sdwis_pfas$Analyte <- case_when(sdwis_pfas$"Analyte Name" == "PERFLUOROBUTANESULFONIC ACID (PFBS)" ~ "s_PFBS",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROCTANE SULFONIC ACID (PFOS)" ~ "s_PFOS",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROCTANOIC ACID (PFOA)" ~ "s_PFOA",
                                sdwis_pfas$"Analyte Name" == "PERFLUORODECANOIC ACID (PFDA)" ~ "s_PFDA",
                                sdwis_pfas$"Analyte Name" == "PERFLUORODODECANOIC ACID (PFDOA)" ~ "s_PFDOA",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROHEPTANOIC ACID (PFHPA)" ~ "s_PFHPA",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROHEXANE SULFONIC ACID (PFHXS)" ~ "s_PFHXS",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROHEXANOIC ACID (PFHXA)" ~ "s_PFHXA",
                                sdwis_pfas$"Analyte Name" == "PERFLUORONONANOIC ACID (PFNA)" ~ "s_PFNA",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROTETRADECANOIC ACID (PFTA)" ~ "s_PFTA",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROTRIDECANOIC ACID (PFTRDA)" ~ "s_PFTRDA",
                                sdwis_pfas$"Analyte Name" == "PERFLUOROUNDECANOIC ACID (PFUNA)" ~ "s_PFUNA")

#remove no longer needed
rm(sdwis, sdwis1, sdwis2, sdwis3)

## Write PFAS data to csv ##
write.csv(sdwis_pfas, "sdwis_pfas_12.21.22.csv", row.names = FALSE)
#write_xlsx(sdwis_pfas, "sdwis_pfas.xlsx")


# assign zero for results below LOD (for now)
sdwis_pfas$Result1 <- case_when(sdwis_pfas$"Less Than Reporting Level" == "Y" ~ 0,
                                sdwis_pfas$"Less Than Reporting Level" == "N" ~ sdwis_pfas$Result)

# new indicator variable for detects (for some reason summarize function below did not work with Y/N variable)
sdwis_pfas$detect <- case_when(sdwis_pfas$"Less Than Reporting Level" == "Y" ~ 0,
                                sdwis_pfas$"Less Than Reporting Level" == "N" ~ 1)

# take average of each separate PFAS by PWSID (includes non-detects assigned 0)
sdwis_avg <- sdwis_pfas %>% group_by(sdwis_pfas$"Water System Number", Analyte) %>% 
                        summarize(count = n(),
                                  detect = sum(detect, na.rm=TRUE),
                                  mean = mean(Result1, na.rm=TRUE))

# need to rename for transpose below to work
sdwis_avg <- sdwis_avg %>% rename("PWSID"='sdwis_pfas$"Water System Number"')

# transpose contaminant variable to wide in order to list by PWSID
sdwis_avg1 <- sdwis_avg %>%
  pivot_wider(names_from = Analyte, values_from = c(count, detect, mean))


# sum the averages of each separate PFAS to get total PFAS
sdwis_avg1$s_TotalPFAS <- rowSums(sdwis_avg1[, c("mean_s_PFBS",
                             "mean_s_PFOS",
                             "mean_s_PFOA",
                             "mean_s_PFDA",
                             "mean_s_PFDOA",
                             "mean_s_PFHPA",
                             "mean_s_PFHXS",
                             "mean_s_PFHXA",
                             "mean_s_PFNA",
                             "mean_s_PFTA",
                             "mean_s_PFTRDA",
                             "mean_s_PFUNA")], na.rm=TRUE)

# join sdwis to CARE SABL data
care_sdwis <- left_join(care_pfas_j, sdwis_avg1, by=c('SABL_PWSID'='PWSID'))

# create indicator variable for whether or not the water system was tested for PFAS in ucmr3
care_sdwis <- care_sdwis %>% mutate(sdwis =
                                      case_when(is.na(SABL_PWSID) ~ as.character(NA),
                                                is.na(s_TotalPFAS) ~ "0",
                                                TRUE ~ "1"))

# create indicator variable for at least one detect from sdwis data
# a few systems do not test for ALL 12 PFAS so need the !is.na in second part
care_sdwis <- care_sdwis %>% mutate(s_detect =
                                      case_when((detect_s_PFBS > 0 |
                                                  detect_s_PFOS > 0 | 
                                                  detect_s_PFOA > 0 | 
                                                  detect_s_PFDA > 0 |
                                                  detect_s_PFDOA > 0 |
                                                  detect_s_PFHPA > 0 |
                                                  detect_s_PFHXS > 0 |
                                                  detect_s_PFHXA > 0 |
                                                  detect_s_PFNA > 0 |
                                                  detect_s_PFTA > 0 |
                                                  detect_s_PFTRDA > 0 |
                                                  detect_s_PFUNA > 0) ~ '1',
                                                (!is.na(detect_s_PFBS) +
                                                   !is.na(detect_s_PFOS) + 
                                                   !is.na(detect_s_PFOA) + 
                                                   !is.na(detect_s_PFDA) +
                                                   !is.na(detect_s_PFDOA) +
                                                   !is.na(detect_s_PFHPA) +
                                                   !is.na(detect_s_PFHXS) +
                                                   !is.na(detect_s_PFHXA) +
                                                   !is.na(detect_s_PFNA) +
                                                   !is.na(detect_s_PFTA) +
                                                   !is.na(detect_s_PFTRDA) +
                                                   !is.na(detect_s_PFUNA) == 0) ~ '0'))

## Write PFAS data to csv ##
#write.csv(care_sdwis, "care_sdwis.csv", row.names = FALSE)
write_xlsx(care_sdwis, "care_sdwis.xlsx")



###### SDWIS and UCMR3 data together ######
# to identify water systems with CARE px high PFAS but NO water testing

care_ucmr31 <- st_drop_geometry(care_ucmr3)
care_ucmr31 <- care_ucmr31[,c(4,76,104:124)]

care_sdwis_ucmr3 <- inner_join(care_sdwis, care_ucmr31, by=c('Patient_ID'='Patient_ID', 'SABL_PWSID' = 'SABL_PWSID'))
rm(care_ucmr31)

# indicator variable for ANY PFAS testing, UCMR3 OR SDWIS
care_sdwis_ucmr3 <- care_sdwis_ucmr3 %>% mutate(anytesting =
                                      case_when((ucmr3 == "1" | sdwis == "1") ~ "1",
                                                (ucmr3 == "0" & sdwis == "0") ~ "0",
                                                (ucmr3 == NA & sdwis == NA) ~ as.character(NA)),
                                      PFOS_PFOA_Top =
                                        case_when((PFOS_Top10 == ">= 6.38" & PFOA_Top10 == ">= 2.36") ~ "Top 10% PFOS and PFOA",
                                                  (PFOS_Top10 == ">= 6.38" | PFOA_Top10 == ">= 2.36") ~ "Top 10% PFOS or PFOA",
                                                   (PFOS_Top10 == "< 6.38" & PFOA_Top10 == "< 2.36") ~ "< 90th percentile"))

write_xlsx(care_sdwis_ucmr3, "care_sdwis_ucmr3.xlsx")


write_xlsx(ucmr3_avg1, "ucmr3_avg1.xlsx")
