#PFAS datathon - if biomonitoring study participants are assigned to a
#water system, this script runs a Wilcox rank sum test (Mann-Whitney U test)
#comparing particpants in water systems with no PFAS detections with those
#in water systems with PFAS detections.

#set up directory and load libraries
if (!dir.exists("~/R_projects")) dir.create("~/R_projects", FALSE); setwd("~/R_projects")
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('foreign')) install.packages('foreign'); library('foreign')

#short function to fix spaces in column names
fix_pun <- function(x) { #fix punctuation issues
  x_names <- make.names(names(x))
  x_names <- gsub(x = x_names, pattern = '\\.\\.', replacement = '_') 
  x_names <- gsub(x = x_names, pattern = '\\.', replacement = '_')
  x_names <- gsub(x = x_names, pattern = '\\_$', replacement = '')
  x_names
}

#load DDW data####
#download file
DDWfilelink <- "https://www.waterboards.ca.gov/pfas/docs/test_results/pfas_monitoring_no_tp.xlsx"
tf <- tempfile(fileext = ".xlsx")
httr::GET(DDWfilelink, httr::write_disk(tf))
df <- read_excel(tf, skip = 2)

#fix column punctuation
colnames(df) <- fix_pun(df)

#count non-detects by system####
d_sys <- df %>% mutate(nondetect = ifelse(Finding_ng_L_ == 0, "nondetect", "detect")) %>%
  filter(nondetect == "detect") %>%
  group_by(System_No, Chemical) %>% 
  summarize(detects = n())
nd_sys <- df %>% mutate(nondetect = ifelse(Finding_ng_L_ == 0, "nondetect", "detect")) %>%
  filter(nondetect == "nondetect") %>%
  group_by(System_No, Chemical) %>% 
  summarize(nondetects = n()) 

#join count of detects with count of nondetects (by chemical and system)
sys_pfas <- full_join(d_sys, nd_sys)

#create empty data frame to count non-tested chemicals
PFASchems <- df %>% select(Chemical) %>% distinct()
Systems <- df %>% select(System_No) %>% distinct()
df_empty <- expand.grid(PFASchems$Chemical, Systems$System_No, KEEP.OUT.ATTRS = T, stringsAsFactors = F) %>%
  filter(!is.na(Var1))
names(df_empty) <- c("Chemical", "System_No")

#join detect/nondetect counts with empty data frame to identify non-tested chemicals
sys_pfas <- full_join(sys_pfas, df_empty) %>% mutate(nottested = ifelse(is.na(detects) & is.na(nondetects), 1, NA))
sys_pfas[is.na(sys_pfas)] <- 0

#note status of each water system/chemical pair
sys_pfas <- sys_pfas %>% mutate(status = ifelse(detects > 0, 'detect',
                                                ifelse(detects == 0 & nottested == 0, 'non-detect', 'not tested')))


#assign participants to random water systems (in reality, should be done based on actual ZIP/latlong)
#load biomonitoring data
biodata <- read.csv("PFAS/WBDatathon_R_mockmedicaldata.csv", stringsAsFactors = F)

#assign particpants to a random water system
sys = Systems %>% mutate(id = row_number())
biodata <- biodata %>% mutate(rand_watersys = round(runif(425, min = 0, max = 161)))
biodata <- left_join(biodata, sys, by = c("rand_watersys" = "id"))

#once participants are assigned to water system, join with water system detection data
comparison <- left_join(biodata, sys_pfas)

#test Wilcox rank sum test for PFOS between water systems with detects and water systems without detects
biodata_PFOSwsdetect <- comparison %>% filter(status == "detect", Chemical == "PERFLUOROOCTANE SULFONIC ACID (PFOS)") %>% 
  select(System_No, New_PFOS, status)
biodata_PFOSwsnondetect <- comparison %>% filter(status == "non-detect", Chemical == "PERFLUOROOCTANE SULFONIC ACID (PFOS)") %>% 
  select(System_No, New_PFOS, status)

res <- wilcox.test(biodata_PFOSwsdetect$New_PFOS, biodata_PFOSwsnondetect$New_PFOS)
res$p.value
