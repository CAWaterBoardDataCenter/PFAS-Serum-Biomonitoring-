
setwd("~/")

library(tidyverse)
library(foreign)
library(ggplot2)

PFAS_data <- read.dbf("PFAS_SPAspatialjoin.dbf", as.is = T)

SPA_avg <- PFAS_data %>% group_by(SPAs_ABBV, CHEMICAL) %>% summarize(spa_chem_mean = mean(RESULTS, na.rm = T))
print.data.frame(SPA_avg)


#biomonitoring data
biodata <- read.csv("PFASSerumData.csv", stringsAsFactors = F)
biodata_long <- biodata %>% gather(CHEMICAL, RESULTS, PFDeA:MeFOS, factor_key = F)

#make column names the same  
SPA_avg_rename <- SPA_avg %>% separate(SPAs_ABBV, into = c("SPA", "SPANo"), sep = " ")
SPA_avg_rename$SPANo <- as.numeric(SPA_avg_rename$SPANo)
SPA_nonulls <- SPA_avg_rename %>% filter(!is.na(SPA))
SPA_nonulls <- SPA_nonulls %>% mutate(concat = paste0(SPANo, CHEMICAL))

biodata_long <- biodata_long %>% mutate(concat = paste0(SPANo, CHEMICAL))

#join bio data with wq data
df <- left_join(biodata_long, SPA_nonulls)
df <- df %>% filter(!is.na(spa_chem_mean))


sp <- ggplot(df, aes(x = spa_chem_mean, y = RESULTS)) + geom_point() +
  facet_wrap(~CHEMICAL)
sp



