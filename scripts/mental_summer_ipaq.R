#17 June 2025
#Aung
#For the dataset of mental health (depression, anxiety), hours spending outside winter, summer, physical activity
setwd("~/Downloads")
#required package
library(dplyr)
#required dataset (raw_social_sleep_ukb.csv, physical_activity_participant.csv)
ukb<-read.csv("/Users/aungphyo/Downloads/autism_phenotype_social_sleep_aung/raw_social_sleep_ukb.csv",header = TRUE)
#After running the whole code, the final output file is mental_summer_ipaq.Rda/csv file.

##mental health symptoms (depression, anxiety)
mental_health<-ukb %>% select(1,210:213)
#freq depression
#table(mental_health$X30487.0.0, useNA= "always")
mental_health$X30487.0.0[mental_health$X30487.0.0 == "Prefer not to answer"] <- NA
mental_health<- mental_health %>%
  filter(!is.na(mental_health$X30487.0.0))
mental_health <- mental_health %>%
  rename(freq_depression = X30487.0.0)

#freq anxiety
#table(mental_health$X30484.0.0, useNA= "always")
mental_health$X30484.0.0[mental_health$X30484.0.0 == "Prefer not to answer"] <- NA
mental_health<- mental_health %>%
  filter(!is.na(mental_health$X30484.0.0))
mental_health <- mental_health %>%
  rename(freq_anxiety = X30484.0.0)

#freq little interest
table(mental_health$X30486.0.0, useNA= "always")
mental_health$X30486.0.0[mental_health$X30486.0.0 == "Prefer not to answer"] <- NA
mental_health<- mental_health %>%
  filter(!is.na(mental_health$X30486.0.0))
mental_health <- mental_health %>%
  rename(freq_little_interest = X30486.0.0)

#freq cannot control worrying
table(mental_health$X30485.0.0, useNA= "always")
mental_health$X30485.0.0[mental_health$X30485.0.0 == "Prefer not to answer"] <- NA
mental_health<- mental_health %>%
  filter(!is.na(mental_health$X30485.0.0))
mental_health <- mental_health %>%
  rename(freq_cannot_control_worrying = X30485.0.0)

#time spent outdoor in summer and winter
weather<-ukb %>% select(1,199,200)
#summer
table(weather$X30482.0.0, useNA= "always")
weather$X30482.0.0[weather$X30482.0.0 %in% c(-1, -3)] <- NA
weather$X30482.0.0[weather$X30482.0.0 == -10] <- 0.5
weather<- weather %>%
  filter(!is.na(weather$X30482.0.0))
weather <- weather %>%
  rename(hours_outdoor_summer = X30482.0.0)

#winter
table(weather$X30483.0.0, useNA= "always")
weather$X30483.0.0[weather$X30483.0.0 %in% c(-1, -3)] <- NA
weather$X30483.0.0[weather$X30483.0.0 == -10] <- 0.5
weather<- weather %>%
  filter(!is.na(weather$X30483.0.0))
weather <- weather %>%
  rename(hours_outdoor_winter = X30483.0.0)

##physical activity
#change your directory
physical<-read.csv("/Users/aungphyo/Downloads/physical_activity_participant.csv",header = TRUE)
#ipaq_activity international physical activity questionnaires
lvl.100700<- c(0,1,2)
lbl.100700<- c("low", "moderate", "high")
physical$X22032.0.0<- ordered(physical$X22032.0.0, levels=lvl.100700, labels=lbl.100700)
#table(physical$X22032.0.0, useNA = "always")
physical <- physical %>%
  filter(!is.na(physical$X22032.0.0))
physical <- physical %>%
  rename(ipaq_activity = X22032.0.0)

physical_final<- physical %>% select(1,2)

#combine datasets
mental1<- inner_join(mental_health, weather, by="eid")
mental2<- inner_join(mental1, physical_final, by="eid")

### Save variables of mental health, hours spent outdoor summer, physical activity as R datafile ######
save(mental2, file="mental_summer_ipaq.Rda")
##create csv file
write.csv(mental2, file="mental_summer_ipaq.csv", row.names= FALSE)

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))
