#23 June 2025
#to create master data file for data analysis
setwd("~/Downloads")

#required package
#install.packages("dplyr")
library(dplyr)
#required dataset (UKB_premaster_core.Rda, sleep_variables_asd_final.Rda, mental_summer_ipaq.Rda, social_final.Rda, withdrawal2024.csv)
load("/Users/aungphyo/Downloads/social_sleep_data_files/UKB_premaster_core.Rda")
load("/Users/aungphyo/Downloads/social_sleep_data_files/sleep_variables_asd_final.Rda")
load("/Users/aungphyo/Downloads/social_sleep_data_files/mental_summer_ipaq.Rda")
load("/Users/aungphyo/Downloads/social_sleep_data_files/social_final.Rda")

#townsend data category
# Create tertiles
UKB_master$townsend_cat_tertile <- cut(
  UKB_master$townsend,
  breaks = quantile(UKB_master$townsend, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Low deprivation", "Mid deprivation", "High deprivation"),
  include.lowest = TRUE
)

# OR quartiles
UKB_master$townsend_cat_quartile <- cut(
  UKB_master$townsend,
  breaks = quantile(UKB_master$townsend, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4 (most deprived)"),
  include.lowest = TRUE
)


#extract interested columns from UKB master dataset
ukb<- UKB_master %>% select(1,2,3,57,14,18,27,28,29,51,58,59,60,61)

mental<- mental2 %>% select(1,2,3,6,7,8)

#combine dataset
dataset1<- inner_join(ukb,sleep3, by="eid")
dataset2<- inner_join(dataset1, social_final, by="eid")
dataset3<- inner_join(dataset2, mental, by="eid")

#exclusion
table(dataset3$health_self_report, useNA = "always")
#to remove poor health self report participants
dataset3$health_self_report[dataset3$health_self_report == "Poor"] <- NA
dataset3<- dataset3 %>%
  filter(!is.na(dataset3$health_self_report))

#to remove shift workers
table(dataset3$shift_work, useNA = "always")
#to remove always shift workers
dataset3 <- subset(dataset3, shift_work != "Always" | is.na(shift_work))

#to remove withdrwal participants
withdraw<-read.csv("/Users/aungphyo/Downloads/social_sleep_data_files/withdrawal2024.csv",header = TRUE)
dataset3 <- dataset3[! dataset3$eid %in% withdraw$X1195908,]

final_data<- dataset3
#save dataset
save(final_data, file="final_data.Rda")
##create csv file
write.csv(final_data, file="final_data.csv", row.names= FALSE)
