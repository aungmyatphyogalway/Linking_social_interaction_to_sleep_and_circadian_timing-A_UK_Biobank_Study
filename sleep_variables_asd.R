#17 June 2025
#Aung

###create dataset with sleep variables (rMEQ, PSQI, insomnia) and asd diagnosis
setwd("~/Downloads")
#required packages
library(dplyr)

##required datasets (sleep_asd_participant.csv, psqi_participant.csv, insomnia_participant.csv)
##rMEQ data
#change your directory
rmeq<-read.csv("/Users/aungphyo/Downloads/sleep_asd_participant.csv",header = TRUE)
#morning_evening
lvl.2546<- c(-3,-1,1,2,3,4)
lbl.2546<- c("Prefer not to answer", "Do not know", "Definitely a morning-type", "Rather more a morning-type than an evening-type", "Rather more an evening-type than a morning-type", "Definitely an evening-type")
rmeq$X30429.0.0<- ordered(rmeq$X30429.0.0, levels=lvl.2546, labels=lbl.2546)
rmeq$X30429.0.0[rmeq$X30429.0.0 %in% c("Prefer not to answer", "Do not know")] <- NA
rmeq <- rmeq %>%
  filter(!is.na(rmeq$X30429.0.0))
rmeq <- rmeq %>%
  rename(morning_evening = X30429.0.0)

#feeling tiredness
lvl.2544<- c(-3,1,2,3,4,5,6)
lbl.2544<- c("Prefer not to answer", "8:00pm-9:00pm", "9:00pm-10:15pm", "10:15pm-12:45am", "12:45am-2:00am", "2:00am-3:00am", "Other time of the day")
rmeq$X30427.0.0<- ordered(rmeq$X30427.0.0, levels=lvl.2544, labels=lbl.2544)
rmeq$X30427.0.0[rmeq$X30427.0.0 %in% c("Prefer not to answer", "Other time of the day")] <- NA
rmeq <- rmeq %>%
  filter(!is.na(rmeq$X30427.0.0))
rmeq <- rmeq %>%
  rename(feeling_tiredness = X30427.0.0)

#prefer time get up
lvl.2542<- c(-3,1,2,3,4,5,6)
lbl.2542<- c("Prefer not to answer", "5:00am-6:30am", "6:30am-7:45am", "7:45am-9:45am", "9:45am-11:00am", "11:00am-12 noon", "Other time of the day")
rmeq$X30425.0.0<- ordered(rmeq$X30425.0.0, levels=lvl.2542, labels=lbl.2542)
rmeq$X30425.0.0[rmeq$X30425.0.0 %in% c("Prefer not to answer", "Other time of the day")] <- NA
rmeq <- rmeq %>%
  filter(!is.na(rmeq$X30425.0.0))
rmeq <- rmeq %>%
  rename(prefer_time_get_up = X30425.0.0)

#time feel best
lvl.2545<- c(-3,1,2,3,4,5)
lbl.2545<- c("Prefer not to answer", "5:00am-8:00am", "8:00am-10:00am", "10:00am-5:00pm", "5:00pm-10:00pm", "10:00pm-5:00am")
rmeq$X30428.0.0<- ordered(rmeq$X30428.0.0, levels=lvl.2545, labels=lbl.2545)
rmeq$X30428.0.0[rmeq$X30428.0.0 == "Prefer not to answer"] <- NA
rmeq <- rmeq %>%
  filter(!is.na(rmeq$X30428.0.0))
rmeq <- rmeq %>%
  rename(time_feel_best = X30428.0.0)

#tiredness after waking
lvl.2543<- c(-3,1,2,3,4)
lbl.2543<- c("Prefer not to answer", "Very tired", "Fairly tired", "Fairly refreshed", "Very refreshed")
rmeq$X30426.0.0<- ordered(rmeq$X30426.0.0, levels=lvl.2543, labels=lbl.2543)
rmeq$X30426.0.0[rmeq$X30426.0.0 == "Prefer not to answer"] <- NA
rmeq <- rmeq %>%
  filter(!is.na(rmeq$X30426.0.0))
rmeq <- rmeq %>%
  rename(tiredness_after_waking = X30426.0.0)

##calculating rMEQ score
rmeq <- rmeq %>%
  mutate(
    # Q1: prefer_time_get_up
    score_q1 = case_when(
      prefer_time_get_up == "5:00am-6:30am" ~ 5,
      prefer_time_get_up == "6:30am-7:45am" ~ 4,
      prefer_time_get_up == "7:45am-9:45am" ~ 3,
      prefer_time_get_up == "9:45am-11:00am" ~ 2,
      prefer_time_get_up == "11:00am-12 noon" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Q2: tiredness_after_waking
    score_q2 = case_when(
      tiredness_after_waking == "Very refreshed" ~ 4,
      tiredness_after_waking == "Fairly refreshed" ~ 3,
      tiredness_after_waking == "Fairly tired" ~ 2,
      tiredness_after_waking == "Very tired" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Q3: feeling_tiredness (preferred bedtime)
    score_q3 = case_when(
      feeling_tiredness == "8:00pm-9:00pm" ~ 5,
      feeling_tiredness == "9:00pm-10:15pm" ~ 4,
      feeling_tiredness == "10:15pm-12:45am" ~ 3,
      feeling_tiredness == "12:45am-2:00am" ~ 2,
      feeling_tiredness == "2:00am-3:00am" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Q4: time_feel_best
    score_q4 = case_when(
      time_feel_best == "5:00am-8:00am" ~ 5,
      time_feel_best == "8:00am-10:00am" ~ 4,
      time_feel_best == "10:00am-5:00pm" ~ 3,
      time_feel_best == "5:00pm-10:00pm" ~ 2,
      time_feel_best == "10:00pm-5:00am" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Q5: morning_evening
    score_q5 = case_when(
      morning_evening == "Definitely a morning-type" ~ 6,
      morning_evening == "Rather more a morning-type than an evening-type" ~ 4,
      morning_evening == "Rather more an evening-type than a morning-type" ~ 2,
      morning_evening == "Definitely an evening-type" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Total rMEQ score
    rMEQ_score = score_q1 + score_q2 + score_q3 + score_q4 + score_q5
  )

#chronotype category
rmeq <- rmeq %>%
  mutate(
    chronotype_category = case_when(
      rMEQ_score >= 18 ~ "Morning Type",
      rMEQ_score >= 12 & rMEQ_score <= 17 ~ "Intermediate Type",
      rMEQ_score <= 11 ~ "Evening Type",
      TRUE ~ NA_character_
    )
  )

rmeq_final<- rmeq %>% select(1,13,14)

##PSQI
#change your directory
psqi<-read.csv("/Users/aungphyo/Downloads/psqi_participant.csv",header = TRUE)
#subjective sleep quality
lvl.2553<- c(-3,1,2,3,4)
lbl.2553<- c("Prefer not to answer", "Very good", "Fairly good", "Fairly bad", "Very bad")
psqi$X30467.0.0<- ordered(psqi$X30467.0.0, levels=lvl.2553, labels=lbl.2553)
psqi$X30467.0.0[psqi$X30467.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30467.0.0))
psqi <- psqi %>%
  rename(subj_sleep_quality = X30467.0.0)

#time taken to sleep
psqi$X30443.0.0[psqi$X30443.0.0 == -3 ] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30443.0.0))
psqi <- psqi %>%
  rename(time_taken_sleep = X30443.0.0)

#cannot sleep 30 mins
lvl.2554<- c(-3,0,1,2,3)
lbl.2554<- c("Prefer not to answer", "Not during the past month", "Less than once a week", "Once or twice a week", "Three or more times a week")
psqi$X30446.0.0<- ordered(psqi$X30446.0.0, levels=lvl.2554, labels=lbl.2554)
psqi$X30446.0.0[psqi$X30446.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30446.0.0))
psqi <- psqi %>%
  rename(cannot_sleep_30mins = X30446.0.0)

#actual sleep duration 
psqi$X30445.0.0[psqi$X30445.0.0 == -3 ] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30445.0.0))
psqi <- psqi %>%
  rename(actual_sleep_duration = X30445.0.0)

#time go to bed
psqi$X30442.0.0[psqi$X30442.0.0 == -3 ] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30442.0.0))
psqi <- psqi %>%
  rename(time_go_to_bed = X30442.0.0)

#time get up
psqi$X30444.0.0[psqi$X30444.0.0 == -3 ] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30444.0.0))
psqi <- psqi %>%
  rename(time_get_up = X30444.0.0)

#trouble wake mid night
psqi$X30447.0.0<- ordered(psqi$X30447.0.0, levels=lvl.2554, labels=lbl.2554)
psqi$X30447.0.0[psqi$X30447.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30447.0.0))
psqi <- psqi %>%
  rename(trouble_wake_mid_night = X30447.0.0)

#trouble bathroom
psqi$X30448.0.0<- ordered(psqi$X30448.0.0, levels=lvl.2554, labels=lbl.2554)
psqi$X30448.0.0[psqi$X30448.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30448.0.0))
psqi <- psqi %>%
  rename(trouble_bathroom = X30448.0.0)

#trouble breathing
psqi$X30449.0.0<- ordered(psqi$X30449.0.0, levels=lvl.2554, labels=lbl.2554)
psqi$X30449.0.0[psqi$X30449.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30449.0.0))
psqi <- psqi %>%
  rename(trouble_breathing = X30449.0.0)

#trouble cough
psqi$X30450.0.0<- ordered(psqi$X30450.0.0, levels=lvl.2554, labels=lbl.2554)
psqi$X30450.0.0[psqi$X30450.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30450.0.0))
psqi <- psqi %>%
  rename(trouble_cough = X30450.0.0)

#trouble cold
psqi$X30451.0.0<- ordered(psqi$X30451.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30451.0.0, useNA = "always")
psqi$X30451.0.0[psqi$X30451.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30451.0.0))
psqi <- psqi %>%
  rename(trouble_cold = X30451.0.0)

#trouble hot
psqi$X30452.0.0<- ordered(psqi$X30452.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30452.0.0, useNA = "always")
psqi$X30452.0.0[psqi$X30452.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30452.0.0))
psqi <- psqi %>%
  rename(trouble_hot = X30452.0.0)

#trouble bad dreams
psqi$X30453.0.0<- ordered(psqi$X30453.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30453.0.0, useNA = "always")
psqi$X30453.0.0[psqi$X30453.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30453.0.0))
psqi <- psqi %>%
  rename(trouble_bad_dreams = X30453.0.0)

#trouble pain
psqi$X30454.0.0<- ordered(psqi$X30454.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30454.0.0, useNA = "always")
psqi$X30454.0.0[psqi$X30454.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30454.0.0))
psqi <- psqi %>%
  rename(trouble_pain = X30454.0.0)

#trouble other
psqi$X30461.0.0<- ordered(psqi$X30461.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30461.0.0, useNA = "always")
psqi$X30461.0.0[psqi$X30461.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30461.0.0))
psqi <- psqi %>%
  rename(trouble_other = X30461.0.0)

#freq otc meds
psqi$X30463.0.0<- ordered(psqi$X30463.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30463.0.0, useNA = "always")
psqi$X30463.0.0[psqi$X30463.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30463.0.0))
psqi <- psqi %>%
  rename(freq_otc_meds = X30463.0.0)

#freq prescribed medicines
psqi$X30464.0.0<- ordered(psqi$X30464.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30464.0.0, useNA = "always")
psqi$X30464.0.0[psqi$X30464.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30464.0.0))
psqi <- psqi %>%
  rename(freq_prescibed_meds = X30464.0.0)

#frequency awake
psqi$X30465.0.0<- ordered(psqi$X30465.0.0, levels=lvl.2554, labels=lbl.2554)
#table(psqi$X30465.0.0, useNA = "always")
psqi$X30465.0.0[psqi$X30465.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30465.0.0))
psqi <- psqi %>%
  rename(freq_awake = X30465.0.0)

#problems enthusiasm
lvl.2557<- c(-3,0,1,2,3)
lbl.2557<- c("Prefer not to answer", "No problem at all", "Only a slight problem", "Somewhat of a problem", " A very big problem")
psqi$X30466.0.0<- ordered(psqi$X30466.0.0, levels=lvl.2557, labels=lbl.2557)
#table(psqi$X30466.0.0, useNA = "always")
psqi$X30466.0.0[psqi$X30466.0.0 == "Prefer not to answer"] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$X30466.0.0))
psqi <- psqi %>%
  rename(problems_enthusiasm = X30466.0.0)

##PSQI scoring
psqi <- psqi %>%
  mutate(
    compo_q1 = case_when(
      subj_sleep_quality == "Very good" ~ 0,
      subj_sleep_quality == "Fairly good" ~ 1,
      subj_sleep_quality == "Fairly bad" ~ 2,
      subj_sleep_quality == "Very bad" ~ 3,
      TRUE ~ NA_real_
    ))

#sleep_latency
# Recode cannot_sleep_30mins to numeric
psqi <- psqi %>%
  mutate(
    cannot_sleep_30mins_score = case_when(
      cannot_sleep_30mins == "Not during the past month" ~ 0,
      cannot_sleep_30mins == "Less than once a week" ~ 1,
      cannot_sleep_30mins == "Once or twice a week" ~ 2,
      cannot_sleep_30mins == "Three or more times a week" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Score for time taken to fall asleep
    time_taken_sleep_score = case_when(
      time_taken_sleep <= 15 ~ 0,
      time_taken_sleep > 15 & time_taken_sleep <= 30 ~ 1,
      time_taken_sleep > 30 & time_taken_sleep <= 60 ~ 2,
      time_taken_sleep > 60 ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Sum the two scores
    sleep_latency_raw = time_taken_sleep_score + cannot_sleep_30mins_score,
    
    # Final Sleep Latency Component Score
    compo_q2 = case_when(
      sleep_latency_raw == 0 ~ 0,
      sleep_latency_raw %in% c(1, 2) ~ 1,
      sleep_latency_raw %in% c(3, 4) ~ 2,
      sleep_latency_raw %in% c(5, 6) ~ 3,
      TRUE ~ NA_real_
    )
  )

#actual_sleep_duration
psqi <- psqi %>%
  mutate(
    sleep_duration_hours = actual_sleep_duration / 60,  # convert to hours
    
    compo_q3 = case_when(
      sleep_duration_hours >= 7 ~ 0,
      sleep_duration_hours >= 6 & sleep_duration_hours < 7 ~ 1,
      sleep_duration_hours >= 5 & sleep_duration_hours < 6 ~ 2,
      sleep_duration_hours < 5 ~ 3,
      TRUE ~ NA_real_
    )
  )

#sleep_efficiency
psqi <- psqi %>%
  mutate(
    # Calculate Time in Bed (account for cases crossing midnight)
    time_in_bed = case_when(
      time_get_up >= time_go_to_bed ~ time_get_up - time_go_to_bed,
      time_get_up < time_go_to_bed ~ (1440 - time_go_to_bed) + time_get_up,  # 1440 mins in a day
      TRUE ~ NA_real_
    ),
    
    # Calculate Sleep Efficiency (%)
    sleep_efficiency = (actual_sleep_duration / time_in_bed) * 100,
    
    # Categorize into PSQI Sleep Efficiency Score
    compo_q4 = case_when(
      sleep_efficiency >= 85 ~ 0,
      sleep_efficiency >= 75 & sleep_efficiency < 85 ~ 1,
      sleep_efficiency >= 65 & sleep_efficiency < 75 ~ 2,
      sleep_efficiency < 65 ~ 3,
      TRUE ~ NA_real_
    )
  )

#check the number of participants with sleep efficiency score > 100 and infinity
sum(psqi$sleep_efficiency== "Inf", na.rm=TRUE)
#remove these participants
psqi$sleep_efficiency[psqi$sleep_efficiency == "Inf"] <- NA
psqi$sleep_efficiency[psqi$sleep_efficiency> 100] <- NA
psqi<- psqi %>%
  filter(!is.na(psqi$sleep_efficiency))

#sleep_disturbances
# List of disturbance items
disturbance_items <- c(
  "trouble_wake_mid_night", "trouble_bathroom", "trouble_breathing",
  "trouble_cough", "trouble_cold", "trouble_hot",
  "trouble_bad_dreams", "trouble_pain", "trouble_other"
)

# Recode each item and calculate component score
psqi <- psqi %>%
  mutate(across(all_of(disturbance_items), ~ case_when(
    . == "Not during the past month" ~ 0,
    . == "Less than once a week" ~ 1,
    . == "Once or twice a week" ~ 2,
    . == "Three or more times a week" ~ 3,
    TRUE ~ NA_real_
  ), .names = "score_{.col}")) %>%
  
  # Sum the individual item scores
  rowwise() %>%
  mutate(
    sleep_disturbance_raw = sum(c_across(starts_with("score_")), na.rm = TRUE),
    
    # Final Component 5 Score (0-3)
    compo_q5 = case_when(
      sleep_disturbance_raw == 0 ~ 0,
      sleep_disturbance_raw >= 1 & sleep_disturbance_raw <= 9 ~ 1,
      sleep_disturbance_raw > 9 & sleep_disturbance_raw <= 18 ~ 2,
      sleep_disturbance_raw >18 ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

rm(disturbance_items)
#sleep_medication
psqi <- psqi %>%
  mutate(
    freq_otc_meds_score = case_when(
      freq_otc_meds == "Not during the past month" ~ 0,
      freq_otc_meds == "Less than once a week" ~ 1,
      freq_otc_meds == "Once or twice a week" ~ 2,
      freq_otc_meds == "Three or more times a week" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Score for prescribed medications
    freq_prescibed_meds_score = case_when(
      freq_prescibed_meds == "Not during the past month" ~ 0,
      freq_prescibed_meds == "Less than once a week" ~ 1,
      freq_prescibed_meds == "Once or twice a week" ~ 2,
      freq_prescibed_meds == "Three or more times a week" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Sum the two scores
    med_raw = freq_otc_meds_score + freq_prescibed_meds_score,
    
    # Final Sleep medication Component Score
    compo_q6 = case_when(
      med_raw == 0 ~ 0,
      med_raw %in% c(1, 2) ~ 1,
      med_raw %in% c(3, 4) ~ 2,
      med_raw %in% c(5, 6) ~ 3,
      TRUE ~ NA_real_
    )
  )

#daytime_dysfunction
psqi <- psqi %>%
  mutate(
    freq_awake_score = case_when(
      freq_awake == "Not during the past month" ~ 0,
      freq_awake == "Less than once a week" ~ 1,
      freq_awake == "Once or twice a week" ~ 2,
      freq_awake == "Three or more times a week" ~ 3,
      TRUE ~ NA_real_
    ),
    
    problems_enthusiasm_score = case_when(
      problems_enthusiasm == "No problem at all" ~ 0,
      problems_enthusiasm == "Only a slight problem" ~ 1,
      problems_enthusiasm == "Somewhat of a problem" ~ 2,
      problems_enthusiasm == " A very big problem" ~ 3,
      TRUE ~ NA_real_
    ),
    
    daytime_dysfunction_raw = freq_awake_score + problems_enthusiasm_score,
    
    compo_q7 = case_when(
      daytime_dysfunction_raw == 0 ~ 0,
      daytime_dysfunction_raw %in% c(1,2) ~ 1,
      daytime_dysfunction_raw %in% c(3,4) ~ 2,
      daytime_dysfunction_raw %in% c(5,6) ~ 3,
      TRUE ~ NA_real_
    )
  )

# Calculate global PSQI score
psqi <- psqi %>%
  mutate(
    psqi_global_score = compo_q1 + compo_q2 + compo_q3 + compo_q4 +
      compo_q5 + compo_q6 + compo_q7
  )

psqi <- psqi %>%
  mutate(
    psqi_score_category = case_when(
      psqi_global_score <= 5  ~ "Good sleep quality" ,
      psqi_global_score > 5  ~ "Poor sleep quality",
      TRUE ~ NA_character_
    ))

#acutal sleep duration hours to category data
psqi$sleep_duration_category <- ifelse(
  psqi$sleep_duration_hours < 7, "Short", 
  ifelse(
    psqi$sleep_duration_hours >= 7 & psqi$sleep_duration_hours <= 9, "Optimal", 
    ifelse(
      psqi$sleep_duration_hours > 9, "Long", 
      NA
    )
  )
)

psqi_final<- psqi %>% select(1,50,51,26,52)

###sleep condition indicator, insomnia
#change your directory
insomnia<-read.csv("/Users/aungphyo/Downloads/insomnia_participant.csv",header = TRUE)

#time taken to sleep
lvl.2563<- c(-3,0,1,2,3,4)
lbl.2563<- c("Prefer not to answer", "0-15 mins", "16-30 mins", "31-45 mins", "46-60 mins", "61 mins or more")
insomnia$X30474.0.0<- ordered(insomnia$X30474.0.0, levels=lvl.2563, labels=lbl.2563)
#table(insomnia$X30474.0.0, useNA = "always")
insomnia$X30474.0.0[insomnia$X30474.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30474.0.0))
insomnia <- insomnia %>%
  rename(time_taken_sleep = X30474.0.0)

#time for awake
insomnia$X30536.0.0<- ordered(insomnia$X30536.0.0, levels=lvl.2563, labels=lbl.2563)
#table(insomnia$X30536.0.0, useNA = "always")
insomnia$X30536.0.0[insomnia$X30536.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30536.0.0))
insomnia <- insomnia %>%
  rename(time_for_awake = X30536.0.0)

#problematic sleep nights
lvl.2565<- c(-3,0,1,2,3,4)
lbl.2565<- c("Prefer not to answer", "0-1", "2", "3", "4", "5-7")
insomnia$X30538.0.0<- ordered(insomnia$X30538.0.0, levels=lvl.2565, labels=lbl.2565)
#table(insomnia$X30538.0.0, useNA = "always")
insomnia$X30538.0.0[insomnia$X30538.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30538.0.0))
insomnia <- insomnia %>%
  rename(problematic_sleep_night = X30538.0.0)

#sleep quality
lvl.2566<- c(-3,0,1,2,3,4)
lbl.2566<- c("Prefer not to answer", "Very good", "Good", "Average", "Poor", "Very poor")
insomnia$X30539.0.0<- ordered(insomnia$X30539.0.0, levels=lvl.2566, labels=lbl.2566)
#table(insomnia$X30539.0.0, useNA = "always")
insomnia$X30539.0.0[insomnia$X30539.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30539.0.0))
insomnia <- insomnia %>%
  rename(sleep_quality = X30539.0.0)

#mood poor sleep
lvl.2567<- c(-3,0,1,2,3,4)
lbl.2567<- c("Prefer not to answer", "Not at all", "A little", "Somewhat", "Much", "Very much")
insomnia$X30540.0.0<- ordered(insomnia$X30540.0.0, levels=lvl.2567, labels=lbl.2567)
#table(insomnia$X30540.0.0, useNA = "always")
insomnia$X30540.0.0[insomnia$X30540.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30540.0.0))
insomnia <- insomnia %>%
  rename(mood_poor_sleep = X30540.0.0)

#concentration poor sleep
insomnia$X30541.0.0<- ordered(insomnia$X30541.0.0, levels=lvl.2567, labels=lbl.2567)
#table(insomnia$X30541.0.0, useNA = "always")
insomnia$X30541.0.0[insomnia$X30541.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30541.0.0))
insomnia <- insomnia %>%
  rename(concentration_poor_sleep = X30541.0.0)

#general poor sleep
insomnia$X30542.0.0<- ordered(insomnia$X30542.0.0, levels=lvl.2567, labels=lbl.2567)
#table(insomnia$X30542.0.0, useNA = "always")
insomnia$X30542.0.0[insomnia$X30542.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30542.0.0))
insomnia <- insomnia %>%
  rename(general_poor_sleep = X30542.0.0)

#duration of problematic sleep
lvl.2568<- c(-3,0,1,2,3,4,5,6,7)
lbl.2568<- c("Prefer not to answer", "I do not have a problem", "Less than 1 month", "1-2 months", "3-6 months", "7-12 months", "1-5 years", "6-10 years", "More than 10 years")
insomnia$X30543.0.0<- ordered(insomnia$X30543.0.0, levels=lvl.2568, labels=lbl.2568)
#table(insomnia$X30543.0.0, useNA = "always")
insomnia$X30543.0.0[insomnia$X30543.0.0 == "Prefer not to answer"] <- NA
insomnia<- insomnia %>%
  filter(!is.na(insomnia$X30543.0.0))
insomnia <- insomnia %>%
  rename(duration_problematic_sleep = X30543.0.0)

#sleep condition indicator SCI scoring
insomnia <- insomnia %>%
  mutate(
    sci_q1 = case_when(
      time_taken_sleep == "0-15 mins" ~ 4,
      time_taken_sleep == "16-30 mins" ~ 3,
      time_taken_sleep == "31-45 mins" ~ 2,
      time_taken_sleep == "46-60 mins" ~ 1,
      time_taken_sleep == "61 mins or more" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q2 = case_when(
      time_for_awake == "0-15 mins" ~ 4,
      time_for_awake == "16-30 mins" ~ 3,
      time_for_awake == "31-45 mins" ~ 2,
      time_for_awake == "46-60 mins" ~ 1,
      time_for_awake == "61 mins or more" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q3 = case_when(
      problematic_sleep_night == "0-1" ~ 4,
      problematic_sleep_night == "2" ~ 3,
      problematic_sleep_night == "3" ~ 2,
      problematic_sleep_night == "4" ~ 1,
      problematic_sleep_night == "5-7" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q4 = case_when(
      sleep_quality == "Very good" ~ 4,
      sleep_quality == "Good" ~ 3,
      sleep_quality == "Average" ~ 2,
      sleep_quality == "Poor" ~ 1,
      sleep_quality == "Very poor" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q5 = case_when(
      mood_poor_sleep == "Not at all" ~ 4,
      mood_poor_sleep == "A little" ~ 3,
      mood_poor_sleep == "Somewhat" ~ 2,
      mood_poor_sleep == "Much" ~ 1,
      mood_poor_sleep == "Very much" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q6 = case_when(
      concentration_poor_sleep == "Not at all" ~ 4,
      concentration_poor_sleep == "A little" ~ 3,
      concentration_poor_sleep == "Somewhat" ~ 2,
      concentration_poor_sleep == "Much" ~ 1,
      concentration_poor_sleep == "Very much" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q7 = case_when(
      general_poor_sleep == "Not at all" ~ 4,
      general_poor_sleep == "A little" ~ 3,
      general_poor_sleep == "Somewhat" ~ 2,
      general_poor_sleep == "Much" ~ 1,
      general_poor_sleep == "Very much" ~ 0,
      TRUE ~ NA_real_
    ),
    sci_q8 = case_when(
      duration_problematic_sleep %in% c("I do not have a problem", "Less than 1 month") ~ 4,
      duration_problematic_sleep == "1-2 months" ~ 3,
      duration_problematic_sleep == "3-6 months" ~ 2,
      duration_problematic_sleep == "7-12 months" ~ 1,
      duration_problematic_sleep %in% c("1-5 years", "6-10 years", "More than 10 years") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Calculate SCI total score
insomnia <- insomnia %>%
  mutate(
    SCI_total = rowSums(select(., sci_q1:sci_q8), na.rm = FALSE),  # strict sum, NA if any missing
    SCI_category = case_when(
      SCI_total >16 ~ "Lower likelihood of insomnia",
      SCI_total <= 16 ~ "Probable insomnia",
      TRUE ~ NA_character_
    )
  )
#rename
insomnia <- insomnia %>%
  rename(SCI_score = SCI_total)

#create final dataset for insomnia data
insomnia_final<- insomnia %>% select(1,18,19)

##asd
table(rmeq$X130971.0.0, useNA = "always")
rmeq <- rmeq %>%
  mutate(X130971.0.0 = case_when(
    is.na(X130971.0.0) ~ "no",
    X130971.0.0 %in% c(30, 31, 40) ~ "yes",
    TRUE ~ as.character(X130971.0.0)
  ))
#rename
rmeq<- rmeq %>%
  rename(asd_dx= X130971.0.0)

asd_final<- rmeq %>% select (1,7)

#combine dataset 
sleep1<- inner_join(rmeq_final, psqi_final, by= "eid")
sleep2<- inner_join(sleep1, insomnia_final, by= "eid")
sleep3<- inner_join(sleep2, asd_final, by= "eid")
rm(sleep1, sleep2)
#combine dataset
sleep4<- inner_join(rmeq, psqi, by="eid")
sleep5<- inner_join(sleep4, insomnia, by="eid")
rm(sleep4)

### Save variables of sleep and asd final as R datafile ######
save(sleep3, file="sleep_variables_asd_final.Rda")
##create csv file
write.csv(sleep3, file="sleep_variables_asd_final.csv", row.names= FALSE)

save(sleep5, file="sleep_variables_asd.Rda")
write.csv(sleep5, file="sleep_variables_asd.csv", row.names=FALSE)

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))
