#19 June 2025
#Aung
#To get social situation variables and scoring
setwd("~/Downloads")
#required package
library(dplyr)
#required dataset (social_situation_participant.csv)
#change your directory
social<-read.csv("/Users/aungphyo/Downloads/social_situation_participant.csv",header = TRUE)

##Resilience to stress
#leveling and labelling
lvl.1931 <- c(0,1,2,3,4)
lbl.1931 <- c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
#hard time through stress
social$X29176.0.0<- ordered(social$X29176.0.0, levels=lvl.1931, labels=lbl.1931)
#table(social$X29176.0.0, useNA = "always")
social<- social %>%
  filter(!is.na(social$X29176.0.0))

#difficult times with little trouble
social$X29179.0.0<- ordered(social$X29179.0.0, levels=lvl.1931, labels=lbl.1931)
#table(social$X29179.0.0, useNA = "always")
social<- social %>%
  filter(!is.na(social$X29179.0.0))

#hard to snap back when bad
social$X29178.0.0<- ordered(social$X29178.0.0, levels=lvl.1931, labels=lbl.1931)
##quick recovery from stress
social$X29177.0.0<- ordered(social$X29177.0.0, levels=lvl.1931, labels=lbl.1931)
#bounce back quickly after hard
social$X29175.0.0<- ordered(social$X29175.0.0, levels=lvl.1931, labels=lbl.1931)
#long time to get over setbacks
social$X29180.0.0<- ordered(social$X29180.0.0, levels=lvl.1931, labels=lbl.1931)

#column name
social <- social %>%
  rename(hard_time_through_stress = X29176.0.0) #disagree is good

social <- social %>%
  rename(difficult_times_with_little_trouble = X29179.0.0) #disagree is good

social <- social %>%
  rename(hard_to_snap_back_when_bad = X29178.0.0) #disagree is good

social <- social %>%
  rename(quick_recovery_from_stress = X29177.0.0) #agree is good

social <- social %>%
  rename(bounce_back_quickly_after_hard = X29175.0.0) #agree is good

social <- social %>%
  rename(long_time_to_get_over_setbacks = X29180.0.0) #disagree is good

#scoring of resilience to stress
social <- social %>%
  mutate(
    hard_time_through_stress_num = recode(hard_time_through_stress,
                                          "Strongly agree" = 1,
                                          "Agree" = 2,
                                          "Neutral" = 3,
                                          "Disagree" = 4,
                                          "Strongly disagree" = 5),
    difficult_times_with_little_trouble_num = recode(difficult_times_with_little_trouble,
                                                     "Strongly agree" = 1,
                                                     "Agree" = 2,
                                                     "Neutral" = 3,
                                                     "Disagree" = 4,
                                                     "Strongly disagree" = 5),
    hard_to_snap_back_when_bad_num = recode(hard_to_snap_back_when_bad,
                                            "Strongly agree" = 1,
                                            "Agree" = 2,
                                            "Neutral" = 3,
                                            "Disagree" = 4,
                                            "Strongly disagree" = 5),
    quick_recovery_from_stress_num = recode(quick_recovery_from_stress,
                                            "Strongly agree" = 5,
                                            "Agree" = 4,
                                            "Neutral" = 3,
                                            "Disagree" = 2,
                                            "Strongly disagree" = 1),
    bounce_back_quickly_after_hard_num = recode(bounce_back_quickly_after_hard,
                                                "Strongly agree" = 5,
                                                "Agree" = 4,
                                                "Neutral" = 3,
                                                "Disagree" = 2,
                                                "Strongly disagree" = 1),
    long_time_to_get_over_setbacks_num = recode(long_time_to_get_over_setbacks,
                                                "Strongly agree" = 1,
                                                "Agree" = 2,
                                                "Neutral" = 3,
                                                "Disagree" = 4,
                                                "Strongly disagree" = 5)
  )

#to calculate total score resilience
social <- social %>%
  mutate(total_score_resilience = hard_time_through_stress_num +
           difficult_times_with_little_trouble_num +
           hard_to_snap_back_when_bad_num +
           quick_recovery_from_stress_num +
           bounce_back_quickly_after_hard_num +
           long_time_to_get_over_setbacks_num
  )
#resilience to stress categorical data
social <- social %>%
  mutate(
    resilience_to_stress_cat = case_when(
      total_score_resilience >= 6 & total_score_resilience <= 10 ~ "Very low",
      total_score_resilience >= 11 & total_score_resilience <= 15 ~ "Low",
      total_score_resilience >= 16 & total_score_resilience <= 20 ~ "Mid",
      total_score_resilience >= 21 & total_score_resilience <= 25 ~ "High",
      total_score_resilience >= 26 & total_score_resilience <= 30 ~ "Very high"
    )
  )
#table(social$resilience_to_stress_cat, useNA = "always")


#Loneliness
lvl.1930<- c(-3,-1,0,1,2)
lbl.1930<- c("Prefer not to answer", "Do not know", "Hardly ever", "Some of the time", "Often")
#feeling in tune
social$X29171.0.0<- ordered(social$X29171.0.0, levels=lvl.1930, labels=lbl.1930)
table(social$X29171.0.0, useNA = "always")
social$X29171.0.0[social$X29171.0.0 %in% c("Prefer not to answer", "Do not know")] <- NA
social<- social %>%
  filter(!is.na(social$X29171.0.0))

#feeling isolated
social$X29174.0.0<- ordered(social$X29174.0.0, levels=lvl.1930, labels=lbl.1930)
social$X29174.0.0[social$X29174.0.0 %in% c("Prefer not to answer", "Do not know")] <- NA
social<- social %>%
  filter(!is.na(social$X29174.0.0))

#feeling left out
social$X29173.0.0<- ordered(social$X29173.0.0, levels=lvl.1930, labels=lbl.1930)
social$X29173.0.0[social$X29173.0.0 %in% c("Prefer not to answer", "Do not know")] <- NA
social<- social %>%
  filter(!is.na(social$X29173.0.0))

#feeling lack companionship
social$X29172.0.0<- ordered(social$X29172.0.0, levels=lvl.1930, labels=lbl.1930)
social$X29172.0.0[social$X29172.0.0 %in% c("Prefer not to answer", "Do not know")] <- NA
social<- social %>%
  filter(!is.na(social$X29172.0.0))

#change column name
social <- social %>%
  rename(feeling_in_tune = X29171.0.0) #often is good

social <- social %>%
  rename(feeling_isolated = X29174.0.0) #hardly ever is good

social <- social %>%
  rename(feeling_left_out = X29173.0.0) #hardly ever is good

social <- social %>%
  rename(feeling_lack_companionship = X29172.0.0) #hardly ever is good

#scoring of loneliness
social <- social %>%
  mutate(
    feeling_in_tune_num = recode(feeling_in_tune,
                                 "Hardly ever" = 1,
                                 "Some of the time" = 2,
                                 "Often" = 3),
    feeling_isolated_num = recode(feeling_isolated,
                                  "Hardly ever" = 3,
                                  "Some of the time" = 2,
                                  "Often" = 1),
    feeling_left_out_num = recode(feeling_left_out,
                                  "Hardly ever" = 3,
                                  "Some of the time" = 2,
                                  "Often" = 1),
    feeling_lack_companionship_num = recode(feeling_lack_companionship,
                                            "Hardly ever" = 3,
                                            "Some of the time" = 2,
                                            "Often" = 1)
  )

social <- social %>%
  mutate(total_score_loneliness = feeling_in_tune_num +
           feeling_isolated_num +
           feeling_left_out_num +
           feeling_lack_companionship_num
  )
#change to categorical data type
social <- social %>%
  mutate(loneliness_cat = case_when(
    total_score_loneliness >= 4 & total_score_loneliness <= 6 ~ "High",
    total_score_loneliness >= 7 & total_score_loneliness <= 9 ~ "Mid",
    total_score_loneliness >= 10 & total_score_loneliness <= 12 ~ "Low"
  ))
table(social$loneliness_cat, useNA = "always")

#social connectivity
lvl.1927<- c(-3,0,1,2,3,4,5)
lbl.1927<- c("Prefer not to answer", "Never or almost never", "Once every few months", "About once a month", "About once a week", "2-4 times a week","Daily or almost daily")
#confiding in someone close
social$X29166.0.0<- ordered(social$X29166.0.0, levels=lvl.1927, labels=lbl.1927)
social$X29166.0.0[social$X29166.0.0 == "Prefer not to answer"] <- NA
social<- social %>%
  filter(!is.na(social$X29166.0.0))

#seeing friends in person
social$X29163.0.0<- ordered(social$X29163.0.0, levels=lvl.1927, labels=lbl.1927)
social$X29163.0.0[social$X29163.0.0 == "Prefer not to answer"] <- NA
social<- social %>%
  filter(!is.na(social$X29163.0.0))

#seeing friends video call
social$X29164.0.0<- ordered(social$X29164.0.0, levels=lvl.1927, labels=lbl.1927)
social$X29164.0.0[social$X29164.0.0 == "Prefer not to answer"] <- NA
social<- social %>%
  filter(!is.na(social$X29164.0.0))

#seeing friends voice call
social$X29165.0.0<- ordered(social$X29165.0.0, levels=lvl.1927, labels=lbl.1927)
social$X29165.0.0[social$X29165.0.0 == "Prefer not to answer"] <- NA
social<- social %>%
  filter(!is.na(social$X29165.0.0))

#change column name
social <- social %>%
  rename(confiding_in_someone_close = X29166.0.0) #daily is good

social <- social %>%
  rename(seeing_friends_in_person = X29163.0.0)

social <- social %>%
  rename(seeing_friends_video_call = X29164.0.0)

social <- social %>%
  rename(seeing_friends_voice_call = X29165.0.0)

#scoring of social connectivity
social <- social %>%
  mutate(
    confiding_in_someone_close_num = recode(confiding_in_someone_close,
                                            "Never or almost never" = 1,
                                            "Once every few months" = 2,
                                            "About once a month" = 3,
                                            "About once a week" = 4,
                                            "2-4 times a week" = 5,
                                            "Daily or almost daily" = 6),
    
    seeing_friends_in_person_num = recode(seeing_friends_in_person,
                                          "Never or almost never" = 1,
                                          "Once every few months" = 2,
                                          "About once a month" = 3,
                                          "About once a week" = 4,
                                          "2-4 times a week" = 5,
                                          "Daily or almost daily" = 6),
    
    seeing_friends_video_call_num = recode(seeing_friends_video_call,
                                           "Never or almost never" = 1,
                                           "Once every few months" = 2,
                                           "About once a month" = 3,
                                           "About once a week" = 4,
                                           "2-4 times a week" = 5,
                                           "Daily or almost daily" = 6),
    
    seeing_friends_voice_call_num = recode(seeing_friends_voice_call,
                                           "Never or almost never" = 1,
                                           "Once every few months" = 2,
                                           "About once a month" = 3,
                                           "About once a week" = 4,
                                           "2-4 times a week" = 5,
                                           "Daily or almost daily" = 6),
    
    total_score_social_connection = confiding_in_someone_close_num +
      seeing_friends_in_person_num +
      seeing_friends_video_call_num +
      seeing_friends_voice_call_num
  )
#change to social connectivity categorical data
social <- social %>%
  mutate(
    social_connectivity_cat = case_when(
      total_score_social_connection >= 4  & total_score_social_connection <= 7.33  ~ "Very low",
      total_score_social_connection >= 7.34  & total_score_social_connection <= 10.66 ~ "Low",
      total_score_social_connection >= 10.67 & total_score_social_connection <= 13.99 ~ "Moderate",
      total_score_social_connection >= 14 & total_score_social_connection <= 17.32 ~ "Good",
      total_score_social_connection >= 17.33 & total_score_social_connection <= 20.65 ~ "High",
      total_score_social_connection >= 20.66 & total_score_social_connection <= 24 ~ "Very High"
    )
  )

#table(social$social_connectivity_cat, useNA = "always")

#other variables
#social activites
social$X29167.0.0[social$X29167.0.0 %in% c(-1, -3)] <- NA
table(social$X29167.0.0, useNA = "always")
social<- social %>%
  filter(!is.na(social$X29167.0.0))

social$social_activity_participation <- ifelse(
  social$X29167.0.0 == "0",
  "Inactive",
  "Active"
)
table(social$social_activity_participation, useNA = "always")

#current situation/ job
table(social$X29169.0.0, useNA = "always")
social$X29169.0.0[social$X29169.0.0 %in% c(-1, -3)] <- NA
social<- social %>%
  filter(!is.na(social$X29169.0.0))

# Create a new column with employment category
social$employment_status <- ifelse(
  grepl("\\b1\\b|\\b2\\b", social$X29169.0.0),  # matches whole 1 or 2
  "Paid Employment",
  "Unpaid/Other"
)
#paid empolyment for employed or self employed
#unpaid/ other are the remaining (retired,look after home, carer for closed family
#childcare for family, unable to work due to sickness, unemployed, volunteer, student)
table(social$employment_status, useNA = "always")

#number of people in the house
lvl.1926<- c(-3,0,1,2,3)
lbl.1926<- c("Prefer not to answer", "Only me", "Me and one other", "More than two but less than five","Five or more")
social$X29162.0.0<- ordered(social$X29162.0.0, levels=lvl.1926, labels=lbl.1926)
table(social$X29162.0.0, useNA = "always")
social$X29162.0.0[social$X29162.0.0 == "Prefer not to answer"] <- NA
social<- social %>%
  filter(!is.na(social$X29162.0.0))
social <- social %>%
  rename(number_of_living_people = X29162.0.0)

#create final dataset
social_before_lca<- social %>% select(1,27,28,33,34,39,40,42,41,18)

### Save variables of social situation as R datafile ######
save(social_before_lca, file="social_variables_before_lca.Rda")
##create csv file
write.csv(social_before_lca, file="social_variables_before_lca.csv", row.names= FALSE)

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))
