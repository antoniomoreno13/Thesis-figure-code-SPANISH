
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("MASS")
library("sfsmisc")
library("QuantPsyc")
library("forcats")
library(lmtest)
library(extrafont)
library(colortools)
library(reshape2)
library(scales)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))


#### Data Handling ####
# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224,222, 277, 242,291, 236, 237:240, 270, 11)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:27] <- c("Gender", "Age","Marital status", "Social class subjective", "Type of community", "Sizeofcommunity" ,"Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Ocupation_last_job", "Bills", "Country")
eu2017["survey"] <- "2017" # TIME VARIABLE


eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The upper middle class of society"] <- "The higher class of society"
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The lower middle class of society"] <- "The working class of society"


eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")


#### Sitting time ####

eu2017$Sit[eu2017$Sit == "DK"] <- NA
eu2017$Sit_med [eu2017$Sit == "1 hour or less"] <- 30
eu2017$Sit_med [eu2017$Sit == "1 hour to 1h30min" | eu2017$Sit == "1 hour to 1 hour and 30 minutes"] <- 75
eu2017$Sit_med [eu2017$Sit == "1h31min to 2h30min" | eu2017$Sit == "1 hour 31 minutes to 2 hours 30 minutes"] <- 120
eu2017$Sit_med [eu2017$Sit == "2h31min to 3h30min" | eu2017$Sit == "2 hours 31 minutes to 3 hours 30 minutes"] <- 180
eu2017$Sit_med [eu2017$Sit == "3h31min to 4h30min" | eu2017$Sit == "3 hours 31 minutes to 4 hours 30 minutes"] <- 240
eu2017$Sit_med [eu2017$Sit == "4h31min to 5h30min" | eu2017$Sit == "4 hours 31 minutes to 5 hours 30 minutes"] <- 300
eu2017$Sit_med [eu2017$Sit == "5h31min to 6h30min" | eu2017$Sit == "5 hours 31 minutes to 6 hours 30 minutes"] <- 360
eu2017$Sit_med [eu2017$Sit == "6h31min to 7h30min" | eu2017$Sit == "6 hours 31 minutes to 7 hours 30 minutes"] <- 420
eu2017$Sit_med [eu2017$Sit == "7h31min to 8h30min" | eu2017$Sit == "7 hours 31 minutes to 8 hours 30 minutes"] <- 480
eu2017$Sit_med [eu2017$Sit == "More than 8h30min" | eu2017$Sit == "More than 8 hours and 30 minutes"] <- 540


#### IPAQ, VPA, MPA, MVPA calculation in METs (and Time per Week) ####

eu2017$Vig_Days <- as.numeric(eu2017$Vig_Days) # TO CONTINIOUS
eu2017$Mod_Days <- as.numeric(eu2017$Mod_Days)
eu2017$Walk_Days <- as.numeric(eu2017$Walk_Days)

eu2017$Vig_Days[which(is.na(eu2017$Vig_Days))] <- 0 # RECODING AND  MISSING VALUES RECOVERY
eu2017$Vig_Days[eu2017$Vig_Days == 8] <- 0
eu2017$Vig_Days[eu2017$Vig_Days == 9] <- NA
eu2017$Vig_Time[which(is.na(eu2017$Vig_Time))] <- "Never do any vigorous physical activity "
eu2017$Vig_Time[eu2017$Vig_Time == "DK"] <- NA

eu2017$Mod_Days[which(is.na(eu2017$Mod_Days))] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 8] <- 0
eu2017$Mod_Days[eu2017$Mod_Days == 9] <- NA
eu2017$Mod_Time[which(is.na(eu2017$Mod_Time))] <- "Never do any moderate physical activity "
eu2017$Mod_Time[eu2017$Mod_Time == "DK"] <- NA

eu2017$Walk_Days[eu2017$Walk_Days == 8] <- 0
eu2017$Walk_Days[eu2017$Walk_Days == 9] <- NA
eu2017$Walk_Time[eu2017$Walk_Time == "DK"] <- NA

# INTERVAL MEDIAN VALUES BY PA TIME      
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "Never do any vigorous physical activity " | eu2017$Vig_Time  == "Never do vigorous physical activities"] <- 0 
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "30 minutes or less" ] <- 15   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Vig_Time_med [ eu2017$Vig_Time  == "More than 120 minutes" ] <- 135   

eu2017$Mod_Time_med [ eu2017$Mod_Time  == "Never do any moderate physical activity " | eu2017$Mod_Time  == "Never do moderate physical activities"] <- 0 
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "30 minutes or less" ] <- 15   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "31 to 60 minutes" ] <- 45   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "61 to 90 minutes" ] <- 75   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "91 to 120 minutes" ] <- 105   
eu2017$Mod_Time_med [ eu2017$Mod_Time  == "More than 120 minutes" ] <- 135  

eu2017$Walk_Time_med [eu2017$Walk_Time == "Never walk for 10 minutes at a time"] <- 0
eu2017$Walk_Time_med [eu2017$Walk_Time == "30 minutes or less"] <- 15
eu2017$Walk_Time_med [eu2017$Walk_Time == "31 to 60 minutes"] <- 45
eu2017$Walk_Time_med [eu2017$Walk_Time == "61 to 90 minutes"] <- 75
eu2017$Walk_Time_med [eu2017$Walk_Time == "91 to 120 minutes"] <- 105
eu2017$Walk_Time_med [eu2017$Walk_Time == "More than 120 minutes"] <- 135


# TIME PER WEEK
eu2017$VPA_tot_time <- eu2017$Vig_Days * eu2017$Vig_Time_med
eu2017$MPA_tot_time <- eu2017$Mod_Days * eu2017$Mod_Time_med
eu2017$Walk_tot_time <- eu2017$Walk_Days * eu2017$Walk_Time_med

eu2017$Mod_plus_walk_tottime <- eu2017$MPA_tot_time + eu2017$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

# METS PER WEEK
eu2017$VPA_met <- eu2017$VPA_tot_time * 8
eu2017$MPA_met <- eu2017$MPA_tot_time * 4
eu2017$Walk_met <- eu2017$Walk_tot_time * 3.3 

# COMPUTE MVPA METs 

eu2017$MVPA_met <- eu2017$VPA_met + eu2017$MPA_met + eu2017$Walk_met
eu2017$MVPA_met_outwalk <- eu2017$VPA_met + eu2017$MPA_met

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA)

eu2017$WHO_prev [eu2017$VPA_tot_time >= 75 | eu2017$Mod_plus_walk_tottime >= 150 | (eu2017$Mod_plus_walk_tottime + (2 * eu2017$VPA_tot_time) >= 150)] <- "Active"
eu2017$WHO_prev [which(is.na(eu2017$WHO_prev))] <-"Inactive"
eu2017$WHO_prev [which(is.na(eu2017$Mod_plus_walk_tottime) & (is.na(eu2017$VPA_tot_time)))] <- NA


#### Covariates Reordering, Assigning missing values and dropping useless levels ####
eu2017$Bills[eu2017$Bills == "Refusal (SPONT.)"] <- NA; eu2017$Bills<- fct_drop(eu2017$Bills, only = "Refusal (SPONT.)")

eu2017$`Type of community`[eu2017$`Type of community` == "DK"] <- NA; eu2017$`Type of community`[eu2017$`Type of community` == "Small or middle sized town"] <- "Small/middle town";
eu2017$`Type of community`<- fct_drop(eu2017$`Type of community`, only = c("DK", "Small or middle sized town"))

eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Small/middle town"] <- "Urban"; eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Large town"] <- "Urban"; 
eu2017$Typeofcommunity2cat[eu2017$`Type of community` == "Rural area or village"] <- "Rural"

eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Towns and suburbs/ small urban area"] <- "Urban"; eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Cities/ large urban area"] <- "Urban"; 
eu2017$Sizeofcommunity2cat[eu2017$Sizeofcommunity == "Rural area"] <- "Rural"



eu2017$`Social class subjective`[eu2017$`Social class subjective` == "DK"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "None (SPONT.)"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "Other (SPONT.)"] <- NA
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "Refusal (SPONT.)"] <- NA
eu2017$`Social class subjective`<- fct_drop(eu2017$`Social class subjective`, only = c("The upper middle class of society", "The lower middle class of society", "Refusal (SPONT.)", "Other (SPONT.)","None (SPONT.)" , "DK"))


eu2017$Age_3clusters [ eu2017$Age >= 18 & eu2017$Age < 45 ] <- "18-44"  
eu2017$Age_3clusters [ eu2017$Age >= 45 & eu2017$Age < 70 ] <- "45-69"  
eu2017$Age_3clusters [ eu2017$Age >= 70] <- "70+"  

eu2017$Age_3clusters2 [ eu2017$Age >= 18 & eu2017$Age < 35 ] <- "18-34"  
eu2017$Age_3clusters2 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters2 [ eu2017$Age >= 65] <- "65+"  

eu2017$Age_3clusters3 [ eu2017$Age < 35 ] <- "15-34"  
eu2017$Age_3clusters3 [ eu2017$Age >= 35 & eu2017$Age < 65 ] <- "35-64"  
eu2017$Age_3clusters3 [ eu2017$Age >= 65] <- "65+"

eu2017$Age_9clusters [ eu2017$Age == 18] <- "18"  
eu2017$Age_9clusters [ eu2017$Age == 19] <- "19"  
eu2017$Age_9clusters [ eu2017$Age >= 20 & eu2017$Age < 30 ] <- "20-29"  
eu2017$Age_9clusters [ eu2017$Age >= 30 & eu2017$Age < 40 ] <- "30-39"  
eu2017$Age_9clusters [ eu2017$Age >= 40 & eu2017$Age < 50 ] <- "40-49"  
eu2017$Age_9clusters [ eu2017$Age >= 50 & eu2017$Age < 60 ] <- "50-59"  
eu2017$Age_9clusters [ eu2017$Age >= 60 & eu2017$Age < 70 ] <- "60-69"  
eu2017$Age_9clusters [ eu2017$Age >= 70 & eu2017$Age < 80 ] <- "70-79"  
eu2017$Age_9clusters [ eu2017$Age >= 80 ] <- "80+"  


eu2017$Country_rec <- eu2017$Country; eu2017$Country_rec <- fct_expand(eu2017$Country_rec, c("DE Germany", "UK United Kingdom")) 
eu2017$Country_rec[eu2017$Country_rec == "DE-W - Germany - West" | eu2017$Country_rec =="DE-E Germany East" ] <- "DE Germany"
eu2017$Country_rec[eu2017$Country_rec ==  "GB-NIR Northern Ireland" | eu2017$Country_rec =="GB-GBN - Great Britain"] <- "UK United Kingdom"
eu2017$Country_rec<- fct_drop(eu2017$Country_rec, only = c("DE-W - Germany - West", "DE-E Germany East", "GB-NIR Northern Ireland", "GB-GBN - Great Britain", 
       "LI - Liechtenstein (NOT INCLUDED)", "IS - Iceland (NOT INCLUDED)", "CH - Switzerland (NOT INCLUDED)", "NO - Norway (NOT INCLUDED)",
       "RS - Serbia (NOT INCLUDED)", "ME - Montenegro (NOT INCLUDED)", "MK - Makedonia/FYROM (NOT INCLUDED)", "CY-TCC - Cyprus TCC (NOT INCLUDED)",
       "TR - Turkey (NOT INCLUDED)", "-"))



eu2017$`Marital status`[eu2017$`Marital status` == "Other (SPONT.)"] <- NA
eu2017$`Marital status`[eu2017$`Marital status` == "Refusal (SPONT.)"] <- NA
eu2017$`Marital status`<- fct_drop(eu2017$`Marital status`, only = c("Refusal (SPONT.)", "Other (SPONT.)"))


eu2017$Education[eu2017$Education == "DK"] <- NA
eu2017$Education[eu2017$Education == "Refusal_duplicated_7"] <- NA
eu2017$Education[eu2017$Education == "Refusal"] <- NA
eu2017$Education[eu2017$Education == "No full-time education"] <- NA
eu2017$Education <- fct_drop(eu2017$Education, only = c("DK", "Refusal_duplicated_7", "Refusal", "No full-time education"))

eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age <= 15] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age > 15 & eu2017$Age < 20] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "Still Studying" & eu2017$Age >= 20] <- "20 years and older"
eu2017$Education_3cat[eu2017$Education == "Up to 15 years"] <- "Up to 15 years"
eu2017$Education_3cat[eu2017$Education == "16-19"] <- "16-19"
eu2017$Education_3cat[eu2017$Education == "20 years and older"] <- "20 years and older"

eu2017$Education_3cat_recod[eu2017$Education_3cat == "Up to 15 years"] <- "Primary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "16-19"] <- "Secondary"
eu2017$Education_3cat_recod[eu2017$Education_3cat == "20 years and older"] <- "University"



eu2017$Ocupation_5cat[eu2017$Ocupation == "Employed position, service job" | eu2017$Ocupation == "Supervisor" | eu2017$Ocupation == "Skilled manual worker" | eu2017$Ocupation == "Unskilled manual worker, etc." ] <- "Manual Workers"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Farmer" | eu2017$Ocupation == "Fisherman" | eu2017$Ocupation == "Professional (lawyer, etc.)" | eu2017$Ocupation == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation == "Business proprietors, etc." | eu2017$Ocupation == "Employed position, at desk" | eu2017$Ocupation == "Employed position, travelling" |  eu2017$Ocupation == "General management, etc."  | eu2017$Ocupation == "Middle management, etc."] <- "Self-employed or White collars workers"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Unemployed, temporarily not working" | eu2017$Ocupation == "Responsible for ordinary shopping, etc."] <- "Economically Inactive"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Retired, unable to work"] <- "Retirees"
eu2017$Ocupation_5cat[eu2017$Ocupation == "Student"] <- "Students"




eu2017$Ocupation_3cat[eu2017$Ocupation == "Supervisor" | eu2017$Ocupation == "Skilled manual worker" | eu2017$Ocupation == "Unskilled manual worker, etc." | eu2017$Ocupation == "Employed position, service job"] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Farmer" | eu2017$Ocupation == "Fisherman" |  eu2017$Ocupation == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation == "Business proprietors, etc." | eu2017$Ocupation == "Employed position, at desk" | eu2017$Ocupation == "Employed position, travelling" | eu2017$Ocupation == "Professional (lawyer, etc.)"] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Employed professional (employed doctor, etc.)"  |  eu2017$Ocupation == "General management, etc."  | eu2017$Ocupation == "Middle management, etc." ] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" | eu2017$Ocupation == "Responsible for ordinary shopping, etc."] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work"] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student"] <- NA

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Supervisor" | eu2017$Ocupation_last_job == "Unskilled manual worker, etc." | eu2017$Ocupation_last_job == "Skilled manual worker" | eu2017$Ocupation_last_job == "Employed position, service job")] <- "V-VII"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Farmer " | eu2017$Ocupation_last_job == "Fisherman" | eu2017$Ocupation_last_job == "Owner of a shop, craftsmen, etc." | eu2017$Ocupation_last_job == "Business proprietors, etc." | eu2017$Ocupation_last_job == "Employed position, at desk" | eu2017$Ocupation_last_job == "Employed position, travelling" |  eu2017$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | eu2017$Ocupation_last_job == "General management, etc." | eu2017$Ocupation_last_job == "Middle management, etc.")] <- "I-II"

eu2017$Ocupation_3cat[eu2017$Ocupation == "Responsible for ordinary shopping, etc." & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Unemployed, temporarily not working" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Retired, unable to work" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA
eu2017$Ocupation_3cat[eu2017$Ocupation == "Student" & (eu2017$Ocupation_last_job == "Never did any paid work")] <- NA



eu2017[42:52] <- lapply(eu2017[42:52], factor) 

eu2017$MVPA_tot_time_outwalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time
eu2017$MVPA_tot_time_pluswalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time + eu2017$Walk_tot_time

eu2017$Age_6clusters [ eu2017$Age >= 15 & eu2017$Age < 25 ] <- "15-24"  
eu2017$Age_6clusters [ eu2017$Age >= 25 & eu2017$Age < 35 ] <- "25-34"  
eu2017$Age_6clusters [ eu2017$Age >= 35 & eu2017$Age < 45 ] <- "35-44"  
eu2017$Age_6clusters [ eu2017$Age >= 45 & eu2017$Age < 55 ] <- "45-54"  
eu2017$Age_6clusters [ eu2017$Age >= 55 & eu2017$Age < 65 ] <- "55-64"  
eu2017$Age_6clusters [ eu2017$Age >= 65 ] <- "65+"  
eu2017$Age_6clusters <- as.factor(eu2017$Age_6clusters)

eu2017$Sit_med_7 <- eu2017$Sit_med * 7


#### Filters ####
# REMOVE ILLOGICAL VALUES AND RECOVERING SOME NAs LOGICAL VALUES

eu2017 <- eu2017[-c(which(eu2017$Vig_Days == 0 & eu2017$Vig_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days == 0 & eu2017$Mod_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days == 0 & eu2017$Walk_Time_med > 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Vig_Days > 0 & eu2017$Vig_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Mod_Days > 0 & eu2017$Mod_Time_med == 0)), ] 
eu2017 <- eu2017[-c(which(eu2017$Walk_Days > 0 & eu2017$Walk_Time_med == 0)), ] 

# REMOVING NA METS AND SIT TIME VALUES TO GET FINAL DATABASE SAMPLE SIZE
eu2017 <- eu2017[which(eu2017$MVPA_met >= 0), ]
eu2017 <- eu2017[which(eu2017$Sit_med >= 0), ]


#### Individual level - Statistical Analyses ####
library(ggplot2)
library(cowplot)
library(reshape2)
library(ggpubr)
library(GGally)
library(FactoMineR)
library(factoextra)

#### Descriptive statistics ####
euMen <- eu2017[which(eu2017$Gender == "Man"),]
euWomen <- eu2017[which(eu2017$Gender == "Woman"),]


# Simple
table(eu2017$Gender);prop.table(table(eu2017$Gender))
table(eu2017$Sizeofcommunity);prop.table(table(eu2017$Sizeofcommunity))
table(eu2017$WHO_prev);prop.table(table(eu2017$WHO_prev))
table(eu2017$Country_rec);prop.table(table(eu2017$Country_rec))
table(eu2017$Age_6clusters);prop.table(table(eu2017$Age_6clusters))
table(eu2017$Ocupation_3cat);prop.table(table(eu2017$Ocupation_3cat))

summary(eu2017$Age); sd(eu2017$Age)
summary(eu2017$Vig_Days); sd(eu2017$Vig_Days)
summary(eu2017$Mod_Days); sd(eu2017$Mod_Days)
summary(eu2017$Walk_Days); sd(eu2017$Walk_Days)
mean(eu2017$VPA_tot_time); sd(eu2017$VPA_tot_time)
mean(eu2017$MPA_tot_time); sd(eu2017$MPA_tot_time)
mean(eu2017$Walk_tot_time); sd(eu2017$Walk_tot_time)
mean(eu2017$Sit_med); sd(eu2017$Sit_med)
mean(eu2017$MVPA_tot_time_outwalk); sd(eu2017$MVPA_tot_time_outwalk)
mean(eu2017$MVPA_tot_time_pluswalk); sd(eu2017$MVPA_tot_time_pluswalk)


table(eu2017$`Marital status`);prop.table(table(eu2017$`Marital status`))
table(eu2017$`Social class subjective`);prop.table(table(eu2017$`Social class subjective`))
table(eu2017$Education_3cat_recod);prop.table(table(eu2017$Education_3cat_recod))
table(eu2017$Ocupation_rec2);prop.table(table(eu2017$Ocupation_rec2))
table(eu2017$Bills);prop.table(table(eu2017$Bills))


## Physical activity descrip and sit across gender with covariates
# ALL - BOTH GENDERS
eu2017 %>% group_by(Ocupation_3cat) %>%
  summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
            Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
            Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
            Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
            Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
            Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75), 
            VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
            "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
            MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
            "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
            Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
            "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
            Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),  Sit_med = median(Sit_med), 
            "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
            MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
            "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
            MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75)) -> ocup_desc_ALL


eu2017 %>% group_by(Sizeofcommunity) %>%
    summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
              Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
              Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
              Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
              Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
              Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75), 
              VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
              "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
              MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
              "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
              Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
              "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),  Sit_med = median(Sit_med), 
              "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
              "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
              MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
              "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75)) -> Size_com_desc_ALL

eu2017 %>% group_by(Country_rec) %>%
    summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
              Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
              Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
              Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
              Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
              Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75),
              VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
              "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
              MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
              "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
              Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
              "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
              Sit_mean = mean(Sit_med), Sit_sd = sd(Sit_med),  Sit_med = median(Sit_med),
              "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
              "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
              MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
              "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75),
              Age_mean = mean(Age)) -> Country_desc_ALL

eu2017 %>% group_by(WHO_prev) %>%
    summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
              Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
              Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
              Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
              Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
              Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75),
              VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
              "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
              MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
              "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
              Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
              "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),  Sit_med = median(Sit_med), 
              "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
              "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
              MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
              "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75)) -> WHO_prev_desc_ALL


eu2017 %>% group_by(Gender) %>%
    summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
              Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
              Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
              Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
              Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
              Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75),
              VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
              "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
              MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
              "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
              Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
              "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
              Sit_mean = mean(Sit_med), Sit_sd = sd(Sit_med), Sit_med = median(Sit_med), 
              "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
              "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
              MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
              "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75)) -> Gender_desc_ALL

eu2017 %>% group_by(Age_6clusters) %>%
  summarise(Days_vig_mean = mean(Vig_Days), Days_vig_median = median(Vig_Days), Days_vig_sd = sd(Vig_Days),
            Days_vig_25th = quantile(Vig_Days, 0.25), Days_vig_75th = quantile(Vig_Days, 0.75), 
            Days_mod_mean = mean(Mod_Days), Days_mod_median = median(Mod_Days), Days_mod_sd = sd(Mod_Days),
            Days_mod_25th = quantile(Mod_Days, 0.25), Days_mod_75th = quantile(Mod_Days, 0.75), 
            Days_walk_mean = mean(Walk_Days), Days_walk_median = median(Walk_Days), Days_walk_sd = sd(Walk_Days),
            Days_walk_25th = quantile(Walk_Days, 0.25), Days_walk_75th = quantile(Walk_Days, 0.75),
            VPA_mean = mean(VPA_tot_time), VPA_med = median(VPA_tot_time), VPA_sd = sd(VPA_tot_time), 
            "VPA_25th" = quantile(VPA_tot_time, 0.25), "VPA_75th" = quantile(VPA_tot_time, 0.75), 
            MPA_mean = mean(MPA_tot_time), MPA_med = median(MPA_tot_time), MPA_sd = sd(MPA_tot_time), 
            "MPA_25th" = quantile(MPA_tot_time, 0.25), "MPA_75th" = quantile(MPA_tot_time, 0.75), 
            Walk_mean = mean(Walk_tot_time), Walk_med = median(Walk_tot_time), Walk_sd = sd(Walk_tot_time), 
            "Walk_25th" = quantile(Walk_tot_time, 0.25), "Walk_75th" = quantile(Walk_tot_time, 0.75), 
            Sit_mean = mean(Sit_med), Sit_sd = sd(Sit_med), Sit_med = median(Sit_med), 
            "Sit_25th" = quantile(Sit_med, 0.25), "Sit_75th" = quantile(Sit_med, 0.75),
            MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_med = median(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk), 
            "MVPA_time_25th" = quantile(MVPA_tot_time_outwalk, 0.25), "MVPA_time_75th" = quantile(MVPA_tot_time_outwalk, 0.75),
            MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_med = median(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk), 
            "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75)) -> Age6_desc_ALL


#### Simple correlations and multiple linear regressions ####
# ALL
cor.test(eu2017$VPA_tot_time, eu2017$MPA_tot_time, exact = T, conf.level = .95)
cor.test(eu2017$VPA_tot_time, eu2017$Walk_tot_time, exact = T, conf.level = .95)
cor.test(eu2017$VPA_tot_time, eu2017$Sit_med, exact = T, conf.level = .95)
cor.test(eu2017$VPA_tot_time, eu2017$Age, exact = T, conf.level = .95)

cor.test(eu2017$MPA_tot_time, eu2017$Walk_tot_time, exact = T, conf.level = .95)
cor.test(eu2017$MPA_tot_time, eu2017$Sit_med, exact = T, conf.level = .95)
cor.test(eu2017$MPA_tot_time, eu2017$Age, exact = T, conf.level = .95)

cor.test(eu2017$Walk_tot_time, eu2017$Sit_med, exact = T, conf.level = .95)
cor.test(eu2017$Walk_tot_time, eu2017$Age, exact = T, conf.level = .95)

cor.test(eu2017$Sit_med, eu2017$Age, exact = T, conf.level = .95)


cor.test(eu2017$MVPA_tot_time_outwalk, eu2017$Walk_tot_time, exact = T, conf.level = .95)
cor.test(eu2017$MVPA_tot_time_outwalk, eu2017$Sit_med, exact = T, conf.level = .95)
cor.test(eu2017$MVPA_tot_time_outwalk, eu2017$Age, exact = T, conf.level = .95)

cor.test(eu2017$MVPA_tot_time_pluswalk, eu2017$Walk_tot_time, exact = T, conf.level = .95)
cor.test(eu2017$MVPA_tot_time_pluswalk, eu2017$Sit_med, exact = T, conf.level = .95)
cor.test(eu2017$MVPA_tot_time_pluswalk, eu2017$Age, exact = T, conf.level = .95)

# MEN
cor.test(euMen$VPA_tot_time, euMen$MPA_tot_time, exact = T, conf.level = .95)
cor.test(euMen$VPA_tot_time, euMen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euMen$VPA_tot_time, euMen$Sit_med, exact = T, conf.level = .95)
cor.test(euMen$VPA_tot_time, euMen$Age, exact = T, conf.level = .95)

cor.test(euMen$MPA_tot_time, euMen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euMen$MPA_tot_time, euMen$Sit_med, exact = T, conf.level = .95)
cor.test(euMen$MPA_tot_time, euMen$Age, exact = T, conf.level = .95)

cor.test(euMen$Walk_tot_time, euMen$Sit_med, exact = T, conf.level = .95)
cor.test(euMen$Walk_tot_time, euMen$Age, exact = T, conf.level = .95)

cor.test(euMen$Sit_med, euMen$Age, exact = T, conf.level = .95)


cor.test(euMen$MVPA_tot_time_outwalk, euMen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euMen$MVPA_tot_time_outwalk, euMen$Sit_med, exact = T, conf.level = .95)
cor.test(euMen$MVPA_tot_time_outwalk, euMen$Age, exact = T, conf.level = .95)

cor.test(euMen$MVPA_tot_time_pluswalk, euMen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euMen$MVPA_tot_time_pluswalk, euMen$Sit_med, exact = T, conf.level = .95)
cor.test(euMen$MVPA_tot_time_pluswalk, euMen$Age, exact = T, conf.level = .95)


# WOMEN
cor.test(euWomen$VPA_tot_time, euWomen$MPA_tot_time, exact = T, conf.level = .95)
cor.test(euWomen$VPA_tot_time, euWomen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euWomen$VPA_tot_time, euWomen$Sit_med, exact = T, conf.level = .95)
cor.test(euWomen$VPA_tot_time, euWomen$Age, exact = T, conf.level = .95)

cor.test(euWomen$MPA_tot_time, euWomen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euWomen$MPA_tot_time, euWomen$Sit_med, exact = T, conf.level = .95)
cor.test(euWomen$MPA_tot_time, euWomen$Age, exact = T, conf.level = .95)

cor.test(euWomen$Walk_tot_time, euWomen$Sit_med, exact = T, conf.level = .95)
cor.test(euWomen$Walk_tot_time, euWomen$Age, exact = T, conf.level = .95)

cor.test(euWomen$Sit_med, euWomen$Age, exact = T, conf.level = .95)


cor.test(euWomen$MVPA_tot_time_outwalk, euWomen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euWomen$MVPA_tot_time_outwalk, euWomen$Sit_med, exact = T, conf.level = .95)
cor.test(euWomen$MVPA_tot_time_outwalk, euWomen$Age, exact = T, conf.level = .95)

cor.test(euWomen$MVPA_tot_time_pluswalk, euWomen$Walk_tot_time, exact = T, conf.level = .95)
cor.test(euWomen$MVPA_tot_time_pluswalk, euWomen$Sit_med, exact = T, conf.level = .95)
cor.test(euWomen$MVPA_tot_time_pluswalk, euWomen$Age, exact = T, conf.level = .95)


# GGPairs
ggpairs(eu2017, columns = c(32:34, 52, 53, 28, 17), lower = list(continuous = "density"))
ggpairs(eu2017, columns = c(32:34,52, 53, 28, 17), ggplot2::aes(colour=Gender), lower = list(continuous = "density")) 
ggpairs(eu2017, columns = c(32:34,52,53, 28, 17), ggplot2::aes(colour=Age_3clusters3), lower = list(continuous = "density")) 
ggpairs(eu2017, columns = c(32:34,52,53, 28, 17), ggplot2::aes(colour=Age_6clusters), lower = list(continuous = "density"),upper = list(continuous = wrap("cor", size = 1.5))) 
ggpairs(eu2017, columns = c(32:34,52,53, 28, 17), ggplot2::aes(colour=Sizeofcommunity), lower = list(continuous = "density")) 

ggpairs(eu2017[which(eu2017$Sizeofcommunity == "Rural area"),], columns = c(32:34,52,53, 28, 17), lower = list(continuous = "density")) 
ggpairs(eu2017[which(eu2017$Sizeofcommunity == "Towns and suburbs/ small urban area"),], columns = c(32:34,52,53, 28, 17), lower = list(continuous = "density")) 
ggpairs(eu2017[which(eu2017$Sizeofcommunity == "Cities/ large urban area"),], columns = c(32:34,52,53, 28, 17), lower = list(continuous = "density")) 


ggsave("Ind_level_cor_.pdf", width = 12, height = 5, units = "in", dpi = 600)


#### Compositional data analysis ####
library(compositions)


compdata <- eu2017[c(52,34,55, 2)]
compdata$MVPA_tot_time_outwalk[which(compdata$MVPA_tot_time_outwalk == 0)] <- 0.01 # pseudozeros
compdata$Walk_tot_time[which(compdata$Walk_tot_time == 0)] <- 0.01

compdata$sum_3_comp <- compdata$MVPA_tot_time_outwalk + compdata$Walk_tot_time + compdata$Sit_med_7
compdata$MVPA_tot_time_percent <- (compdata$MVPA_tot_time_outwalk * 100) / compdata$sum_3_comp
compdata$Walk_tot_time_percent <- (compdata$Walk_tot_time * 100) / compdata$sum_3_comp
compdata$Sit_med_7_percent <- (compdata$Sit_med_7 * 100) / compdata$sum_3_comp

compdata$ln_MVPA <- log(compdata$MVPA_tot_time_outwalk) # ln
compdata$ln_WALK <- log(compdata$Walk_tot_time)
compdata$ln_sit_7 <- log(compdata$Sit_med_7)


geometricmean(compdata$MVPA_tot_time_outwalk) # geometric mean
geometricmean(compdata$Walk_tot_time)
geometricmean(compdata$Sit_med_7)
sum <- geometricmean(compdata$MVPA_tot_time_outwalk) + geometricmean(compdata$Walk_tot_time) + geometricmean(compdata$Sit_med_7)
geometricmean(compdata$MVPA_tot_time_outwalk) / sum # geometric mean as %
geometricmean(compdata$Walk_tot_time)  / sum
geometricmean(compdata$Sit_med_7)  / sum

# Descriptive differences
compdata2 <- eu2017
compdata2$MVPA_tot_time_outwalk[which(compdata2$MVPA_tot_time_outwalk == 0)] <- 0.01 # pseudozeros
compdata2$Walk_tot_time[which(compdata2$Walk_tot_time == 0)] <- 0.01

compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "15-24"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "25-34"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "35-44"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "45-54"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "55-64"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "65+"), "MVPA_tot_time_outwalk"] %>% geometricmean()

compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "15-24"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "25-34"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "35-44"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "45-54"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "55-64"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "65+"), "Walk_tot_time"] %>% geometricmean()

compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "15-24"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "25-34"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "35-44"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "45-54"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "55-64"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Woman" & compdata2$Age_6clusters == "65+"), "Sit_med_7"] %>% geometricmean()



compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "15-24"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "25-34"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "35-44"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "45-54"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "55-64"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "65+"), "MVPA_tot_time_outwalk"] %>% geometricmean()

compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "15-24"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "25-34"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "35-44"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "45-54"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "55-64"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "65+"), "Walk_tot_time"] %>% geometricmean()

compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "15-24"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "25-34"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "35-44"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "45-54"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "55-64"), "Sit_med_7"] %>% geometricmean()
compdata2[which(compdata2$Gender == "Man" & compdata2$Age_6clusters == "65+"), "Sit_med_7"] %>% geometricmean()


compdata2[which(compdata2$Sizeofcommunity == "Rural area"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Rural area"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Rural area"), "Sit_med_7"] %>% geometricmean()

compdata2[which(compdata2$Sizeofcommunity == "Towns and suburbs/ small urban area"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Towns and suburbs/ small urban area"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Towns and suburbs/ small urban area"), "Sit_med_7"] %>% geometricmean()

compdata2[which(compdata2$Sizeofcommunity == "Cities/ large urban area"), "MVPA_tot_time_outwalk"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Cities/ large urban area"), "Walk_tot_time"] %>% geometricmean()
compdata2[which(compdata2$Sizeofcommunity == "Cities/ large urban area"), "Sit_med_7"] %>% geometricmean()

Geomeans_gen_age <- read.csv2("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/Geometric_means_Gender_Age.csv")
Geomeans_resident <- read.csv2("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/Geometric_means_Resident.csv")

library(ggtern)
library(questionr)
Geomeans_gen_age <- rename.variable(Geomeans_gen_age, "Walk", "Caminar")
Geomeans_gen_age <- rename.variable(Geomeans_gen_age, "Sit_7", "Sedentarismo")
Geomeans_resident <- rename.variable(Geomeans_resident, "Walk", "Caminar")
Geomeans_resident <- rename.variable(Geomeans_resident, "Sit_7", "Sedentarismo")


Geomeans_gen_age$Gender <- factor( Geomeans_gen_age$Gender , levels = levels( Geomeans_gen_age$Gender  )[ c( 2,1) ] )

ggtern(Geomeans_gen_age, aes(x=MVPA, y=Sedentarismo, z=Caminar)) +
  theme_rgbw() + theme(legend.title =  element_text(face = "bold", family = "Times"),
                       axis.text = element_text(family = "Times"),
                       axis.title = element_text(family = "Times"),
                       legend.text = element_text(family = "Times"),
                       strip.text = element_text(family = "Times", face = "bold", size = 14),
                       strip.background = element_blank())+ geom_point(aes(fill=Edad), size=2.5, shape=21, color="black") + facet_grid(. ~ Gender) + theme_zoom_T(0.2) + theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"))

ggsave("Ternary_Age_Gender.pdf", width = 14, height = 6, units = "in", dpi = 600)


Geomeans_resident$Lugar.de.residencia <- factor( Geomeans_resident$Lugar.de.residencia , levels = levels( Geomeans_resident$Lugar.de.residencia  )[ c(2,3,1) ] )

my_colors <- RColorBrewer::brewer.pal(3, "Accent")[1]
my_colors2 <- RColorBrewer::brewer.pal(3, "Accent")[2]
my_colors3 <- RColorBrewer::brewer.pal(3, "Accent")[3]
my_colors5 <- RColorBrewer::brewer.pal(3, "Accent")[1:3]
my_colors6 <- list(my_colors, my_colors3, my_colors2)


Geomeans_resident$Lugar.de.residencia <- factor(Geomeans_resident$Lugar.de.residencia, levels = c("Áreas rurales", "Pequeñas áreas urbanas", "Grandes áreas urbanas"))

ggtern(Geomeans_resident, aes(x=MVPA, y=Sedentarismo, z=Caminar)) + labs(fill = "Lugar de residencia")+ 
  theme_rgbw() + theme(legend.title =  element_text(face = "bold", family = "Times"),
                       axis.text = element_text(family = "Times"),
                       axis.title = element_text(family = "Times"),
                       legend.text = element_text(family = "Times"))+
  geom_point(aes(fill=Lugar.de.residencia), size=2.5, shape=21, color="black") + scale_fill_manual(values = my_colors6)+
  theme_zoom_T(0.15)
  
ggsave("Ternary_Geomeans_Resident_place.tiff", width = 8, height = 7, units = "in", dpi = 600)



variation.acomp(compdata[9:11]) # variation matrix with ln - upper triangule

compdata$ln_WALK_MVPA <- log((compdata$Walk_tot_time / compdata$MVPA_tot_time_outwalk)) # pairwise ln ratios
compdata$ln_Sit_MVPA <- log((compdata$Sit_med_7 / compdata$MVPA_tot_time_outwalk)) # pairwise ln ratios
compdata$ln_Sit_Walk <- log((compdata$Sit_med_7 / compdata$Walk_tot_time)) # pairwise ln ratios

mean(compdata$ln_WALK_MVPA) # Mean of the pairwise logratio
mean(compdata$ln_Sit_MVPA)
mean(compdata$ln_Sit_Walk)

# isometric logratio coordinates
robCompositions::pivotCoord(x = compdata[1:3], pivotvar = 1) -> a
robCompositions::pivotCoord(x = compdata[1:3], pivotvar = 2) -> b
robCompositions::pivotCoord(x = compdata[1:3], pivotvar = 3) -> c

compdata$MVPA_pivot <- a$`MVPA_tot_time_outwalk_Wa-Si`
compdata$Walk_pivot <- b$`Walk_tot_time_MV-Si`
compdata$Sit_pivot <- c$`Sit_med_7_MV-Wa`

eu2017$MVPA_pivot <- compdata$MVPA_pivot
eu2017$Walk_pivot <- compdata$Walk_pivot
eu2017$Sit_pivot <- compdata$Sit_pivot

# Compositional regressions
library("nlme")

a <- lme(MVPA_pivot ~ Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
            method = "ML"); summary(a); intervals(a, which = "fixed")

b <- lme(Walk_pivot ~ Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
            method = "ML"); summary(b); intervals(b, which = "fixed")

c <- lme(Sit_pivot ~ Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
            method = "ML"); summary(c); intervals(c, which = "fixed")



ggtern::ggtern(compdata, aes(x=MVPA_tot_time_outwalk, y=Sit_med_7, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none")



#### Ternary plots ####
library(ggtern)
ggtern(eu2017, aes(x=MVPA_tot_time_outwalk, y=Sit_med, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none")


ggtern(eu2017, aes(x=MVPA_tot_time_outwalk, y=Sit_med, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none") +
  facet_grid(Gender ~ Age_6clusters)




ggtern(eu2017, aes(x=MVPA_tot_time_outwalk, y=Sit_med, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none") +
  facet_grid(. ~ Gender)


ggtern(eu2017, aes(x=MVPA_tot_time_outwalk, y=Sit_med, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none") +
  facet_grid(. ~ Age_6clusters)


ggtern(eu2017, aes(x=MVPA_tot_time_outwalk, y=Sit_med, z=Walk_tot_time)) +
  stat_density_tern(geom='polygon', aes(fill  = ..level.., alpha = ..level..), bdl = 0.001) +
  theme_rgbw() + 
  scale_fill_gradient(low = "blue",high = "red")  + guides(fill = "none", alpha = "none") +
  facet_grid(. ~ Sizeofcommunity)


ggsave("Ternary_Age_Gender.pdf", width = 14, height = 6, units = "in", dpi = 600)


# LINEAR REGRESSIONS
summary(lm(VPA_tot_time ~ MPA_tot_time + Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, data = eu2017))
summary(lm(MPA_tot_time ~ VPA_tot_time + Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, data = eu2017))
summary(lm(Walk_tot_time ~ VPA_tot_time + MPA_tot_time + Sit_med + Age + Gender + Sizeofcommunity, data = eu2017))
summary(lm(Sit_med ~ VPA_tot_time + MPA_tot_time + Walk_tot_time + Age + Gender + Sizeofcommunity, data = eu2017))
summary(lm(MVPA_tot_time_outwalk ~ Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, data = eu2017))

summary(lm(MVPA_tot_time_pluswalk ~ Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, data = eu2017))


#### Hierarchical linear regressions ####
library("nlme")

summary(lme(VPA_tot_time ~ MPA_tot_time + Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))

summary(lme(MPA_tot_time ~ VPA_tot_time + Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))

summary(lme(Walk_tot_time ~ VPA_tot_time + MPA_tot_time + Sit_med + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))

summary(lme(Sit_med ~ VPA_tot_time + MPA_tot_time + Walk_tot_time + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))

summary(lme(MVPA_tot_time_outwalk ~ Walk_tot_time + Sit_med + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))

summary(lme(MVPA_tot_time_pluswalk ~ Sit_med + Age + Gender + Sizeofcommunity, random =  ~Age|Country_rec, data = eu2017,
          method = "ML"))



#### Analisis multivariante MIXTO ####

res.pca <- PCA(eu2017[c(32:34)], graph = FALSE, scale.unit = T)
fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_biplot(res.pca, geom.ind="point", col.var = "#2E9FDF", col.ind = "#696969")

res.pca2 <- PCA(eu2017[c(52, 34, 28)], graph = FALSE, scale.unit = T)
fviz_eig(res.pca2, addlabels = TRUE)
fviz_pca_biplot(res.pca2, geom.ind="point", col.var = "#2E9FDF", col.ind = "#696969")

res.pca3 <- PCA(eu2017[c(32:34, 28)], graph = FALSE, scale.unit = T)
fviz_eig(res.pca3, addlabels = TRUE)
fviz_pca_biplot(res.pca3, geom.ind="point", col.var = "#2E9FDF", col.ind = "#696969")

ggsave("PCA.pdf", width = 7.5, height = 5, units = "in", dpi = 600)



res.famd <- FAMD(eu2017[c(32:34, 28, 16,17, 21)], ncp = 5, graph = FALSE)
fviz_screeplot(res.famd)

var <- get_famd_var(res.famd)
fviz_famd_var(res.famd, repel = TRUE) # Plot of variables
fviz_contrib(res.famd, "var", axes = 1) # Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 2) # Contribution to the second dimension

quanti.var <- get_famd_var(res.famd, "quanti.var")
fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")

quali.var <- get_famd_var(res.famd, "quali.var")
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) 

ind <- get_famd_ind(res.famd)
fviz_famd_ind(res.famd, col.ind = "cos2", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), geom = "point")

fviz_mfa_ind(res.famd, habillage = "Gender", # color by groups 
    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
    addEllipses = TRUE, ellipse.type = "confidence", geom = "point") 

fviz_mfa_ind(res.famd, habillage = "Sizeofcommunity", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", geom = "point") 

ggsave("FAMD.pdf", width = 7.5, height = 5, units = "in", dpi = 600)

#### Country level - statistics ####
## Correlations
# ALL
cor.test(Country_desc_ALL$VPA, Country_desc_ALL$MPA, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$VPA, Country_desc_ALL$Walking, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$VPA, Country_desc_ALL$Sitting, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$VPA, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)

cor.test(Country_desc_ALL$MPA, Country_desc_ALL$Walking, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MPA, Country_desc_ALL$Sitting, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MPA, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)

cor.test(Country_desc_ALL$Walking, Country_desc_ALL$Sitting, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$Walking, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)

cor.test(Country_desc_ALL$Sitting, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)


cor.test(Country_desc_ALL$MVPA, Country_desc_ALL$Walking, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MVPA, Country_desc_ALL$Sitting, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MVPA, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)

cor.test(Country_desc_ALL$MVPA_pluswalk_time_mean, Country_desc_ALL$Walk_mean, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MVPA_pluswalk_time_mean, Country_desc_ALL$Sitting, exact = T, conf.level = .95)
cor.test(Country_desc_ALL$MVPA_pluswalk_time_mean, Country_desc_ALL$Age_mean, exact = T, conf.level = .95)


# GGPairs
ggpairs(Country_desc_ALL, columns = c(2,7, 12,17, 22, 27, 32, 37, 42, 47))
ggpairs(Country_desc_MEN, columns = c(2,7, 12,17, 22, 27, 32, 37, 42, 47)) 
ggpairs(Country_desc_WOMEN, columns = c(2,7, 12,17, 22, 27, 32, 37, 42, 47)) 

plot(Country_desc_ALL$MVPA_time_mean, Country_desc_ALL$Sit_mean)
plot(Country_desc_ALL$MVPA_pluswalk_time_mean, Country_desc_ALL$Sit_mean)

ggsave("Country_level_cor_ALL.pdf", width = 12, height = 5, units = "in", dpi = 600)


## LINEAR REGRESSIONS
m1 <-lm(VPA ~ MPA + Walking + Sitting + Age_mean, data = Country_desc_ALL); lm.beta(m1)
shapiro.test(rstudent(m1));plot(rstudent(m1)) # normalidad de residuos
bptest(m1) # homocedasticidad 
dwtest(VPA ~ MPA + Walking + Sitting + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m1)


m10 <- rlm(Country_desc_ALL$VPA ~ Country_desc_ALL$MPA + Country_desc_ALL$Walking + 
             Country_desc_ALL$Sitting + Country_desc_ALL$Age_mean, method = "M") ; summary(m10)
f.robftest(m10, var = "Country_desc_ALL$MPA")
f.robftest(m10, var = "Country_desc_ALL$Walking")
f.robftest(m10, var = "Country_desc_ALL$Sitting")
f.robftest(m10, var = "Country_desc_ALL$Age_mean")
f.robftest(m10)
anova(m10)
lm.beta(m10)

0.6461 + (1.96 * 0.0826)
0.6461 - (1.96 * 0.0826)


m2 <-lm(MPA ~ VPA + Walking + Sitting + Age_mean, data = Country_desc_ALL); summary(m2); lm.beta(m2); confint(m2)
shapiro.test(rstudent(m2));plot(rstudent(m2)) # normalidad de residuos
bptest(m2) # homocedasticidad 
dwtest(MPA ~ VPA + Walking + Sitting + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m2)


m3 <-lm(Walking ~ VPA + MPA + Sitting + Age_mean, data = Country_desc_ALL); summary(m3); lm.beta(m3); confint(m3)
shapiro.test(rstudent(m3));plot(rstudent(m3)) # normalidad de residuos
bptest(m3) # homocedasticidad 
dwtest(Walking ~ VPA + MPA + Sitting + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m3)

m30 <- rlm(Country_desc_ALL$Walking ~ Country_desc_ALL$VPA + Country_desc_ALL$MPA + 
             Country_desc_ALL$Sitting + Country_desc_ALL$Age_mean, method = "M") ; summary(m30)
f.robftest(m30, var = "Country_desc_ALL$VPA")
f.robftest(m30, var = "Country_desc_ALL$MPA")
f.robftest(m30, var = "Country_desc_ALL$Sitting")
f.robftest(m30, var = "Country_desc_ALL$Age_mean")
f.robftest(m30)
anova(m30)
lm.beta(m30)

0.5855 + (1.96 * 0.3001)
0.5855 - (1.96 * 0.3001)


m4 <-lm(Sitting ~ VPA + MPA + Walking + Age_mean, data = Country_desc_ALL); summary(m4); lm.beta(m4); confint(m4)
shapiro.test(rstudent(m4));plot(rstudent(m4)) # normalidad de residuos
bptest(m4) # homocedasticidad 
dwtest(Sitting ~ VPA + MPA + Walking + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m4)


m40 <- rlm(Country_desc_ALL$Sitting ~ Country_desc_ALL$VPA + Country_desc_ALL$MPA + 
             Country_desc_ALL$Walking + Country_desc_ALL$Age_mean, method = "M"); summary(m40)
f.robftest(m40, var = "Country_desc_ALL$VPA")
f.robftest(m40, var = "Country_desc_ALL$MPA")
f.robftest(m40, var = "Country_desc_ALL$Walking")
f.robftest(m40, var = "Country_desc_ALL$Age_mean")
f.robftest(m40)
anova(m40)
lm.beta(m40)

0.5376 + (1.96 * 0.2827)
0.5376 - (1.96 * 0.2827)

m5 <-lm(MVPA ~ Walking + Sitting + Age_mean, data = Country_desc_ALL); summary(m5); lm.beta(m5); confint(m5)
shapiro.test(rstudent(m5));plot(rstudent(m5)) # normalidad de residuos
bptest(m5) # homocedasticidad 
dwtest(MVPA ~ Walking + Sitting + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m5)


m6 <-lm(MVPA_pluswalk_time_mean ~ Sitting + Age_mean, data = Country_desc_ALL); summary(m6); lm.beta(m6); confint(m6)
shapiro.test(rstudent(m6));plot(rstudent(m6)) # normalidad de residuos
bptest(m6) # homocedasticidad 
dwtest(MVPA_pluswalk_time_mean ~ Sitting + Age_mean, data = Country_desc_ALL) # incorrelación de residuos
vif(m6)


#### EU-28 MAP ####
library("eurostat")


query <- search_eurostat(pattern = "fertility rate", type = "table", 
                         fixed = F)

ct <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
        "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", "LV", "MT", 
        "NL", "NO","PL", "PT", "RO", "SE", "SI", "SK", "UK")


dat <- get_eurostat(id="tps00199", time_format = "num", filters = list(geo = ct))
dat[1:2,]
dat <- label_eurostat(dat)
dat[1:3,]

dat <- get_eurostat(id="tps00199", filters = list(geo = ct))

ggplot(dat, aes(x = time, y = values, color = geo, label = geo)) + 
  geom_line(alpha = .5) + 
  geom_text(data = dat %>% group_by(geo) %>% 
              filter(time == max(time)), size = 2.6) + 
  theme(legend.position = "none") + labs(title = "Total Fertility rate, 2006-2017", 
                                         x = "Year", y = "%")

dat_2015 <- dat %>% filter(time == "2015-01-01")
ggplot(dat_2015, aes(x = reorder(geo, values), y = values)) + 
  geom_col(color = "white", fill = "grey80") + 
  theme(axis.text.x = element_text(size = 6)) + 
  labs(title = "Total fertility rate, 2015", y = "%", x = NULL)


mapdata <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2015) %>% mutate(cat = cut_to_classes(values, n=4, decimals=1))


ggplot(mapdata, aes(fill = cat)) + scale_fill_brewer(palette = "RdYlBu") + 
  geom_sf(color = alpha("white", 1/3), alpha = .6) + 
  xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                            subtitle = "Avg", fill = "%")


## Final DATA FRAME
mapdata <- mapdata[-c(2,3, 5, 6, 8, 9, 12)]

mapdata$Country_rec[mapdata$id == "AT"] <- "AT - Austria"
mapdata$Country_rec[mapdata$id == "BE"] <- "BE - Belgium"
mapdata$Country_rec[mapdata$id == "BG"] <- "BG - Bulgaria"
mapdata$Country_rec[mapdata$id == "CH"] <- "CH - Switzerland"
mapdata$Country_rec[mapdata$id == "CY"] <- "CY - Cyprus (Republic)"
mapdata$Country_rec[mapdata$id == "CZ"] <- "CZ - Czech Republic"
mapdata$Country_rec[mapdata$id == "DE"] <- "DE Germany"
mapdata$Country_rec[mapdata$id == "DK"] <- "DK - Denmark"
mapdata$Country_rec[mapdata$id == "EE"] <- "EE - Estonia"
mapdata$Country_rec[mapdata$id == "EL"] <- "GR - Greece"
mapdata$Country_rec[mapdata$id == "ES"] <- "ES -Spain"
mapdata$Country_rec[mapdata$id == "FI"] <- "FI - Finland"
mapdata$Country_rec[mapdata$id == "FR"] <- "FR - France"
mapdata$Country_rec[mapdata$id == "HR"] <- "HR - Croatia"
mapdata$Country_rec[mapdata$id == "HU"] <- "HU - Hungary"
mapdata$Country_rec[mapdata$id == "IE"] <- "IE - Ireland"
mapdata$Country_rec[mapdata$id == "IS"] <- "IS - Iceland"
mapdata$Country_rec[mapdata$id == "IT"] <- "IT - Italy"
mapdata$Country_rec[mapdata$id == "LI"] <- "LI - Liechtenstein"
mapdata$Country_rec[mapdata$id == "LT"] <- "LT - Lithuania"
mapdata$Country_rec[mapdata$id == "LU"] <- "LU - Luxembourg"
mapdata$Country_rec[mapdata$id == "LV"] <- "LV - Latvia"
mapdata$Country_rec[mapdata$id == "MT"] <- "MT - Malta"
mapdata$Country_rec[mapdata$id == "NL"] <- "NL - The Netherlands"
mapdata$Country_rec[mapdata$id == "NO"] <- "NO - Norway"
mapdata$Country_rec[mapdata$id == "PL"] <- "PL - Poland"
mapdata$Country_rec[mapdata$id == "PT"] <- "PT - Portugal"
mapdata$Country_rec[mapdata$id == "RO"] <- "RO - Romania"
mapdata$Country_rec[mapdata$id == "SE"] <- "SE - Sweden"
mapdata$Country_rec[mapdata$id == "SI"] <- "SI - Slovenia"
mapdata$Country_rec[mapdata$id == "SK"] <- "SK - Slovakia"
mapdata$Country_rec[mapdata$id == "UK"] <- "UK United Kingdom"

mapdata_ALL <- merge(mapdata, Country_desc_ALL, by =  "Country_rec")
mapdata_ALL <- mapdata_ALL[-c(3:5)]

library("RColorBrewer")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc1 <- scale_fill_gradientn(colours = myPalette(100), limits=c(75, 400))
sc2 <- scale_fill_gradientn(colours = myPalette(100), limits=c(240, 690))
sc3 <- scale_fill_gradientn(colours = myPalette(100), limits=c(150, 320))
sc4 <- scale_fill_gradientn(colours = myPalette(100), limits=c(240, 390))


# MVPA Outwalk

map_mvpa_outwalk <- ggplot(mapdata_ALL, aes(fill = MVPA_time_mean)) +
  geom_sf(color = alpha("black", 1/3)) + sc1 +
  xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "MVPA (min/wk)") + 
  theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none",
                          title = element_text(size = 10, face = "bold"))

map_mvpa_outwalk

# MVPA Pluswalk
map_mvpa_pluswalk <- ggplot(mapdata_ALL, aes(fill = MVPA_pluswalk_time_mean)) +
  geom_sf(color = alpha("black", 1/3)) + sc2 +
  xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Total MVPA (min/wk)") + 
  theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
                          title = element_text(size = 10, face = "bold"))

map_mvpa_pluswalk

# WALK
map_walk <- ggplot(mapdata_ALL, aes(fill = Walk_mean)) +
  geom_sf(color = alpha("black", 1/3)) + sc3 + 
  xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Walking (min/wk)") + 
  theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
                          title = element_text(size = 10, face = "bold"))

map_walk

# SIT 
map_sit <- ggplot(mapdata_ALL, aes(fill = Sit_mean)) + 
  geom_sf(color = alpha("black", 1/3)) + sc4 +
  xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Sitting time (min/day)") + 
  theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none",
                          title = element_text(size = 10, face = "bold"))

map_sit

legend1 <- cowplot::get_legend(map_mvpa_outwalk)
legend2 <- cowplot::get_legend(map_sit)


ggdraw(ylim = c(0,1),xlim = c(0,1)) +
  draw_plot(map_mvpa_outwalk, x = 0, y = 0, width = .5, height = 1) +
  draw_plot(map_sit, x = .5, y = 0, width = .5, height = 1)+
  draw_plot(legend1, x = 0.29, y = 0.022, width = 0.25) +
  draw_plot(legend2, x = 0.81, y = 0.022, width = 0.25) +

ggsave("map.pdf", width = 12, height = 6, units = "in", dpi = 600)

#### PCA ####
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "AT - Austria"] <- "AUT"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "BE - Belgium"] <- "BEL"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "BG - Bulgaria"] <- "BGR"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "CY - Cyprus (Republic)"] <- "CYP"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "CZ - Czech Republic"] <- "CZE"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "DE Germany"] <- "DEU"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "DK - Denmark"] <- "DNK"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "EE - Estonia"] <- "EST"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "GR - Greece"] <- "GRC"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "ES -Spain"] <- "ESP"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "FI - Finland"] <- "FIN"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "FR - France"] <- "FRA"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "HR - Croatia"] <- "HRV"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "HU - Hungary"] <- "HUN"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "IE - Ireland"] <- "IRL"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "IT - Italy"] <- "ITA"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "LT - Lithuania"] <- "LTU"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "LU - Luxembourg"] <- "LUX"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "LV - Latvia"] <- "LVA"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "MT - Malta"] <- "MLT"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "NL - The Netherlands"] <- "NLD"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "PL - Poland"] <- "POL"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "PT - Portugal"] <- "PRT"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "RO - Romania"] <- "ROU"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "SE - Sweden"] <- "SWE"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "SI - Slovenia"] <- "SVN"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "SK - Slovakia"] <- "SVK"
Country_desc_ALL$ID[Country_desc_ALL$Country_rec == "UK United Kingdom"] <- "GRB"

row.names(Country_desc_ALL) <- Country_desc_ALL$ID
Country_desc_ALL <- as.data.frame(Country_desc_ALL)


# VPA + MPA + WALK + SITTING
res.pca_country <- PCA(Country_desc_ALL[c(17, 22, 27, 32)], graph = F, scale.unit = T)
fviz_eig(res.pca_country, addlabels = TRUE)
fviz_pca_var(res.pca_country, col.var = "black")
fviz_pca_biplot(res.pca_country, geom.ind="point", col.var = "#2E9FDF", col.ind = "#696969")

PCA_PA_sit <- fviz_pca_biplot(res.pca_country,
                          col.ind = "black", fill.ind = "seagreen2", pointshape = 21, pointsize = 2.5,
                          col.var = "contrib", repel = T, alpha.ind = 0.75,
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          legend.title = list(color = "Contribution"))

ggpubr::ggpar(PCA_PA_sit,
              title = "Principal Component Analysis",
              subtitle = "Physical Activity Components and Sitting time per day",
              xlab = "PC1 (65.6%)", ylab = "PC2 (22.2%)",
              legend.position = "top")

# MVPA + WALK + SITTING
library(questionr)
Country_desc_ALL <- rename.variable(Country_desc_ALL, "VPA_mean", "VPA")
Country_desc_ALL <- rename.variable(Country_desc_ALL, "MPA_mean", "MPA")
Country_desc_ALL <- rename.variable(Country_desc_ALL, "MVPA_time_mean", "MVPA")
Country_desc_ALL <- rename.variable(Country_desc_ALL, "Walking", "Caminar")
Country_desc_ALL <- rename.variable(Country_desc_ALL, "Sitting", "Sedentarismo")

res.pca_country2 <- PCA(Country_desc_ALL[c(37, 27, 32)], graph = F, scale.unit = T)
fviz_eig(res.pca_country2, addlabels = TRUE)
fviz_pca_var(res.pca_country2, col.var = "black")
fviz_pca_biplot(res.pca_country2, geom.ind="point", col.var = "#2E9FDF", col.ind = "#696969")

PCA_MVPA_WALK_sit <- fviz_pca_biplot(res.pca_country2, 
                              col.ind = "black", fill.ind = "seagreen2", pointshape = 21, pointsize = 2.5,
                              col.var = "contrib", repel = T, alpha.ind = 0.75,
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              legend.title = list(color = "Contribución"))

plot_pca_sit <- ggpubr::ggpar(PCA_MVPA_WALK_sit,title = "MVPA y Caminar semanal, y tiempo diario sentado",
              xlab = "CP1 (62.1%)", ylab = "CP2 (30.3%)",xlim = c(-3.25, 2.6), ylim = c(-1.5,2.7),
              legend.position = "top", font.family = "Times", font.main = "bold")

plot_pca_sit

# VPA + MPA + WALK
res.pca_country3 <- PCA(Country_desc_ALL[c(17, 22, 27)], graph = F, scale.unit = T)
fviz_eig(res.pca_country3, addlabels = TRUE)
fviz_pca_var(res.pca_country3, col.var = "black")


PCA_PA <- fviz_pca_biplot(res.pca_country3,
                col.ind = "black", fill.ind = "seagreen2", pointshape = 21, pointsize = 2.5,
                col.var = "contrib", repel = T, alpha.ind = 0.75,
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(color = "Contribución"))

plot_pca_pa <- ggpubr::ggpar(PCA_PA, title = "Componentes de la actividad física",
              xlim = c(-3.25, 2.6), ylim = c(-1.5,2.7),
              xlab = "CP1 (86%)", ylab = "CP2 (11.1%)",
              legend.position = "top", font.family = "Times", font.main = "bold")
plot_pca_pa

ggarrange(plot_pca_pa, plot_pca_sit, ncol = 2, nrow = 1)

ggsave("PCA_MVPA_WALK_SIT.pdf", width = 7.5, height = 5, units = "in", dpi = 600)
ggsave("PCA_plots.pdf", width = 14, height = 5, units = "in", dpi = 600)



#### Percentiles ####
library("reshape2")

eu2017 %>% group_by(Gender, Age_6clusters) %>%
  summarise(n = n(), MVPA_pluswalk_time_mean = mean(MVPA_tot_time_pluswalk), MVPA_pluswalk_time_sd = sd(MVPA_tot_time_pluswalk),
            "MVPA_pluswalk_time_5th" = quantile(MVPA_tot_time_pluswalk, 0.05), 
            "MVPA_pluswalk_time_10th" = quantile(MVPA_tot_time_pluswalk, 0.10), "MVPA_pluswalk_time_25th" = quantile(MVPA_tot_time_pluswalk, 0.25),  
            "MVPA_pluswalk_time_50th" = quantile(MVPA_tot_time_pluswalk, 0.5), "MVPA_pluswalk_time_75th" = quantile(MVPA_tot_time_pluswalk, 0.75),
            "MVPA_pluswalk_time_90th" = quantile(MVPA_tot_time_pluswalk, 0.9), "MVPA_pluswalk_time_95th" = quantile(MVPA_tot_time_pluswalk, 0.95),  
            
            MVPA_met_mean = mean(MVPA_met), MVPA_met_sd = sd(MVPA_met),
            "MVPA_met_5th" = quantile(MVPA_met, 0.05), 
            "MVPA_met_10th" = quantile(MVPA_met, 0.10), "MVPA_met_25th" = quantile(MVPA_met, 0.25),  
            "MVPA_met_50th" = quantile(MVPA_met, 0.5), "MVPA_met_75th" = quantile(MVPA_met, 0.75),
            "MVPA_met_90th" = quantile(MVPA_met, 0.9), "MVPA_met_95th" = quantile(MVPA_met, 0.95),
            
            Sit_mean = mean(Sit_med), Sit_sd = sd(Sit_med),
            "Sit_5th" = quantile(Sit_med, 0.05), 
            "Sit_10th" = quantile(Sit_med, 0.10), "Sit_25th" = quantile(Sit_med, 0.25),  
            "Sit_50th" = quantile(Sit_med, 0.5), "Sit_75th" = quantile(Sit_med, 0.75),
            "Sit_90th" = quantile(Sit_med, 0.9), "Sit_95th" = quantile(Sit_med, 0.95),
            ) -> Percentiles_data



Percentiles_data_mvpa <- melt(Percentiles_data, id = c( "Gender", "Age_6clusters"),
             measure = c("MVPA_met_5th", "MVPA_met_10th", "MVPA_met_25th",
                         "MVPA_met_50th", "MVPA_met_75th", "MVPA_met_90th",
                         "MVPA_met_95th"),
             variable.name = "Percentile",
             value.name = "MVPA METs/week")

line_men_mvpa <- ggplot(Percentiles_data_mvpa[which(Percentiles_data_mvpa$Gender == "Man"),], aes(x=Age_6clusters, y=`MVPA METs/week`, group=Percentile)) + 
  geom_line(aes(color=Percentile)) + labs(y="Actividad física total (METs-min/sem)") + theme_minimal()+
  labs(title = "Hombres")+
  theme(axis.title.x =  element_blank(), axis.title.y = element_blank(), axis.text = element_text(family = "Times"),
        axis.title = element_text(face = "bold", family = "Times"), title = element_text(face = "bold", family = "Times")) + 
  scale_y_continuous(breaks = c(seq(0,10000,1000)), limits = c(0,10000)) + guides(color= F)
  line_men_mvpa


line_women_mvpa <- ggplot(Percentiles_data_mvpa[which(Percentiles_data_mvpa$Gender == "Woman"),], aes(x=Age_6clusters, 
  y=`MVPA METs/week`, group=Percentile)) + 
  labs(title = "Mujeres")+
  geom_line(aes(color=Percentile)) + labs(y="Actividad física total (METs-min/sem)") + theme_minimal()+
  theme(axis.title.x =  element_blank(),axis.text = element_text(family = "Times"),
        title = element_text(face = "bold", family = "Times"),
        axis.title = element_text(face = "bold", family = "Times")) + scale_y_continuous(breaks = c(seq(0,10000,1000)),  limits = c(0,10000)) + guides(color= F)
line_women_mvpa


ggarrange(line_women_mvpa, line_men_mvpa, 
          ncol = 2, nrow = 1, common.legend = T, align = "hv", font.label = list(family = "Times"))
ggsave("Percentiles.tiff", width = 12, height = 5, units = "in", dpi = 600)

