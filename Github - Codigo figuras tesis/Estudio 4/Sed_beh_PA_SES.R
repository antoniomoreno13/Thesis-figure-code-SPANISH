
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("MASS")
library("sfsmisc")
library("QuantPsyc")
library("forcats")
library("moments")
library(reshape2)
library(lmtest)
library(extrafont)
library(colortools)
library(reshape2)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))

#### Data Handling ####
# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224,222, 277, 242,291, 236, 237:240, 270, 11)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:28] <- c("Gender", "Age","Marital status", "Social class subjective", "Type of community", "Sizeofcommunity" ,"Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Ocupation_last_job", "Bills", "Country")
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

eu2017$Sit_med_7 <- eu2017$Sit_med * 7

eu2017$Sit_4_groups [eu2017$Sit == "DK"] <- NA
eu2017$Sit_4_groups [eu2017$Sit == "1 hour or less"] <- "<=4.5h"
eu2017$Sit_4_groups [eu2017$Sit == "1 hour to 1 hour and 30 minutes"] <- "<=4.5h"
eu2017$Sit_4_groups [eu2017$Sit == "1 hour 31 minutes to 2 hours 30 minutes"] <- "<=4.5h"
eu2017$Sit_4_groups [eu2017$Sit == "2 hours 31 minutes to 3 hours 30 minutes"] <- "<=4.5h"
eu2017$Sit_4_groups [eu2017$Sit == "3 hours 31 minutes to 4 hours 30 minutes"] <- "<=4.5h"
eu2017$Sit_4_groups [eu2017$Sit == "4 hours 31 minutes to 5 hours 30 minutes"] <- ">4.5h - <=6.5h"
eu2017$Sit_4_groups [eu2017$Sit == "5 hours 31 minutes to 6 hours 30 minutes"] <- ">4.5h - <=6.5h"
eu2017$Sit_4_groups [eu2017$Sit == "6 hours 31 minutes to 7 hours 30 minutes"] <- ">6.5h - <=8.5h"
eu2017$Sit_4_groups [eu2017$Sit == "7 hours 31 minutes to 8 hours 30 minutes"] <- ">6.5h - <=8.5h"
eu2017$Sit_4_groups [eu2017$Sit == "More than 8 hours and 30 minutes"] <- ">8.5h"
eu2017$Sit_4_groups <- as.factor(eu2017$Sit_4_groups)


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

eu2017$MVPA_tot_time_outwalk  <- eu2017$VPA_tot_time + eu2017$MPA_tot_time
eu2017$MVPA_tot_time_pluswalk <- eu2017$VPA_tot_time + eu2017$MPA_tot_time + eu2017$Walk_tot_time

# METS PER WEEK
eu2017$VPA_met <- eu2017$VPA_tot_time * 8
eu2017$MPA_met <- eu2017$MPA_tot_time * 4
eu2017$Walk_met <- eu2017$Walk_tot_time * 3.3 

# COMPUTE MVPA METs 

eu2017$MVPA_met <- eu2017$VPA_met + eu2017$MPA_met + eu2017$Walk_met
eu2017$MVPA_met_outwalk <- eu2017$VPA_met + eu2017$MPA_met

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA) and quartiles

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

eu2017$Age_6clusters [ eu2017$Age >= 15 & eu2017$Age < 25 ] <- "15-24"  
eu2017$Age_6clusters [ eu2017$Age >= 25 & eu2017$Age < 35 ] <- "25-34"  
eu2017$Age_6clusters [ eu2017$Age >= 35 & eu2017$Age < 45 ] <- "35-44"  
eu2017$Age_6clusters [ eu2017$Age >= 45 & eu2017$Age < 55 ] <- "45-54"  
eu2017$Age_6clusters [ eu2017$Age >= 55 & eu2017$Age < 65 ] <- "55-64"  
eu2017$Age_6clusters [ eu2017$Age >= 65 ] <- "65+"  

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
eu2017 <- eu2017[which(eu2017$Age >= 18 & eu2017$Age < 65), ]


quantile(eu2017$MVPA_tot_time_pluswalk, na.rm = T)


eu2017$MVPA_met_quartiles [eu2017$MVPA_tot_time_pluswalk <= 105] <- "Met_Q4"
eu2017$MVPA_met_quartiles [eu2017$MVPA_tot_time_pluswalk > 105 & eu2017$MVPA_tot_time_pluswalk <= 315] <- "Met_Q3"
eu2017$MVPA_met_quartiles [eu2017$MVPA_tot_time_pluswalk > 315 & eu2017$MVPA_tot_time_pluswalk <= 675] <- "Met_Q2"
eu2017$MVPA_met_quartiles [eu2017$MVPA_tot_time_pluswalk > 675] <- "Met_Q1"
eu2017$MVPA_met_quartiles <- as.factor(eu2017$MVPA_met_quartiles)

eu2017[c(46:59)] <- lapply(eu2017[c(46:59)], factor) 

#### Descriptive ####

table(eu2017$Age_6clusters);prop.table(table(eu2017$Age_6clusters))
table(eu2017$Gender);prop.table(table(eu2017$Gender))
table(eu2017$Sizeofcommunity);prop.table(table(eu2017$Sizeofcommunity))
table(eu2017$WHO_prev);prop.table(table(eu2017$WHO_prev))

table(eu2017$MVPA_met_quartiles);prop.table(table(eu2017$MVPA_met_quartiles))
table(eu2017$Sit_4_groups);prop.table(table(eu2017$Sit_4_groups))

table(eu2017$`Marital status`);prop.table(table(eu2017$`Marital status`))
table(eu2017$`Social class subjective`);prop.table(table(eu2017$`Social class subjective`))
table(eu2017$Education_3cat_recod);prop.table(table(eu2017$Education_3cat_recod))
table(eu2017$Ocupation_5cat);prop.table(table(eu2017$Ocupation_5cat))
table(eu2017$Ocupation_3cat);prop.table(table(eu2017$Ocupation_3cat))
table(eu2017$Bills);prop.table(table(eu2017$Bills))

mean(eu2017$MVPA_tot_time_outwalk); sd(eu2017$MVPA_tot_time_outwalk)
mean(eu2017$MVPA_tot_time_pluswalk); sd(eu2017$MVPA_tot_time_pluswalk)

mean(eu2017$MVPA_met); sd(eu2017$MVPA_met)
mean(eu2017$Sit_med); sd(eu2017$Sit_med)


## Physical activity descrip and sit across gender with covariates
# ALL - BOTH GENDERS
ALL_desc <- eu2017 %>% 
    summarise(n=n(), HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

Gender_desc <- eu2017 %>% group_by(Gender) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

wilcox.test(MVPA_tot_time_pluswalk ~ Gender, data = eu2017)
wilcox.test(MVPA_tot_time_outwalk ~ Gender, data = eu2017)
wilcox.test(Walk_tot_time ~ Gender, data = eu2017)
wilcox.test(Sit_med ~ Gender, data = eu2017)


Age_desc <- eu2017 %>% group_by(Age_6clusters) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))


kruskal.test(MVPA_tot_time_pluswalk ~ Age_6clusters, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ Age_6clusters, data = eu2017)
kruskal.test(Walk_tot_time ~ Age_6clusters, data = eu2017)
kruskal.test(Sit_med ~ Age_6clusters, data = eu2017)


Educ_desc <- eu2017 %>% group_by(Education_3cat_recod) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

kruskal.test(MVPA_tot_time_pluswalk ~ Education_3cat_recod, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ Education_3cat_recod, data = eu2017)
kruskal.test(Walk_tot_time ~ Education_3cat_recod, data = eu2017)
kruskal.test(Sit_med ~ Education_3cat_recod, data = eu2017)


OSC_desc <- eu2017 %>% group_by(Ocupation_3cat) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

kruskal.test(MVPA_tot_time_pluswalk ~ Ocupation_3cat, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ Ocupation_3cat, data = eu2017)
kruskal.test(Walk_tot_time ~ Ocupation_3cat, data = eu2017)
kruskal.test(Sit_med ~ Ocupation_3cat, data = eu2017)


Bills_desc <- eu2017 %>% group_by(Bills) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

kruskal.test(MVPA_tot_time_pluswalk ~ Bills, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ Bills, data = eu2017)
kruskal.test(Walk_tot_time ~ Bills, data = eu2017)
kruskal.test(Sit_med ~ Bills, data = eu2017)


Marital_desc <- eu2017 %>% group_by(`Marital status`) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

kruskal.test(MVPA_tot_time_pluswalk ~ `Marital status`, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ `Marital status`, data = eu2017)
kruskal.test(Walk_tot_time ~ `Marital status`, data = eu2017)
kruskal.test(Sit_med ~ `Marital status`, data = eu2017)


Place_desc <- eu2017 %>% group_by(Sizeofcommunity) %>%
    summarise(n=n(),HEPA_time_mean = mean(MVPA_tot_time_pluswalk), HEPA_time_sd = sd(MVPA_tot_time_pluswalk),
              HEPA_time_med = median(MVPA_tot_time_pluswalk),
              HEPA_time_5th = quantile(MVPA_tot_time_pluswalk, 0.05),
              HEPA_time_95th = quantile(MVPA_tot_time_pluswalk, 0.95),
              MVPA_time_mean = mean(MVPA_tot_time_outwalk), MVPA_time_sd = sd(MVPA_tot_time_outwalk),
              MVPA_time_med = median(MVPA_tot_time_outwalk),
              MVPA_time_5th = quantile(MVPA_tot_time_outwalk, 0.05),
              MVPA_time_95th = quantile(MVPA_tot_time_outwalk, 0.95),
              VPA_time_mean = mean(VPA_tot_time), VPA_time_sd = sd(VPA_tot_time),
              VPA_time_med = median(VPA_tot_time),
              VPA_time_5th = quantile(VPA_tot_time, 0.05),
              VPA_time_95th = quantile(VPA_tot_time, 0.95),
              MPA_time_mean = mean(MPA_tot_time), MPA_time_sd = sd(MPA_tot_time),
              MPA_time_med = median(MPA_tot_time),
              MPA_time_5th = quantile(MPA_tot_time, 0.05),
              MPA_time_95th = quantile(MPA_tot_time, 0.95),
              Walk_time_mean = mean(Walk_tot_time), Walk_time_sd = sd(Walk_tot_time),
              Walk_time_med = median(Walk_tot_time),
              Walk_time_5th = quantile(Walk_tot_time, 0.05),
              Walk_time_95th = quantile(Walk_tot_time, 0.95),
              Sit_mean = mean(Sit_med),Sit_sd = sd(Sit_med),
              Sit_time_med = median(Sit_med),
              Sit_time_5th = quantile(Sit_med, 0.05),
              Sit_time_95th = quantile(Sit_med, 0.95))

kruskal.test(MVPA_tot_time_pluswalk ~ Sizeofcommunity, data = eu2017)
kruskal.test(MVPA_tot_time_outwalk ~ Sizeofcommunity, data = eu2017)
kruskal.test(Walk_tot_time ~ Sizeofcommunity, data = eu2017)
kruskal.test(Sit_med ~ Sizeofcommunity, data = eu2017)


#### Inequality Gini and Lorenz ####
library(ineq)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(gglorenz)

ineq(eu2017$MVPA_tot_time_pluswalk)
ineq(eu2017$MVPA_tot_time_outwalk)
ineq(eu2017$VPA_tot_time)
ineq(eu2017$MPA_tot_time)
ineq(eu2017$Walk_tot_time)
ineq(eu2017$Sit_med)

# AGE N GENDER 

Ineq_Gender <- eu2017 %>% group_by(Gender) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
    Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
    Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
    Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

eu2017 %>% group_by(Age_6clusters) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_desc <- eu2017 %>% group_by(Gender, Age_6clusters) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_desc2 <- melt(Ineq_desc, id = c("Gender", "Age_6clusters"),
                  measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
                  variable.name = "Activity",
                  value.name = "Gini" ) 

Ineq_desc2$Gender <- relevel(Ineq_desc2$Gender, ref = "Woman")
Ineq_desc2$Activity <- ordered(Ineq_desc2$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))

line_ineq <- ggplot(Ineq_desc2, aes(x=Age_6clusters, y=Gini, group=Activity)) + 
    geom_bar(aes(fill=Activity), stat="identity",  position=position_dodge()) + labs(y="Inequality Distribution (Gini Coefficient)") +
    theme_classic() + theme(legend.title = element_blank(), axis.title.x =  element_blank(), strip.background = element_rect(color = "white", fill = "grey95")) + 
    coord_cartesian(ylim = c(0.035,0.8)) + facet_wrap(. ~ Gender) + 
    scale_fill_ordinal(labels = c("VPA", "MPA", "Walking", "HEPA", "Sitting"))
line_ineq

ggsave("Gini_age_gender_5 measures.pdf", width = 7.5, height = 5, units = "in", dpi = 600)


# SES AND GENDER
Ineq_EDUC <- eu2017 %>% group_by(Gender, Education_3cat_recod) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_EDUC2 <- melt(Ineq_EDUC, id = c("Gender", "Education_3cat_recod"),
                   measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
                   variable.name = "Activity",
                   value.name = "Gini" ) 

Ineq_EDUC2 <- Ineq_EDUC2[which(complete.cases(Ineq_EDUC2)),]

Ineq_EDUC2$Gender <- relevel(Ineq_EDUC2$Gender, ref = "Woman")
Ineq_EDUC2$Activity <- ordered(Ineq_EDUC2$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))

Ineq_EDUC2$Education_3cat_recod <- factor(Ineq_EDUC2$Education_3cat_recod, levels = c("Primary", "Secondary", "University"),
                            labels = c("Primaria", "Secundaria", "Universitario"))


line_educ <- ggplot(Ineq_EDUC2, aes(x=Education_3cat_recod, y=Gini, group=Activity)) + 
    geom_bar(aes(fill=Activity), stat="identity", size=0.5, position=position_dodge()) + labs(y="Distribución desigual (Coeficiente Gini)",
                                                                                     subtitle = "Nivel educativo") +
    theme_classic() + theme(legend.title = element_blank(), axis.text = element_text(family = "Times"), legend.text = element_text(family = "Times"),
                            axis.title.x =  element_blank(), strip.background = element_blank(),
                            strip.text = element_blank(), plot.subtitle = element_text(face = "bold",hjust = 0.5, family = "Times"), axis.title.y = element_text(face = "bold", family = "Times")) + 
    coord_cartesian(ylim = c(0.035,0.85)) + facet_wrap(. ~ Gender) + 
    scale_fill_manual(labels = c("VPA", "MPA", "Caminar", "HEPA", "Sedentarismo"), values = c("red", "orange1", "yellow1", "springgreen1", "steelblue1"))
line_educ


Ineq_OSC <- eu2017 %>% group_by(Gender, Ocupation_3cat) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_OSC2 <- melt(Ineq_OSC, id = c("Gender", "Ocupation_3cat"),
                   measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
                   variable.name = "Activity",
                   value.name = "Gini" ) 
Ineq_OSC2 <- Ineq_OSC2[which(complete.cases(Ineq_OSC2)),]

Ineq_OSC2$Gender <- relevel(Ineq_OSC2$Gender, ref = "Woman")
Ineq_OSC2$Activity <- ordered(Ineq_OSC2$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))


Ineq_OSC2$Ocupation_3cat <- factor(Ineq_OSC2$Ocupation_3cat, order = TRUE, 
                       levels = c("V-VII", "III-IV", "I-II"))

line_OSC <- ggplot(Ineq_OSC2, aes(x=Ocupation_3cat, y=Gini, group=Activity)) + 
    geom_bar(aes(fill=Activity), stat="identity", size=0.5, position=position_dodge()) + labs(y="Distribución desigual (Coeficiente Gini)",
                                                                                     subtitle = "Clase social ocupacional") +
    theme_classic() + theme(legend.title = element_blank(), axis.text = element_text(family = "Times"), legend.text = element_text(family = "Times"),
                            axis.title.x =  element_blank(), strip.background = element_blank(),
                            strip.text = element_blank(), plot.subtitle = element_text(face = "bold",hjust = 0.5, family = "Times"), axis.title.y = element_text(face = "bold", family = "Times")) + 
    coord_cartesian(ylim = c(0.035,0.85)) + facet_wrap(. ~ Gender) + 
    scale_fill_manual(labels = c("VPA", "MPA", "Caminar", "HEPA", "Sedentarismo"), values = c("red", "orange1", "yellow1", "springgreen1", "steelblue1"))
line_OSC


Ineq_BILLS <- eu2017 %>% group_by(Gender, Bills) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_BILLS2 <- melt(Ineq_BILLS, id = c("Gender", "Bills"),
                   measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
                   variable.name = "Activity",
                   value.name = "Gini" ) 
Ineq_BILLS2 <- Ineq_BILLS2[which(complete.cases(Ineq_BILLS2)),]

Ineq_BILLS2$Gender <- relevel(Ineq_BILLS2$Gender, ref = "Woman")
Ineq_BILLS2$Activity <- ordered(Ineq_BILLS2$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))

Ineq_BILLS2$Bills <- factor(Ineq_BILLS2$Bills, levels = c("Most of the time", "From time to time", "Almost never/never"),
                  labels = c("Mayoria del tiempo", "Ocasionalmente", "Nunca"))

levels(Ineq_BILLS2$Bills)

line_BILLS <- ggplot(Ineq_BILLS2, aes(x=Bills, y=Gini, group=Activity)) + 
    geom_bar(aes(fill=Activity), size=0.5, stat="identity",  position=position_dodge()) + labs(y="Distribución desigual (Coeficiente Gini)",
                subtitle = "Dificultades económicas") +
    theme_classic() + theme(legend.title = element_blank(), axis.text = element_text(family = "Times"), legend.text = element_text(family = "Times"),
                            axis.title.x =  element_blank(), strip.background = element_blank(),
                strip.text = element_blank(), plot.subtitle = element_text(face = "bold",hjust = 0.5, family = "Times"), axis.title.y = element_text(face = "bold", family = "Times")) + 
    coord_cartesian(ylim = c(0.035,0.85)) + facet_wrap(. ~ Gender) + 
    scale_fill_manual(labels = c("VPA", "MPA", "Caminar", "HEPA", "Sedentarismo"), values = c("red", "orange1", "yellow1", "springgreen1", "steelblue1"))
line_BILLS

ggunit <- ggarrange(line_educ, line_OSC, line_BILLS, legend = "right",
          ncol = 1, nrow = 3, common.legend = T, align = "hv")

library(grid)
text1 <- grobTree(textGrob("Mujeres", x=0.26,  y=1.02, gp=gpar(fontface = "bold", fontfamily="Times")))
text2 <- grobTree(textGrob("Hombres", x=0.705,  y=1.02, gp=gpar(fontface = "bold", fontfamily="Times"))) 
ggunit + theme(plot.margin = unit(c(0.75,0,0,0), "cm")) + annotation_custom(text1) + annotation_custom(text2)

ggsave("Gini_SES_gender_time_5_measures.tiff", width = 9, height = 10, units = "in", dpi = 600)


# Resident place
Ineq_place <- eu2017 %>% group_by(Gender, Sizeofcommunity) %>% 
    summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_VPA_time = ineq(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time))

Ineq_place2 <- melt(Ineq_place, id = c("Gender", "Sizeofcommunity"),
                    measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
                    variable.name = "Activity",
                    value.name = "Gini" ) 
Ineq_place2 <- Ineq_place2[which(complete.cases(Ineq_place2)),]

Ineq_place2$Gender <- relevel(Ineq_place2$Gender, ref = "Woman")
Ineq_place2$Activity <- ordered(Ineq_place2$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))
        
line_place <- ggplot(Ineq_place2, aes(x=Sizeofcommunity, y=Gini, group=Activity)) + 
    geom_bar(aes(fill=Activity), stat="identity",  position=position_dodge()) + labs(y="Inequality Distribution (Gini Coefficient)") +
    theme_classic() + theme(legend.title = element_blank(), axis.title.x =  element_blank(), 
                            strip.background = element_rect(color = "white", fill = "grey95")) + 
    coord_cartesian(ylim = c(0.035,0.8)) + facet_wrap(. ~ Gender) + 
    scale_fill_ordinal(labels = c("VPA", "MPA", "Walking", "HEPA", "Sitting")) +
    scale_x_discrete(labels = c("Rural", "Small urban", "Large urban"))
line_place

ggsave("Gini_resident_gender_5_measures.pdf", width = 7.5, height = 5, units = "in", dpi = 600)


# LORENZ CURVES

Ineq_data <- melt(eu2017, id = c( "ID", "Gender", "Education_3cat_recod", "Ocupation_3cat", "Bills"),
           measure = c("MVPA_met", "Sit_med"),
           variable.name = "AF or SB",
           value.name = "Mets or Time" ) 

Ineq_data %>% ggplot(aes(x = `Mets or Time`, fill = `AF or SB`)) +
    stat_lorenz(geom = "polygon", alpha = 0.65) + geom_abline(linetype = "dashed")+
    coord_fixed() + hrbrthemes::scale_x_percent() + hrbrthemes::scale_y_percent() +
    theme(legend.title = element_blank()) + facet_grid(. ~ Gender) +
    labs(x = "Cumulative Percentage of Population", y = "Cumulative Percentage of activity")

ggsave("lorenz_mets_SB_gender.pdf", width = 7.5, height = 5, units = "in", dpi = 600)


Ineq_data2 <- melt(eu2017, id = c( "ID", "Gender", "Education_3cat_recod", "Ocupation_3cat", "Bills"),
                  measure = c("MVPA_tot_time_pluswalk", "Sit_med"),
                  variable.name = "AF or SB",
                  value.name = "Time" ) 



Ineq_data2 %>% ggplot(aes(x = Time, fill = `AF or SB`)) +
    stat_lorenz(geom = "polygon", alpha = 0.65) + geom_abline(linetype = "dashed")+
    coord_fixed() + hrbrthemes::scale_x_percent() + hrbrthemes::scale_y_percent() +
    theme(legend.title = element_blank()) + facet_grid(. ~ Gender) +
    labs(x = "Cumulative Percentage of Population", y = "Cumulative Percentage of activity")

ggsave("lorenz_time_SB_gender.pdf", width = 7.5, height = 5, units = "in", dpi = 600)


Ineq_data3 <- melt(eu2017, id = c( "ID", "Gender", "Education_3cat_recod", "Ocupation_3cat", "Bills"),
                   measure = c("MVPA_tot_time_pluswalk", "VPA_tot_time", "MPA_tot_time", "Walk_tot_time", "Sit_med"),
                   variable.name = "Activity",
                   value.name = "Time" ) 

Ineq_data3$Gender <- relevel(Ineq_data3$Gender, ref = "Woman")
Ineq_data3$Activity <- ordered(Ineq_data3$Activity, levels = c("VPA_tot_time", "MPA_tot_time", "Walk_tot_time", "MVPA_tot_time_pluswalk", "Sit_med"))


Ineq_data3$Gender <- factor(Ineq_data3$Gender, levels = c("Woman", "Man"),
                  labels = c("Mujeres", "Hombres"))

Ineq_data3 %>% ggplot(aes(x = Time, color = Activity)) +
    stat_lorenz() + geom_abline(linetype = "dashed")+
    coord_fixed() + hrbrthemes::scale_x_percent() + hrbrthemes::scale_y_percent() +theme_classic() +
    theme(legend.title = element_blank(), strip.background = element_blank(), strip.text = element_text(face = "bold", size = 14, family = "Times"),
          panel.spacing.x = unit(1.5, "lines"), axis.text = element_text(family = "Times"), axis.title = element_text(family = "Times", face = "bold"),
          legend.text = element_text(family = "Times")) + 
    facet_grid(. ~ Gender) + 
    labs(x = "Porcentaje acumulativo de la población (%)", y = "Porcentaje acumulativo de actividad (%)") + 
    scale_color_manual(labels = c("VPA", "MPA", "Caminar", "HEPA", "Sedentarismo"), values = c("red", "darkorange1", "gold", "springgreen1", "steelblue1"))

ggsave("lorenz_activity_gender.tiff", width = 8.5, height = 5, units = "in", dpi = 600)


#### COUNTRY LEVEL ####
## By gender
Country_level <- eu2017 %>% group_by(Country_rec, Gender) %>% summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
                            Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
                            Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time), Mean_HEPA_time = mean(MVPA_tot_time_pluswalk), 
                            Mean_MVPA_time = mean(MVPA_tot_time_outwalk), Mean_Walk_time = mean(Walk_tot_time), Mean_sit = mean(Sit_med))

Country_level_educ <- eu2017 %>% group_by(Country_rec, Gender, Education_3cat_recod) %>% summarise(Mean_HEPA_time = mean(MVPA_tot_time_pluswalk), 
                            Mean_MVPA_time = mean(MVPA_tot_time_outwalk), Mean_Walk_time = mean(Walk_tot_time), Mean_sit = mean(Sit_med))
Country_level_educ <- Country_level_educ[which(complete.cases(Country_level_educ)),]


Country_level_ocup <- eu2017 %>% group_by(Country_rec, Gender, Ocupation_3cat) %>% summarise(Mean_HEPA_time = mean(MVPA_tot_time_pluswalk), 
                            Mean_MVPA_time = mean(MVPA_tot_time_outwalk), Mean_Walk_time = mean(Walk_tot_time), Mean_sit = mean(Sit_med))
Country_level_ocup <- Country_level_ocup[which(complete.cases(Country_level_ocup)),]


Country_level_bills <- eu2017 %>% group_by(Country_rec, Gender, Bills) %>% summarise(Mean_HEPA_time = mean(MVPA_tot_time_pluswalk), 
                            Mean_MVPA_time = mean(MVPA_tot_time_outwalk), Mean_Walk_time = mean(Walk_tot_time), Mean_sit = mean(Sit_med))
Country_level_bills <- Country_level_bills[which(complete.cases(Country_level_bills)),]


Country_level_educ_hepa <- dcast(Country_level_educ, Country_rec + Gender ~ Education_3cat_recod, value.var = "Mean_HEPA_time")
colnames(Country_level_educ_hepa)[colnames(Country_level_educ_hepa) == "Primary"] <- "Primary_hepa"
colnames(Country_level_educ_hepa)[colnames(Country_level_educ_hepa) == "Secondary"] <- "Secondary_hepa"
colnames(Country_level_educ_hepa)[colnames(Country_level_educ_hepa) == "University"] <- "University_hepa"

Country_level_educ_mvpa <- dcast(Country_level_educ, Country_rec + Gender ~ Education_3cat_recod, value.var = "Mean_MVPA_time")
colnames(Country_level_educ_mvpa)[colnames(Country_level_educ_mvpa) == "Primary"] <- "Primary_mvpa"
colnames(Country_level_educ_mvpa)[colnames(Country_level_educ_mvpa) == "Secondary"] <- "Secondary_mvpa"
colnames(Country_level_educ_mvpa)[colnames(Country_level_educ_mvpa) == "University"] <- "University_mvpa"

Country_level_educ_walk <- dcast(Country_level_educ, Country_rec + Gender ~ Education_3cat_recod, value.var = "Mean_Walk_time")
colnames(Country_level_educ_walk)[colnames(Country_level_educ_walk) == "Primary"] <- "Primary_walk"
colnames(Country_level_educ_walk)[colnames(Country_level_educ_walk) == "Secondary"] <- "Secondary_walk"
colnames(Country_level_educ_walk)[colnames(Country_level_educ_walk) == "University"] <- "University_walk"

Country_level_educ_sit <- dcast(Country_level_educ, Country_rec + Gender ~ Education_3cat_recod, value.var = "Mean_sit")
colnames(Country_level_educ_sit)[colnames(Country_level_educ_sit) == "Primary"] <- "Primary_sit"
colnames(Country_level_educ_sit)[colnames(Country_level_educ_sit) == "Secondary"] <- "Secondary_sit"
colnames(Country_level_educ_sit)[colnames(Country_level_educ_sit) == "University"] <- "University_sit"


Country_level_ocup_hepa <- dcast(Country_level_ocup, Country_rec + Gender ~ Ocupation_3cat, value.var = "Mean_HEPA_time")
colnames(Country_level_ocup_hepa)[colnames(Country_level_ocup_hepa) == "I-II"] <- "I_II_hepa"
colnames(Country_level_ocup_hepa)[colnames(Country_level_ocup_hepa) == "III-IV"] <- "III_IV_hepa"
colnames(Country_level_ocup_hepa)[colnames(Country_level_ocup_hepa) == "V-VII"] <- "V_VII_hepa"

Country_level_ocup_mvpa <- dcast(Country_level_ocup, Country_rec + Gender ~ Ocupation_3cat, value.var = "Mean_MVPA_time")
colnames(Country_level_ocup_mvpa)[colnames(Country_level_ocup_mvpa) == "I-II"] <- "I_II_mvpa"
colnames(Country_level_ocup_mvpa)[colnames(Country_level_ocup_mvpa) == "III-IV"] <- "III_IV_mvpa"
colnames(Country_level_ocup_mvpa)[colnames(Country_level_ocup_mvpa) == "V-VII"] <- "V_VII_mvpa"

Country_level_ocup_walk <- dcast(Country_level_ocup, Country_rec + Gender ~ Ocupation_3cat, value.var = "Mean_Walk_time")
colnames(Country_level_ocup_walk)[colnames(Country_level_ocup_walk) == "I-II"] <- "I_II_walk"
colnames(Country_level_ocup_walk)[colnames(Country_level_ocup_walk) == "III-IV"] <- "III_IV_walk"
colnames(Country_level_ocup_walk)[colnames(Country_level_ocup_walk) == "V-VII"] <- "V_VII_walk"

Country_level_ocup_sit <- dcast(Country_level_ocup, Country_rec + Gender ~ Ocupation_3cat, value.var = "Mean_sit")
colnames(Country_level_ocup_sit)[colnames(Country_level_ocup_sit) == "I-II"] <- "I_II_sit"
colnames(Country_level_ocup_sit)[colnames(Country_level_ocup_sit) == "III-IV"] <- "III_IV_sit"
colnames(Country_level_ocup_sit)[colnames(Country_level_ocup_sit) == "V-VII"] <- "V_VII_sit"

Country_level_bills_hepa <- dcast(Country_level_bills, Country_rec + Gender ~ Bills, value.var = "Mean_HEPA_time")
colnames(Country_level_bills_hepa)[colnames(Country_level_bills_hepa) == "Most of the time"] <- "Most_hepa"
colnames(Country_level_bills_hepa)[colnames(Country_level_bills_hepa) == "From time to time"] <- "Time_time_hepa"
colnames(Country_level_bills_hepa)[colnames(Country_level_bills_hepa) == "Almost never/never"] <- "Never_hepa"

Country_level_bills_mvpa <- dcast(Country_level_bills, Country_rec + Gender ~ Bills, value.var = "Mean_MVPA_time")
colnames(Country_level_bills_mvpa)[colnames(Country_level_bills_mvpa) == "Most of the time"] <- "Most_mvpa"
colnames(Country_level_bills_mvpa)[colnames(Country_level_bills_mvpa) == "From time to time"] <- "Time_time_mvpa"
colnames(Country_level_bills_mvpa)[colnames(Country_level_bills_mvpa) == "Almost never/never"] <- "Never_mvpa"

Country_level_bills_walk <- dcast(Country_level_bills, Country_rec + Gender ~ Bills, value.var = "Mean_Walk_time")
colnames(Country_level_bills_walk)[colnames(Country_level_bills_walk) == "Most of the time"] <- "Most_walk"
colnames(Country_level_bills_walk)[colnames(Country_level_bills_walk) == "From time to time"] <- "Time_time_walk"
colnames(Country_level_bills_walk)[colnames(Country_level_bills_walk) == "Almost never/never"] <- "Never_walk"

Country_level_bills_sit <- dcast(Country_level_bills, Country_rec + Gender ~ Bills, value.var = "Mean_sit")
colnames(Country_level_bills_sit)[colnames(Country_level_bills_sit) == "Most of the time"] <- "Most_sit"
colnames(Country_level_bills_sit)[colnames(Country_level_bills_sit) == "From time to time"] <- "Time_time_sit"
colnames(Country_level_bills_sit)[colnames(Country_level_bills_sit) == "Almost never/never"] <- "Never_sit"

Country_level <- merge(Country_level, Country_level_educ_hepa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_educ_mvpa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_educ_walk, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_educ_sit, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_ocup_hepa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_ocup_mvpa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_ocup_walk, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_ocup_sit, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_bills_hepa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_bills_mvpa, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_bills_walk, by =  c("Country_rec", "Gender"))
Country_level <- merge(Country_level, Country_level_bills_sit, by =  c("Country_rec", "Gender"))


Country_level$Educ_gap_HEPA <- (Country_level$University_hepa - Country_level$Primary_hepa)/Country_level$University_hepa
Country_level$Educ_gap_MVPA <- (Country_level$University_mvpa - Country_level$Primary_mvpa)/Country_level$University_mvpa
Country_level$Educ_gap_Walk <- (Country_level$University_walk - Country_level$Primary_walk)/Country_level$University_walk
Country_level$Educ_gap_Sit <- (Country_level$University_sit - Country_level$Primary_sit)/Country_level$University_si
Country_level$Educ_rat_HEPA <- Country_level$University_hepa / Country_level$Primary_hepa
Country_level$Educ_rat_MVPA <- Country_level$University_mvpa / Country_level$Primary_mvpa
Country_level$Educ_rat_Walk <- Country_level$University_walk / Country_level$Primary_walk
Country_level$Educ_rat_Sit <- Country_level$University_sit / Country_level$Primary_sit

Country_level$Ocup_gap_HEPA <- (Country_level$I_II_hepa - Country_level$V_VII_hepa)/Country_level$I_II_hepa
Country_level$Ocup_gap_MVPA <- (Country_level$I_II_mvpa - Country_level$V_VII_mvpa)/Country_level$I_II_mvpa
Country_level$Ocup_gap_Walk <- (Country_level$I_II_walk - Country_level$V_VII_walk)/Country_level$I_II_walk
Country_level$Ocup_gap_Sit <- (Country_level$I_II_sit - Country_level$V_VII_sit)/Country_level$I_II_sit
Country_level$Ocup_rat_HEPA <- Country_level$I_II_hepa / Country_level$V_VII_hepa
Country_level$Ocup_rat_MVPA <- Country_level$I_II_mvpa / Country_level$V_VII_mvpa
Country_level$Ocup_rat_Walk <- Country_level$I_II_walk / Country_level$V_VII_walk
Country_level$Ocup_rat_Sit <- Country_level$I_II_sit / Country_level$V_VII_sit

Country_level$Bills_gap_HEPA <- (Country_level$Never_hepa - Country_level$Most_hepa)/Country_level$Never_hepa
Country_level$Bills_gap_MVPA <- (Country_level$Never_mvpa - Country_level$Most_mvpa)/Country_level$Never_mvpa
Country_level$Bills_gap_Walk <- (Country_level$Never_walk - Country_level$Most_walk)/Country_level$Never_walk
Country_level$Bills_gap_Sit <- (Country_level$Never_sit - Country_level$Most_sit)/Country_level$Never_sit
Country_level$Bills_rat_HEPA <- Country_level$Never_hepa / Country_level$Most_hepa
Country_level$Bills_rat_MVPA <- Country_level$Never_mvpa / Country_level$Most_mvpa
Country_level$Bills_rat_Walk <- Country_level$Never_walk / Country_level$Most_walk
Country_level$Bills_rat_Sit <- Country_level$Never_sit / Country_level$Most_sit


## Correlations
library(corrplot)
library(colorRamps)
Men_data <- Country_level[which(Country_level$Gender == "Man"),]
Women_data <- Country_level[which(Country_level$Gender == "Woman"),]

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Gini and Total activity
corr_men <- round(cor(Men_data[ ,c(6,7,9,4,10:13)]), 2)
corrplot(corr_men, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

corr_women <- round(cor(Women_data[ ,c(6,7,9,4,10:13)]), 2)
corrplot(corr_women, method="color", col=col(200),  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


# Gini and SES gap

corr_men_gini_ses <- round(cor(Men_data[ ,c(7,4,51,53,59,61,67,69)]), 2)
corrplot(corr_men_gini_ses, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


corr_women_gini_ses <- round(cor(Women_data[ ,c(7,4,51,53,59,61,67,69)]), 2)
corrplot(corr_women_gini_ses, method="color", col=col(200),  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)





# All
Country_level_ALL <- eu2017 %>% group_by(Country_rec) %>% summarise(Gini_HEPA_mets = ineq(MVPA_met), Gini_sit_med = ineq(Sit_med), 
              Gini_MVPA_mets = ineq(MVPA_met_outwalk), Gini_HEPA_time = ineq(MVPA_tot_time_pluswalk), Gini_MVPA_time = ineq(MVPA_tot_time_outwalk),
              Gini_Walk_mets = ineq(Walk_met), Gini_Walk_time = ineq(Walk_tot_time), Mean_HEPA_time = mean(MVPA_tot_time_pluswalk), 
              Mean_MVPA_time = mean(MVPA_tot_time_outwalk), Mean_Walk_time = mean(Walk_tot_time), Mean_sit = mean(Sit_med),
              Gini_VPA_time = ineq(VPA_tot_time), Mean_VPA_time = mean(VPA_tot_time), Gini_MPA_time = ineq(MPA_tot_time),
              Mean_MPA_time = mean(MPA_tot_time))

Sociodemographic <- read.spss("C:/Users/anton/Desktop/Country-level eurobarometer.sav", to.data.frame = T, use.missings = T)
Sociodemographic <- Sociodemographic[c(1:2,214)]
colnames(Sociodemographic)[colnames(Sociodemographic) == "Country"] <- "Country_rec"


Sociodemographic$Country_rec <- fct_expand(Sociodemographic$Country_rec, c("AT - Austria", 
                                                                           "BE - Belgium", "BG - Bulgaria", "CY - Cyprus (Republic)",
                                                                           "CZ - Czech Republic", "DE Germany", "DK - Denmark", "EE - Estonia",
                                                                           "ES -Spain", "FI - Finland", "FR - France", "GR - Greece",
                                                                           "HR - Croatia", "HU - Hungary", "IE - Ireland", "IT - Italy",
                                                                           "LT - Lithuania", "LU - Luxembourg", "LV - Latvia", "MT - Malta",
                                                                           "NL - The Netherlands", "PL - Poland", "PT - Portugal", "RO - Romania",
                                                                           "SE - Sweden", "SI - Slovenia", "SK - Slovakia", "UK United Kingdom")) 

Sociodemographic$Country_rec[Sociodemographic$Country_rec == "AT - Austria          "] <- "AT - Austria"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "BE - Belgium          "] <- "BE - Belgium"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "BG - Bulgaria         "] <-"BG - Bulgaria"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "CY - Cyprus (Republic)"] <- "CY - Cyprus (Republic)"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "CZ - Czech Republic   "] <- "CZ - Czech Republic"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "DK - Denmark          "] <- "DK - Denmark"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "EE - Estonia          "] <- "EE - Estonia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "FI - Finland          "] <- "FI - Finland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "FR - France           "] <- "FR - France" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "DE Germany            " ] <- "DE Germany"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "GR - Greece           "] <- "GR - Greece"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "HR - Croatia          "] <- "HR - Croatia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "HU - Hungary          " ] <- "HU - Hungary"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "IE - Ireland          "] <- "IE - Ireland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "IT - Italy            " ] <- "IT - Italy"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LV - Latvia           "] <- "LV - Latvia" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LT - Lithuania        "] <- "LT - Lithuania"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LU - Luxembourg       "] <- "LU - Luxembourg"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "MT - Malta            " ] <- "MT - Malta"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "NL - The Netherlands  "] <- "NL - The Netherlands"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "PL - Poland           " ] <- "PL - Poland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "PT - Portugal         "] <- "PT - Portugal"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "RO - Romania          " ] <- "RO - Romania"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SE - Sweden           " ] <- "SE - Sweden"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SI - Slovenia         "] <-  "SI - Slovenia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SK - Slovakia         "] <- "SK - Slovakia" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "ES -Spain             "] <- "ES -Spain"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "UK United Kingdom     "] <- "UK United Kingdom"
Sociodemographic$Country_rec<- fct_drop(Sociodemographic$Country_rec, only = c("AT - Austria          ", 
                                                                               "BE - Belgium          ", "BG - Bulgaria         ", "CY - Cyprus (Republic)",
                                                                               "CZ - Czech Republic   ", "DE Germany            ", "DK - Denmark          ", "EE - Estonia          ",
                                                                               "ES -Spain             ", "FI - Finland          ", "FR - France           ", "GR - Greece           ",
                                                                               "HR - Croatia          ", "HU - Hungary          ", "IE - Ireland          ", "IT - Italy            ",
                                                                               "LT - Lithuania        ", "LU - Luxembourg       ", "LV - Latvia           ", "MT - Malta            ",
                                                                               "NL - The Netherlands  ", "PL - Poland           ", "PT - Portugal         ", "RO - Romania          ",
                                                                               "SE - Sweden           ", "SI - Slovenia         ", "SK - Slovakia         ", "UK United Kingdom     "))


Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "AT - Austria"] <- "Austria"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "BE - Belgium"] <- "Belgium"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "BG - Bulgaria"] <-"Bulgaria"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "CY - Cyprus (Republic)"] <- "Cyprus"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "CZ - Czech Republic"] <- "Czech Republic"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "DK - Denmark"] <- "Denmark"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "EE - Estonia"] <- "Estonia"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "FI - Finland"] <- "Finland"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "FR - France"] <- "France" 
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "DE Germany" ] <- "Germany"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "GR - Greece"] <- "Greece"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "HR - Croatia"] <- "Croatia"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "HU - Hungary" ] <- "Hungary"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "IE - Ireland"] <- "Ireland"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "IT - Italy" ] <- "Italy"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "LV - Latvia"] <- "Latvia" 
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "LT - Lithuania"] <- "Lithuania"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "LU - Luxembourg"] <- "Luxembourg"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "MT - Malta" ] <- "Malta"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "NL - The Netherlands"] <- "The Netherlands"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "PL - Poland" ] <- "Poland"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "PT - Portugal"] <- "Portugal"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "RO - Romania" ] <- "Romania"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "SE - Sweden" ] <- "Sweden"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "SI - Slovenia"] <-  "Slovenia"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "SK - Slovakia"] <- "Slovakia" 
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "ES -Spain"] <- "Spain"
Sociodemographic$Country_rec2[Sociodemographic$Country_rec == "UK United Kingdom"] <- "United Kingdom"


Country_level_ALL <- merge(Sociodemographic, Country_level_ALL, by =  "Country_rec")

# Country levels Gini plot
melt(data = Country_level_ALL, id = c( "Country_rec2", "Abr.x"),
         measure = c("Gini_HEPA_time", "Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_sit_med"),
         variable.name = "Activity",
         value.name = "Gini coefficient" ) -> Gini_country

Gini_country$Activity <- ordered(Gini_country$Activity, levels = c("Gini_VPA_time", "Gini_MPA_time", "Gini_Walk_time", "Gini_HEPA_time", "Gini_sit_med"))

Country_plot <- ggplot(Gini_country, aes(x=Country_rec2, y=`Gini coefficient`, fill=Activity)) +
    geom_bar(aes(fill=Activity), stat="identity",color="black", position=position_dodge(), width = 0.7) +
    labs(y="Inequality Distribution (Gini Coefficient)") + theme_classic() + 
    scale_fill_ordinal(labels = c("VPA", "MPA", "Walking", "HEPA", "Sitting")) + 
    theme(axis.title.y = element_blank(), legend.title = element_blank())+
    coord_flip(ylim = c(0.035,0.8)) + scale_x_discrete(limits = rev)
Country_plot

ggsave("Country_gini.pdf", width = 8.5, height = 20, units = "in", dpi = 600)


# Gini activity and Gini income

shapiro.test(Country_level_ALL$GINI_2017)
shapiro.test(Country_level_ALL$Gini_HEPA_time)
shapiro.test(Country_level_ALL$Gini_MVPA_time)
shapiro.test(Country_level_ALL$Gini_Walk_time)
shapiro.test(Country_level_ALL$Gini_sit_med)
shapiro.test(Country_level_ALL$Mean_HEPA_time)
shapiro.test(Country_level_ALL$Mean_MVPA_time)
shapiro.test(Country_level_ALL$Mean_Walk_time)

shapiro.test(Country_level_ALL$Mean_sit)
densityPlot(Country_level_ALL$Mean_sit)
Country_level_ALL$log_Mean_sit <- log10(Country_level_ALL$Mean_sit)
densityPlot(Country_level_ALL$log_Mean_sit)
shapiro.test(Country_level_ALL$log_Mean_sit)


lm_g1 <-lm(Gini_HEPA_time ~ Gini_sit_med, data = Country_level_ALL); summary(lm_g1); lm.beta(lm_g1); confint(lm_g1)
ggplot(Country_level_ALL, aes(x=Gini_sit_med, y=Gini_MVPA_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g1)); plot(rstudent(lm_g1))# normalidad de residuos
bptest(lm_g1) # homocedasticidad 
dwtest(Gini_MVPA_time ~ Gini_sit_med, data = Country_level_ALL, alternative = "two.sided") # incorrelación de residuos

cor.test(Country_level_ALL$Gini_sit_med, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g2 <-lm(Gini_sit_med ~ GINI_2017, data = Country_level_ALL); summary(lm_g2); lm.beta(lm_g2); confint(lm_g2)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Gini_sit_med)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g2)); plot(rstudent(lm_g2))# normalidad de residuos
bptest(lm_g2) # homocedasticidad 
dwtest(Gini_sit_med ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Gini_HEPA_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g3 <-lm(Gini_HEPA_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g3); lm.beta(lm_g3); confint(lm_g3)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Gini_HEPA_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g3)); plot(rstudent(lm_g3))# normalidad de residuos
bptest(lm_g3) # homocedasticidad 
dwtest(Gini_HEPA_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Gini_MVPA_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g4 <-lm(Gini_MVPA_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g4); lm.beta(lm_g4); confint(lm_g4)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Gini_MVPA_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g4)); plot(rstudent(lm_g4))# normalidad de residuos
bptest(lm_g4) # homocedasticidad 
dwtest(Gini_MVPA_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Gini_Walk_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g5 <-lm(Gini_Walk_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g5); lm.beta(lm_g5); confint(lm_g5)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Gini_Walk_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g5)); plot(rstudent(lm_g5))# normalidad de residuos
bptest(lm_g5) # homocedasticidad 
dwtest(Gini_Walk_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos



# Average and Gini income
cor.test(Country_level_ALL$Mean_HEPA_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g6 <-lm(Mean_HEPA_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g6); lm.beta(lm_g6); confint(lm_g6)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Mean_HEPA_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g6)); plot(rstudent(lm_g6))# normalidad de residuos
bptest(lm_g6) # homocedasticidad 
dwtest(Mean_HEPA_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Mean_MVPA_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g7 <-lm(Mean_MVPA_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g7); lm.beta(lm_g7); confint(lm_g7)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Mean_MVPA_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g7)); plot(rstudent(lm_g7))# normalidad de residuos
bptest(lm_g7) # homocedasticidad 
dwtest(Mean_MVPA_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Mean_Walk_time, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g8 <-lm(Mean_Walk_time ~ GINI_2017, data = Country_level_ALL); summary(lm_g8); lm.beta(lm_g8); confint(lm_g8)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Mean_Walk_time)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g8)); plot(rstudent(lm_g8))# normalidad de residuos
bptest(lm_g8) # homocedasticidad 
dwtest(Mean_Walk_time ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos

cor.test(Country_level_ALL$Mean_sit, Country_level_ALL$GINI_2017, conf.level = .95)
lm_g9 <-lm(Mean_sit ~ GINI_2017, data = Country_level_ALL); summary(lm_g9); lm.beta(lm_g9); confint(lm_g9)
ggplot(Country_level_ALL, aes(x=GINI_2017, y=Mean_sit)) + geom_smooth(method=lm) + geom_point()
shapiro.test(rstudent(lm_g9)); plot(rstudent(lm_g9))# normalidad de residuos
bptest(lm_g9) # homocedasticidad 
dwtest(Mean_sit ~ GINI_2017, alternative = "two.sided", data = Country_level_ALL) # incorrelación de residuos


#### INDIVIDUAL LEVEL #####
# multinomial
library(nnet)
library(sjPlot)
eu2017$MVPA_met_quartiles2 <- relevel(eu2017$MVPA_met_quartiles, ref = "Met_Q4")
mm1 <- multinom(MVPA_met_quartiles2 ~ Sit_med + Education_3cat_recod + Ocupation_3cat + Bills + Age + Gender + `Marital status` + Sizeofcommunity, data = eu2017); summary(mm1); exp(coef(mm1)); exp(confint(mm1))
z1 <- summary(mm1)$coefficients/summary(mm1)$standard.errors; z1
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2; p1 # 2-tailed z test


eu2017$Sit_4_groups2 <- relevel(eu2017$Sit_4_groups, ref = "<=4.5h")
mm2 <- multinom(Sit_4_groups2 ~ MVPA_tot_time_pluswalk + Education_3cat_recod + Ocupation_3cat + Bills + Age + Gender + `Marital status` + Sizeofcommunity, data = eu2017); summary(mm2); exp(coef(mm2)); exp(confint(mm2))
z2 <- summary(mm2)$coefficients/summary(mm2)$standard.errors; z2
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2; p2 # 2-tailed z test

tab_model(mm1, mm2)


#### Pooled plot ####
Pool <- read.csv("C:/Users/anton/Desktop/Figuras tesis/archivos datos/OR, SB, PA and SES_time_2.0_pooled.csv", header = T, dec = ",", sep = ";")

Pool$SES_cat <- factor(Pool$SES_cat, order = TRUE, 
                levels = c("Secundaria", "Universitario", "III-IV", "I-II", "Ocasionalmente", "Nunca"))

Pool$Activity <- factor(Pool$Activity, order = TRUE, 
                       levels = c("Actividad Física Total", "Sedentarismo"))

ggplot(Pool, aes(x=Quartile, y=OR, color=Color)) + 
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    geom_point(position=position_dodge(.5), shape=16, size=2.5) +
    geom_errorbar(aes(ymin=LL, ymax=UL), width=.3, position=position_dodge(.5)) + 
    facet_grid(SES_cat ~ Activity, switch = "y") + coord_flip() + 
    geom_text(data = Pool, label = Pool$Text, nudge_x = 0.35, y= 3.25, size = 3, color="black", family="Times")+
    ylim(0.51, 4) + theme_classic() + theme(axis.title.y = element_blank(), strip.background = element_rect(color = "white",
        fill = "grey95"), legend.position = "none", axis.title.x = element_text(face = "bold", family = "Times"),
        strip.text.x = element_text(family = "Times", face = "bold"), strip.text.y = element_text(family = "Times"),
        axis.text = element_text(family = "Times")) + scale_color_manual(values = c("dodgerblue3", "dodgerblue4", "firebrick4", "orange1", "red", "gold"))+
    ylab("Odds Ratio IC 95%")

ggsave("OR.tiff", width = 8.5, height = 7, units = "in", dpi = 600)


col2rgb("gold", alpha = FALSE)

#### Proportion Plot ####
library(scales)

Educ_quartiles_AF_3 <- melt(Educ_quartiles_AF_2, id = c("Education_3cat_recod"),
                            measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                            variable.name = "Quartile",
                            value.name = "prev" ) 

Educ_quartiles_AF_3$Quartile <- factor(Educ_quartiles_AF_3$Quartile, order = TRUE, 
                                       levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
Educ_quartiles_AF_3$prev <- Educ_quartiles_AF_3$prev/100


Educ_quartiles_AF_3$Education_3cat_recod <- factor(Educ_quartiles_AF_3$Education_3cat_recod, levels = c("Primary", "Secondary", "University"),
                                          labels = c("Primaria", "Secundaria", "Universitario"))


a <- ggplot(Educ_quartiles_AF_3, aes(x=reorder(Education_3cat_recod,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.07, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"),
          legend.text = element_text(family="Times"))+ 
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))

a


Educ_quartiles_SB_3 <- melt(Educ_quartiles_SB_2, id = c("Education_3cat_recod"),
                            measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                            variable.name = "Quartile",
                            value.name = "prev" ) 

Educ_quartiles_SB_3$Quartile <- factor(Educ_quartiles_SB_3$Quartile, order = TRUE, 
                                       levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
Educ_quartiles_SB_3$prev <- Educ_quartiles_SB_3$prev/100


Educ_quartiles_SB_3$Education_3cat_recod <- factor(Educ_quartiles_SB_3$Education_3cat_recod, levels = c("Primary", "Secondary", "University"),
                                          labels = c("Primaria", "Secundaria", "Universitario"))


b <- ggplot(Educ_quartiles_SB_3, aes(x=reorder(Education_3cat_recod,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.07, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"))+
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))






SC_quartiles_AF_3 <- melt(SC_quartiles_AF_2, id = c("Ocupation_3cat"),
                          measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                          variable.name = "Quartile",
                          value.name = "prev" ) 

SC_quartiles_AF_3$Quartile <- factor(SC_quartiles_AF_3$Quartile, order = TRUE, 
                                     levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
SC_quartiles_AF_3$prev <- SC_quartiles_AF_3$prev/100


SC_quartiles_AF_3$Ocupation_3cat <- factor(SC_quartiles_AF_3$Ocupation_3cat, order = TRUE, 
                                   levels = c("V-VII", "III-IV", "I-II"))

c <- ggplot(SC_quartiles_AF_3, aes(x=reorder(Ocupation_3cat,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.07, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"))+
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))




SC_quartiles_SB_3 <- melt(SC_quartiles_SB_2, id = c("Ocupation_3cat"),
                          measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                          variable.name = "Quartile",
                          value.name = "prev" ) 

SC_quartiles_SB_3$Quartile <- factor(SC_quartiles_SB_3$Quartile, order = TRUE, 
                                     levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
SC_quartiles_SB_3$prev <- SC_quartiles_SB_3$prev/100


SC_quartiles_SB_3$Ocupation_3cat <- factor(SC_quartiles_SB_3$Ocupation_3cat, order = TRUE, 
                                   levels = c("V-VII", "III-IV", "I-II"))

d <- ggplot(SC_quartiles_SB_3, aes(x=reorder(Ocupation_3cat,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.01, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"))+
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))







Bills_quartiles_AF_3 <- melt(Bills_quartiles_AF_2, id = c("Bills"),
                             measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                             variable.name = "Quartile",
                             value.name = "prev" ) 

Bills_quartiles_AF_3$Quartile <- factor(Bills_quartiles_AF_3$Quartile, order = TRUE, 
                                        levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
Bills_quartiles_AF_3$prev <- Bills_quartiles_AF_3$prev/100

Bills_quartiles_AF_3$Bills <- factor(Bills_quartiles_AF_3$Bills, levels = c("Most of the time", "From time to time", "Almost never/never"),
                            labels = c("Mayoria del tiempo", "Ocasionalmente", "Nunca"))


e <- ggplot(Bills_quartiles_AF_3, aes(x=reorder(Bills,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.07, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"))+
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))



Bills_quartiles_SB_3 <- melt(Bills_quartiles_SB_2, id = c("Bills"),
                             measure = c("Q4_prev", "Q3_prev", "Q2_prev", "Q1_prev"),
                             variable.name = "Quartile",
                             value.name = "prev" ) 

Bills_quartiles_SB_3$Quartile <- factor(Bills_quartiles_SB_3$Quartile, order = TRUE, 
                                        levels = c("Q1_prev", "Q2_prev", "Q3_prev", "Q4_prev"))
Bills_quartiles_SB_3$prev <- Bills_quartiles_SB_3$prev/100

Bills_quartiles_SB_3$Bills <- factor(Bills_quartiles_SB_3$Bills, levels = c("Most of the time", "From time to time", "Almost never/never"),
                            labels = c("Mayoria del tiempo", "Ocasionalmente", "Nunca"))


f <- ggplot(Bills_quartiles_SB_3, aes(x=reorder(Bills,Quartile,function(x)+sum(prev)), y=prev, fill=Quartile)) +
    geom_bar(stat='identity',  width = .5, size=0.5) +
    scale_y_continuous(labels = percent_format()) +
    geom_text(aes(label=ifelse(prev >= 0.07, paste0(sprintf("%.0f", prev*100),"%"),"")),
              position=position_stack(vjust=0.5), colour="black", family="Times")+
    labs(y="", x="", fill="Quartile") + theme_classic()+
    theme(legend.title = element_blank(), axis.text = element_text(family="Times"))+
    coord_cartesian(ylim = c(0.035,1))+
    scale_fill_manual(labels = c("Q1", "Q2", "Q3", "Q4"), values = c("red", "orange1", "yellow1", "springgreen1"))

rgb("gold")

library(grid)
ggunit2 <- ggarrange(a, b, c, d, e, f, legend = "right",
                     ncol = 2, nrow = 3, common.legend = T, align = "hv")

text3 <- grobTree(textGrob("Actividad Física Total", x=0.26,  y=1.065, gp=gpar(fontface = "bold", fontfamily="Times")))
text4 <- grobTree(textGrob("Sedentarismo", x=0.725,  y=1.065, gp=gpar(fontface = "bold", fontfamily="Times")))

text5 <- grobTree(textGrob("Nivel educativo", x=0.495,  y=1.02, gp=gpar(fontface = "bold", fontfamily="Times")))
text6 <- grobTree(textGrob("Clase social ocupacional", x=0.495,  y=.68, gp=gpar(fontface = "bold", fontfamily="Times")))
text7 <- grobTree(textGrob("Dificultades económicas", x=0.495,  y=.35, gp=gpar(fontface = "bold", fontfamily="Times"))) 


ggunit2 + theme(plot.margin = unit(c(2,0,0,0), "cm")) + annotation_custom(text3) + annotation_custom(text4) + annotation_custom(text5)+
    annotation_custom(text6)+ annotation_custom(text7)

ggsave("Quartiles_SES.tiff", width = 9, height = 9, units = "in", dpi = 600)









