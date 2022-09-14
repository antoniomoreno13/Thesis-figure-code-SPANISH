
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("MASS")
library("sfsmisc")
library("QuantPsyc")
library("forcats")
library(reshape2)
library(extrafont)
library(colortools)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))


#### SURVEYS HANDLING ####
# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224, 277, 242, 236, 237:239, 270, 11, 104:106, 240, 407)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:30] <- c("Gender", "Age", "Social class subjective", "Type of community","Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Bills", "Country", "Oport_area", "Oport_clubs", "Oport_autorities_not_enough", "Ocupation_last_job", "weight")
eu2017["survey"] <- "2017" # TIME VARIABLE


eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The upper middle class of society"] <- "The higher class of society"
eu2017$`Social class subjective`[eu2017$`Social class subjective` == "The lower middle class of society"] <- "The working class of society"


eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")

# 2013
eu2013 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2013.sav", to.data.frame = T, use.missings = T)
eu2013 <- eu2013[c(6, 8, 374:375, 376:377, 378:382, 383, 386:387, 390:391, 394, 395:396, 465, 466,500,474,464, 469:471,494, 10, 442:444, 472, 12)]
names(eu2013)[2] <- "ID"
names(eu2013)[3:19] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_Daily","Sport_PA_Few_times","Sport_PA_occasionally","Sport_PA_never", "Sport_PA_DK",  "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2013)[20:34] <- c("Gender", "Age", "Social class subjective", "Type of community","Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Bills", "Country", "Oport_area", "Oport_clubs", "Oport_autorities_not_enough", "Ocupation_last_job", "weight")
eu2013["ID"] <- seq(from=28032,to=55950) # CONTINUE 2017 NUMBER OF CASES
eu2013["survey"] <- "2013" # TIME VARIABLE
eu2013 <- eu2013[which(eu2013$Sport_PA_DK == "Not mentioned"), ] # REMOVE NOT VALID CASES

eu2013$Sport_PA_Daily2[eu2013$Sport_PA_Daily == "Mentioned"] <- 1000 ; eu2013$Sport_PA_Daily2[eu2013$Sport_PA_Daily != "Mentioned"] <- 0  # JOINT IN A UNIQUE VARIABLE LIKE EU 2017
eu2013$Sport_PA_Few_times2[ eu2013$Sport_PA_Few_times == "Mentioned"] <- 100  ;eu2013$Sport_PA_Few_times2[ eu2013$Sport_PA_Few_times != "Mentioned"] <- 0  
eu2013$Sport_PA_occasionally2 [ eu2013$Sport_PA_occasionally == "Mentioned" ] <- 10  ;eu2013$Sport_PA_occasionally2 [ eu2013$Sport_PA_occasionally != "Mentioned" ] <- 0  
eu2013$Sport_PA_never2 [ eu2013$Sport_PA_never == "Mentioned" ] <- 1  ;eu2013$Sport_PA_never2 [ eu2013$Sport_PA_never != "Mentioned" ] <- 0  

eu2013$Sport_PA_freq2 <- eu2013$Sport_PA_Daily2 + eu2013$Sport_PA_Few_times2 + eu2013$Sport_PA_occasionally2 + eu2013$Sport_PA_never2

eu2013$Sport_PA_freq [ eu2013$Sport_PA_freq2 >= 1000 ] <- "Almost daily"
eu2013$Sport_PA_freq [ eu2013$Sport_PA_freq2 >= 100 & eu2013$Sport_PA_freq2 < 1000] <- "A few times a week"
eu2013$Sport_PA_freq [ eu2013$Sport_PA_freq2 >= 10 & eu2013$Sport_PA_freq2 < 100 ] <- "Occasionally"
eu2013$Sport_PA_freq [ eu2013$Sport_PA_freq2 >= 1 & eu2013$Sport_PA_freq2 < 10 ] <- "Never"

eu2013$Sport_PA_freq <- as.factor(eu2013$Sport_PA_freq)

# 2005
eu2005 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Trajectories of rural-urban differences in physical activity in the European Union_EN REVISION/Modified Euro 2005, 2002/Eu 2005.sav", to.data.frame = T, use.missings = T)
eu2005 <- eu2005[c(6, 867:881, 1060, 808, 1065, 1058:1059, 1063, 1170, 7, 886, 887, 890, 1064, 9)]
names(eu2005)[1] <- "ID"
names(eu2005)[2:16] <- c("Vig_Days", "Vig_Time",  "Vig_Time_hrs", "Vig_Time_med", "Mod_Days", "Mod_Time", "Mod_Time_hrs", "Mod_Time_med","Walk_Days", "Walk_Time", "Walk_Time_hrs", "Walk_Time_med", "Sit_rec", "Sit_rec2", "Sit_med") 
names(eu2005)[17:29] <- c( "Age", "Gender", "Type of community","Education2", "Education" , "Ocupation", "Ocupation_rec1", "Country", "Oport_area", "Oport_clubs", "Oport_autorities_not_enough", "Ocupation_last_job", "weight")
eu2005["ID"] <- seq(from=55951,to=85143) # CONTINUE 2005 NUMBER OF CASES
eu2005["survey"] <- "2005" # TIME VARIABLE
eu2005 <- eu2005[-c(3,4,7,8,11,12,14,15)] # Be careful, in this survey Time (min) is a week

      # Treat PA questions
eu2005$Vig_Days <- as.numeric(eu2005$Vig_Days); eu2005$Vig_Days <- eu2005$Vig_Days - 1
eu2005$Mod_Days <- as.numeric(eu2005$Mod_Days); eu2005$Mod_Days <- eu2005$Mod_Days - 1
eu2005$Walk_Days <- as.numeric(eu2005$Walk_Days); eu2005$Walk_Days <- eu2005$Walk_Days - 1

eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   == 0 ] <- 0        # Recoding IPAQ time per day answer from 2005 to 2017 categories
eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   > 0  & eu2005$Vig_Time_med   <= 30] <- 15
eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   > 30 & eu2005$Vig_Time_med   <= 60] <- 45
eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   > 60 & eu2005$Vig_Time_med   <= 90] <- 75
eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   > 90 & eu2005$Vig_Time_med   <= 120] <- 105
eu2005$Vig_Time_med  [ eu2005$Vig_Time_med   > 120 ] <- 135

eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   == 0 ] <- 0
eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   > 0  & eu2005$Mod_Time_med   <= 30] <- 15
eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   > 30 & eu2005$Mod_Time_med   <= 60] <- 45
eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   > 60 & eu2005$Mod_Time_med   <= 90] <- 75
eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   > 90 & eu2005$Mod_Time_med   <= 120] <- 105
eu2005$Mod_Time_med  [ eu2005$Mod_Time_med   > 120 ] <- 135

eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   == 0 ] <- 0
eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   > 0  & eu2005$Walk_Time_med   <= 30] <- 15
eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   > 30 & eu2005$Walk_Time_med   <= 60] <- 45
eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   > 60 & eu2005$Walk_Time_med   <= 90] <- 75
eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   > 90 & eu2005$Walk_Time_med   <= 120] <- 105
eu2005$Walk_Time_med  [ eu2005$Walk_Time_med   > 120 ] <- 135


eu2005$VPA_tot_time <- eu2005$Vig_Days * eu2005$Vig_Time_med 
eu2005$MPA_tot_time <- eu2005$Mod_Days * eu2005$Mod_Time_med 
eu2005$Walk_tot_time<- eu2005$Walk_Days * eu2005$Walk_Time_med 

eu2005$Mod_plus_walk_tottime <- eu2005$MPA_tot_time + eu2005$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

eu2005$VPA_met <- eu2005$VPA_tot_time * 8
eu2005$MPA_met <- eu2005$MPA_tot_time * 4
eu2005$Walk_met <- eu2005$Walk_tot_time * 3.3 

eu2005$WHO_prev [eu2005$VPA_tot_time >= 75 | eu2005$Mod_plus_walk_tottime >= 150 | (eu2005$Mod_plus_walk_tottime + (2 * eu2005$VPA_tot_time) >= 150)] <- "Active"
eu2005$WHO_prev [which(is.na(eu2005$WHO_prev))] <-"Inactive"
eu2005$WHO_prev [which(is.na(eu2005$Mod_plus_walk_tottime) & (is.na(eu2005$VPA_tot_time)))] <- NA

# 2002
eu2002 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Trajectories of rural-urban differences in physical activity in the European Union_EN REVISION/Modified Euro 2005, 2002/Eu 2002.sav", to.data.frame = T, use.missings = T)
eu2002 <- eu2002[c(6, 226:236, 357, 358, 366, 355:356, 361, 441, 10, 241:243, 362, 11)]
names(eu2002)[1] <- "ID"
names(eu2002)[2:12] <- c("Vig_Days", "Vig_Time", "Vig_Time_med", "Mod_Days", "Mod_Time", "Mod_Time_med","Walk_Days", "Walk_Time", "Walk_Time_med", "Sit", "Sit_med") 
names(eu2002)[13:25] <- c("Gender", "Age", "Type of community","Education2", "Education", "Ocupation", "Ocupation_rec1", "Country", "Oport_area", "Oport_clubs", "Oport_autorities_not_enough", "Ocupation_last_job", "weight")
eu2002["ID"] <- seq(from=85144,to=101373) # CONTINUE 2002 NUMBER OF CASES
eu2002["survey"] <- "2002" # TIME VARIABLE
eu2002 <- eu2002[-c(3,6,9,11)]

# Treat PA questions

eu2002$Vig_Days <- as.numeric(eu2002$Vig_Days);eu2002$Vig_Days <- eu2002$Vig_Days - 1
eu2002$Mod_Days <- as.numeric(eu2002$Mod_Days); eu2002$Mod_Days <- eu2002$Mod_Days - 1
eu2002$Walk_Days <- as.numeric(eu2002$Walk_Days); eu2002$Walk_Days <- eu2002$Walk_Days - 1

eu2002$Vig_Time_med  [eu2002$Vig_Time_med  == 999] <- 0
eu2002$Mod_Time_med  [eu2002$Mod_Time_med  == 9999] <- 0
eu2002$Walk_Time_med [eu2002$Walk_Time_med  == 999] <- 0

        # Recoding IPAQ time per day answer from 2002 to 2017 categories
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   == 0 ] <- 0
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   > 0  & eu2002$Vig_Time_med   <= 30] <- 15
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   > 30 & eu2002$Vig_Time_med   <= 60] <- 45
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   > 60 & eu2002$Vig_Time_med   <= 90] <- 75
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   > 90 & eu2002$Vig_Time_med   <= 120] <- 105
eu2002$Vig_Time_med  [ eu2002$Vig_Time_med   > 120 ] <- 135

eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   == 0 ] <- 0
eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   > 0  & eu2002$Mod_Time_med   <= 30] <- 15
eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   > 30 & eu2002$Mod_Time_med   <= 60] <- 45
eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   > 60 & eu2002$Mod_Time_med   <= 90] <- 75
eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   > 90 & eu2002$Mod_Time_med   <= 120] <- 105
eu2002$Mod_Time_med  [ eu2002$Mod_Time_med   > 120 ] <- 135

eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   == 0 ] <- 0
eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   > 0  & eu2002$Walk_Time_med   <= 30] <- 15
eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   > 30 & eu2002$Walk_Time_med   <= 60] <- 45
eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   > 60 & eu2002$Walk_Time_med   <= 90] <- 75
eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   > 90 & eu2002$Walk_Time_med   <= 120] <- 105
eu2002$Walk_Time_med  [ eu2002$Walk_Time_med   > 120 ] <- 135


eu2002$VPA_tot_time <- eu2002$Vig_Days * eu2002$Vig_Time_med 
eu2002$MPA_tot_time <- eu2002$Mod_Days * eu2002$Mod_Time_med 
eu2002$Walk_tot_time <- eu2002$Walk_Days * eu2002$Walk_Time_med 

eu2002$Mod_plus_walk_tottime <- eu2002$MPA_tot_time + eu2002$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

eu2002$VPA_met <- eu2002$VPA_tot_time * 8
eu2002$MPA_met <- eu2002$MPA_tot_time * 4
eu2002$Walk_met <- eu2002$Walk_tot_time * 3.3 

eu2002$WHO_prev [eu2002$VPA_tot_time >= 75 | eu2002$Mod_plus_walk_tottime >= 150 | (eu2002$Mod_plus_walk_tottime + (2 * eu2002$VPA_tot_time) >= 150)] <- "Active"
eu2002$WHO_prev [which(is.na(eu2002$WHO_prev))] <-"Inactive"
eu2002$WHO_prev [which(is.na(eu2002$Mod_plus_walk_tottime) & (is.na(eu2002$VPA_tot_time)))] <- NA

# Joint Eurobarometers 2005 & 2002
euTotal2 <- bind_rows(eu2005, eu2002)
cols2 <- c(10:20, 22 ,30); euTotal2[cols2] <- lapply(euTotal2[cols2], factor) 

# Joint Eurobarometers 2017 & 2013
euTotal <- bind_rows(eu2017, eu2013); euTotal <- euTotal[-c(31:40)]
cols <- c(1, 3:16, 18:29); euTotal[cols] <- lapply(euTotal[cols], factor) 

# SITTING TIME 

euTotal$Sit[euTotal$Sit == "DK"] <- NA
euTotal$Sit_med [euTotal$Sit == "1 hour or less"] <- 30
euTotal$Sit_med [euTotal$Sit == "1 hour to 1h30min" | euTotal$Sit == "1 hour to 1 hour and 30 minutes"] <- 75
euTotal$Sit_med [euTotal$Sit == "1h31min to 2h30min" | euTotal$Sit == "1 hour 31 minutes to 2 hours 30 minutes"] <- 120
euTotal$Sit_med [euTotal$Sit == "2h31min to 3h30min" | euTotal$Sit == "2 hours 31 minutes to 3 hours 30 minutes"] <- 180
euTotal$Sit_med [euTotal$Sit == "3h31min to 4h30min" | euTotal$Sit == "3 hours 31 minutes to 4 hours 30 minutes"] <- 240
euTotal$Sit_med [euTotal$Sit == "4h31min to 5h30min" | euTotal$Sit == "4 hours 31 minutes to 5 hours 30 minutes"] <- 300
euTotal$Sit_med [euTotal$Sit == "5h31min to 6h30min" | euTotal$Sit == "5 hours 31 minutes to 6 hours 30 minutes"] <- 360
euTotal$Sit_med [euTotal$Sit == "6h31min to 7h30min" | euTotal$Sit == "6 hours 31 minutes to 7 hours 30 minutes"] <- 420
euTotal$Sit_med [euTotal$Sit == "7h31min to 8h30min" | euTotal$Sit == "7 hours 31 minutes to 8 hours 30 minutes"] <- 480
euTotal$Sit_med [euTotal$Sit == "More than 8h30min" | euTotal$Sit == "More than 8 hours and 30 minutes"] <- 540

euTotal2$Sit_med [ euTotal2$Sit_med < 60 ] <- 30                           # Recoding continous sitting time from 2002 & 2005 to 2017 categories and median values
euTotal2$Sit_med [ euTotal2$Sit_med >= 60 &  euTotal2$Sit_med <= 90] <- 75
euTotal2$Sit_med [ euTotal2$Sit_med > 90 &   euTotal2$Sit_med <= 150] <- 120
euTotal2$Sit_med [ euTotal2$Sit_med > 150 &  euTotal2$Sit_med <= 210] <- 180
euTotal2$Sit_med [ euTotal2$Sit_med > 210 &  euTotal2$Sit_med <= 270] <- 240
euTotal2$Sit_med [ euTotal2$Sit_med > 270 &  euTotal2$Sit_med <= 330] <- 300
euTotal2$Sit_med [ euTotal2$Sit_med > 330 &  euTotal2$Sit_med <= 390] <- 360
euTotal2$Sit_med [ euTotal2$Sit_med > 390 &  euTotal2$Sit_med <= 450] <- 420
euTotal2$Sit_med [ euTotal2$Sit_med > 450 &  euTotal2$Sit_med <= 510] <- 480
euTotal2$Sit_med [ euTotal2$Sit_med > 510 ] <- 540


# IPAQ, VPA, MPA, MVPA calculation in METs (and Time per Week)

euTotal$Vig_Days <- as.numeric(euTotal$Vig_Days) # TO CONTINIOUS
euTotal$Mod_Days <- as.numeric(euTotal$Mod_Days)
euTotal$Walk_Days <- as.numeric(euTotal$Walk_Days)

euTotal$Vig_Days[which(is.na(euTotal$Vig_Days))] <- 0 # RECODING AND  MISSING VALUES RECOVERY
euTotal$Vig_Days[euTotal$Vig_Days == 8] <- 0
euTotal$Vig_Days[euTotal$Vig_Days == 9] <- NA
euTotal$Vig_Time[which(is.na(euTotal$Vig_Time))] <- "Never do vigorous physical activities"
euTotal$Vig_Time[euTotal$Vig_Time == "DK"] <- NA

euTotal$Mod_Days[which(is.na(euTotal$Mod_Days))] <- 0
euTotal$Mod_Days[euTotal$Mod_Days == 8] <- 0
euTotal$Mod_Days[euTotal$Mod_Days == 9] <- NA
euTotal$Mod_Time[which(is.na(euTotal$Mod_Time))] <- "Never do moderate physical activities"
euTotal$Mod_Time[euTotal$Mod_Time == "DK"] <- NA

euTotal$Walk_Days[euTotal$Walk_Days == 8] <- 0
euTotal$Walk_Days[euTotal$Walk_Days == 9] <- NA
euTotal$Walk_Time[euTotal$Walk_Time == "DK"] <- NA

      # INTERVAL MEDIAN VALUES BY PA TIME      
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "Never do any vigorous physical activity " | euTotal$Vig_Time  == "Never do vigorous physical activities"] <- 0 
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "30 minutes or less" ] <- 15   
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "31 to 60 minutes" ] <- 45   
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "61 to 90 minutes" ] <- 75   
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "91 to 120 minutes" ] <- 105   
euTotal$Vig_Time_med [ euTotal$Vig_Time  == "More than 120 minutes" ] <- 135   

euTotal$Mod_Time_med [ euTotal$Mod_Time  == "Never do any moderate physical activity " | euTotal$Mod_Time  == "Never do moderate physical activities"] <- 0 
euTotal$Mod_Time_med [ euTotal$Mod_Time  == "30 minutes or less" ] <- 15   
euTotal$Mod_Time_med [ euTotal$Mod_Time  == "31 to 60 minutes" ] <- 45   
euTotal$Mod_Time_med [ euTotal$Mod_Time  == "61 to 90 minutes" ] <- 75   
euTotal$Mod_Time_med [ euTotal$Mod_Time  == "91 to 120 minutes" ] <- 105   
euTotal$Mod_Time_med [ euTotal$Mod_Time  == "More than 120 minutes" ] <- 135  

euTotal$Walk_Time_med [euTotal$Walk_Time == "Never walk for 10 minutes at a time"] <- 0
euTotal$Walk_Time_med [euTotal$Walk_Time == "30 minutes or less"] <- 15
euTotal$Walk_Time_med [euTotal$Walk_Time == "31 to 60 minutes"] <- 45
euTotal$Walk_Time_med [euTotal$Walk_Time == "61 to 90 minutes"] <- 75
euTotal$Walk_Time_med [euTotal$Walk_Time == "91 to 120 minutes"] <- 105
euTotal$Walk_Time_med [euTotal$Walk_Time == "More than 120 minutes"] <- 135


      # TIME PER WEEK
euTotal$VPA_tot_time <- euTotal$Vig_Days * euTotal$Vig_Time_med
euTotal$MPA_tot_time <- euTotal$Mod_Days * euTotal$Mod_Time_med
euTotal$Walk_tot_time <- euTotal$Walk_Days * euTotal$Walk_Time_med

euTotal$Mod_plus_walk_tottime <- euTotal$MPA_tot_time + euTotal$Walk_tot_time # MOD + WALK WEEKLY TIME TO WHO COMPLIANCE 

      # METS PER WEEK
euTotal$VPA_met <- euTotal$VPA_tot_time * 8
euTotal$MPA_met <- euTotal$MPA_tot_time * 4
euTotal$Walk_met <- euTotal$Walk_tot_time * 3.3 

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA)

euTotal$WHO_prev [euTotal$VPA_tot_time >= 75 | euTotal$Mod_plus_walk_tottime >= 150 | (euTotal$Mod_plus_walk_tottime + (2 * euTotal$VPA_tot_time) >= 150)] <- "Active"
euTotal$WHO_prev [which(is.na(euTotal$WHO_prev))] <-"Inactive"
euTotal$WHO_prev [which(is.na(euTotal$Mod_plus_walk_tottime) & (is.na(euTotal$VPA_tot_time)))] <- NA


# Joint four eurobarometers
euTotal3 <- bind_rows(euTotal, euTotal2)
cols3 <- c(1,16,19:29, 42); euTotal3[cols3] <- lapply(euTotal3[cols3], factor) 
euTotal3 <- euTotal3[-c(22,23,43, 3:7, 15)]

# COMPUTE MVPA METs 

euTotal3$MVPA_met <- euTotal3$VPA_met + euTotal3$MPA_met + euTotal3$Walk_met
euTotal3$MVPA_met_outwalk <- euTotal3$VPA_met + euTotal3$MPA_met


#### COVARIATES REORDERING, ASIGNING MISSING VALUES AND DROPPING USELESS LEVELS ####
euTotal3$Bills[euTotal3$Bills == "Refusal (SPONT.)"] <- NA; euTotal3$Bills<- fct_drop(euTotal3$Bills, only = "Refusal (SPONT.)")

euTotal3$`Type of community`[euTotal3$`Type of community` == "DK"] <- NA; euTotal3$`Type of community`[euTotal3$`Type of community` == "Small or middle sized town"] <- "Small/middle town";
euTotal3$`Type of community`<- fct_drop(euTotal3$`Type of community`, only = c("DK", "Small or middle sized town"))

euTotal3$Typeofcommunity2cat[euTotal3$`Type of community` == "Small/middle town"] <- "Urban"; euTotal3$Typeofcommunity2cat[euTotal3$`Type of community` == "Large town"] <- "Urban"; 
euTotal3$Typeofcommunity2cat[euTotal3$`Type of community` == "Rural area or village"] <- "Rural"


euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "DK"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "None (SPONTANEOUS)"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "None (SPONT.)"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "Other (SPONT.)"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "Other (SPONTANEOUS)"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "Refusal (SPONT.)"] <- NA
euTotal3$`Social class subjective`[euTotal3$`Social class subjective` == "The upper class of society"] <- "The higher class of society"
euTotal3$`Social class subjective`<- fct_drop(euTotal3$`Social class subjective`, only = c("The upper class of society", "Refusal (SPONT.)", "Other (SPONTANEOUS)", "Other (SPONT.)","None (SPONT.)" , "None (SPONTANEOUS)", "DK"))

euTotal3$Gender[euTotal3$Gender == "Male"] <- "Man"; euTotal3$Gender[euTotal3$Gender == "Female"] <- "Woman"
euTotal3$Gender<- fct_drop(euTotal3$Gender, only = c("Female", "Male"))

euTotal3$Age <- as.numeric(euTotal3$Age)

euTotal3$Age_3clusters [ euTotal3$Age >= 18 & euTotal3$Age < 45 ] <- "18-44"  
euTotal3$Age_3clusters [ euTotal3$Age >= 45 & euTotal3$Age < 70 ] <- "45-69"  
euTotal3$Age_3clusters [ euTotal3$Age >= 70] <- "70+"  

euTotal3$Age_3clusters2 [ euTotal3$Age >= 18 & euTotal3$Age < 35 ] <- "18-34"  
euTotal3$Age_3clusters2 [ euTotal3$Age >= 35 & euTotal3$Age < 65 ] <- "35-64"  
euTotal3$Age_3clusters2 [ euTotal3$Age >= 65] <- "65+"  

euTotal3$Age_6clusters [ euTotal3$Age >= 15 & euTotal3$Age < 25 ] <- "15-24"  
euTotal3$Age_6clusters [ euTotal3$Age >= 25 & euTotal3$Age < 35 ] <- "25-34"  
euTotal3$Age_6clusters [ euTotal3$Age >= 35 & euTotal3$Age < 45 ] <- "35-44"  
euTotal3$Age_6clusters [ euTotal3$Age >= 45 & euTotal3$Age < 55 ] <- "45-54"  
euTotal3$Age_6clusters [ euTotal3$Age >= 55 & euTotal3$Age < 65 ] <- "55-64"  
euTotal3$Age_6clusters [ euTotal3$Age >= 65 ] <- "65+"  

euTotal3$Age_9clusters [ euTotal3$Age == 18] <- "18"  
euTotal3$Age_9clusters [ euTotal3$Age == 19] <- "19"  
euTotal3$Age_9clusters [ euTotal3$Age >= 20 & euTotal3$Age < 30 ] <- "20-29"  
euTotal3$Age_9clusters [ euTotal3$Age >= 30 & euTotal3$Age < 40 ] <- "30-39"  
euTotal3$Age_9clusters [ euTotal3$Age >= 40 & euTotal3$Age < 50 ] <- "40-49"  
euTotal3$Age_9clusters [ euTotal3$Age >= 50 & euTotal3$Age < 60 ] <- "50-59"  
euTotal3$Age_9clusters [ euTotal3$Age >= 60 & euTotal3$Age < 70 ] <- "60-69"  
euTotal3$Age_9clusters [ euTotal3$Age >= 70 & euTotal3$Age < 80 ] <- "70-79"  
euTotal3$Age_9clusters [ euTotal3$Age >= 80 ] <- "80+"  



euTotal3$Country[euTotal3$Country == "Austria"] <- "AT - Austria"
euTotal3$Country[euTotal3$Country == "Belgium"] <- "BE - Belgium"
euTotal3$Country[euTotal3$Country == "Bulgaria"] <-"BG - Bulgaria"
euTotal3$Country[euTotal3$Country == "Cyprus (Republic)"] <- "CY - Cyprus (Republic)"
euTotal3$Country[euTotal3$Country == "Czech Republic"] <- "CZ - Czech Republic"
euTotal3$Country[euTotal3$Country == "Denmark"] <- "DK - Denmark"
euTotal3$Country[euTotal3$Country == "Estonia"] <- "EE - Estonia"
euTotal3$Country[euTotal3$Country == "Finland"] <- "FI - Finland"
euTotal3$Country[euTotal3$Country == "France"] <- "FR - France" 
euTotal3$Country[euTotal3$Country == "Germany (East)" | euTotal3$Country == "Germany East" ] <- "DE-E Germany East"
euTotal3$Country[euTotal3$Country == "Germany (West)" | euTotal3$Country ==  "Germany West" ] <- "DE-W - Germany - West"
euTotal3$Country[euTotal3$Country == "Greece"] <- "GR - Greece"
euTotal3$Country[euTotal3$Country == "Croatia"] <- "HR - Croatia"
euTotal3$Country[euTotal3$Country == "Hungary" ] <- "HU - Hungary"
euTotal3$Country[euTotal3$Country == "Ireland"] <- "IE - Ireland"
euTotal3$Country[euTotal3$Country == "Italy" ] <- "IT - Italy"
euTotal3$Country[euTotal3$Country == "Latvia"] <- "LV - Latvia" 
euTotal3$Country[euTotal3$Country == "Lithuania"] <- "LT - Lithuania"
euTotal3$Country[euTotal3$Country == "Luxembourg"] <- "LU - Luxembourg"
euTotal3$Country[euTotal3$Country == "Malta" ] <- "MT - Malta"
euTotal3$Country[euTotal3$Country == "The Netherlands"] <- "NL - The Netherlands"
euTotal3$Country[euTotal3$Country == "Northern Ireland"] <- "GB-NIR Northern Ireland"
euTotal3$Country[euTotal3$Country == "Poland" ] <- "PL - Poland"
euTotal3$Country[euTotal3$Country == "Portugal"] <- "PT - Portugal"
euTotal3$Country[euTotal3$Country == "Romania" ] <- "RO - Romania"
euTotal3$Country[euTotal3$Country == "Sweden" ] <- "SE - Sweden"
euTotal3$Country[euTotal3$Country ==  "Slovenia"] <-  "SI - Slovenia"
euTotal3$Country[euTotal3$Country == "Slovakia"] <- "SK - Slovakia" 
euTotal3$Country[euTotal3$Country == "Spain"] <- "ES -Spain"
euTotal3$Country[euTotal3$Country == "Great Britain"] <- "GB-GBN - Great Britain"
euTotal3$Country[euTotal3$Country == "Austria"] <- "AT - Austria"
euTotal3$Country<- fct_drop(euTotal3$Country, only = c("Austria","Belgium","Bulgaria", "Cyprus (Republic)", "Czech Republic", "Denmark", "Estonia" ,"Finland", "France",
                                                     "Germany (East)", "Germany East","Germany (West)", "Germany West", "Greece","Croatia", "Hungary", "Ireland", "Italy",
                                                     "Latvia","Lithuania","Luxembourg", "Malta", "The Netherlands", "Northern Ireland", "Poland", "Portugal", "Romania", "Sweden",
                                                     "Slovenia", "Slovakia", "Spain", "Great Britain","Austria" ))

euTotal3$Country_rec <- euTotal3$Country; euTotal3$Country_rec <- fct_expand(euTotal3$Country_rec, c("DE Germany", "UK United Kingdom")) 
euTotal3$Country_rec[euTotal3$Country_rec == "DE-W - Germany - West" | euTotal3$Country_rec =="DE-E Germany East" ] <- "DE Germany"
euTotal3$Country_rec[euTotal3$Country_rec ==  "GB-NIR Northern Ireland" | euTotal3$Country_rec =="GB-GBN - Great Britain"] <- "UK United Kingdom"
euTotal3$Country_rec<- fct_drop(euTotal3$Country_rec, only = c("DE-W - Germany - West", "DE-E Germany East", "GB-NIR Northern Ireland", "GB-GBN - Great Britain"))




euTotal3$Education[euTotal3$Education == "Still Studying"] <- "Still studying"
euTotal3$Education[euTotal3$Education == "16-19"] <- "16-19 years"
euTotal3$Education[euTotal3$Education == "15 years"] <- "Up to 15 years"
euTotal3$Education[euTotal3$Education == "16 years" | euTotal3$Education == "17 years" | euTotal3$Education == "18 years" | euTotal3$Education == "19 years"] <- "16-19 years"
euTotal3$Education[euTotal3$Education == "20 years" | euTotal3$Education == "21 years" | euTotal3$Education == "22 years and older" ] <- "20 years and older"
euTotal3$Education[euTotal3$Education == "Refusal_duplicated_7"] <- NA
euTotal3$Education<- fct_drop(euTotal3$Education, only = c("Still Studying", "16-19", "Refusal_duplicated_7",  "15 years",  "16 years",  "17 years",  "18 years",  "19 years", "20 years", "21 years", "22 years and older"))

euTotal3$Education_3cat[euTotal3$Education == "Still studying" & euTotal3$Age <= 15] <- "Up to 15 years"
euTotal3$Education_3cat[euTotal3$Education == "Still studying" & euTotal3$Age > 15 & euTotal3$Age < 20] <- "16-19 years"
euTotal3$Education_3cat[euTotal3$Education == "Still studying" & euTotal3$Age >= 20] <- "20 years and older"
euTotal3$Education_3cat[euTotal3$Education == "Up to 14 years"] <- "Up to 15 years or less"
euTotal3$Education_3cat[euTotal3$Education == "Up to 15 years"] <- "Up to 15 years or less"
euTotal3$Education_3cat[euTotal3$Education == "16-19 years"] <- "16-19 years"
euTotal3$Education_3cat[euTotal3$Education == "20 years and older"] <- "20 years and older"
euTotal3$Education_3cat <- as.factor(euTotal3$Education_3cat)

euTotal3$Education_3cat_recod[euTotal3$Education_3cat == "Up to 15 years" | euTotal3$Education_3cat == "Up to 15 years or less"] <- "Primary"
euTotal3$Education_3cat_recod[euTotal3$Education_3cat == "16-19 years"] <- "Secondary"
euTotal3$Education_3cat_recod[euTotal3$Education_3cat == "20 years and older"] <- "University"
euTotal3$Education_3cat_recod <- as.factor(euTotal3$Education_3cat_recod)




euTotal3$Ocupation[euTotal3$Ocupation == "Business proprietors, etc."] <- "Business proprietors, owner (full or partner) of a company"
euTotal3$Ocupation[euTotal3$Ocupation == "Employed position, service job"] <- "Employed position, not at a desk, but in a service job"
euTotal3$Ocupation[euTotal3$Ocupation == "Employed position, travelling"] <- "Employed position, not at a desk, but travelling"
euTotal3$Ocupation[euTotal3$Ocupation == "Employed position,at desk"] <- "Employed position, at desk"
euTotal3$Ocupation[euTotal3$Ocupation == "Employed position, working mainly at a desk"] <- "Employed position, at desk"
euTotal3$Ocupation[euTotal3$Ocupation == "Employed professional (employed doctor, etc.)"] <- "Employed professional (employed doctor, lawyer, etc.)"
euTotal3$Ocupation[euTotal3$Ocupation == "General management, etc."] <- "General management, director or top management"
euTotal3$Ocupation[euTotal3$Ocupation == "Middle management, etc."] <- "Middle management, other management"
euTotal3$Ocupation[euTotal3$Ocupation == "Owner of a shop, craftsmen, etc."] <- "Owner of a shop, craftsmen, other self employed"
euTotal3$Ocupation[euTotal3$Ocupation == "Professional (lawyer, etc.)"] <- "Professional (lawyer, medical practitioner, etc.)"
euTotal3$Ocupation[euTotal3$Ocupation == "Retired, unable to work"] <- "Retired or unable to work through illness"
euTotal3$Ocupation[euTotal3$Ocupation == "Unemployed, temporarily not working"] <- "Unemployed or temporarily not working"
euTotal3$Ocupation[euTotal3$Ocupation == "Unskilled manual worker, etc."] <- "Other (unskilled) manual worker, servant"
euTotal3$Ocupation<- fct_drop(euTotal3$Ocupation, only = c("Business proprietors, etc.", "Employed position, service job", "Employed position, travelling", "Employed position,at desk", "Employed position, working mainly at a desk", "Employed professional (employed doctor, etc.)", 
                                                           "General management, etc.", "Middle management, etc.", "Owner of a shop, craftsmen, etc.", "Professional (lawyer, etc.)", "Retired, unable to work", "Unemployed, temporarily not working",
                                                           "Unskilled manual worker, etc."))

euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Farmer" | euTotal3$Ocupation == "Fisherman" | euTotal3$Ocupation == "Professional (lawyer, medical practitioner, etc.)" | euTotal3$Ocupation == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation == "Business proprietors, owner (full or partner) of a company"] <- "Self-employed"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Employed professional (employed doctor, lawyer, etc.)"  | euTotal3$Ocupation == "General management, director or top management"  | euTotal3$Ocupation == "Middle management, other management" ] <- "Managers"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Employed position, at desk" | euTotal3$Ocupation == "Employed position, not at a desk, but travelling"] <- "Other White Collars"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Employed position, not at a desk, but in a service job"| euTotal3$Ocupation == "Supervisor" | euTotal3$Ocupation == "Skilled manual worker" | euTotal3$Ocupation == "Other (unskilled) manual worker, servant" ] <- "Manual Workers"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Responsible for ordinary shopping, etc." ] <- "House Persons"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Unemployed or temporarily not working" ] <- "Unemployed"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Retired or unable to work through illness"] <- "Retired"
euTotal3$Ocupation_8cat[euTotal3$Ocupation == "Student"] <- "Students"

euTotal3$Ocupation_5cat[euTotal3$Ocupation == "Employed position, not at a desk, but in a service job" | euTotal3$Ocupation == "Supervisor" | euTotal3$Ocupation == "Skilled manual worker" | euTotal3$Ocupation == "Other (unskilled) manual worker, servant" ] <- "Manual Workers"
euTotal3$Ocupation_5cat[euTotal3$Ocupation == "Farmer" | euTotal3$Ocupation == "Fisherman" | euTotal3$Ocupation == "Professional (lawyer, medical practitioner, etc.)" | euTotal3$Ocupation == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation == "Employed position, at desk" | euTotal3$Ocupation == "Employed position, not at a desk, but travelling" |  euTotal3$Ocupation == "General management, director or top management"  | euTotal3$Ocupation == "Middle management, other management" | euTotal3$Ocupation == "Employed professional (employed doctor, lawyer, etc.)"] <- "Self-employed or White collars workers"
euTotal3$Ocupation_5cat[euTotal3$Ocupation == "Unemployed or temporarily not working" | euTotal3$Ocupation == "Responsible for ordinary shopping, etc."] <- "Economically Inactive"
euTotal3$Ocupation_5cat[euTotal3$Ocupation == "Retired or unable to work through illness"] <- "Retirees"
euTotal3$Ocupation_5cat[euTotal3$Ocupation == "Student"] <- "Students"


euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Supervisor" | euTotal3$Ocupation == "Skilled manual worker" | euTotal3$Ocupation == "Other (unskilled) manual worker, servant" | euTotal3$Ocupation == "Employed position, not at a desk, but in a service job"] <- "V-VII"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Farmer" | euTotal3$Ocupation == "Fisherman" |  euTotal3$Ocupation == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation == "Employed position, at desk" | euTotal3$Ocupation == "Employed position, not at a desk, but travelling" | euTotal3$Ocupation == "Professional (lawyer, medical practitioner, etc.)"] <- "III-IV"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Employed professional (employed doctor, lawyer, etc.)"  |  euTotal3$Ocupation == "General management, director or top management"  | euTotal3$Ocupation == "Middle management, other management" ] <- "I-II"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Unemployed or temporarily not working" | euTotal3$Ocupation == "Responsible for ordinary shopping, etc."] <- NA
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Retired or unable to work through illness"] <- NA
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Student"] <- NA

euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Responsible for ordinary shopping, etc." & (euTotal3$Ocupation_last_job == "Supervisor" | euTotal3$Ocupation_last_job == "Other (unskilled) manual worker, servant" | euTotal3$Ocupation_last_job == "Skilled manual worker" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but in a service job" | euTotal3$Ocupation_last_job == "Unskilled manual worker, etc." | euTotal3$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Unemployed or temporarily not working" & (euTotal3$Ocupation_last_job == "Supervisor" | euTotal3$Ocupation_last_job == "Other (unskilled) manual worker, servant" | euTotal3$Ocupation_last_job == "Skilled manual worker" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but in a service job"| euTotal3$Ocupation_last_job == "Unskilled manual worker, etc." | euTotal3$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Retired or unable to work through illness" & (euTotal3$Ocupation_last_job == "Supervisor" | euTotal3$Ocupation_last_job == "Other (unskilled) manual worker, servant" | euTotal3$Ocupation_last_job == "Skilled manual worker" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but in a service job"| euTotal3$Ocupation_last_job == "Unskilled manual worker, etc." | euTotal3$Ocupation_last_job == "Employed position, service job")] <- "V-VII"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Student" & (euTotal3$Ocupation_last_job == "Supervisor" | euTotal3$Ocupation_last_job == "Other (unskilled) manual worker, servant" | euTotal3$Ocupation_last_job == "Skilled manual worker" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but in a service job"| euTotal3$Ocupation_last_job == "Unskilled manual worker, etc." | euTotal3$Ocupation_last_job == "Employed position, service job")] <- "V-VII"

euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Responsible for ordinary shopping, etc." & (euTotal3$Ocupation_last_job == "Farmer " | euTotal3$Ocupation_last_job == "Fisherman" | euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation_last_job == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation_last_job == "Employed position, at desk" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but travelling" |  euTotal3$Ocupation_last_job == "Professional (lawyer, medical practitioner, etc.)" |  euTotal3$Ocupation_last_job == "Employed position, working mainly at a desk" |  euTotal3$Ocupation_last_job == "Employed position, travelling" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed person" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, etc."  |  euTotal3$Ocupation_last_job == "Business proprietors, etc." |  euTotal3$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Unemployed or temporarily not working" & (euTotal3$Ocupation_last_job == "Farmer " | euTotal3$Ocupation_last_job == "Fisherman" | euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation_last_job == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation_last_job == "Employed position, at desk" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but travelling" |  euTotal3$Ocupation_last_job == "Professional (lawyer, medical practitioner, etc.)" |  euTotal3$Ocupation_last_job == "Employed position, working mainly at a desk" |  euTotal3$Ocupation_last_job == "Employed position, travelling" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed person" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, etc."  |  euTotal3$Ocupation_last_job == "Business proprietors, etc." |  euTotal3$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Retired or unable to work through illness" & (euTotal3$Ocupation_last_job == "Farmer " | euTotal3$Ocupation_last_job == "Fisherman" | euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation_last_job == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation_last_job == "Employed position, at desk" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but travelling" |  euTotal3$Ocupation_last_job == "Professional (lawyer, medical practitioner, etc.)" |  euTotal3$Ocupation_last_job == "Employed position, working mainly at a desk" |  euTotal3$Ocupation_last_job == "Employed position, travelling" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed person" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, etc."  |  euTotal3$Ocupation_last_job == "Business proprietors, etc." |  euTotal3$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Student" & (euTotal3$Ocupation_last_job == "Farmer " | euTotal3$Ocupation_last_job == "Fisherman" | euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed" | euTotal3$Ocupation_last_job == "Business proprietors, owner (full or partner) of a company" | euTotal3$Ocupation_last_job == "Employed position, at desk" | euTotal3$Ocupation_last_job == "Employed position, not at a desk, but travelling" |  euTotal3$Ocupation_last_job == "Professional (lawyer, medical practitioner, etc.)" |  euTotal3$Ocupation_last_job == "Employed position, working mainly at a desk" |  euTotal3$Ocupation_last_job == "Employed position, travelling" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, other self employed person" |  euTotal3$Ocupation_last_job == "Owner of a shop, craftsmen, etc."  |  euTotal3$Ocupation_last_job == "Business proprietors, etc." |  euTotal3$Ocupation_last_job == "Professional (lawyer, etc.)")] <- "III-IV"

euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Responsible for ordinary shopping, etc." & (euTotal3$Ocupation_last_job == "Employed professional (employed doctor, lawyer, etc.)" | euTotal3$Ocupation_last_job == "General management, director or top management" | euTotal3$Ocupation_last_job == "Middle management, other management" | euTotal3$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | euTotal3$Ocupation_last_job == "General management, etc." | euTotal3$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Unemployed or temporarily not working" & (euTotal3$Ocupation_last_job == "Employed professional (employed doctor, lawyer, etc.)" | euTotal3$Ocupation_last_job == "General management, director or top management" | euTotal3$Ocupation_last_job == "Middle management, other management" | euTotal3$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | euTotal3$Ocupation_last_job == "General management, etc." | euTotal3$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Retired or unable to work through illness" & (euTotal3$Ocupation_last_job == "Employed professional (employed doctor, lawyer, etc.)" | euTotal3$Ocupation_last_job == "General management, director or top management" | euTotal3$Ocupation_last_job == "Middle management, other management" | euTotal3$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | euTotal3$Ocupation_last_job == "General management, etc." | euTotal3$Ocupation_last_job == "Middle management, etc.")] <- "I-II"
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Student" & (euTotal3$Ocupation_last_job == "Employed professional (employed doctor, lawyer, etc.)" | euTotal3$Ocupation_last_job == "General management, director or top management" | euTotal3$Ocupation_last_job == "Middle management, other management" | euTotal3$Ocupation_last_job == "Employed professional (employed doctor, etc.)" | euTotal3$Ocupation_last_job == "General management, etc." | euTotal3$Ocupation_last_job == "Middle management, etc.")] <- "I-II"

euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Responsible for ordinary shopping, etc." & (euTotal3$Ocupation_last_job == "Never did any paid work")] <- NA
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Unemployed or temporarily not working" & (euTotal3$Ocupation_last_job == "Never did any paid work")] <- NA
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Retired or unable to work through illness" & (euTotal3$Ocupation_last_job == "Never did any paid work")] <- NA
euTotal3$Ocupation_3cat[euTotal3$Ocupation == "Student" & (euTotal3$Ocupation_last_job == "Never did any paid work")] <- NA



euTotal3$Oport_area_rec[euTotal3$Oport_area == "Totally agree" | euTotal3$Oport_area == "Tend to agree" | euTotal3$Oport_area == "Strongly agree"]<- "Agree"
euTotal3$Oport_area_rec[euTotal3$Oport_area == "Totally disagree" | euTotal3$Oport_area == "Tend to disagree" | euTotal3$Oport_area == "Strongly disagree"]<- "Disagree"
euTotal3$Oport_area_rec[euTotal3$Oport_area == "DK"]<- NA

euTotal3$Oport_clubs_rec[euTotal3$Oport_clubs == "Totally agree" | euTotal3$Oport_clubs == "Tend to agree" | euTotal3$Oport_clubs == "Strongly agree"]<- "Agree"
euTotal3$Oport_clubs_rec[euTotal3$Oport_clubs == "Totally disagree" | euTotal3$Oport_clubs == "Tend to disagree" | euTotal3$Oport_clubs == "Strongly disagree"]<- "Disagree"
euTotal3$Oport_clubs_rec[euTotal3$Oport_clubs == "DK"]<- NA

euTotal3$Oport_autorities_not_enough_rec[euTotal3$Oport_autorities_not_enough == "Totally agree" | euTotal3$Oport_autorities_not_enough == "Tend to agree" | euTotal3$Oport_autorities_not_enough == "Strongly agree"]<- "Agree"
euTotal3$Oport_autorities_not_enough_rec[euTotal3$Oport_autorities_not_enough == "Totally disagree" | euTotal3$Oport_autorities_not_enough == "Tend to disagree" | euTotal3$Oport_autorities_not_enough == "Strongly disagree"]<- "Disagree"
euTotal3$Oport_autorities_not_enough_rec[euTotal3$Oport_autorities_not_enough == "DK"]<- NA



euTotal3[37:50] <- lapply(euTotal3[37:50], factor) 

#### FILTERS ####
# ONLY >= 18 YEARS OLD 

euTotal3 <- euTotal3[which(euTotal3$Age >= 18), ]

# REMOVING TURKEY POPULATION

euTotal3 <- euTotal3[-c(which(euTotal3$Country_rec == "Turkey" | euTotal3$Country_rec == "Cyprus (TCC)")), ]
euTotal3$Country_rec<- fct_drop(euTotal3$Country_rec, only = c("Turkey", "Cyprus (TCC)")) 
                                                           
# REMOVE ILLOGICAL VALUES AND RECOVERING SOME NAs LOGICAL VALUES

euTotal3 <- euTotal3[-c(which(euTotal3$Vig_Days == 0 & euTotal3$Vig_Time_med > 0)), ] 
euTotal3 <- euTotal3[-c(which(euTotal3$Mod_Days == 0 & euTotal3$Mod_Time_med > 0)), ] 
euTotal3 <- euTotal3[-c(which(euTotal3$Walk_Days == 0 & euTotal3$Walk_Time_med > 0)), ] 
euTotal3 <- euTotal3[-c(which(euTotal3$Vig_Days > 0 & euTotal3$Vig_Time_med == 0)), ] 
euTotal3 <- euTotal3[-c(which(euTotal3$Mod_Days > 0 & euTotal3$Mod_Time_med == 0)), ] 
euTotal3 <- euTotal3[-c(which(euTotal3$Walk_Days > 0 & euTotal3$Walk_Time_med == 0)), ] 

# REMOVING NA METS and Type of community VALUES TO GET FINAL DATABASE SAMPLE SIZE
euTotal3 <- euTotal3[which(euTotal3$MVPA_met >= 0), ]
euTotal3 <- euTotal3[-c(which(is.na(euTotal3$Typeofcommunity2cat))), ]



#### DESCRIPTIVES ####
# VD
table(euTotal3$WHO_prev);prop.table(table(euTotal3$WHO_prev))
table(euTotal3$Oport_area_rec);prop.table(table(euTotal3$Oport_area_rec))
table(euTotal3$Oport_clubs_rec);prop.table(table(euTotal3$Oport_clubs_rec))
table(euTotal3$Oport_autorities_not_enough_rec);prop.table(table(euTotal3$Oport_autorities_not_enough_rec))

# VI
table(euTotal3$Typeofcommunity2cat);prop.table(table(euTotal3$Typeofcommunity2cat))

# COVARIABLES
table(euTotal3$Age_6clusters);prop.table(table(euTotal3$Age_6clusters))
table(euTotal3$Gender);prop.table(table(euTotal3$Gender))
table(euTotal3$Education_3cat_recod);prop.table(table(euTotal3$Education_3cat_recod))
table(euTotal3$Ocupation_3cat);prop.table(table(euTotal3$Ocupation_3cat))
table(euTotal3$survey);prop.table(table(euTotal3$survey))


# CROSS-TABLES BY ACTIVE/INACTIVE PREVALENCE

euTotal3 %>%
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

gender_inac_desc <- euTotal3 %>% group_by(Gender) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

age_inac_desc <- euTotal3 %>% group_by(Age_6clusters) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

educ_inac_desc <- euTotal3 %>% group_by(Education_3cat_recod) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

ocup_inac_desc <- euTotal3 %>% group_by(Ocupation_3cat) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

survey_inac_desc <- euTotal3 %>% group_by(survey) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

place_inac_desc <- euTotal3 %>% group_by(Typeofcommunity2cat) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

euTotal3 %>% group_by(Oport_area_rec) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

euTotal3 %>% group_by(Oport_clubs_rec) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

euTotal3 %>% group_by(Oport_autorities_not_enough_rec) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))



# CROSS-TABLES BY URBAN-RURAL
euTotal3 %>% group_by(Oport_area_rec, Typeofcommunity2cat) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

euTotal3 %>% group_by(Oport_clubs_rec, Typeofcommunity2cat) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))

euTotal3 %>% group_by(Oport_autorities_not_enough_rec, Typeofcommunity2cat) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))



Place_WHO_area <- euTotal3 %>% group_by(WHO_prev, Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree", na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree", na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

Place_WHO_club <- euTotal3 %>% group_by(WHO_prev, Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree", na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree", na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

Place_WHO_aut <- euTotal3 %>% group_by(WHO_prev, Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree", na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree", na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

# CROSS-TABLES BY URBAN-RURAL AND YEAR OF SURVEY

Place_year_desc <- euTotal3 %>% group_by(Typeofcommunity2cat, survey, Gender) %>% 
  summarise(n = n(), Inactive_n = sum(WHO_prev == "Inactive"), 
            Active_n = sum(WHO_prev == "Active"), Active_prev = (sum(WHO_prev == "Active")*100/n()),
            Inactive_prev = (sum(WHO_prev == "Inactive")*100/n()))



# CROSS-TABLES BY AGREE/DISAGREE OPORT PERCEPTION
# AREA

ALL_area_desc <- euTotal3 %>%
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

gender_area_desc <- euTotal3 %>% group_by(Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

age_area_desc <- euTotal3 %>% group_by(Age_6clusters) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

educ_area_desc <- euTotal3 %>% group_by(Education_3cat_recod) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

ocup_area_desc <- euTotal3 %>% group_by(Ocupation_3cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

survey_area_desc <- euTotal3 %>% group_by(survey) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

Place_area_desc <- euTotal3 %>% group_by(Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

Place_opor_area_desc <- euTotal3 %>% group_by(Typeofcommunity2cat, survey, Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))

euTotal3 %>% group_by(WHO_prev) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_area_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_area_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_area_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_area_rec == "Disagree", na.rm = T)*100/n()))



# LOCAL CLUBS

ALL_club_desc <- euTotal3 %>%
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

gender_club_desc <- euTotal3 %>% group_by(Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

age_club_desc <- euTotal3 %>% group_by(Age_6clusters) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

educ_club_desc <- euTotal3 %>% group_by(Education_3cat_recod) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

ocup_club_desc <- euTotal3 %>% group_by(Ocupation_3cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

survey_club_desc <- euTotal3 %>% group_by(survey) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

place_club_desc <- euTotal3 %>% group_by(Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

Place_clubs_desc <- euTotal3 %>% group_by(Typeofcommunity2cat, survey, Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))

euTotal3 %>% group_by(WHO_prev) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_clubs_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_clubs_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_clubs_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_clubs_rec == "Disagree", na.rm = T)*100/n()))


# LOCAL AUTORITIES NOT ENOUGH

ALL_aut_desc <- euTotal3 %>%
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

gender_aut_desc <- euTotal3 %>% group_by(Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

age_aut_desc <- euTotal3 %>% group_by(Age_6clusters) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

educ_aut_desc <- euTotal3 %>% group_by(Education_3cat_recod) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

ocup_aut_desc <- euTotal3 %>% group_by(Ocupation_3cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

survey_aut_desc <- euTotal3 %>% group_by(survey) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

place_aut_desc <- euTotal3 %>% group_by(Typeofcommunity2cat) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

Place_aut_not_enough_desc <- euTotal3 %>% group_by(Typeofcommunity2cat, survey, Gender) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))

euTotal3 %>% group_by(WHO_prev) %>% 
  summarise(n = n(), Disagree_n = sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T), 
            Agree_n = sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T), Agree_prev = (sum(Oport_autorities_not_enough_rec == "Agree" , na.rm = T)*100/n()),
            Disagree_prev = (sum(Oport_autorities_not_enough_rec == "Disagree", na.rm = T)*100/n()))


## URBAN-RURAL DIFFERENCES ACROSS YEARS 
# ACTIVE
Place_year_desc <- Place_year_desc[with(Place_year_desc, order(survey, Gender)), ]

Place_year_desc$diff <-  Place_year_desc$Active_prev - lead(Place_year_desc$Active_prev, 1) # compute urban-rural difference
Place_year_desc$diff2 <- Place_year_desc$Active_prev - lag(Place_year_desc$Active_prev, 1)

Place_year_desc$diff[which(Place_year_desc$Typeofcommunity2cat == "Urban")] <- NA
Place_year_desc$diff2[which(Place_year_desc$Typeofcommunity2cat == "Rural")] <- NA
Place_year_desc$diff [which(is.na(Place_year_desc$diff))] <-0
Place_year_desc$diff2 [which(is.na(Place_year_desc$diff2))] <-0

Place_year_desc$urban_rural_diff_ACTIVE <- Place_year_desc$diff + Place_year_desc$diff2

Place_year_desc <- Place_year_desc[-c(4:10)]


# OPPORT AREA
Place_opor_area_desc <- Place_opor_area_desc[with(Place_opor_area_desc, order(survey, Gender)), ]

Place_opor_area_desc$diff <-  Place_opor_area_desc$Agree_prev - lead(Place_opor_area_desc$Agree_prev, 1) # compute urban-rural difference
Place_opor_area_desc$diff2 <- Place_opor_area_desc$Agree_prev - lag(Place_opor_area_desc$Agree_prev, 1)

Place_opor_area_desc$diff[which(Place_opor_area_desc$Typeofcommunity2cat == "Urban")] <- NA
Place_opor_area_desc$diff2[which(Place_opor_area_desc$Typeofcommunity2cat == "Rural")] <- NA
Place_opor_area_desc$diff [which(is.na(Place_opor_area_desc$diff))] <-0
Place_opor_area_desc$diff2 [which(is.na(Place_opor_area_desc$diff2))] <-0

Place_opor_area_desc$urban_rural_diff_AREA <- Place_opor_area_desc$diff + Place_opor_area_desc$diff2

Place_opor_area_desc <- Place_opor_area_desc[-c(4:10)]


# LOCAL SPORT CLUBS
Place_clubs_desc <- Place_clubs_desc[with(Place_clubs_desc, order(survey, Gender)), ]

Place_clubs_desc$diff <-  Place_clubs_desc$Agree_prev - lead(Place_clubs_desc$Agree_prev, 1) # compute urban-rural difference
Place_clubs_desc$diff2 <- Place_clubs_desc$Agree_prev - lag(Place_clubs_desc$Agree_prev, 1)

Place_clubs_desc$diff[which(Place_clubs_desc$Typeofcommunity2cat == "Urban")] <- NA
Place_clubs_desc$diff2[which(Place_clubs_desc$Typeofcommunity2cat == "Rural")] <- NA
Place_clubs_desc$diff [which(is.na(Place_clubs_desc$diff))] <-0
Place_clubs_desc$diff2 [which(is.na(Place_clubs_desc$diff2))] <-0

Place_clubs_desc$urban_rural_diff_LOCAL_CLUBS <- Place_clubs_desc$diff + Place_clubs_desc$diff2

Place_clubs_desc <- Place_clubs_desc[-c(4:10)]

# LOCAL AUTHORITIES NOT ENOUGH
Place_aut_not_enough_desc <- Place_aut_not_enough_desc[with(Place_aut_not_enough_desc, order(survey, Gender)), ]

Place_aut_not_enough_desc$diff <-  Place_aut_not_enough_desc$Agree_prev - lead(Place_aut_not_enough_desc$Agree_prev, 1) # compute urban-rural difference
Place_aut_not_enough_desc$diff2 <- Place_aut_not_enough_desc$Agree_prev - lag(Place_aut_not_enough_desc$Agree_prev, 1)

Place_aut_not_enough_desc$diff[which(Place_aut_not_enough_desc$Typeofcommunity2cat == "Urban")] <- NA
Place_aut_not_enough_desc$diff2[which(Place_aut_not_enough_desc$Typeofcommunity2cat == "Rural")] <- NA
Place_aut_not_enough_desc$diff [which(is.na(Place_aut_not_enough_desc$diff))] <-0
Place_aut_not_enough_desc$diff2 [which(is.na(Place_aut_not_enough_desc$diff2))] <-0

Place_aut_not_enough_desc$urban_rural_diff_LOCAL_AUT <- Place_aut_not_enough_desc$diff + Place_aut_not_enough_desc$diff2

Place_aut_not_enough_desc <- Place_aut_not_enough_desc[-c(4:10)]


# PLOT
library("ggplot2")
library("ggpubr")

Place_year_desc <- merge(Place_year_desc, Place_opor_area_desc, by =  c("Typeofcommunity2cat", "survey", "Gender"))
Place_year_desc <- merge(Place_year_desc, Place_clubs_desc, by =  c("Typeofcommunity2cat", "survey", "Gender"))
Place_year_desc <- merge(Place_year_desc, Place_aut_not_enough_desc, by =  c("Typeofcommunity2cat", "survey", "Gender"))

Place_year_desc2 <- melt(Place_year_desc, id = c("Typeofcommunity2cat", "survey", "Gender"),
                   measure = c("urban_rural_diff_ACTIVE", "urban_rural_diff_AREA", "urban_rural_diff_LOCAL_CLUBS", "urban_rural_diff_LOCAL_AUT"),
                   variable.name = "Urban_rural_diff",
                   value.name = "Prevalence" ) 


Place_year_desc2$Gender <- factor(Place_year_desc2$Gender, levels = c("Man", "Woman"),
                                 labels = c("Men", "Women"))

Place_year_desc2$Gender <- factor( Place_year_desc2$Gender , levels = levels( Place_year_desc2$Gender  )[ c( 2,1) ] )

Place_year_desc2$Gender <- mapvalues(Place_year_desc2$Gender, from = c("Women", "Men"), to = c("Mujeres", "Hombres"))
Place_year_desc2$Gender <- relevel(Place_year_desc2$Gender, ref = "Mujeres")


line_diff <- Place_year_desc2[which(Place_year_desc2$Typeofcommunity2cat == "Urban"),] %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>% ggplot(aes(x=survey, y=Prevalence, group=Urban_rural_diff)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(aes(color=Urban_rural_diff)) + 
  geom_point(aes(color=Urban_rural_diff)) + labs(y="Diferencia Urbano-Rural") + theme_pubclean()+
  theme(axis.title.x =  element_blank(), legend.title = element_blank(), axis.title.y = element_text(face = "bold", family="Times"),
        strip.background = element_rect(fill = "grey95"), strip.text = element_text(face = "bold", family="Times"), 
        axis.text = element_text(family="Times"), legend.text = element_text(family="Times")) +  
  scale_color_brewer(palette = "Set1", 
    labels = c("Prevalencia activos", "Oportunidades en el área", "Oportunidades por clubes locales", "Oportunidades insuficientes por autoridades")) +
  facet_wrap(. ~ Gender) 
line_diff

ggsave("Lineplot.tiff", width = 10, height = 5, units = "in", dpi = 600)


#### ADJUSTED DESCRIPTIVES ACROSS YEARS  ####
## CREATE CASES AS GENDER, AGE GROUP, YEAR, COUNTRY AND PLACE OF RESIDENT COMBINATIONS - COUNTRY-LEVEL DATAFRAME with mets and sitting time

euTotal3 %>% group_by(Gender, survey, Typeofcommunity2cat, Age_3clusters) %>%
  summarise(n = n(), Inactive = sum(WHO_prev == "Inactive"), 
            Active = sum(WHO_prev == "Active")) -> ADJ_countrylevel

ADJ_countrylevel$Inactive_perc <- (ADJ_countrylevel$Inactive * 100)/ ADJ_countrylevel$n
ADJ_countrylevel$Active_perc <- (ADJ_countrylevel$Active * 100)/ ADJ_countrylevel$n

## AGE STANDARIZATION BY EUROSTAT STANDARD 
Weight_18_44 <- 32.5 / 78.5 
Weight_45_69 <- 32 / 78.5
Weight_70 <- 14 / 78.5

ADJ_countrylevel$Age_weights[ADJ_countrylevel$Age_3clusters == "18-44"] <- Weight_18_44
ADJ_countrylevel$Age_weights[ADJ_countrylevel$Age_3clusters == "45-69"] <- Weight_45_69
ADJ_countrylevel$Age_weights[ADJ_countrylevel$Age_3clusters == "70+"] <- Weight_70

ADJ_countrylevel %>% group_by(Gender, survey, Typeofcommunity2cat) %>%
  summarise(Active_perc = weighted.mean(Active_perc, Age_weights)) -> ADJ_countrylevel




#### ACCOMMODATE DATA ####
# Men
Ruraldata_men <- ADJ_countrylevel[which(ADJ_countrylevel$Typeofcommunity2cat == "Rural" & ADJ_countrylevel$Gender == "Man"),]
rural_men2 <- dcast(Ruraldata_men, Country_rec ~ survey, value.var = "Active_perc")
rural_men2$Inac_Dif_2002_2017_rural <- rural_men2$`2017`- rural_men2$`2002`

Urbandata_men <- ADJ_countrylevel[which(ADJ_countrylevel$Typeofcommunity2cat == "Urban" & ADJ_countrylevel$Gender == "Man"),]
urban_men2 <- dcast(Urbandata_men, Country_rec ~ survey, value.var = "Active_perc")
urban_men2$Inac_Dif_2002_2017_urban <- urban_men2$`2017`- urban_men2$`2002`


# Women

Ruraldata_women <- ADJ_countrylevel[which(ADJ_countrylevel$Typeofcommunity2cat == "Rural" & ADJ_countrylevel$Gender == "Woman"),]
rural_women2 <- dcast(Ruraldata_women, Country_rec ~ survey, value.var = "Active_perc")
rural_women2$Inac_Dif_2002_2017_rural <- rural_women2$`2017`- rural_women2$`2002`


Urbandata_women <- ADJ_countrylevel[which(ADJ_countrylevel$Typeofcommunity2cat == "Urban" & ADJ_countrylevel$Gender == "Woman"),]
urban_women2 <- dcast(Urbandata_women, Country_rec ~ survey, value.var = "Active_perc")
urban_women2$Inac_Dif_2002_2017_urban <- urban_women2$`2017`- urban_women2$`2002`

# Sociodemographic
Sociodemographic <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Trajectories of rural-urban differences in physical activity in the European Union_EN REVISION/MMR/Trends in PA country-level eurobarometer.sav", to.data.frame = T, use.missings = T)
Sociodemographic <- Sociodemographic[-c(93:165)]
Sociodemographic$Urbanization_Dif_2002_2017 <- Sociodemographic$Urban_pop_per_2017- Sociodemographic$Urban_pop_per_2002
Sociodemographic$Ruralization_Dif_2002_2017 <- Sociodemographic$Rural_pop_per_2017- Sociodemographic$Rural_pop_per_2002
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



# Men
colnames(rural_men2)[colnames(rural_men2) == "2002"] <- "2002_rural"
colnames(rural_men2)[colnames(rural_men2) == "2005"] <- "2005_rural"
colnames(rural_men2)[colnames(rural_men2) == "2006"] <- "2006_rural"
colnames(rural_men2)[colnames(rural_men2) == "2013"] <- "2013_rural"
colnames(rural_men2)[colnames(rural_men2) == "2017"] <- "2017_rural"

colnames(urban_men2)[colnames(urban_men2) == "2002"] <- "2002_urban"
colnames(urban_men2)[colnames(urban_men2) == "2005"] <- "2005_urban"
colnames(urban_men2)[colnames(urban_men2) == "2013"] <- "2013_urban"
colnames(urban_men2)[colnames(urban_men2) == "2017"] <- "2017_urban"


mapdata_men <- merge(urban_men2, rural_men2, by = "Country_rec")
mapdata_men$Inac_dif_2002_urban_rural <- mapdata_men$`2002_urban` - mapdata_men$`2002_rural`
mapdata_men$Inac_dif_2005_urban_rural <- mapdata_men$`2005_urban` - mapdata_men$`2005_rural`
mapdata_men$Inac_dif_2013_urban_rural <- mapdata_men$`2013_urban` - mapdata_men$`2013_rural`
mapdata_men$Inac_dif_2017_urban_rural <- mapdata_men$`2017_urban` - mapdata_men$`2017_rural`

mapdata_men <- merge(mapdata_men, Sociodemographic, by = "Country_rec")
mapdata_men <- mapdata_men[-c(53:106)]

PopTot2002 <- sum(mapdata_men$Pob_2002); PopTot2005 <- sum(mapdata_men$Pob_2005)
PopTot2013 <- sum(mapdata_men$Pob_2013); PopTot2017 <- sum(mapdata_men$Pob_2017)

mapdata_men$weight_tot_2002 <- (mapdata_men$Pob_2002)/PopTot2002
mapdata_men$weight_tot_2005 <- (mapdata_men$Pob_2005)/PopTot2005
mapdata_men$weight_tot_2013 <- (mapdata_men$Pob_2013)/PopTot2013
mapdata_men$weight_tot_2017 <- (mapdata_men$Pob_2017)/PopTot2017

PopUrb2002 <- sum(mapdata_men$Pop_urb_2002); PopUrb2005 <- sum(mapdata_men$Pop_urb_2005)
PopUrb2013 <- sum(mapdata_men$Pop_urb_2013); PopUrb2017 <- sum(mapdata_men$Pop_urb_2017)

mapdata_men$weight_urb_2002 <- (mapdata_men$Pop_urb_2002)/PopUrb2002
mapdata_men$weight_urb_2005 <- (mapdata_men$Pop_urb_2005)/PopUrb2005
mapdata_men$weight_urb_2013 <- (mapdata_men$Pop_urb_2013)/PopUrb2013
mapdata_men$weight_urb_2017 <- (mapdata_men$Pop_urb_2017)/PopUrb2017

PopRur2002 <- sum(mapdata_men$Pop_rur_2002); PopRur2005 <- sum(mapdata_men$Pop_rur_2005)
PopRur2013 <- sum(mapdata_men$Pop_rur_2013); PopRur2017 <- sum(mapdata_men$Pop_rur_2017)

mapdata_men$weight_rur_2002 <- (mapdata_men$Pop_rur_2002)/PopRur2002
mapdata_men$weight_rur_2005 <- (mapdata_men$Pop_rur_2005)/PopRur2005
mapdata_men$weight_rur_2013 <- (mapdata_men$Pop_rur_2013)/PopRur2013
mapdata_men$weight_rur_2017 <- (mapdata_men$Pop_rur_2017)/PopRur2017











#### MULTI-LEVEL BINOMIAL LOGISTIC MODELS ####
library("moments")
library(lme4)
library(sjPlot)

euTotal3$WHO_prev <- relevel(euTotal3$WHO_prev, ref = "Inactive")
euTotal3$Education_3cat_recod <- relevel(euTotal3$Education_3cat_recod, ref = "Primary")
euTotal3$Ocupation_3cat <- relevel(euTotal3$Ocupation_3cat, ref = "V-VII")
euTotal3$survey <- relevel(euTotal3$survey, ref = "2002")
euTotal3$Oport_area_rec <- relevel(euTotal3$Oport_area_rec, ref = "Disagree")
euTotal3$Oport_clubs_rec <- relevel(euTotal3$Oport_clubs_rec, ref = "Disagree")
euTotal3$Oport_autorities_not_enough_rec <- relevel(euTotal3$Oport_autorities_not_enough_rec, ref = "Disagree")

# 1º/ ACTIVE PROB ACROSS YEARS BY URB-RUR

model1 <- glmer(WHO_prev ~ Age + Gender + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se1 <- sqrt(diag(vcov(model1)))
tab1 <- cbind(Est = fixef(model1), LL = fixef(model1) - 1.96 * se1, UL = fixef(model1) + 1.96 * se1)
exp(tab1)
tab_model(model1, digits.re = 3)

# 2º/ AGREE WITH OPORTUNITIES PERCEPTION PROB ACROSS YEARS BY URB-RUR

model21 <- glmer(Oport_area_rec ~ Age + Gender + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se21 <- sqrt(diag(vcov(model21)))
tab21 <- cbind(Est = fixef(model21), LL = fixef(model21) - 1.96 * se21, UL = fixef(model21) + 1.96 * se21)
exp(tab21)
tab_model(model21, digits.re = 3)


model22 <- glmer(Oport_clubs_rec ~ Age + Gender + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se22 <- sqrt(diag(vcov(model22)))
tab22 <- cbind(Est = fixef(model22), LL = fixef(model22) - 1.96 * se22, UL = fixef(model22) + 1.96 * se22)
exp(tab22)
tab_model(model22, digits.re = 3)


model23 <- glmer(Oport_autorities_not_enough_rec ~ Age + Gender + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se23 <- sqrt(diag(vcov(model23)))
tab23 <- cbind(Est = fixef(model23), LL = fixef(model23) - 1.96 * se23, UL = fixef(model23) + 1.96 * se23)
exp(tab23)
tab_model(model23, digits.re = 3)


# 3º/ ACTIVE PROB INCLUDING ONE OPORT PERCEPTION VARIABLE

model31 <- glmer(WHO_prev ~ Age + Gender + Oport_area_rec + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se31 <- sqrt(diag(vcov(model31)))
tab31 <- cbind(Est = fixef(model31), LL = fixef(model31) - 1.96 * se31, UL = fixef(model31) + 1.96 * se31)
exp(tab31)
tab_model(model31, digits.re = 3)


model32 <- glmer(WHO_prev ~ Age + Gender + Oport_clubs_rec + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se32 <- sqrt(diag(vcov(model32)))
tab32 <- cbind(Est = fixef(model32), LL = fixef(model32) - 1.96 * se32, UL = fixef(model32) + 1.96 * se32)
exp(tab32)
tab_model(model32, digits.re = 3)


model33 <- glmer(WHO_prev ~ Age + Gender + Oport_autorities_not_enough_rec + Education_3cat_recod + Ocupation_3cat + survey*Typeofcommunity2cat + 
                  (1 | Country_rec), data = euTotal3, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

se33 <- sqrt(diag(vcov(model33)))
tab33 <- cbind(Est = fixef(model33), LL = fixef(model33) - 1.96 * se33, UL = fixef(model33) + 1.96 * se33)
exp(tab33)
tab_model(model33, digits.re = 3)


#### PLOTS ####
library("ggplot2")
library("ggpubr")

Place_year_desc$Gender <- factor( Place_year_desc$Gender , levels = levels( Place_year_desc$Gender  )[ c( 2,1) ] )
Place_opor_area_desc$Gender <- factor( Place_opor_area_desc$Gender , levels = levels( Place_opor_area_desc$Gender  )[ c( 2,1) ] )
Place_clubs_desc$Gender <- factor( Place_clubs_desc$Gender , levels = levels( Place_clubs_desc$Gender  )[ c( 2,1) ] )
Place_aut_not_enough_desc$Gender <- factor( Place_aut_not_enough_desc$Gender , levels = levels( Place_aut_not_enough_desc$Gender  )[ c( 2,1) ] )


Place_year_desc$Gender <- mapvalues(Place_year_desc$Gender, from = c("Woman", "Man"), to = c("Mujeres", "Hombres"))
Place_year_desc$Gender <- relevel(Place_year_desc$Gender, ref = "Mujeres")


a <- Place_year_desc %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>%
ggplot(aes(x=survey, y=Active_prev, fill=Typeofcommunity2cat)) +  
  geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
  scale_fill_brewer(palette = "Accent", labels = c("Rural", "Urbano")) +
  ylab("Activos (%)") + labs(fill = "Lugar de residencia", title = "Tendencia temporal de prevalencia activos")+
  theme_pubclean()+ theme(axis.title.x =  element_blank(), strip.background = element_rect(fill = "grey95"),
  axis.title.y = element_text(face = "bold", family="Times"), strip.text = element_text(face = "bold", family="Times"), legend.title = element_text(face = "bold", family="Times"),
  axis.text = element_text(family="Times"), legend.text = element_text(family="Times"), title = element_text(family="Times")) + 
  coord_cartesian(ylim = c(20,90))+ facet_wrap(. ~ Gender)

a



ADJ_countrylevel$Gender <- factor( ADJ_countrylevel$Gender , levels = levels( ADJ_countrylevel$Gender  )[ c( 2,1) ] )

aa <- ADJ_countrylevel %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>%
  ggplot(aes(x=survey, y=Active_perc, fill=Typeofcommunity2cat)) +  
  geom_bar(stat="identity", position=position_dodge(), color="black", width = 0.7) +
  scale_fill_brewer(palette = "Accent", labels = c("Rural", "Urban")) +
  ylab("Active (%)") + labs(fill = "Resident place", title = "Active prevalence time trend")+
  theme_pubclean()+ theme(axis.title.x =  element_blank()) + coord_cartesian(ylim = c(20,90))+
  facet_wrap(. ~ Gender)
aa



Place_opor_area_desc$Gender <- mapvalues(Place_opor_area_desc$Gender, from = c("Woman", "Man"), to = c("Mujeres", "Hombres"))
Place_opor_area_desc$Gender <- relevel(Place_opor_area_desc$Gender, ref = "Mujeres")


b <- Place_opor_area_desc %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>%
  ggplot(aes(x=survey, y=Agree_prev, fill=Typeofcommunity2cat)) +  
  geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
  scale_fill_brewer(palette = "Accent", labels = c("Rural", "Urbano")) +
    ylab("De acuerdo (%)") + labs(fill = "Lugar de residencia", title = "Oportunidades en el área")+
  theme_pubclean()+ theme(axis.title.x =  element_blank(), strip.background = element_rect(fill = "grey95"),
                          axis.title.y = element_text(face = "bold", family="Times"), strip.text = element_text(face = "bold", family="Times"), legend.title = element_text(face = "bold", family="Times"),
                          axis.text = element_text(family="Times"), legend.text = element_text(family="Times"), title = element_text(family="Times"))+ 
  coord_cartesian(ylim = c(20,90))+
  facet_wrap(. ~ Gender)




Place_clubs_desc$Gender <- mapvalues(Place_clubs_desc$Gender, from = c("Woman", "Man"), to = c("Mujeres", "Hombres"))
Place_clubs_desc$Gender <- relevel(Place_clubs_desc$Gender, ref = "Mujeres")


c <- Place_clubs_desc %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>%
  ggplot(aes(x=survey, y=Agree_prev, fill=Typeofcommunity2cat)) +  
  geom_bar(stat="identity", position=position_dodge(),  width = 0.7) +
  scale_fill_brewer(palette = "Accent", labels = c("Rural", "Urbano")) +
  ylab("De acuerdo (%)") + labs(fill = "Lugar de residencia", title = "Oportunidades por clubes locales")+
  theme_pubclean()+ theme(axis.title.x =  element_blank(), strip.background = element_rect(fill = "grey95"),
                          axis.title.y = element_text(face = "bold", family="Times"), strip.text = element_text(face = "bold", family="Times"), legend.title = element_text(face = "bold", family="Times"),
                          axis.text = element_text(family="Times"), legend.text = element_text(family="Times"), title = element_text(family="Times"))+ 
  coord_cartesian(ylim = c(20,90))+
  facet_wrap(. ~ Gender)





Place_aut_not_enough_desc$Gender <- mapvalues(Place_aut_not_enough_desc$Gender, from = c("Woman", "Man"), to = c("Mujeres", "Hombres"))
Place_aut_not_enough_desc$Gender <- relevel(Place_aut_not_enough_desc$Gender, ref = "Mujeres")


d <- Place_aut_not_enough_desc %>% mutate(survey = fct_relevel(survey, "2002", "2005", "2013", "2017")) %>%
  ggplot(aes(x=survey, y=Agree_prev, fill=Typeofcommunity2cat)) +  
  geom_bar(stat="identity", position=position_dodge(),  width = 0.7) +
  scale_fill_brewer(palette = "Accent", labels = c("Rural", "Urbano")) +
  ylab("De acuerdo (%)") + labs(fill = "Lugar de residencia", title = "Oportunidades insuficientes por las autoridades")+
  theme_pubclean()+ theme(axis.title.x =  element_blank(), strip.background = element_rect(fill = "grey95"),
                          axis.title.y = element_text(face = "bold", family="Times"), strip.text = element_text(face = "bold", family="Times"), legend.title = element_text(face = "bold", family="Times"),
                          axis.text = element_text(family="Times"), legend.text = element_text(family="Times"), title = element_text(family="Times"))+ 
  coord_cartesian(ylim = c(20,90))+
  facet_wrap(. ~ Gender)


ggarrange(a, b, c, d,
          ncol = 2, nrow = 2, common.legend = T, align = "hv")
ggsave("Barplot.tiff", width = 10, height = 6.5, units = "in", dpi = 600)
