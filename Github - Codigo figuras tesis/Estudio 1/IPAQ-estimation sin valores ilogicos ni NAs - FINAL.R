
library("foreign")
library("plyr")
library("dplyr")
library("car")
library("lmtest")
library("MASS")
library("robust")
library("sfsmisc")
library("QuantPsyc")
library("forcats")

# 2017
eu2017 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2017.sav", to.data.frame = T, use.missings = T)
eu2017 <- eu2017[c(6, 9, 43:44, 45:46, 47, 48, 50:51, 53:54, 56, 57:58, 223, 224, 277, 242, 236, 237:239, 270)]
names(eu2017)[2] <- "ID"
names(eu2017)[3:15] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_freq", "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2017)[16:24] <- c("Gender", "Age", "Social class", "Type of community","Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Bills")
eu2017["survey"] <- "2017" # TIME VARIABLE


eu2017$`Social class`[eu2017$`Social class` == "The upper middle class of society"] <- "The higher class of society"
eu2017$`Social class`[eu2017$`Social class` == "The lower middle class of society"] <- "The working class of society"


eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "Never" & eu2017$Sport_Freq == "DK")), ] # REMOVE NOT VALID CASES
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "Never")), ]
eu2017 <- eu2017[-c(which(eu2017$PA_Freq == "DK" & eu2017$Sport_Freq == "DK")), ]
eu2017$Sport_PA_freq <- fct_recode(eu2017$Sport_PA_freq, Never = "Never/DK")

# 2013
eu2013 <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/06. Concurrent and convergent validity of a single, brief question for physical activity assessment_PUBLICADO/MMR/Modified eu 2013.sav", to.data.frame = T, use.missings = T)
eu2013 <- eu2013[c(6, 8, 374:375, 376:377, 378:382, 383, 386:387, 390:391, 394, 395:396, 465, 466,500,474,464, 469:471,494)]
names(eu2013)[2] <- "ID"
names(eu2013)[3:19] <- c("Sport_Freq", "Sport_Freq_rec", "PA_Freq", "PA_Freq_rec", "Sport_PA_Daily","Sport_PA_Few_times","Sport_PA_occasionally","Sport_PA_never", "Sport_PA_DK",  "Vig_Days", "Vig_Time", "Mod_Days", "Mod_Time", "Walk_Days", "Walk_Time", "Sit", "Sit_rec") 
names(eu2013)[20:28] <- c("Gender", "Age", "Social class", "Type of community","Education", "Ocupation", "Ocupation_rec1", "Ocupation_rec2", "Bills")
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

# Joint Eurobarometers
euTotal <- bind_rows(eu2017, eu2013); euTotal <- euTotal[-c(25:34)]
cols <- c(1, 3:16, 18:24); euTotal[cols] <- lapply(euTotal[cols], factor) 

# CREATE VAR WITHOUT NEVER; WITH 3 CAT (FEW TIMES + DAILY); WITH 2 CAT (WITHOUT NEVER)

euTotal$Sport_PA_freq_NEV <- euTotal$Sport_PA_freq; euTotal$Sport_PA_freq_NEV[euTotal$Sport_PA_freq_NEV == "Never"] <- NA
euTotal$Sport_PA_freq_3 <- euTotal$Sport_PA_freq; euTotal$Sport_PA_freq_3[euTotal$Sport_PA_freq_3 == "A few times a week"] <- "Almost daily"; euTotal$Sport_PA_freq_3 <- fct_drop(euTotal$Sport_PA_freq_3, only = c("A few times a week"))
euTotal$Sport_PA_freq_2 <- euTotal$Sport_PA_freq_3; euTotal$Sport_PA_freq_2[euTotal$Sport_PA_freq_2 == "Never"] <- NA

# COVARIATES REORDERING, ASIGNING MISSING VALUES AND DROPPING USELESS LEVELS
euTotal$Bills[euTotal$Bills == "Refusal (SPONT.)"] <- NA; euTotal$Bills<- fct_drop(euTotal$Bills, only = "Refusal (SPONT.)")

euTotal$`Type of community`[euTotal$`Type of community` == "DK"] <- NA; euTotal$`Type of community`<- fct_drop(euTotal$`Type of community`, only = "DK")

euTotal$`Social class`[euTotal$`Social class` == "DK"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "None (SPONTANEOUS)"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "None (SPONT.)"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "Other (SPONT.)"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "Other (SPONTANEOUS)"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "Refusal (SPONT.)"] <- NA
euTotal$`Social class`[euTotal$`Social class` == "The upper class of society"] <- "The higher class of society"
euTotal$`Social class`<- fct_drop(euTotal$`Social class`, only = c("The upper class of society", "Refusal (SPONT.)", "Other (SPONTANEOUS)", "Other (SPONT.)","None (SPONT.)" , "None (SPONTANEOUS)", "DK"))

euTotal$Gender[euTotal$Gender == "Male"] <- "Man"; euTotal$Gender[euTotal$Gender == "Female"] <- "Woman"
euTotal$Gender<- fct_drop(euTotal$Gender, only = c("Female", "Male"))

euTotal$Education[euTotal$Education == "Still Studying"] <- "Still studying"
euTotal$Education[euTotal$Education == "16-19"] <- "16-19 years"
euTotal$Education[euTotal$Education == "DK"] <- NA
euTotal$Education[euTotal$Education == "Refusal_duplicated_7"] <- NA
euTotal$Education<- fct_drop(euTotal$Education, only = c("Still Studying", "16-19", "DK", "Refusal_duplicated_7"))

# ONLY >= 18 YEARS OLD 

euTotal <- euTotal[which(euTotal$Age >= 18), ]

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

      # METS PER WEEK
euTotal$VPA_met <- euTotal$VPA_tot_time * 8
euTotal$MPA_met <- euTotal$MPA_tot_time * 4
euTotal$Walk_met <- euTotal$Walk_tot_time * 3.3 

# WHO PREVALENCE - 150' MPA, 75' VPA or a equivalent combination (VPA = 2* MPA)

euTotal$WHO_prev [euTotal$VPA_tot_time >= 75 | euTotal$MPA_tot_time >= 150 | (euTotal$MPA_tot_time + (2 * euTotal$VPA_tot_time) >= 150)] <- "Active"
euTotal$WHO_prev [which(is.na(euTotal$WHO_prev))] <-"Inactive"
euTotal$WHO_prev [which(is.na(euTotal$MPA_tot_time) & (is.na(euTotal$VPA_tot_time)))] <- NA

## CREATE 2 DATA FRAME - WITH & WITHOUT WALK METS 

euTotal_outwalk <- euTotal

euTotal <- euTotal[-c(which(euTotal$PA_Freq == "DK" | euTotal$Sport_Freq == "DK")), ] 
euTotal <- euTotal[-c(which(is.na(euTotal$PA_Freq))), ]
euTotal <- euTotal[-c(which(is.na(euTotal$Sport_Freq))), ]
euTotal$MVPA_met <- euTotal$VPA_met + euTotal$MPA_met + euTotal$Walk_met

# REMOVE ILLOGICAL VALUES AND RECOVERING SOME NAs LOGICAL VALUES

euTotal <- euTotal[-c(which(euTotal$Vig_Days == 0 & euTotal$Vig_Time_med > 0)), ] 
euTotal <- euTotal[-c(which(euTotal$Mod_Days == 0 & euTotal$Mod_Time_med > 0)), ] 
euTotal <- euTotal[-c(which(euTotal$Walk_Days == 0 & euTotal$Walk_Time_med > 0)), ] 
euTotal <- euTotal[-c(which(euTotal$Vig_Days > 0 & euTotal$Vig_Time_med == 0)), ] 
euTotal <- euTotal[-c(which(euTotal$Mod_Days > 0 & euTotal$Mod_Time_med == 0)), ] 
euTotal <- euTotal[-c(which(euTotal$Walk_Days > 0 & euTotal$Walk_Time_med == 0)), ] 

# REMOVING NA METS VALUES TO GET FINAL DATABASE SAMPLE SIZE
euTotal <- euTotal[which(euTotal$MVPA_met >= 0), ]

# STATISTICAL ANALYSES
# MULTINOMIAL LOGISTIC REGRESSION (IN OTHER R SCRIPT)
require("Matrix")
library("glmnet")
require("nnet")
require("reshape2")
require("mlogit")
require("forcats")

# MULTIPLE LOGISTIC REGRESSION MODELS AND ROC CURVES ANALYSIS
library("caret")
library("pROC")

# ADULTS AND OLDER PEOPLE 

euTotal$Sport_PA_freq2 <- relevel(euTotal$Sport_PA_freq, ref = "Never")

euTotal$Sport_PA_freq__1 <- euTotal$Sport_PA_freq2 ; euTotal$Sport_PA_freq__1[euTotal$Sport_PA_freq__1 == "A few times a week" | euTotal$Sport_PA_freq__1 == "Almost daily" ] <- NA
euTotal$Sport_PA_freq__1 <- fct_drop(euTotal$Sport_PA_freq__1, only = c("A few times a week", "Almost daily"))

euTotal$Sport_PA_freq__2 <- euTotal$Sport_PA_freq2 ; euTotal$Sport_PA_freq__2[euTotal$Sport_PA_freq__2 == "Occasionally" | euTotal$Sport_PA_freq__2 == "Almost daily" ] <- NA
euTotal$Sport_PA_freq__2 <- fct_drop(euTotal$Sport_PA_freq__2, only = c("Occasionally", "Almost daily"))

euTotal$Sport_PA_freq__3 <- euTotal$Sport_PA_freq2 ; euTotal$Sport_PA_freq__3[euTotal$Sport_PA_freq__3 == "Occasionally" | euTotal$Sport_PA_freq__3 == "A few times a week" ] <- NA
euTotal$Sport_PA_freq__3 <- fct_drop(euTotal$Sport_PA_freq__3, only = c("Occasionally", "A few times a week"))

euTotal$Sport_PA_freq__4 <- euTotal$Sport_PA_freq2 ; euTotal$Sport_PA_freq__4[euTotal$Sport_PA_freq__4 == "Never" | euTotal$Sport_PA_freq__4 == "Almost daily" ] <- NA
euTotal$Sport_PA_freq__4 <- fct_drop(euTotal$Sport_PA_freq__4, only = c("Never", "Almost daily"))

        # MVPA (INCLUDING WALK METs)

euTotal_1 <- euTotal[which(euTotal$Sport_PA_freq__1 == "Never" | euTotal$Sport_PA_freq__1 == "Occasionally"), ]
euTotal_2 <- euTotal[which(euTotal$Sport_PA_freq__2 == "Never" | euTotal$Sport_PA_freq__2 == "A few times a week"), ]
euTotal_3 <- euTotal[which(euTotal$Sport_PA_freq__3 == "Never" | euTotal$Sport_PA_freq__3 == "Almost daily"), ]
euTotal_4 <- euTotal[which(euTotal$Sport_PA_freq__4 == "Occasionally" | euTotal$Sport_PA_freq__4 == "A few times a week"), ]


roccurve1 <- roc(euTotal_1$Sport_PA_freq__1, euTotal_1$MVPA_met, na.rm = T); plot(roccurve1); auc(roccurve1); ci.auc(roccurve1); ci.thresholds(roccurve1)
optcuff1<-OptimalCutpoints::optimal.cutpoints(X = "MVPA_met", status = "Sport_PA_freq__1", tag.healthy = "Never", methods = "Youden", data = euTotal_1, pop.prev = NULL, control = OptimalCutpoints::control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = F) ; summary(optcuff1)
threshold1 = optcuff1$Youden$Global$optimal.cutoff$cutoff[1]; predicted_values1<-ifelse(euTotal_1$MVPA_met>=threshold1,1,0)
conf_matrix1 <-table(predicted_values1,euTotal_1$Sport_PA_freq__1); conf_matrix1 

plot(optcuff1)


roccurve2 <- roc(euTotal_2$Sport_PA_freq__2, euTotal_2$MVPA_met, na.rm = T); plot(roccurve2); auc(roccurve2); ci.auc(roccurve2); ci.thresholds(roccurve2)
optcuff2<-OptimalCutpoints::optimal.cutpoints(X = "MVPA_met", status = "Sport_PA_freq__2", tag.healthy = "Never", methods = "Youden", data = euTotal_2, pop.prev = NULL, control = OptimalCutpoints::control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE) ; summary(optcuff2)
threshold2 = optcuff2$Youden$Global$optimal.cutoff$cutoff[1]; predicted_values2<-ifelse(euTotal_2$MVPA_met>=threshold2,1,0)
conf_matrix2 <-table(predicted_values2, breg2$y); conf_matrix2 

plot(optcuff2)


roccurve3 <- roc(euTotal_3$Sport_PA_freq__3, euTotal_3$MVPA_met, na.rm = T); plot(roccurve3); auc(roccurve3); ci.auc(roccurve3); ci.thresholds(roccurve3)
optcuff3<-OptimalCutpoints::optimal.cutpoints(X = "MVPA_met", status = "Sport_PA_freq__3", tag.healthy = "Never", methods = "Youden", data = euTotal_3, pop.prev = NULL, control = OptimalCutpoints::control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE) ; summary(optcuff3)
threshold3 = optcuff3$Youden$Global$optimal.cutoff$cutoff[1]; predicted_values3<-ifelse(euTotal_3$MVPA_met>=threshold3,1,0)
conf_matrix3 <-table(predicted_values3, breg3$y); conf_matrix3 


library("ggrepel")
library("ggpubr")
library("ggthemes")
library("ggsci")
library("cowplot")
library("ggfortify")
library("kableExtra")
library("gridExtra")
library("grid")

library(extrafont)
loadfonts(device = "win")
windowsFonts(Times=windowsFont("TT Times New Roman"))


ggroc1 <- ggroc(list(Daily=roccurve3, Few_times=roccurve2, Occas=roccurve1),aes = "color", legacy.axes = T); ggroc1
ggroc2 <- ggroc1 + theme_classic() + labs(color = "Frecuencia") + ylab("Sensibilidad") + xlab("1 - Especificidad") +
  theme(plot.title = element_text(size=14, face="bold", family="Times", hjust = 0.5),  legend.text = element_text(family = "Times"),
        axis.title.x = element_text(size=12, face="bold", family="Times"), legend.position = c(0.75, 0.4), axis.text = element_text(family = "Times"),
        axis.title.y = element_text(size=12, face="bold", family="Times"), legend.title.align = 0.5, legend.title =  element_text(face = "bold", family="Times")) +
  scale_color_brewer(palette = "Set1", labels = c("Casi diariamente", "Algunas veces a la semana", "Ocasionalmente"), direction = -1) +
  hrbrthemes::scale_x_percent() + hrbrthemes::scale_y_percent() + geom_abline(linetype = "dashed")+theme(
  plot.margin = unit(c(0.25,0.5,0.25,0.25), "cm"))

ggroc2

ggsave("plot_concurrent.pdf", width = 5, height = 6, units = "in", dpi = 600)


roc.test(roccurve1, roccurve2) # Occasionally VS Few times
roc.test(roccurve1, roccurve3) # Occasionally VS Daily
roc.test(roccurve2, roccurve3) # Few times VS Daily



library("ggpubr")

