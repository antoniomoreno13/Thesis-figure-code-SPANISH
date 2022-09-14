
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

library(ineq)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(gglorenz)



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

ggsave("Figura14.tiff", width = 9, height = 9, units = "in", dpi = 600)




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

ggsave("Figura15.tiff", width = 8.5, height = 5, units = "in", dpi = 600)




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

ggsave("Figura16.tiff", width = 9, height = 10, units = "in", dpi = 600)





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

ggsave("Figura17.tiff", width = 8.5, height = 7, units = "in", dpi = 600)


col2rgb("gold", alpha = FALSE)


