
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


#### PLOTS ####
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

ggsave("Figura28.tiff", width = 10, height = 5, units = "in", dpi = 600)





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
ggsave("Figura27.tiff", width = 10, height = 6.5, units = "in", dpi = 600)
