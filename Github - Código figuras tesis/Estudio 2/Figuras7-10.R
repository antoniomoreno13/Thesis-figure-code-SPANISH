
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

library(ggplot2)
library(cowplot)
library(reshape2)
library(ggpubr)
library(GGally)
library(FactoMineR)
library(factoextra)


#### Percentiles ####
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
ggsave("Figura7.tiff", width = 12, height = 5, units = "in", dpi = 600)




#### Compositional data analysis ####
library(compositions)

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

ggsave("Figura8.pdf", width = 14, height = 6, units = "in", dpi = 600)


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
  
ggsave("Figura9.tiff", width = 8, height = 7, units = "in", dpi = 600)





#### Analisis multivariante MIXTO ####
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
ggsave("Figura10.pdf", width = 14, height = 5, units = "in", dpi = 600)



