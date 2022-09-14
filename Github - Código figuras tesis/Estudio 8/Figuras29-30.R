library("foreign")

DESI <- read.csv2("C:/Users/anton/Desktop/Figuras tesis/Archivos datos/Datos por paises en digitalizacion y tiempo sentado.csv", header = T, dec = ",")

library("ggplot2")
library("ggrepel")
library("ggpubr")
library("ggthemes")
library("dplyr")
library("ggsci")
library("lmtest")
library("car")
library("grid")
library("cowplot")
library("ggfortify")
library("MASS")
library("robust")
library("sfsmisc")
library("QuantPsyc")
library("ggstatsplot")
library(extrafont)
library(colortools)


loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))


p1 <- ggplot(data = DESI, aes(x = Tiempo.sentado..min.día.,  y = DESI)) +
    geom_smooth(method=lm, color="blue", fill = "royalblue4", alpha=0.3, size=0.8) + 
    geom_text_repel(size = 3,aes(label=Abreviatura), alpha=1, family = "Times", max.overlaps = 100) +
    geom_point(shape=21, size=2, fill="blue3", color="white") + 
    theme_classic2() + labs(subtitle = expression(paste(italic("β"), " = 0.418, ", italic("IC 95%"), " (0.053, 0.684), ", italic("p"), " = 0.027, ", italic("n"), " = 28")), y = "Índice de Economía y Sociedad Digital", x = "Media Tiempo Sentado (min/día)")+
    theme(plot.title = element_text(size=12, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times", size = 12))+ 
    theme(axis.text = element_text(family = "Times"))+
    scale_y_continuous(breaks = c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90), limits = c(0.3, 0.78)) + 
    scale_x_continuous(breaks = c(250, 300, 350, 400, 450), limits = c(250, 400))

p1

ggsave("Figura29.tiff", width = 6, height = 4, units = "in", dpi = 600)




# E-DEVICES
Device <- read.csv2("C:/Users/anton/Desktop/Results_edevices.csv", header = T, dec = ",")

Device$Sample <- as.character(Device$Sample)
Device$Sample[Device$Sample == "Global"] <- "Total"

Device$Sample <- factor(Device$Sample, order = TRUE, 
                       levels = c("Total", "Mujeres", "Hombres"))

p2 <- ggplot(Device, aes(x=Edevices, y=OR, color=Sample)) + 
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    geom_point(position=position_dodge(.5), shape=16, size=3) +
    geom_errorbar(aes(ymin=LL, ymax=HL), width=.2, position=position_dodge(.5)) + 
    theme_classic2() + theme(axis.title.y = element_blank(), legend.position="none",
                             axis.title.x = element_text(family = "Times", face = "bold"),
                             axis.ticks.y = element_blank(),
                             axis.line = element_line(size = 0.5), 
                             axis.text = element_text(family = "Times"),
                             strip.text.x = element_text(face="bold", family = "Times", size = 12),
                             strip.background = element_blank(),
                             panel.spacing = unit(2, "lines")) + 
    scale_y_continuous(name ="Odds Ratio IC 95%", limits=c(0.5, 2.6), breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) + 
    scale_x_discrete(limits=c("Conexión a Internet", "Smartphone", "Tablet", "Índice PC", "Portátil", "Ordenador Sobremesa", "Reproductor CD", "Reproductor DVD", "TV")) + 
    facet_grid(. ~ Sample, scales = "free") +
    scale_color_manual(values = c("black", "red", "blue")) + coord_flip() + 
    geom_text(data = Device, label = Device$Text, nudge_x = 0.025,  y= 2.1, size = 3.5, color="black", family="Times")

p2

ggsave("Figura30.tiff", width = 12, height = 6, units = "in", dpi = 600)


