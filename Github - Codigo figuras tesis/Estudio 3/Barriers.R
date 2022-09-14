library("foreign")
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
library(reshape2)
library(scales)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))

Device$Sample <- as.character(Device$Sample)
Device$Sample[Device$Sample == "Global"] <- "Total"

Device$Sample <- factor(Device$Sample, order = TRUE, 
                        levels = c("Total", "Mujeres", "Hombres"))



prev_barr_sc <- read.csv2("C:/Users/anton/Desktop/Figuras tesis/Archivos datos/prev_barriers_sc.csv", header = T, dec = ",")


# DOT PLOT PREV BARRIERS
prev_barr_sc$low <- prev_barr_sc$low * 100
prev_barr_sc$middle <- prev_barr_sc$middle * 100
prev_barr_sc$high <- prev_barr_sc$high * 100


prev_barr_sc <- melt(prev_barr_sc, id = c( "Barreras", "Genero"),
                    measure = c("low", "middle", "high"),
                    variable.name = "SC",
                    value.name = "Percentage" )

prev_barr_sc <- prev_barr_sc [order(prev_barr_sc$Genero), ]
prev_barr_sc <- prev_barr_sc [order(prev_barr_sc$Barreras), ]

prev_barr_sc$diff <-  prev_barr_sc$Percentage - lead(prev_barr_sc$Percentage, 1) # compute urban-rural difference

prev_barr_sc$diff[which(prev_barr_sc$SC == "middle")] <- NA
prev_barr_sc$diff[which(prev_barr_sc$SC == "high")] <- NA
prev_barr_sc$diff [which(is.na(prev_barr_sc$diff))] <-0


prev_barr_sc$diff2 <- prev_barr_sc$Percentage - lead(prev_barr_sc$Percentage, 1)
prev_barr_sc$diff2[which(prev_barr_sc$SC == "low")] <- NA
prev_barr_sc$diff2[which(prev_barr_sc$SC == "high")] <- NA
prev_barr_sc$diff2 [which(is.na(prev_barr_sc$diff2))] <-0


prev_barr_sc$diff3 <- prev_barr_sc$Percentage - lag(prev_barr_sc$Percentage, 2)
prev_barr_sc$diff3[which(prev_barr_sc$SC == "low")] <- NA
prev_barr_sc$diff3[which(prev_barr_sc$SC == "middle")] <- NA
prev_barr_sc$diff3 [which(is.na(prev_barr_sc$diff3))] <-0


prev_barr_sc$urban_rural_diff <- prev_barr_sc$diff + prev_barr_sc$diff2 + prev_barr_sc$diff3
prev_barr_sc$urban_rural_diff2 <- prev_barr_sc$Percentage - prev_barr_sc$urban_rural_diff

prev_barr_sc$Genero <- factor(prev_barr_sc$Genero, order = TRUE, 
                        levels = c("Mujeres", "Hombres"))

dotprev_barr_sc <- ggplot(prev_barr_sc, aes(x=Barreras, y=Percentage)) + 
    geom_segment(aes(x=Barreras, xend=Barreras, y=Percentage, yend=urban_rural_diff2), 
                 color="grey75", size=0.75) +
    geom_point(aes(fill = SC), shape=21, color="black",  size=3 ) +
    coord_flip() + theme_minimal() + scale_fill_manual(labels = c("V-VII", "III-IV", "I-II"), values = c("#663300", "#FF9900", "#6666FF"),
                                                       guide = guide_legend(reverse = TRUE))+
    labs(fill = "Clase social ocupacional") + 
    scale_x_discrete(limits=c("Riesgo de lesión", "Falta de motivación", "Discriminación", "Falta de amigos", "Enfermedad o discapacidad", "Falta de infraestructuras", "Competitividad", "Demasiado caro", "Falta de tiempo"))+
    ylab("Prevalencia (%)")+ facet_grid(.~ Genero)+
    theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"),
          axis.ticks.y = element_blank(), axis.text = element_text(family = "Times"),
          legend.title = element_text(size = 10, face = "bold", family = "Times"), 
          legend.text = element_text(family = "Times"),
          strip.text.x = element_text(face="bold", family = "Times", size = 12),
          strip.background = element_blank())
dotprev_barr_sc

ggsave("p2.tiff", width = 10, height = 5.5, units = "in", dpi = 600)


# OR BARRIERS

OR_barr <- read.csv("C:/Users/anton/Desktop/Figuras tesis/Archivos datos/OR_Barriers.csv", header = T, dec = ",", sep = ";")

legend <- cowplot::get_legend(dotprev_barr_sc)

OR_barr$Sample <- factor(OR_barr$Sample, order = TRUE, 
                              levels = c("Mujeres", "Hombres"))

a <- ggplot(OR_barr, aes(x=Barriers, y=OR, color=SC, group=SC)) + 
    geom_hline(yintercept=1, linetype="dashed", color = "#6666FF") +
    geom_point(position=position_dodge(.5), size=2.5) +
    scale_x_discrete(limits=c("Riesgo de lesión", "Falta de motivación", "Discriminación", "Falta de amigos", "Enfermedad o discapacidad", "Falta de infraestructuras", "Competitividad", "Demasiado caro", "Falta de tiempo"))+
    scale_color_manual(labels = c("V-VII", "III-IV"), values = c("#663300", "#FF9900"),
                      guide = guide_legend(reverse = TRUE), name="Clase social ocupacional")+
    geom_errorbar(aes(ymin=LL, ymax=HL, color=SC), width=.3, position=position_dodge(.5))+
    facet_grid(.~Sample, drop = T) + scale_y_continuous(trans='log10', limits = c(0.01, 100),breaks=c(0.01, 0.1, 1, 10, 100),
                                                        labels = c("0.01", "0.1", "1", "10", "100"))+
    coord_flip() + 
    theme_classic() + theme(axis.title.y = element_blank(), strip.background = element_blank(), strip.text.y = element_text(size = 7, family = "Times"), title = element_text(size = 8, family = "Times"), legend.title = element_text(face = "bold", family = "Times", size = 10),
                            axis.ticks.y = element_blank(), legend.position = "right", 
                            axis.title.x = element_text(family = "Times", size = 10, face = "bold"),
                            axis.text = element_text(family = "Times"),
                            strip.text.x = element_text(face="bold", family = "Times", size = 12),
                            legend.text = element_text(family = "Times")) + 
    ylab("Odds Ratio IC 95%")
a


ggsave("p3.pdf", width = 10, height = 5.5, units = "in", dpi = 600)
