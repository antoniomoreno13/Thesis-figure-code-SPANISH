
library(foreign)
library(maps)
library(mapdata)
library(ggplot2)
library(ggthemes)
library(colorRamps)
library(grDevices)
library(ggpubr)

library("ggrepel")
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
library(writexl)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))



# Graficos de barras por quartiles según GII 16 y 14 (stepsnature)

# Creacion de las tablas

dfmean1 <- aggregate(GE$AF_activos_gender_gap, by = list(GE$GII16_TERCILES),na.rm=T, FUN = mean)
dfsd1 <- aggregate(GE$AF_activos_gender_gap, by = list(GE$GII16_TERCILES), na.rm=T, FUN = sd)
names(dfmean1)[2] <- "mean"
names(dfsd1)[2] <- "sd"
hist1 <- merge(dfmean1, dfsd1, by = "Group.1")


dfmean2 <- aggregate(stepsnat$steps_gender_gap, by = list(stepsnat$GII14_TERCILES),na.rm=T, FUN = mean)
dfsd2 <- aggregate(stepsnat$steps_gender_gap, by = list(stepsnat$GII14_TERCILES), na.rm=T, FUN = sd)
names(dfmean2)[2] <- "mean"
names(dfsd2)[2] <- "sd"
hist2 <- merge(dfmean2, dfsd2, by = "Group.1")


dfmean3 <- aggregate(GE$Rio2016_porcen_gendergap, by = list(GE$GII16_TERCILES),na.rm=T, FUN = mean)
dfsd3 <- aggregate(GE$Rio2016_porcen_gendergap, by = list(GE$GII16_TERCILES), na.rm=T, FUN = sd)
names(dfmean3)[2] <- "mean"
names(dfsd3)[2] <- "sd"
hist3 <- merge(dfmean3, dfsd3, by = "Group.1")

# Gráficos de barras      

h1 <- ggplot(data=hist1, aes(x = hist1$Group.1, y=hist1$mean)) + 
    geom_errorbar(data=hist1, aes(ymin=mean, ymax=mean+sd),size=0.5, width=.1) +
    geom_bar(stat="identity", aes(fill=Group.1, color=Group.1) ,size=0.5, width=0.25) + theme_classic() + 
    labs(y = "", title = "Prevalencia población activa")+
    scale_fill_manual(values = c("lightpink", "violetred2", "violetred4"))+
    scale_color_manual(values = c("lightpink", "violetred2", "violetred4"))+
    theme(axis.title.y = element_text(size=10, face="bold", family = "Times"), axis.title.x = element_blank()) +
    scale_x_discrete(labels = c("Alta Igualdad", "", "Alta Desigualdad")) +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "Times"),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size=11, face = "bold", family = "Times"))+
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), legend.position = "none")

h1


h2 <- ggplot(data=hist2, aes(x = Group.1, y=mean)) +
    geom_errorbar(data=hist2, aes(ymin=mean, ymax=mean+sd),size=0.5, width=.1) +
    geom_bar(stat="identity", aes(fill=Group.1, color=Group.1) , size=0.5,width=0.25) + theme_classic() + 
    labs( y = "Brecha de Género", title = "Media de pasos diarios") + 
    scale_fill_manual(values = c("lightpink", "violetred2", "violetred4"))+
    scale_color_manual(values = c("lightpink", "violetred2", "violetred4"))+
    theme(axis.title.y = element_text(size=15, face="bold", family = "Times"), axis.title.x = element_blank()) + 
    scale_x_discrete(labels = c("Alta Igualdad", "", "Alta Desigualdad")) +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "Times"),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size=11, face = "bold"))+
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), legend.position = "none")

h2


h3 <- ggplot(data=hist3, aes(x = Group.1, y=mean)) +
    geom_errorbar(data=hist3, aes(ymin=mean, ymax=mean+sd), size=0.5,width=.1) +
    geom_bar(stat="identity", aes(fill=Group.1, color=Group.1), size=0.5,width=0.25) + theme_classic() + 
    labs( y = "", title = "Participación Olímpica (Rio 2016)") +
    scale_fill_manual(values = c("lightpink", "violetred2", "violetred4"))+
    scale_color_manual(values = c("lightpink", "violetred2", "violetred4"))+
    theme(axis.title.y = element_text(size=10, face="bold", family = "Times"), axis.title.x = element_blank()) +
    scale_x_discrete(labels = c("Alta Igualdad", "", "Alta Desigualdad")) +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, family = "Times"),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size=11, face = "bold", family = "Times"))+
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), legend.position = "none")

h3



# Map

  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank())

  mytheme <- theme_few(base_size=16, base_family="Helvetica")


  world_map_all <- map_data("world")
world_map = world_map_all[world_map_all$region != 'Antarctica', ]

  df = read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/04. Development of gender equality elicits women participation in physical activity_ENVIADO/TRABAJO PREVIO/An Mo Ll/MMR Gender inequality/Plots/Gender Maps/GII16_WorldMap.csv", sep = ";", dec = ",")
  names(df)[1] <- "region"
  

  world_map_data_wsteps = merge(world_map, df, by = "region", all.x = TRUE)

world_map_data_wsteps = world_map_data_wsteps[with(world_map_data_wsteps, order(order)), ]

# Map with GII16 Data

  p1 <- ggplot() + 
  geom_polygon(aes(long,lat, group=group, fill=GII16), data=world_map_data_wsteps) +
  geom_polygon(data = world_map_data_wsteps, fill = NA, color = "white", size=.03, aes(long,lat, group=group)) + 
  scale_fill_gradient(high = "violetred4", low = "lightpink", na.value = "gray85", labels = scales::number_format(decimal.mark = ".", accuracy = 0.1),
                       guide = guide_colorbar(title="Índice de Desigualdad de Género", 
                                              direction = "horizontal", title.position="top",
                                              title.hjust = 0.5, barwidth = 12, barheight = 0.5)) +
  mytheme + 
  coord_fixed(1.3) + 
  ditch_the_axes + 
  theme(legend.position = "bottom", legend.text = element_text(size=10, family = "Times"), 
        legend.title = element_text(size=12, face = "bold", family = "Times")) +
    xlim(-170,195) + ylim(-55,95) 
p1


ggarrange(p1,   widths = c(1.75,1),                                             
          ggarrange(h1, h2, h3, nrow=3, align = "hv"),
          ncol = 2)

ggarrange(p1,    widths = c(1.9,1),                                              
          ggarrange(h1, h2, h3, nrow=3, align = "v"),
          ncol = 2)

ggsave("Figura18.pdf", width = 12, height = 6.5, units = "in", dpi = 600)






# Regresiones lineales - Figuras 19-21

p1 <- ggscatterstats(marginal = F, data = NEWDF1, x = RES1b, point.args = aes(size = 0.2), smooth.line.args = aes(size=0.2),k=3, results.subtitle = F, subtitle = expression(paste(italic("β"), " = 0.402, ", italic("IC 95%"), " (0.203, 0.569), ", italic("p"), " < 0.001, ", italic("n"), " = 82")),
                     y = RES1, ylab = "Índice de Desigualdad de Género\n(Log)", xlab = "Proporción Hombre/Mujer (Inversa)",
                     title = "Prevalencia población activa", messages = FALSE, bf.message = F, line.color = "violetred3") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, size=1) + 
    geom_point(shape=16, size=1.5, color="violetred4") + 
    theme_classic2() +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".")) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))

p1





p2 <- ggscatterstats(marginal = F, data = NEWDF2, x = RES2b, point.args = aes(size = 0.2),smooth.line.args = aes(size=0.2),k=3,results.subtitle = F, subtitle = expression(paste(italic("β"), " = 0.542, ", italic("IC 95%"), " (0.285, 0.726), ", italic("p"), " < 0.001, ", italic("n"), " = 42")),
                     y = RES1,  xlab = "Proporción Hombre/Mujer (Inversa)", ylab = "Índice de Desigualdad de Género\n(Log)",
                     title = "Media de pasos diarios", messages = FALSE, bf.message = F,
                     line.color = "violetred3") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, se = T, size=1) + 
    geom_point(shape=16, size=1.5, color="violetred4") + 
    theme_classic2() +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".",  accuracy = 0.01)) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))

p2



p3 <- ggscatterstats(marginal = F, data = NEWDF3, x = RES1b, point.args = aes(size = 0.2),smooth.line.args = aes(size=0.2),k=3,results.subtitle = F, subtitle = expression(paste(italic("β"), " = 0.346, ", italic("IC 95%"), " (0.148, 0.517), ", italic("p"), " = 0.001, ", italic("n"), " = 89")),
                     y = RES1,  xlab = "Proporción Hombre/Mujer (Log)", ylab = "Índice de Desigualdad de Género\n(Log)",
                     title = "Participación Olímpica (Rio 2016)", messages = FALSE, bf.message = F,
                     line.color = "violetred3") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, se = T, size=1) + 
    geom_point(shape=16, size = 1.5, color="violetred4") + 
    theme_classic2() +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x =  element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".")) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))
p3

ggdraw(xlim = c(-0.01, 1)) +
    draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(p3, x = -0.008, y = 0, width = 1, height = 0.5)
ggsave("Figura19.tiff", width = 12, height = 6, units = "in", dpi = 600)






p10 <- ggscatterstats(marginal = F, data = NEWDF10, x = RES1, point.args = aes(size = 0.2), smooth.line.args = aes(size=0.2),k=3,results.subtitle = F, subtitle = expression(paste(italic("β"), " = −0.702, ", italic("IC 95%"), " (−0.797, −0.572), ", italic("p"), " < 0.001, ", italic("n"), " = 82")),
                      y = RES1b,  ylab = "Prevalencia de activos\ntotal (%)", xlab = "Proporción Hombre/Mujer (Inversa)",
                      messages = FALSE, bf.message = F, line.color = "violetred3", title = "Prevalencia población activa") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, size=1) + 
    geom_point(shape=16, color="violetred4", size=1.5) + 
    theme_classic2() +
    theme(
        plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
        axis.title.x = element_text(size=10, face="bold", family = "Times"), 
        axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".")) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))

p10



p20 <- ggscatterstats(marginal = F, data = NEWDF20, x = RES2b, point.args = aes(size = 0.2), smooth.line.args = aes(size=0.2),k=3,results.subtitle = F, subtitle = expression(paste(italic("β"), " = −0.680, ", italic("IC 95%"), " (−0.816, −0.475), ", italic("p"), " < 0.001, ", italic("n"), " = 42")),
                      y = RES1,  ylab = "Media de pasos diarios total", xlab = "Proporción Hombre/Mujer (Inversa)",
                      messages = FALSE, bf.message = F,title = "Media de pasos diarios",
                      line.color = "violetred3") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, se = T, size=1) + 
    geom_point(shape=16, color="violetred4", size=1.5) + 
    theme_classic2() +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".",  accuracy = 0.01)) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))

p20



p30 <- ggscatterstats(marginal = F, data = NEWDF30, x = RES1b, point.args = aes(size = 0.2), smooth.line.args = aes(size=0.2),k=3,results.subtitle = F, subtitle = expression(paste(italic("β"), " = −0.234, ", italic("IC 95%"), " (−0.422, −0.027), ", italic("p"), " = 0.027, ", italic("n"), " = 89")),
                      y = RES1,  ylab = "Participación Olímpica\ntotal (Log)", xlab = "Proporción Hombre/Mujer (Log)",
                      messages = FALSE, bf.message = F, title = "Participación Olímpica (Rio 2016)",
                      line.color = "violetred3") +
    geom_smooth(method=lm, color="violetred3", fill = "lightpink", alpha=0.2, se = T, size=1) + 
    geom_point(shape=16, color="violetred4", size=1.5) + 
    theme_classic2() +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+ 
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".")) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"))
p30


ggdraw(xlim = c(-0.01, 1)) +
    draw_plot(p10, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(p20, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(p30, x = -0.008, y = 0, width = 1, height = 0.5)
ggsave("Figura20.pdf", width = 12, height = 6, units = "in", dpi = 600)






Inactive_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/Inactive.csv",
                       header = T, sep = ";", dec = ",")


p100 <- ggplot(data = Inactive_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
    xlab(label = "Proporción Hombre/Mujer (Inversa)") + ylab(label = "Prevalencia de activos (%)")+
    geom_smooth(method=lm, aes(color=Genero, fill = Genero), alpha=0.2, size=1) + 
    geom_point(shape=16, size=1.5, aes(color=Genero)) + 
    theme_classic2() + labs(title = "Prevalencia población activa", subtitle = expression(paste(italic("β"[Hombres]), " = −0.453, ", italic("IC 95%"), " (−0.610, −0.262), ", italic("p"), " < 0.001, ", italic("n"), " = 82"))) +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    scale_x_continuous(labels = scales::number_format(decimal.mark = ".")) +
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), 
          legend.title = element_blank(), legend.text = element_text(family = "Times"), legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) 

p100

legend <- cowplot::get_legend(p10)



steps_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/steps.csv",
                    header = T, sep = ";", dec = ",")


p200 <- ggplot(data = steps_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
    xlab(label = "Proporción Hombre/Mujer (Inversa)") + ylab(label = "Media de pasos diarios")+
    geom_smooth(method=lm, aes(color=Genero, fill = Genero), alpha=0.2, size=1) + 
    geom_point(shape=16, size=1.5, aes(color=Genero)) + 
    theme_classic2() + labs(title = "Media de pasos diarios", subtitle = expression(paste(italic("β"[Hombres]), " = −0.461, ", italic("IC 95%"), " (−0.671, −0.183), ", italic("p"), " = 0.002, ", italic("n"), " = 42"))) +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), 
          legend.title = element_blank(), legend.text = element_text(family = "Times"), legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) 

p200



rio_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/rio.csv",
                  header = T, sep = ";", dec = ",")


p300 <- ggplot(data = rio_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
    xlab(label = "Proporción Hombre/Mujer (Log)") + ylab(label = "Participación Olímpica (Log)")+
    geom_smooth(method=lm, aes(color=Genero, fill = Genero), alpha=0.2, size=1) + 
    geom_point(shape=16, size=1.5, aes(color=Genero)) + 
    theme_classic2() + labs(title = "Participación Olímpica (Rio 2016)", subtitle = expression(paste(italic("β"[Hombres]), " = 0.023, ", italic("IC 95%"), " (−0.186, −0.230), ", italic("p"), " = 0.832, ", italic("n"), " = 89"))) +
    theme(plot.title = element_text(size=15, face="bold", hjust = 0.5, family = "Times"), 
          axis.title.x = element_text(size=10, face="bold", family = "Times"), 
          axis.title.y = element_text(size=10, face="bold", family = "Times")) +
    theme(plot.subtitle = element_text(hjust = 0.5, family = "Times"))+
    scale_y_continuous(labels = scales::number_format(decimal.mark = ".")) + 
    theme(axis.text.x = element_text(family = "Times"), axis.text.y = element_text(family = "Times"), 
          legend.title = element_blank(), legend.text = element_text(family = "Times"), legend.position = "none") + 
    scale_color_manual(values = c("blue", "red")) +
    scale_fill_manual(values = c("blue", "red")) 

p300



ggsave("Figura21.tiff", width = 12, height = 6, units = "in", dpi = 600)
ggdraw(xlim = c(-0.01, 1)) +
    draw_plot(p100, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(p200, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(p300, x = -0.008, y = 0, width = 1, height = 0.5)+
    draw_plot(legend, x = 0.02, y = 0.45)






