
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

ggsave("Figura6.pdf", width = 5, height = 6, units = "in", dpi = 600)
