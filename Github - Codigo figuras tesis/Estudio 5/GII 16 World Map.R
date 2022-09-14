
library(foreign)
library(maps)
library(mapdata)
library(ggplot2)
library(ggthemes)
library(colorRamps)
library(grDevices)
library(ggpubr)

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
# order is critical for drawing
  
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



# Second Map - PA prevalence WHO 2016

p2 <- ggplot() + 
  geom_polygon(aes(long,lat, group=group, fill=AF_activos_gender_gap), data=world_map_data_wsteps) +
  geom_polygon(data = world_map_data_wsteps, fill = NA, color = "white", size=.03, aes(long,lat, group=group)) + 
  scale_fill_gradient(high = "steelblue4",low = "steelblue1", na.value = "grey85",
                        guide = guide_colorbar(title="PA prevalence \ngender gap", direction = "horizontal", title.position="top",
                                               title.hjust = 0.5, barwidth = 10, barheight = 0.5)) +
  mytheme + 
  coord_fixed(1.3) + 
  ditch_the_axes + 
  theme(legend.position = "bottom", legend.text = element_text( size=8), 
        legend.title = element_text(size=9, face = "bold")) +
  xlim(-170,195) + ylim(-55,95) 
p2

# Third map - steps gender gap 2014

p3 <- ggplot() + 
  geom_polygon(aes(long,lat, group=group, fill=steps_gender_gap), data=world_map_data_wsteps) +
  geom_polygon(data = world_map_data_wsteps, fill = NA, color = "white", size=.03, aes(long,lat, group=group)) + 
  scale_fill_gradient(high = "firebrick4",low = "firebrick1", na.value = "grey85",
                      guide = guide_colorbar(title="Average daily steps\ngender gap", direction = "horizontal", title.position="top",
                                             title.hjust = 0.5, barwidth = 10, barheight = 0.5)) +
  mytheme + 
  coord_fixed(1.3) + 
  ditch_the_axes + 
  theme(legend.position = "bottom", legend.text = element_text(size=8), 
        legend.title = element_text(size=9, face = "bold")) +
  xlim(-170,195) + ylim(-55,95) 
p3


# Fourth map - with gender gap participation in Rio 2016

p4 <- ggplot() + 
  geom_polygon(aes(long,lat, group=group, fill=Rio2016_porcen_gendergap), data=world_map_data_wsteps) +
  geom_polygon(data = world_map_data_wsteps, fill = NA, color = "white", size=.03, aes(long,lat, group=group)) + 
  scale_fill_gradient(high = "seagreen4",low = "chartreuse1", na.value = "grey85",
                      guide = guide_colorbar(title="Participation in Rio 2016 \ngender gap", direction = "horizontal", title.position="top",
                                             title.hjust = 0.5, barwidth = 10, barheight = 0.5)) +
  mytheme + 
  coord_fixed(1.3) + 
  ditch_the_axes + 
  theme(legend.position = "bottom", legend.text = element_text( size=8), 
        legend.title = element_text(size=9, face = "bold")) +
  xlim(-170,195) + ylim(-55,95) 
p4


# Unión de gráficos

ggarrange(p1,   widths = c(1.75,1),                                             
          ggarrange(h1, h2, h3, nrow=3, align = "hv"),
          ncol = 2)

tiff("plot.tiff", width = 15, height = 6.5, units = "in", res = 600)
ggarrange(p2, p3, p4, nrow=1, align = "hv", ncol = 3)
dev.off()


ggsave("map.pdf", width = 12, height = 6.5, units = "in", dpi = 600)
ggarrange(p1,    widths = c(1.9,1),                                              
          ggarrange(h1, h2, h3, nrow=3, align = "v"),
          ncol = 2)
