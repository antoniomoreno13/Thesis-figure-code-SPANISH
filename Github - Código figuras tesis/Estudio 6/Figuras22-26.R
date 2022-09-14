
## STATISTICAL ANALYSES OF PA URBANIZATION TRENDS 2002-2017
library("reshape2")
library("plyr")
library("foreign")
library("dplyr")
library("forcats")
library("eurostat")
library("ggplot2")
library("ggpubr")
library("cowplot")
library(extrafont)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))


#### LINE PLOTS ####
## LINE PLOTS ACROSS TIME

line_men <- ggplot(ALL_PREDICTS_MEN, aes(x=year, y=predict)) +
    geom_smooth(aes(color=Typeofcommunity2cat), size=0.75, se = F)  +
    ylab("Physically Inactive prevalence (%)") + labs(color = "Resident Place", title = "Men") +
    theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold", size=10),
                       legend.title = element_text(size = 10, face = "bold")) + coord_cartesian(ylim = c(20,35))

line_women <- ggplot(ALL_PREDICTS_WOMEN, aes(x=year, y=predict)) +
    geom_smooth(aes(color=Typeofcommunity2cat),size=0.75, se = F)+ 
    ylab("Physically Inactive prevalence (%)") + labs(color = "Resident Place", title = "Women") +
    theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
                       legend.title = element_text(size = 10, face = "bold")) + coord_cartesian(ylim = c(20,35))

ggarrange(line_men, line_women, nrow=1, align = "v", ncol = 2, labels = c("a", "b"), common.legend = T, legend = "right")
ggsave("line_all.pdf", width = 10, height = 3, units = "in", dpi = 600)


## LINES URBAN - RURAL DIFFERENCES
# MEN
Diff_urb_rur_men <- melt(mapdata_men, id = c( "Country_rec"),
                         measure = c("Inac_dif_2002_urban_rural", "Inac_dif_2003_urban_rural", "Inac_dif_2004_urban_rural", "Inac_dif_2005_urban_rural",
                                     "Inac_dif_2006_urban_rural", "Inac_dif_2007_urban_rural", "Inac_dif_2008_urban_rural", "Inac_dif_2009_urban_rural",
                                     "Inac_dif_2010_urban_rural", "Inac_dif_2011_urban_rural", "Inac_dif_2012_urban_rural", "Inac_dif_2013_urban_rural", 
                                     "Inac_dif_2014_urban_rural", "Inac_dif_2015_urban_rural", "Inac_dif_2016_urban_rural", "Inac_dif_2017_urban_rural"),
                         variable.name = "Year",
                         value.name = "Diff_urban_rural")

Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2002_urban_rural"] <- 2002;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2003_urban_rural"] <- 2003
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2004_urban_rural"] <- 2004;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2005_urban_rural"] <- 2005
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2006_urban_rural"] <- 2006;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2007_urban_rural"] <- 2007
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2008_urban_rural"] <- 2008;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2009_urban_rural"] <- 2009
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2010_urban_rural"] <- 2010;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2011_urban_rural"] <- 2011
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2012_urban_rural"] <- 2012;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2013_urban_rural"] <- 2013
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2014_urban_rural"] <- 2014;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2015_urban_rural"] <- 2015
Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2016_urban_rural"] <- 2016;Diff_urb_rur_men$Year1[Diff_urb_rur_men$Year == "Inac_dif_2017_urban_rural"] <- 2017

line_diff_m <- ggplot() +
    geom_smooth(data = Diff_urb_rur_men, aes(x=Year1, y=Diff_urban_rural),size=0.75, se = T, alpha = 0.2)  +
    geom_smooth(data = e_men, aes(x=year, y=Weighted_prev),size=0.75, se = F, alpha = 0.2, linetype="dashed")  +
    ylab("Diferencia Urbano-Rural\nen la prevalencia de inactividad")+
    scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))+
    theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold", size=10, family = "Times"),
                            legend.title = element_text(size = 10, face = "bold", family = "Times"),
                            axis.text = element_text(family = "Times"))+ 
    coord_cartesian(ylim = c(-2.25,2.25))


line_diff_m


# WOMEN
Diff_urb_rur_women <- melt(mapdata_women, id = c( "Country_rec"),
                           measure = c("Inac_dif_2002_urban_rural", "Inac_dif_2003_urban_rural", "Inac_dif_2004_urban_rural", "Inac_dif_2005_urban_rural",
                                       "Inac_dif_2006_urban_rural", "Inac_dif_2007_urban_rural", "Inac_dif_2008_urban_rural", "Inac_dif_2009_urban_rural",
                                       "Inac_dif_2010_urban_rural", "Inac_dif_2011_urban_rural", "Inac_dif_2012_urban_rural", "Inac_dif_2013_urban_rural", 
                                       "Inac_dif_2014_urban_rural", "Inac_dif_2015_urban_rural", "Inac_dif_2016_urban_rural", "Inac_dif_2017_urban_rural"),
                           variable.name = "Year",
                           value.name = "Diff_urban_rural")

Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2002_urban_rural"] <- 2002;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2003_urban_rural"] <- 2003
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2004_urban_rural"] <- 2004;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2005_urban_rural"] <- 2005
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2006_urban_rural"] <- 2006;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2007_urban_rural"] <- 2007
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2008_urban_rural"] <- 2008;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2009_urban_rural"] <- 2009
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2010_urban_rural"] <- 2010;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2011_urban_rural"] <- 2011
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2012_urban_rural"] <- 2012;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2013_urban_rural"] <- 2013
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2014_urban_rural"] <- 2014;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2015_urban_rural"] <- 2015
Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2016_urban_rural"] <- 2016;Diff_urb_rur_women$Year1[Diff_urb_rur_women$Year == "Inac_dif_2017_urban_rural"] <- 2017

line_diff_w <- ggplot() +
    geom_smooth(data = Diff_urb_rur_women, aes(x=Year1, y=Diff_urban_rural),color= "red",fill="red", size=0.75, se = T, alpha = 0.1)  +
    geom_smooth(data = f_women, aes(x=year, y=Weighted_prev),size=0.75, color="red", se = F, alpha = 0.2, linetype="dashed")  +
    ylab("Urban-Rural difference in\ninactivity prevalence") + 
    scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))+
    theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
                            legend.title = element_text(size = 10, face = "bold", family = "Times"),
                            axis.text = element_text(family = "Times"))+ coord_cartesian(ylim = c(-2.25,2.25))

line_diff_w

ggarrange(line_men, line_women, line_diff_m, line_diff_w, nrow=2, align = "v", ncol = 2, common.legend = T)
ggsave("Figura22.pdf", width = 13, height = 7, units = "in", dpi = 600)




### EU 28 MAPS


query <- search_eurostat(pattern = "fertility rate", type = "table", 
                         fixed = F)

ct <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
        "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", "LV", "MT", 
        "NL", "NO","PL", "PT", "RO", "SE", "SI", "SK", "UK")


dat <- get_eurostat(id="tps00199", time_format = "num", filters = list(geo = ct))
dat[1:2,]
dat <- label_eurostat(dat)
dat[1:3,]

dat <- get_eurostat(id="tps00199", filters = list(geo = ct))

ggplot(dat, aes(x = time, y = values, color = geo, label = geo)) + 
        geom_line(alpha = .5) + 
        geom_text(data = dat %>% group_by(geo) %>% 
                          filter(time == max(time)), size = 2.6) + 
        theme(legend.position = "none") + labs(title = "Total Fertility rate, 2006-2017", 
                                               x = "Year", y = "%")

dat_2015 <- dat %>% filter(time == "2015-01-01")
ggplot(dat_2015, aes(x = reorder(geo, values), y = values)) + 
        geom_col(color = "white", fill = "grey80") + 
        theme(axis.text.x = element_text(size = 6)) + 
        labs(title = "Total fertility rate, 2015", y = "%", x = NULL)



mapdata <- get_eurostat_geospatial(nuts_level = 0) %>%
        right_join(dat_2015) %>% mutate(cat = cut_to_classes(values, n=4, decimals=1))

head(select(mapdata, geo, values, cat),3)

ggplot(mapdata, aes(fill = cat)) + scale_fill_brewer(palette = "RdYlBu") + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%")


#### MAPS ####
my_colors4 <- RColorBrewer::brewer.pal(3, "Accent")
palette_red <- RColorBrewer::brewer.pal(9, "Reds")
palette_blue <- RColorBrewer::brewer.pal(9, "Blues")


## WOMEN
urb_2002_w <- ggplot(mapdata_women, aes(fill = `2002_urban`)) +  scale_fill_steps(low = "#FEE0D2" , high = "#A50F15", limits = c(15,65), n.breaks = 10,guide = guide_coloursteps(even.steps = T, frame.colour="white",ticks.colour="white",
                                                                                                                                                                                 ticks = T, title.position="top", title.hjust = 0.5, barwidth = 12, barheight = 0.5, legend.direction = "horizontal")) + 
    geom_sf(color = "white", size=.05) + 
    xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Prevalencia Inactividad (%)") +annotate(geom="text", x=-2, y=67, label="Urbano\n2002",
                                                                                              size=5.25, family = "Times")+
    theme_classic() + theme(legend.text = element_text(family = "Times"), axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                            title = element_text(size = 12, face = "bold", family = "Times"), legend.title = element_text(size = 11)) 

urb_2002_w


urb_2017_w <- ggplot(mapdata_women, aes(fill = `2017_urban`)) +  scale_fill_steps(low = "#FEE0D2" , high = "#A50F15", limits = c(15,65), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                                 ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
    geom_sf(color = "white", size=.05) + 
    xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Urbano\n2017",
                                                                                            size=5.25, family = "Times")+
    theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                            title = element_text(size = 12, face = "bold", family = "Times")) 

urb_2017_w


rur_2002_w <- ggplot(mapdata_women, aes(fill = `2002_rural`)) +  scale_fill_steps(low = "#FEE0D2" , high = "#A50F15", limits = c(15,65), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                                 ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
    geom_sf(color = "white", size=.05) + 
    xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Rural\n2002",
                                                                                            size=5.25, family = "Times")+
    theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                            title = element_text(size = 12, face = "bold", family = "Times")) 

rur_2002_w


rur_2017_w <- ggplot(mapdata_women, aes(fill = `2017_rural`)) +  scale_fill_steps(low = "#FEE0D2" , high = "#A50F15", limits = c(15,65), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                                 ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
    geom_sf(color = "white", size=.05) + 
    xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Rural\n2017",
                                                                                            size=5.25, family = "Times")+
    theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                            title = element_text(size = 12, face = "bold", family = "Times")) 

rur_2017_w

legend <- cowplot::get_legend(urb_2002_w)
ggdraw(ylim = c(0,1),xlim = c(0.05,0.85)) +
    draw_plot(rur_2002_w, x = 0, y = 0.0, width = .5, height = 0.45) +
    draw_plot(rur_2017_w, x = .4, y = 0.0, width = .5, height = 0.45) +
    draw_plot(urb_2002_w, x = 0, y = 0.45, width = .5, height = 0.45) +
    draw_plot(urb_2017_w, x = .4, y = 0.45, width = .5, height = 0.45)+
    draw_plot(legend, x = 0.315, y = 0.45, width = 0.25)

ggsave("Figura23.pdf", width = 8, height = 6, units = "in", dpi = 600)



## MEN
urb_2002_m <- ggplot(mapdata_men, aes(fill = `2002_urban`)) +  scale_fill_steps(low = "#DEEBF7" , high = "#08519C", limits = c(10,50), n.breaks = 10, guide = guide_coloursteps(even.steps = T, frame.colour="white",ticks.colour="white",
                                                                                                           ticks = T, title.position="top", title.hjust = 0.5, barwidth = 12, barheight = 0.5, legend.direction = "horizontal")) + 
        geom_sf(color = "white", size=.05) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Prevalencia Inactividad (%)") +annotate(geom="text", x=-2, y=67, label="Urbano\n2002",
                                                                                                   size=5.25, family = "Times")+
        theme_classic() + theme(legend.text = element_text(family = "Times"), axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none", 
                                title = element_text(size = 12, face = "bold", family = "Times"), legend.title = element_text(size = 11, family = "Times")) 

urb_2002_m


urb_2017_m <- ggplot(mapdata_men, aes(fill = `2017_urban`)) +  scale_fill_steps(low = "#DEEBF7" , high = "#08519C", limits = c(10,50), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                               ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = "white", size=.05) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Urbano\n2017",
                                                                                                   size=5.25, family = "Times")+
        theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                                title = element_text(size = 12, face = "bold", family = "Times")) 

urb_2017_m

rur_2002_m <- ggplot(mapdata_men, aes(fill = `2002_rural`)) +  scale_fill_steps(low = "#DEEBF7" , high = "#08519C", limits = c(10,50), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                               ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = "white", size=.05) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Rural\n2002",
                                                                                                   size=5.25, family = "Times")+
        theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                                title = element_text(size = 12, face = "bold", family = "Times")) 

rur_2002_m


rur_2017_m <- ggplot(mapdata_men, aes(fill = `2017_rural`)) +  scale_fill_steps(low = "#DEEBF7" , high = "#08519C", limits = c(10,50), n.breaks = 10,guide = guide_coloursteps(even.steps = T, 
                                                                                                                                                                               ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = "white", size=.05) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(fill = "Inactivity prevalence (%)") +annotate(geom="text", x=-2, y=67, label="Rural\n2017",
                                                                                                size=5.25, family = "Times")+
        theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none",
                           title = element_text(size = 12, face = "bold", family = "Times")) 

rur_2017_m

legend <- cowplot::get_legend(urb_2002_m)
ggdraw(ylim = c(0,1),xlim = c(0.05,0.85)) +
        draw_plot(rur_2002_m, x = 0, y = 0.0, width = .5, height = 0.45) +
        draw_plot(rur_2017_m, x = .4, y = 0.0, width = .5, height = 0.45) +
        draw_plot(urb_2002_m, x = 0, y = 0.45, width = .5, height = 0.45) +
        draw_plot(urb_2017_m, x = .4, y = 0.45, width = .5, height = 0.45)+
        draw_plot(legend, x = 0.315, y = 0.45, width = 0.25)

ggsave("Figura24.tiff", width = 8, height = 6, units = "in", dpi = 600)





#### RANKING PLOTS ####
my_colors <- RColorBrewer::brewer.pal(3, "Accent")[1]
my_colors2 <- RColorBrewer::brewer.pal(3, "Accent")[2]
my_colors3 <- RColorBrewer::brewer.pal(3, "Accent")[3]

# MEN
mapdata_men %>%
    mutate(names = fct_reorder(names, names, .desc=T)) %>%
    ggplot()+ geom_segment(aes(xend= names, x = names, y = national_inac_2002, yend=national_inac_2017),arrow = arrow(angle = 45, length = unit(0.06, "inches")), size=1.2, color="steelblue") +
    theme_minimal() + 
    ylab("Prevalencia Inactividad Nacional (%)") + labs(subtitle = "2002-2017", title = "Hombres") + 
    scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30, 35, 40, 45, 50, 55))+
    theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"),
          axis.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold"), plot.subtitle = element_text(family = "Times")) + 
    coord_flip(ylim = c(0,45)) ->r1
r1

mapdata_men %>%
    mutate(Country_rec = fct_reorder(Country_rec, national_inac_2017)) %>%
    ggplot(aes(x=Country_rec, y=national_inac_2017)) +
    geom_segment( aes(xend=Country_rec, yend=0))  +
    geom_point(size=3, color=my_colors) +
    theme_bw()+
    ylab("Physically Inactive National Prevalence (%)") + labs(subtitle = "2017", title = "Men") + 
    scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30, 35, 40, 45, 50, 55))+
    theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10)) + 
    coord_flip(ylim = c(0,45)) -> r2
r2

# WOMEN
mapdata_women %>%
    mutate(names = fct_reorder(names, names, .desc = T)) %>%
    ggplot()+ geom_segment(aes(xend= names, x = names, y = national_inac_2002, yend=national_inac_2017),arrow = arrow(angle = 45, length = unit(0.06, "inches")), size=1.2, color="red") +
    theme_minimal() +
    ylab("Prevalencia Inactividad Nacional (%)") + labs(subtitle = "2002-2017", title = "Mujeres") + 
    scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30, 35, 40, 45, 50, 55,60, 65))+
    theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"),
          axis.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold"), plot.subtitle = element_text(family = "Times")) + 
    coord_flip(ylim = c(0,65)) -> r3
r3

mapdata_women %>%
    mutate(Country_rec = fct_reorder(Country_rec, national_inac_2017)) %>%
    ggplot(aes(x=Country_rec, y=national_inac_2017)) +
    geom_segment( aes(xend=Country_rec, yend=0))  +
    geom_point(size=3, color=my_colors2) +
    theme_bw() +
    ylab("Physically Inactive National Prevalence (%)") + labs(subtitle = "2017", title = "Women") + 
    scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30, 35, 40, 45, 50, 55, 60, 65))+
    theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10)) + 
    coord_flip(ylim = c(0,65)) -> r4
r4

# URBANIZATION
mapdata_men %>%
    mutate(names = fct_reorder(names, names, .desc = T)) %>%
    ggplot()+ geom_segment(aes(xend= names, x = names, y = Urban_pop_per_2002, yend=Urban_pop_per_2017),arrow = arrow(angle = 45, length = unit(0.06, "inches")), size=1.2, color="grey55") +
    theme_minimal() +
    ylab("Urbanización (%)") + labs(subtitle = "2002-2017", title = "Urbanización") + 
    scale_y_continuous(breaks = c(45, 50, 60, 70, 80, 90, 100))+
    theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"),
          axis.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold"), plot.subtitle = element_text(family = "Times")) + 
    coord_flip(ylim = c(50,100))  -> r5
r5


mapdata_men %>%
    mutate(Country_rec = fct_reorder(Country_rec, Urban_pop_per_2017)) %>%
    ggplot(aes(x=Country_rec, y=Urban_pop_per_2017)) +
    geom_segment( aes(xend=Country_rec, yend=50))  +
    geom_point(size=3, color=my_colors3) +
    theme_bw() +
    ylab("Urbanization (%)") + labs(subtitle = "2017", title = "Urbanization") + 
    scale_y_continuous(breaks = c(45, 50, 60, 70, 80, 90, 100))+
    theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10)) + 
    coord_flip(ylim = c(50,100)) -> r6
r6

ggarrange(r1, r3, r5,
          nrow=1, ncol = 3, common.legend = T, legend = "top")
ggsave("Figura25.tiff", width = 15, height = 6, units = "in", dpi = 600)




#### CONTRIBUTIONS PLOTS ####
# MEN
# Absolute Contributions
my_colors00 <- RColorBrewer::brewer.pal(3, "Accent")[1]
my_colors01 <- RColorBrewer::brewer.pal(3, "Accent")[2]
my_colors02 <- "grey55"
my_colors0 <- list(my_colors00 , my_colors01, my_colors02)

mapdata_men %>%
        mutate(names = fct_reorder(names, Total_contribution)) %>% 
        melt(id = c( "names"),
             measure = c("Rural_contribution", "Urban_contribution", "Urbanization_contribution"),
             variable.name = "Type_Contrib",
             value.name = "Abs_contribution" ) -> Contrib_men1

ca1 <- ggplot(Contrib_men1, aes(x=names, y=Abs_contribution, fill=Type_Contrib)) +
        geom_bar(stat="identity", width = 0.75) + theme_minimal() + scale_fill_manual(values = my_colors0, labels = c("Rural", "Urbano", "Urbanización")) +
        ylab("Contribución absoluta") + labs(fill = "Contribución", title = "Hombres") + scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30,-5, -10, -15, -20))+
        theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"), axis.ticks.y = element_blank(),
        legend.title = element_text(size = 10, face = "bold", family = "Times"), axis.text = element_text(family = "Times"),
        legend.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold")) + coord_flip(ylim = c(-20,30))
ca1

# Relative contributions
mapdata_men %>%
        mutate(names = fct_reorder(names, Total_contribution)) %>% 
        melt(id = c( "names"),
             measure = c("Rural_contrib_relative", "Urban_contrib_relative", "Urbanization_contrib_relative"),
             variable.name = "Type_Contrib",
             value.name = "Rel_contribution" ) -> Contrib_men2

cr1 <- ggplot(Contrib_men2, aes(x=names, y=Rel_contribution, fill=Type_Contrib)) +
        geom_bar(stat="identity",width = 0.75) + theme_minimal() + scale_fill_manual(values = my_colors0, labels = c("Rural", "Urbano", "Urbanización")) +
        ylab("Contribución relativa (%)") + labs(fill = "Contribución", title = "Hombres") + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, -20))+
        theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"), axis.ticks.y = element_blank(),
              legend.title = element_text(size = 10, face = "bold", family = "Times"), axis.text = element_text(family = "Times"),
              legend.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold"))  + coord_flip(ylim = c(-20, 130))
cr1

# WOMEN
# Absolute Contributions
mapdata_women %>%
        mutate(names = fct_reorder(names, Total_contribution)) %>% 
        melt(id = c( "names"),
             measure = c("Rural_contribution", "Urban_contribution", "Urbanization_contribution"),
             variable.name = "Type_Contrib",
             value.name = "Abs_contribution" ) -> Contrib_women1

ca2 <- ggplot(Contrib_women1, aes(x=names, y=Abs_contribution, fill=Type_Contrib)) +
        geom_bar(stat="identity", width = .75)  + theme_minimal() + scale_fill_manual(values = my_colors0, labels = c("Rural", "Urbano", "Urbanización")) +
        ylab("Contribución absoluta") + labs(fill = "Contribución", title = "Mujeres") + scale_y_continuous(breaks = c(0,5, 10, 15, 20, 25,30,-5, -10, -15, -20))+
        theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"), axis.ticks.y = element_blank(),
              legend.title = element_text(size = 10, face = "bold", family = "Times"), axis.text = element_text(family = "Times"),
              legend.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold")) + coord_flip(ylim = c(-20,30))
ca2

# Relative contributions
mapdata_women %>%
        mutate(names = fct_reorder(names, Total_contribution)) %>% 
        melt(id = c( "names"),
             measure = c("Rural_contrib_relative", "Urban_contrib_relative", "Urbanization_contrib_relative"),
             variable.name = "Type_Contrib",
             value.name = "Rel_contribution" ) -> Contrib_women2

cr2 <- ggplot(Contrib_women2, aes(x=names, y=Rel_contribution, fill=Type_Contrib)) +
        geom_bar(stat="identity", width = .75) + theme_minimal() + scale_fill_manual(values = my_colors0, labels = c("Rural", "Urbano", "Urbanización")) +
        ylab("Contribución relativa (%)") + labs(fill = "Contribución", title = "Mujeres") + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120,140, -20))+
        theme(axis.title.y = element_blank(), axis.title.x = element_text(face = "bold", size=10, family = "Times"), axis.ticks.y = element_blank(),
              legend.title = element_text(size = 10, face = "bold", family = "Times"), axis.text = element_text(family = "Times"),
              legend.text = element_text(family = "Times"), plot.title = element_text(family = "Times", face = "bold"))  + coord_flip(ylim = c(-20, 130))
cr2


ggarrange(ca1, cr1, ca2, cr2, nrow=2, ncol = 2, hjust = "hv",common.legend = T, legend = "top")
ggsave("Figura26.tiff", width = 9, height = 11, units = "in", dpi = 600)




