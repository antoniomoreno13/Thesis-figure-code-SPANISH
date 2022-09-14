
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

#### ACCOMMODATE DATA ####
# Men
Ruraldata_men <- ALL_PREDICTS_MEN[which(ALL_PREDICTS_MEN$Typeofcommunity2cat == "Rural"),]
rural_men2 <- dcast(Ruraldata_men, Country_rec ~ year, value.var = "predict")
rural_men2$Inac_Dif_2002_2017_rural <- rural_men2$`2017`- rural_men2$`2002`

Urbandata_men <- ALL_PREDICTS_MEN[which(ALL_PREDICTS_MEN$Typeofcommunity2cat == "Urban"),]
urban_men2 <- dcast(Urbandata_men, Country_rec ~ year, value.var = "predict")
urban_men2$Inac_Dif_2002_2017_urban <- urban_men2$`2017`- urban_men2$`2002`


# Women

Ruraldata_women <- ALL_PREDICTS_WOMEN[which(ALL_PREDICTS_WOMEN$Typeofcommunity2cat == "Rural"),]
rural_women2 <- dcast(Ruraldata_women, Country_rec ~ year, value.var = "predict")
rural_women2$Inac_Dif_2002_2017_rural <- rural_women2$`2017`- rural_women2$`2002`


Urbandata_women <- ALL_PREDICTS_WOMEN[which(ALL_PREDICTS_WOMEN$Typeofcommunity2cat == "Urban"),]
urban_women2 <- dcast(Urbandata_women, Country_rec ~ year, value.var = "predict")
urban_women2$Inac_Dif_2002_2017_urban <- urban_women2$`2017`- urban_women2$`2002`


# Reading Sociodemographic country variables and adding its variables to previous dataframe 

Sociodemographic <- read.spss("C:/Users/anton/Desktop/Umubox/EUROBAROMETER/7. Trends PA Eurobarometer/MMR/Trends in PA country-level eurobarometer.sav", to.data.frame = T, use.missings = T)
Sociodemographic <- Sociodemographic[-c(93:165)]
Sociodemographic$Urbanization_Dif_2002_2017 <- Sociodemographic$Urban_pop_per_2017- Sociodemographic$Urban_pop_per_2002
Sociodemographic$Ruralization_Dif_2002_2017 <- Sociodemographic$Rural_pop_per_2017- Sociodemographic$Rural_pop_per_2002
colnames(Sociodemographic)[colnames(Sociodemographic) == "Country"] <- "Country_rec"


Sociodemographic$Country_rec <- fct_expand(Sociodemographic$Country_rec, c("AT - Austria", 
        "BE - Belgium", "BG - Bulgaria", "CY - Cyprus (Republic)",
        "CZ - Czech Republic", "DE Germany", "DK - Denmark", "EE - Estonia",
        "ES -Spain", "FI - Finland", "FR - France", "GR - Greece",
        "HR - Croatia", "HU - Hungary", "IE - Ireland", "IT - Italy",
        "LT - Lithuania", "LU - Luxembourg", "LV - Latvia", "MT - Malta",
        "NL - The Netherlands", "PL - Poland", "PT - Portugal", "RO - Romania",
        "SE - Sweden", "SI - Slovenia", "SK - Slovakia", "UK United Kingdom")) 

Sociodemographic$Country_rec[Sociodemographic$Country_rec == "AT - Austria          "] <- "AT - Austria"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "BE - Belgium          "] <- "BE - Belgium"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "BG - Bulgaria         "] <-"BG - Bulgaria"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "CY - Cyprus (Republic)"] <- "CY - Cyprus (Republic)"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "CZ - Czech Republic   "] <- "CZ - Czech Republic"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "DK - Denmark          "] <- "DK - Denmark"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "EE - Estonia          "] <- "EE - Estonia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "FI - Finland          "] <- "FI - Finland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "FR - France           "] <- "FR - France" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "DE Germany            " ] <- "DE Germany"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "GR - Greece           "] <- "GR - Greece"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "HR - Croatia          "] <- "HR - Croatia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "HU - Hungary          " ] <- "HU - Hungary"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "IE - Ireland          "] <- "IE - Ireland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "IT - Italy            " ] <- "IT - Italy"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LV - Latvia           "] <- "LV - Latvia" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LT - Lithuania        "] <- "LT - Lithuania"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "LU - Luxembourg       "] <- "LU - Luxembourg"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "MT - Malta            " ] <- "MT - Malta"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "NL - The Netherlands  "] <- "NL - The Netherlands"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "PL - Poland           " ] <- "PL - Poland"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "PT - Portugal         "] <- "PT - Portugal"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "RO - Romania          " ] <- "RO - Romania"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SE - Sweden           " ] <- "SE - Sweden"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SI - Slovenia         "] <-  "SI - Slovenia"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "SK - Slovakia         "] <- "SK - Slovakia" 
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "ES -Spain             "] <- "ES -Spain"
Sociodemographic$Country_rec[Sociodemographic$Country_rec == "UK United Kingdom     "] <- "UK United Kingdom"
Sociodemographic$Country_rec<- fct_drop(Sociodemographic$Country_rec, only = c("AT - Austria          ", 
                                                       "BE - Belgium          ", "BG - Bulgaria         ", "CY - Cyprus (Republic)",
                                                       "CZ - Czech Republic   ", "DE Germany            ", "DK - Denmark          ", "EE - Estonia          ",
                                                       "ES -Spain             ", "FI - Finland          ", "FR - France           ", "GR - Greece           ",
                                                       "HR - Croatia          ", "HU - Hungary          ", "IE - Ireland          ", "IT - Italy            ",
                                                       "LT - Lithuania        ", "LU - Luxembourg       ", "LV - Latvia           ", "MT - Malta            ",
                                                       "NL - The Netherlands  ", "PL - Poland           ", "PT - Portugal         ", "RO - Romania          ",
                                                       "SE - Sweden           ", "SI - Slovenia         ", "SK - Slovakia         ", "UK United Kingdom     "))


### EU 28 MAPS AND LINES PLOTS


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


## Final DATA FRAME
mapdata <- mapdata[-c(2,3, 5, 6, 8, 9, 12)]

mapdata$Country_rec[mapdata$id == "AT"] <- "AT - Austria"
mapdata$Country_rec[mapdata$id == "BE"] <- "BE - Belgium"
mapdata$Country_rec[mapdata$id == "BG"] <- "BG - Bulgaria"
mapdata$Country_rec[mapdata$id == "CH"] <- "CH - Switzerland"
mapdata$Country_rec[mapdata$id == "CY"] <- "CY - Cyprus (Republic)"
mapdata$Country_rec[mapdata$id == "CZ"] <- "CZ - Czech Republic"
mapdata$Country_rec[mapdata$id == "DE"] <- "DE Germany"
mapdata$Country_rec[mapdata$id == "DK"] <- "DK - Denmark"
mapdata$Country_rec[mapdata$id == "EE"] <- "EE - Estonia"
mapdata$Country_rec[mapdata$id == "EL"] <- "GR - Greece"
mapdata$Country_rec[mapdata$id == "ES"] <- "ES -Spain"
mapdata$Country_rec[mapdata$id == "FI"] <- "FI - Finland"
mapdata$Country_rec[mapdata$id == "FR"] <- "FR - France"
mapdata$Country_rec[mapdata$id == "HR"] <- "HR - Croatia"
mapdata$Country_rec[mapdata$id == "HU"] <- "HU - Hungary"
mapdata$Country_rec[mapdata$id == "IE"] <- "IE - Ireland"
mapdata$Country_rec[mapdata$id == "IS"] <- "IS - Iceland"
mapdata$Country_rec[mapdata$id == "IT"] <- "IT - Italy"
mapdata$Country_rec[mapdata$id == "LI"] <- "LI - Liechtenstein"
mapdata$Country_rec[mapdata$id == "LT"] <- "LT - Lithuania"
mapdata$Country_rec[mapdata$id == "LU"] <- "LU - Luxembourg"
mapdata$Country_rec[mapdata$id == "LV"] <- "LV - Latvia"
mapdata$Country_rec[mapdata$id == "MT"] <- "MT - Malta"
mapdata$Country_rec[mapdata$id == "NL"] <- "NL - The Netherlands"
mapdata$Country_rec[mapdata$id == "NO"] <- "NO - Norway"
mapdata$Country_rec[mapdata$id == "PL"] <- "PL - Poland"
mapdata$Country_rec[mapdata$id == "PT"] <- "PT - Portugal"
mapdata$Country_rec[mapdata$id == "RO"] <- "RO - Romania"
mapdata$Country_rec[mapdata$id == "SE"] <- "SE - Sweden"
mapdata$Country_rec[mapdata$id == "SI"] <- "SI - Slovenia"
mapdata$Country_rec[mapdata$id == "SK"] <- "SK - Slovakia"
mapdata$Country_rec[mapdata$id == "UK"] <- "UK United Kingdom"


# Men
mapdata_men <- merge(mapdata, rural_men2, by =  "Country_rec")
mapdata_men <- mapdata_men[-c(2:5)]
colnames(mapdata_men)[colnames(mapdata_men) == "2002"] <- "2002_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2003"] <- "2003_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2004"] <- "2004_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2005"] <- "2005_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2006"] <- "2006_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2007"] <- "2007_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2008"] <- "2008_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2009"] <- "2009_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2010"] <- "2010_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2011"] <- "2011_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2012"] <- "2012_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2013"] <- "2013_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2014"] <- "2014_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2015"] <- "2015_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2016"] <- "2016_rural"
colnames(mapdata_men)[colnames(mapdata_men) == "2017"] <- "2017_rural"

mapdata_men <- merge(mapdata_men, urban_men2, by = "Country_rec")
colnames(mapdata_men)[colnames(mapdata_men) == "2002"] <- "2002_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2003"] <- "2003_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2004"] <- "2004_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2005"] <- "2005_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2006"] <- "2006_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2007"] <- "2007_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2008"] <- "2008_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2009"] <- "2009_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2010"] <- "2010_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2011"] <- "2011_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2012"] <- "2012_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2013"] <- "2013_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2014"] <- "2014_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2015"] <- "2015_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2016"] <- "2016_urban"
colnames(mapdata_men)[colnames(mapdata_men) == "2017"] <- "2017_urban"

mapdata_men$Inac_dif_2002_urban_rural <- mapdata_men$`2002_urban` - mapdata_men$`2002_rural`
mapdata_men$Inac_dif_2003_urban_rural <- mapdata_men$`2003_urban` - mapdata_men$`2003_rural`
mapdata_men$Inac_dif_2004_urban_rural <- mapdata_men$`2004_urban` - mapdata_men$`2004_rural`
mapdata_men$Inac_dif_2005_urban_rural <- mapdata_men$`2005_urban` - mapdata_men$`2005_rural`
mapdata_men$Inac_dif_2006_urban_rural <- mapdata_men$`2006_urban` - mapdata_men$`2006_rural`
mapdata_men$Inac_dif_2007_urban_rural <- mapdata_men$`2007_urban` - mapdata_men$`2007_rural`
mapdata_men$Inac_dif_2008_urban_rural <- mapdata_men$`2008_urban` - mapdata_men$`2008_rural`
mapdata_men$Inac_dif_2009_urban_rural <- mapdata_men$`2009_urban` - mapdata_men$`2009_rural`
mapdata_men$Inac_dif_2010_urban_rural <- mapdata_men$`2010_urban` - mapdata_men$`2010_rural`
mapdata_men$Inac_dif_2011_urban_rural <- mapdata_men$`2011_urban` - mapdata_men$`2011_rural`
mapdata_men$Inac_dif_2012_urban_rural <- mapdata_men$`2012_urban` - mapdata_men$`2012_rural`
mapdata_men$Inac_dif_2013_urban_rural <- mapdata_men$`2013_urban` - mapdata_men$`2013_rural`
mapdata_men$Inac_dif_2014_urban_rural <- mapdata_men$`2014_urban` - mapdata_men$`2014_rural`
mapdata_men$Inac_dif_2015_urban_rural <- mapdata_men$`2015_urban` - mapdata_men$`2015_rural`
mapdata_men$Inac_dif_2016_urban_rural <- mapdata_men$`2016_urban` - mapdata_men$`2016_rural`
mapdata_men$Inac_dif_2017_urban_rural <- mapdata_men$`2017_urban` - mapdata_men$`2017_rural`

mapdata_men <- merge(mapdata_men, Sociodemographic, by = "Country_rec")
mapdata_men <- mapdata_men[-c(52)]

PopTot2002 <- sum(mapdata_men$Pob_2002); PopTot2003 <- sum(mapdata_men$Pob_2003) ## TOTAL POPULATION PER YEAR
PopTot2004 <- sum(mapdata_men$Pob_2004); PopTot2005 <- sum(mapdata_men$Pob_2005)
PopTot2006 <- sum(mapdata_men$Pob_2006); PopTot2007 <- sum(mapdata_men$Pob_2007)
PopTot2008 <- sum(mapdata_men$Pob_2008); PopTot2009 <- sum(mapdata_men$Pob_2009)
PopTot2010 <- sum(mapdata_men$Pob_2010); PopTot2011 <- sum(mapdata_men$Pob_2011)
PopTot2012 <- sum(mapdata_men$Pob_2012); PopTot2013 <- sum(mapdata_men$Pob_2013)
PopTot2014 <- sum(mapdata_men$Pob_2014); PopTot2015 <- sum(mapdata_men$Pob_2015)
PopTot2016 <- sum(mapdata_men$Pob_2016); PopTot2017 <- sum(mapdata_men$Pob_2017)

mapdata_men$weight_tot_2002 <- (mapdata_men$Pob_2002)/PopTot2002
mapdata_men$weight_tot_2003 <- (mapdata_men$Pob_2003)/PopTot2003
mapdata_men$weight_tot_2004 <- (mapdata_men$Pob_2004)/PopTot2004
mapdata_men$weight_tot_2005 <- (mapdata_men$Pob_2005)/PopTot2005
mapdata_men$weight_tot_2006 <- (mapdata_men$Pob_2006)/PopTot2006
mapdata_men$weight_tot_2007 <- (mapdata_men$Pob_2007)/PopTot2007
mapdata_men$weight_tot_2008 <- (mapdata_men$Pob_2008)/PopTot2008
mapdata_men$weight_tot_2009 <- (mapdata_men$Pob_2009)/PopTot2009
mapdata_men$weight_tot_2010 <- (mapdata_men$Pob_2010)/PopTot2010
mapdata_men$weight_tot_2011 <- (mapdata_men$Pob_2011)/PopTot2011
mapdata_men$weight_tot_2012 <- (mapdata_men$Pob_2012)/PopTot2012
mapdata_men$weight_tot_2013 <- (mapdata_men$Pob_2013)/PopTot2013
mapdata_men$weight_tot_2014 <- (mapdata_men$Pob_2014)/PopTot2014
mapdata_men$weight_tot_2015 <- (mapdata_men$Pob_2015)/PopTot2015
mapdata_men$weight_tot_2016 <- (mapdata_men$Pob_2016)/PopTot2016
mapdata_men$weight_tot_2017 <- (mapdata_men$Pob_2017)/PopTot2017





PopUrb2002 <- sum(mapdata_men$Pop_urb_2002); PopUrb2003 <- sum(mapdata_men$Pop_urb_2003) ## TOTAL URBAN POPULATION PER YEAR
PopUrb2004 <- sum(mapdata_men$Pop_urb_2004); PopUrb2005 <- sum(mapdata_men$Pop_urb_2005)
PopUrb2006 <- sum(mapdata_men$Pop_urb_2006); PopUrb2007 <- sum(mapdata_men$Pop_urb_2007)
PopUrb2008 <- sum(mapdata_men$Pop_urb_2008); PopUrb2009 <- sum(mapdata_men$Pop_urb_2009)
PopUrb2010 <- sum(mapdata_men$Pop_urb_2010); PopUrb2011 <- sum(mapdata_men$Pop_urb_2011)
PopUrb2012 <- sum(mapdata_men$Pop_urb_2012); PopUrb2013 <- sum(mapdata_men$Pop_urb_2013)
PopUrb2014 <- sum(mapdata_men$Pop_urb_2014); PopUrb2015 <- sum(mapdata_men$Pop_urb_2015)
PopUrb2016 <- sum(mapdata_men$Pop_urb_2016); PopUrb2017 <- sum(mapdata_men$Pop_urb_2017)

mapdata_men$weight_urb_2002 <- (mapdata_men$Pop_urb_2002)/PopUrb2002
mapdata_men$weight_urb_2003 <- (mapdata_men$Pop_urb_2003)/PopUrb2003
mapdata_men$weight_urb_2004 <- (mapdata_men$Pop_urb_2004)/PopUrb2004
mapdata_men$weight_urb_2005 <- (mapdata_men$Pop_urb_2005)/PopUrb2005
mapdata_men$weight_urb_2006 <- (mapdata_men$Pop_urb_2006)/PopUrb2006
mapdata_men$weight_urb_2007 <- (mapdata_men$Pop_urb_2007)/PopUrb2007
mapdata_men$weight_urb_2008 <- (mapdata_men$Pop_urb_2008)/PopUrb2008
mapdata_men$weight_urb_2009 <- (mapdata_men$Pop_urb_2009)/PopUrb2009
mapdata_men$weight_urb_2010 <- (mapdata_men$Pop_urb_2010)/PopUrb2010
mapdata_men$weight_urb_2011 <- (mapdata_men$Pop_urb_2011)/PopUrb2011
mapdata_men$weight_urb_2012 <- (mapdata_men$Pop_urb_2012)/PopUrb2012
mapdata_men$weight_urb_2013 <- (mapdata_men$Pop_urb_2013)/PopUrb2013
mapdata_men$weight_urb_2014 <- (mapdata_men$Pop_urb_2014)/PopUrb2014
mapdata_men$weight_urb_2015 <- (mapdata_men$Pop_urb_2015)/PopUrb2015
mapdata_men$weight_urb_2016 <- (mapdata_men$Pop_urb_2016)/PopUrb2016
mapdata_men$weight_urb_2017 <- (mapdata_men$Pop_urb_2017)/PopUrb2017





PopRur2002 <- sum(mapdata_men$Pop_rur_2002); PopRur2003 <- sum(mapdata_men$Pop_rur_2003) ## TOTAL RURAL POPULATION PER YEAR
PopRur2004 <- sum(mapdata_men$Pop_rur_2004); PopRur2005 <- sum(mapdata_men$Pop_rur_2005)
PopRur2006 <- sum(mapdata_men$Pop_rur_2006); PopRur2007 <- sum(mapdata_men$Pop_rur_2007)
PopRur2008 <- sum(mapdata_men$Pop_rur_2008); PopRur2009 <- sum(mapdata_men$Pop_rur_2009)
PopRur2010 <- sum(mapdata_men$Pop_rur_2010); PopRur2011 <- sum(mapdata_men$Pop_rur_2011)
PopRur2012 <- sum(mapdata_men$Pop_rur_2012); PopRur2013 <- sum(mapdata_men$Pop_rur_2013)
PopRur2014 <- sum(mapdata_men$Pop_rur_2014); PopRur2015 <- sum(mapdata_men$Pop_rur_2015)
PopRur2016 <- sum(mapdata_men$Pop_rur_2016); PopRur2017 <- sum(mapdata_men$Pop_rur_2017)

mapdata_men$weight_rur_2002 <- (mapdata_men$Pop_rur_2002)/PopRur2002
mapdata_men$weight_rur_2003 <- (mapdata_men$Pop_rur_2003)/PopRur2003
mapdata_men$weight_rur_2004 <- (mapdata_men$Pop_rur_2004)/PopRur2004
mapdata_men$weight_rur_2005 <- (mapdata_men$Pop_rur_2005)/PopRur2005
mapdata_men$weight_rur_2006 <- (mapdata_men$Pop_rur_2006)/PopRur2006
mapdata_men$weight_rur_2007 <- (mapdata_men$Pop_rur_2007)/PopRur2007
mapdata_men$weight_rur_2008 <- (mapdata_men$Pop_rur_2008)/PopRur2008
mapdata_men$weight_rur_2009 <- (mapdata_men$Pop_rur_2009)/PopRur2009
mapdata_men$weight_rur_2010 <- (mapdata_men$Pop_rur_2010)/PopRur2010
mapdata_men$weight_rur_2011 <- (mapdata_men$Pop_rur_2011)/PopRur2011
mapdata_men$weight_rur_2012 <- (mapdata_men$Pop_rur_2012)/PopRur2012
mapdata_men$weight_rur_2013 <- (mapdata_men$Pop_rur_2013)/PopRur2013
mapdata_men$weight_rur_2014 <- (mapdata_men$Pop_rur_2014)/PopRur2014
mapdata_men$weight_rur_2015 <- (mapdata_men$Pop_rur_2015)/PopRur2015
mapdata_men$weight_rur_2016 <- (mapdata_men$Pop_rur_2016)/PopRur2016
mapdata_men$weight_rur_2017 <- (mapdata_men$Pop_rur_2017)/PopRur2017


mapdata_men$national_inac_2002 <- ((mapdata_men$`2002_urban` * mapdata_men$Urban_pop_per_2002)+(mapdata_men$`2002_rural` * mapdata_men$Rural_pop_per_2002))/100
mapdata_men$national_inac_2003 <- ((mapdata_men$`2003_urban` * mapdata_men$Urban_pop_per_2003)+(mapdata_men$`2003_rural` * mapdata_men$Rural_pop_per_2003))/100
mapdata_men$national_inac_2004 <- ((mapdata_men$`2004_urban` * mapdata_men$Urban_pop_per_2004)+(mapdata_men$`2004_rural` * mapdata_men$Rural_pop_per_2004))/100
mapdata_men$national_inac_2005 <- ((mapdata_men$`2005_urban` * mapdata_men$Urban_pop_per_2005)+(mapdata_men$`2005_rural` * mapdata_men$Rural_pop_per_2005))/100
mapdata_men$national_inac_2006 <- ((mapdata_men$`2006_urban` * mapdata_men$Urban_pop_per_2006)+(mapdata_men$`2006_rural` * mapdata_men$Rural_pop_per_2006))/100
mapdata_men$national_inac_2007 <- ((mapdata_men$`2007_urban` * mapdata_men$Urban_pop_per_2007)+(mapdata_men$`2007_rural` * mapdata_men$Rural_pop_per_2007))/100
mapdata_men$national_inac_2008 <- ((mapdata_men$`2008_urban` * mapdata_men$Urban_pop_per_2008)+(mapdata_men$`2008_rural` * mapdata_men$Rural_pop_per_2008))/100
mapdata_men$national_inac_2009 <- ((mapdata_men$`2009_urban` * mapdata_men$Urban_pop_per_2009)+(mapdata_men$`2009_rural` * mapdata_men$Rural_pop_per_2009))/100
mapdata_men$national_inac_2010 <- ((mapdata_men$`2010_urban` * mapdata_men$Urban_pop_per_2010)+(mapdata_men$`2010_rural` * mapdata_men$Rural_pop_per_2010))/100
mapdata_men$national_inac_2011 <- ((mapdata_men$`2011_urban` * mapdata_men$Urban_pop_per_2011)+(mapdata_men$`2011_rural` * mapdata_men$Rural_pop_per_2011))/100
mapdata_men$national_inac_2012 <- ((mapdata_men$`2012_urban` * mapdata_men$Urban_pop_per_2012)+(mapdata_men$`2012_rural` * mapdata_men$Rural_pop_per_2012))/100
mapdata_men$national_inac_2013 <- ((mapdata_men$`2013_urban` * mapdata_men$Urban_pop_per_2013)+(mapdata_men$`2013_rural` * mapdata_men$Rural_pop_per_2013))/100
mapdata_men$national_inac_2014 <- ((mapdata_men$`2014_urban` * mapdata_men$Urban_pop_per_2014)+(mapdata_men$`2014_rural` * mapdata_men$Rural_pop_per_2014))/100
mapdata_men$national_inac_2015 <- ((mapdata_men$`2015_urban` * mapdata_men$Urban_pop_per_2015)+(mapdata_men$`2015_rural` * mapdata_men$Rural_pop_per_2015))/100
mapdata_men$national_inac_2016 <- ((mapdata_men$`2016_urban` * mapdata_men$Urban_pop_per_2016)+(mapdata_men$`2016_rural` * mapdata_men$Rural_pop_per_2016))/100
mapdata_men$national_inac_2017 <- ((mapdata_men$`2017_urban` * mapdata_men$Urban_pop_per_2017)+(mapdata_men$`2017_rural` * mapdata_men$Rural_pop_per_2017))/100


mapdata_men$Inac_Dif_2002_2017 <- mapdata_men$national_inac_2017 - mapdata_men$national_inac_2002
mapdata_men$Pop_tot_mean_2002_2017 <- (mapdata_men$Pob_2002 + mapdata_men$Pob_2017)/2
PobTot_mean_2002_2017 <- sum(mapdata_men$Pop_tot_mean_2002_2017)
mapdata_men$weight_Pop_2002_2017_mean <- (mapdata_men$Pop_tot_mean_2002_2017)/PobTot_mean_2002_2017


# Women
mapdata_women <- merge(mapdata, rural_women2, by =  "Country_rec")
mapdata_women <- mapdata_women[-c(2:5)]
colnames(mapdata_women)[colnames(mapdata_women) == "2002"] <- "2002_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2003"] <- "2003_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2004"] <- "2004_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2005"] <- "2005_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2006"] <- "2006_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2007"] <- "2007_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2008"] <- "2008_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2009"] <- "2009_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2010"] <- "2010_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2011"] <- "2011_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2012"] <- "2012_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2013"] <- "2013_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2014"] <- "2014_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2015"] <- "2015_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2016"] <- "2016_rural"
colnames(mapdata_women)[colnames(mapdata_women) == "2017"] <- "2017_rural"

mapdata_women <- merge(mapdata_women, urban_women2, by = "Country_rec")
colnames(mapdata_women)[colnames(mapdata_women) == "2002"] <- "2002_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2003"] <- "2003_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2004"] <- "2004_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2005"] <- "2005_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2006"] <- "2006_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2007"] <- "2007_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2008"] <- "2008_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2009"] <- "2009_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2010"] <- "2010_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2011"] <- "2011_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2012"] <- "2012_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2013"] <- "2013_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2014"] <- "2014_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2015"] <- "2015_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2016"] <- "2016_urban"
colnames(mapdata_women)[colnames(mapdata_women) == "2017"] <- "2017_urban"

mapdata_women$Inac_dif_2002_urban_rural <- mapdata_women$`2002_urban` - mapdata_women$`2002_rural`
mapdata_women$Inac_dif_2003_urban_rural <- mapdata_women$`2003_urban` - mapdata_women$`2003_rural`
mapdata_women$Inac_dif_2004_urban_rural <- mapdata_women$`2004_urban` - mapdata_women$`2004_rural`
mapdata_women$Inac_dif_2005_urban_rural <- mapdata_women$`2005_urban` - mapdata_women$`2005_rural`
mapdata_women$Inac_dif_2006_urban_rural <- mapdata_women$`2006_urban` - mapdata_women$`2006_rural`
mapdata_women$Inac_dif_2007_urban_rural <- mapdata_women$`2007_urban` - mapdata_women$`2007_rural`
mapdata_women$Inac_dif_2008_urban_rural <- mapdata_women$`2008_urban` - mapdata_women$`2008_rural`
mapdata_women$Inac_dif_2009_urban_rural <- mapdata_women$`2009_urban` - mapdata_women$`2009_rural`
mapdata_women$Inac_dif_2010_urban_rural <- mapdata_women$`2010_urban` - mapdata_women$`2010_rural`
mapdata_women$Inac_dif_2011_urban_rural <- mapdata_women$`2011_urban` - mapdata_women$`2011_rural`
mapdata_women$Inac_dif_2012_urban_rural <- mapdata_women$`2012_urban` - mapdata_women$`2012_rural`
mapdata_women$Inac_dif_2013_urban_rural <- mapdata_women$`2013_urban` - mapdata_women$`2013_rural`
mapdata_women$Inac_dif_2014_urban_rural <- mapdata_women$`2014_urban` - mapdata_women$`2014_rural`
mapdata_women$Inac_dif_2015_urban_rural <- mapdata_women$`2015_urban` - mapdata_women$`2015_rural`
mapdata_women$Inac_dif_2016_urban_rural <- mapdata_women$`2016_urban` - mapdata_women$`2016_rural`
mapdata_women$Inac_dif_2017_urban_rural <- mapdata_women$`2017_urban` - mapdata_women$`2017_rural`

mapdata_women <- merge(mapdata_women, Sociodemographic, by = "Country_rec")
mapdata_women <- mapdata_women[-c(52)]

mapdata_women$weight_tot_2002 <- mapdata_men$weight_tot_2002;  mapdata_women$weight_tot_2003 <- mapdata_men$weight_tot_2003;
mapdata_women$weight_tot_2004 <- mapdata_men$weight_tot_2004;  mapdata_women$weight_tot_2005 <- mapdata_men$weight_tot_2005;
mapdata_women$weight_tot_2006 <- mapdata_men$weight_tot_2006;  mapdata_women$weight_tot_2007 <- mapdata_men$weight_tot_2007;
mapdata_women$weight_tot_2008 <- mapdata_men$weight_tot_2008;  mapdata_women$weight_tot_2009 <- mapdata_men$weight_tot_2009;
mapdata_women$weight_tot_2010 <- mapdata_men$weight_tot_2010;  mapdata_women$weight_tot_2011 <- mapdata_men$weight_tot_2011;
mapdata_women$weight_tot_2012 <- mapdata_men$weight_tot_2012;  mapdata_women$weight_tot_2013 <- mapdata_men$weight_tot_2013;
mapdata_women$weight_tot_2014 <- mapdata_men$weight_tot_2014;  mapdata_women$weight_tot_2015 <- mapdata_men$weight_tot_2015;
mapdata_women$weight_tot_2016 <- mapdata_men$weight_tot_2016;  mapdata_women$weight_tot_2017 <- mapdata_men$weight_tot_2017;


mapdata_women$weight_urb_2002 <- mapdata_men$weight_urb_2002;  mapdata_women$weight_urb_2003 <- mapdata_men$weight_urb_2003;
mapdata_women$weight_urb_2004 <- mapdata_men$weight_urb_2004;  mapdata_women$weight_urb_2005 <- mapdata_men$weight_urb_2005;
mapdata_women$weight_urb_2006 <- mapdata_men$weight_urb_2006;  mapdata_women$weight_urb_2007 <- mapdata_men$weight_urb_2007;
mapdata_women$weight_urb_2008 <- mapdata_men$weight_urb_2008;  mapdata_women$weight_urb_2009 <- mapdata_men$weight_urb_2009;
mapdata_women$weight_urb_2010 <- mapdata_men$weight_urb_2010;  mapdata_women$weight_urb_2011 <- mapdata_men$weight_urb_2011;
mapdata_women$weight_urb_2012 <- mapdata_men$weight_urb_2012;  mapdata_women$weight_urb_2013 <- mapdata_men$weight_urb_2013;
mapdata_women$weight_urb_2014 <- mapdata_men$weight_urb_2014;  mapdata_women$weight_urb_2015 <- mapdata_men$weight_urb_2015;
mapdata_women$weight_urb_2016 <- mapdata_men$weight_urb_2016;  mapdata_women$weight_urb_2017 <- mapdata_men$weight_urb_2017;


mapdata_women$weight_rur_2002 <- mapdata_men$weight_rur_2002;  mapdata_women$weight_rur_2003 <- mapdata_men$weight_rur_2003;
mapdata_women$weight_rur_2004 <- mapdata_men$weight_rur_2004;  mapdata_women$weight_rur_2005 <- mapdata_men$weight_rur_2005;
mapdata_women$weight_rur_2006 <- mapdata_men$weight_rur_2006;  mapdata_women$weight_rur_2007 <- mapdata_men$weight_rur_2007;
mapdata_women$weight_rur_2008 <- mapdata_men$weight_rur_2008;  mapdata_women$weight_rur_2009 <- mapdata_men$weight_rur_2009;
mapdata_women$weight_rur_2010 <- mapdata_men$weight_rur_2010;  mapdata_women$weight_rur_2011 <- mapdata_men$weight_rur_2011;
mapdata_women$weight_rur_2012 <- mapdata_men$weight_rur_2012;  mapdata_women$weight_rur_2013 <- mapdata_men$weight_rur_2013;
mapdata_women$weight_rur_2014 <- mapdata_men$weight_rur_2014;  mapdata_women$weight_rur_2015 <- mapdata_men$weight_rur_2015;
mapdata_women$weight_rur_2016 <- mapdata_men$weight_rur_2016;  mapdata_women$weight_rur_2017 <- mapdata_men$weight_rur_2017;


mapdata_women$national_inac_2002 <- ((mapdata_women$`2002_urban` * mapdata_women$Urban_pop_per_2002)+(mapdata_women$`2002_rural` * mapdata_women$Rural_pop_per_2002))/100
mapdata_women$national_inac_2003 <- ((mapdata_women$`2003_urban` * mapdata_women$Urban_pop_per_2003)+(mapdata_women$`2003_rural` * mapdata_women$Rural_pop_per_2003))/100
mapdata_women$national_inac_2004 <- ((mapdata_women$`2004_urban` * mapdata_women$Urban_pop_per_2004)+(mapdata_women$`2004_rural` * mapdata_women$Rural_pop_per_2004))/100
mapdata_women$national_inac_2005 <- ((mapdata_women$`2005_urban` * mapdata_women$Urban_pop_per_2005)+(mapdata_women$`2005_rural` * mapdata_women$Rural_pop_per_2005))/100
mapdata_women$national_inac_2006 <- ((mapdata_women$`2006_urban` * mapdata_women$Urban_pop_per_2006)+(mapdata_women$`2006_rural` * mapdata_women$Rural_pop_per_2006))/100
mapdata_women$national_inac_2007 <- ((mapdata_women$`2007_urban` * mapdata_women$Urban_pop_per_2007)+(mapdata_women$`2007_rural` * mapdata_women$Rural_pop_per_2007))/100
mapdata_women$national_inac_2008 <- ((mapdata_women$`2008_urban` * mapdata_women$Urban_pop_per_2008)+(mapdata_women$`2008_rural` * mapdata_women$Rural_pop_per_2008))/100
mapdata_women$national_inac_2009 <- ((mapdata_women$`2009_urban` * mapdata_women$Urban_pop_per_2009)+(mapdata_women$`2009_rural` * mapdata_women$Rural_pop_per_2009))/100
mapdata_women$national_inac_2010 <- ((mapdata_women$`2010_urban` * mapdata_women$Urban_pop_per_2010)+(mapdata_women$`2010_rural` * mapdata_women$Rural_pop_per_2010))/100
mapdata_women$national_inac_2011 <- ((mapdata_women$`2011_urban` * mapdata_women$Urban_pop_per_2011)+(mapdata_women$`2011_rural` * mapdata_women$Rural_pop_per_2011))/100
mapdata_women$national_inac_2012 <- ((mapdata_women$`2012_urban` * mapdata_women$Urban_pop_per_2012)+(mapdata_women$`2012_rural` * mapdata_women$Rural_pop_per_2012))/100
mapdata_women$national_inac_2013 <- ((mapdata_women$`2013_urban` * mapdata_women$Urban_pop_per_2013)+(mapdata_women$`2013_rural` * mapdata_women$Rural_pop_per_2013))/100
mapdata_women$national_inac_2014 <- ((mapdata_women$`2014_urban` * mapdata_women$Urban_pop_per_2014)+(mapdata_women$`2014_rural` * mapdata_women$Rural_pop_per_2014))/100
mapdata_women$national_inac_2015 <- ((mapdata_women$`2015_urban` * mapdata_women$Urban_pop_per_2015)+(mapdata_women$`2015_rural` * mapdata_women$Rural_pop_per_2015))/100
mapdata_women$national_inac_2016 <- ((mapdata_women$`2016_urban` * mapdata_women$Urban_pop_per_2016)+(mapdata_women$`2016_rural` * mapdata_women$Rural_pop_per_2016))/100
mapdata_women$national_inac_2017 <- ((mapdata_women$`2017_urban` * mapdata_women$Urban_pop_per_2017)+(mapdata_women$`2017_rural` * mapdata_women$Rural_pop_per_2017))/100


mapdata_women$Inac_Dif_2002_2017 <- mapdata_women$national_inac_2017 - mapdata_women$national_inac_2002
mapdata_women$Pop_tot_mean_2002_2017 <- (mapdata_women$Pob_2002 + mapdata_women$Pob_2017)/2
mapdata_women$weight_Pop_2002_2017_mean <- (mapdata_women$Pop_tot_mean_2002_2017)/PobTot_mean_2002_2017



#### COMPUTE CONTRIBUTIONS - Absolute and relatives ####
# Men 
mapdata_men$Rural_contribution <- (mapdata_men$Inac_Dif_2002_2017_rural * mapdata_men$Rural_pop_per_2002)/100
mapdata_men$Urban_contribution <- (mapdata_men$Inac_Dif_2002_2017_urban * mapdata_men$Urban_pop_per_2002)/100
mapdata_men$Urbanization_contribution <- (mapdata_men$Urbanization_Dif_2002_2017 * mapdata_men$Inac_dif_2017_urban_rural)/100

mapdata_men$Total_contribution <- mapdata_men$Rural_contribution + mapdata_men$Urban_contribution + 
        mapdata_men$Urbanization_contribution ## Total contribution is also the change in Physical inactive % from 2002 to 2017

mapdata_men$Rural_contrib_relative <- (mapdata_men$Rural_contribution * 100)/mapdata_men$Total_contribution
mapdata_men$Urban_contrib_relative <- (mapdata_men$Urban_contribution * 100)/mapdata_men$Total_contribution
mapdata_men$Urbanization_contrib_relative <- (mapdata_men$Urbanization_contribution * 100)/mapdata_men$Total_contribution



# Women
mapdata_women$Rural_contribution <- (mapdata_women$Inac_Dif_2002_2017_rural * mapdata_women$Rural_pop_per_2002)/100
mapdata_women$Urban_contribution <- (mapdata_women$Inac_Dif_2002_2017_urban * mapdata_women$Urban_pop_per_2002)/100
mapdata_women$Urbanization_contribution <- (mapdata_women$Urbanization_Dif_2002_2017 * mapdata_women$Inac_dif_2017_urban_rural)/100



mapdata_women$Total_contribution <- mapdata_women$Rural_contribution + mapdata_women$Urban_contribution + 
        mapdata_women$Urbanization_contribution ## Total contribution is also the change in Physical inactive % from 2002 to 2017

mapdata_women$Rural_contrib_relative <- (mapdata_women$Rural_contribution * 100)/mapdata_women$Total_contribution
mapdata_women$Urban_contrib_relative <- (mapdata_women$Urban_contribution * 100)/mapdata_women$Total_contribution
mapdata_women$Urbanization_contrib_relative <- (mapdata_women$Urbanization_contribution * 100)/mapdata_women$Total_contribution


#### MEANS AND WEIGHTED MEANS ####
# Men
mean(mapdata_men$`2002_rural`); weighted.mean(mapdata_men$`2002_rural`, w = mapdata_men$weight_rur_2002)-> wmrural_men2002
mean(mapdata_men$`2003_rural`); weighted.mean(mapdata_men$`2003_rural`, w = mapdata_men$weight_rur_2003)-> wmrural_men2003
mean(mapdata_men$`2004_rural`); weighted.mean(mapdata_men$`2004_rural`, w = mapdata_men$weight_rur_2004)-> wmrural_men2004
mean(mapdata_men$`2005_rural`); weighted.mean(mapdata_men$`2005_rural`, w = mapdata_men$weight_rur_2005)-> wmrural_men2005
mean(mapdata_men$`2006_rural`); weighted.mean(mapdata_men$`2006_rural`, w = mapdata_men$weight_rur_2006)-> wmrural_men2006
mean(mapdata_men$`2007_rural`); weighted.mean(mapdata_men$`2007_rural`, w = mapdata_men$weight_rur_2007)-> wmrural_men2007
mean(mapdata_men$`2008_rural`); weighted.mean(mapdata_men$`2008_rural`, w = mapdata_men$weight_rur_2008)-> wmrural_men2008
mean(mapdata_men$`2009_rural`); weighted.mean(mapdata_men$`2009_rural`, w = mapdata_men$weight_rur_2009)-> wmrural_men2009
mean(mapdata_men$`2010_rural`); weighted.mean(mapdata_men$`2010_rural`, w = mapdata_men$weight_rur_2010)-> wmrural_men2010
mean(mapdata_men$`2011_rural`); weighted.mean(mapdata_men$`2011_rural`, w = mapdata_men$weight_rur_2011)-> wmrural_men2011
mean(mapdata_men$`2012_rural`); weighted.mean(mapdata_men$`2012_rural`, w = mapdata_men$weight_rur_2012)-> wmrural_men2012
mean(mapdata_men$`2013_rural`); weighted.mean(mapdata_men$`2013_rural`, w = mapdata_men$weight_rur_2013)-> wmrural_men2013
mean(mapdata_men$`2014_rural`); weighted.mean(mapdata_men$`2014_rural`, w = mapdata_men$weight_rur_2014)-> wmrural_men2014
mean(mapdata_men$`2015_rural`); weighted.mean(mapdata_men$`2015_rural`, w = mapdata_men$weight_rur_2015)-> wmrural_men2015
mean(mapdata_men$`2016_rural`); weighted.mean(mapdata_men$`2016_rural`, w = mapdata_men$weight_rur_2016)-> wmrural_men2016
mean(mapdata_men$`2017_rural`); weighted.mean(mapdata_men$`2017_rural`, w = mapdata_men$weight_rur_2017)-> wmrural_men2017


mean(mapdata_men$`2002_urban`); weighted.mean(mapdata_men$`2002_urban`, w = mapdata_men$weight_urb_2002)-> wmurban_men2002
mean(mapdata_men$`2003_urban`); weighted.mean(mapdata_men$`2003_urban`, w = mapdata_men$weight_urb_2003)-> wmurban_men2003
mean(mapdata_men$`2004_urban`); weighted.mean(mapdata_men$`2004_urban`, w = mapdata_men$weight_urb_2004)-> wmurban_men2004
mean(mapdata_men$`2005_urban`); weighted.mean(mapdata_men$`2005_urban`, w = mapdata_men$weight_urb_2005)-> wmurban_men2005
mean(mapdata_men$`2006_urban`); weighted.mean(mapdata_men$`2006_urban`, w = mapdata_men$weight_urb_2006)-> wmurban_men2006
mean(mapdata_men$`2007_urban`); weighted.mean(mapdata_men$`2007_urban`, w = mapdata_men$weight_urb_2007)-> wmurban_men2007
mean(mapdata_men$`2008_urban`); weighted.mean(mapdata_men$`2008_urban`, w = mapdata_men$weight_urb_2008)-> wmurban_men2008
mean(mapdata_men$`2009_urban`); weighted.mean(mapdata_men$`2009_urban`, w = mapdata_men$weight_urb_2009)-> wmurban_men2009
mean(mapdata_men$`2010_urban`); weighted.mean(mapdata_men$`2010_urban`, w = mapdata_men$weight_urb_2010)-> wmurban_men2010
mean(mapdata_men$`2011_urban`); weighted.mean(mapdata_men$`2011_urban`, w = mapdata_men$weight_urb_2011)-> wmurban_men2011
mean(mapdata_men$`2012_urban`); weighted.mean(mapdata_men$`2012_urban`, w = mapdata_men$weight_urb_2012)-> wmurban_men2012
mean(mapdata_men$`2013_urban`); weighted.mean(mapdata_men$`2013_urban`, w = mapdata_men$weight_urb_2013)-> wmurban_men2013
mean(mapdata_men$`2014_urban`); weighted.mean(mapdata_men$`2014_urban`, w = mapdata_men$weight_urb_2014)-> wmurban_men2014
mean(mapdata_men$`2015_urban`); weighted.mean(mapdata_men$`2015_urban`, w = mapdata_men$weight_urb_2015)-> wmurban_men2015
mean(mapdata_men$`2016_urban`); weighted.mean(mapdata_men$`2016_urban`, w = mapdata_men$weight_urb_2016)-> wmurban_men2016
mean(mapdata_men$`2017_urban`); weighted.mean(mapdata_men$`2017_urban`, w = mapdata_men$weight_urb_2017)-> wmurban_men2017

c_men <- data.frame(Weighted_prev=c(wmrural_men2002,wmrural_men2003,wmrural_men2004,wmrural_men2005,wmrural_men2006,wmrural_men2007,wmrural_men2008,
                                    wmrural_men2009,wmrural_men2010,wmrural_men2011,wmrural_men2012,wmrural_men2013,wmrural_men2014,wmrural_men2015,
                                    wmrural_men2016,wmrural_men2017))
c_men$Typeofcommunity2cat <- "Rural"
c_men$year <- c(seq(from=2002, to=2017, by=1))


d_men <- data.frame(Weighted_prev=c(wmurban_men2002,wmurban_men2003,wmurban_men2004,wmurban_men2005,wmurban_men2006,wmurban_men2007,wmurban_men2008,
                                    wmurban_men2009,wmurban_men2010,wmurban_men2011,wmurban_men2012,wmurban_men2013,wmurban_men2014,wmurban_men2015,
                                    wmurban_men2016,wmurban_men2017))
d_men$Typeofcommunity2cat <- "Urban"
d_men$year <- c(seq(from=2002, to=2017, by=1))

a <- bind_rows(c_men, d_men)

mean(mapdata_men$`2002_urban`) - mean(mapdata_men$`2002_rural`)
weighted.mean(mapdata_men$`2002_urban`, w = mapdata_men$weight_urb_2002) - weighted.mean(mapdata_men$`2002_rural`, w = mapdata_men$weight_rur_2002)-> wmdif_men2002

mean(mapdata_men$`2003_urban`) - mean(mapdata_men$`2003_rural`)
weighted.mean(mapdata_men$`2003_urban`, w = mapdata_men$weight_urb_2003) - weighted.mean(mapdata_men$`2003_rural`, w = mapdata_men$weight_rur_2003)-> wmdif_men2003

mean(mapdata_men$`2004_urban`) - mean(mapdata_men$`2004_rural`)
weighted.mean(mapdata_men$`2004_urban`, w = mapdata_men$weight_urb_2004) - weighted.mean(mapdata_men$`2004_rural`, w = mapdata_men$weight_rur_2004)-> wmdif_men2004

mean(mapdata_men$`2005_urban`) - mean(mapdata_men$`2005_rural`)
weighted.mean(mapdata_men$`2005_urban`, w = mapdata_men$weight_urb_2005) - weighted.mean(mapdata_men$`2005_rural`, w = mapdata_men$weight_rur_2005)-> wmdif_men2005

mean(mapdata_men$`2006_urban`) - mean(mapdata_men$`2006_rural`)
weighted.mean(mapdata_men$`2006_urban`, w = mapdata_men$weight_urb_2006) - weighted.mean(mapdata_men$`2006_rural`, w = mapdata_men$weight_rur_2006)-> wmdif_men2006

mean(mapdata_men$`2007_urban`) - mean(mapdata_men$`2007_rural`)
weighted.mean(mapdata_men$`2007_urban`, w = mapdata_men$weight_urb_2007) - weighted.mean(mapdata_men$`2007_rural`, w = mapdata_men$weight_rur_2007)-> wmdif_men2007

mean(mapdata_men$`2008_urban`) - mean(mapdata_men$`2008_rural`)
weighted.mean(mapdata_men$`2008_urban`, w = mapdata_men$weight_urb_2008) - weighted.mean(mapdata_men$`2008_rural`, w = mapdata_men$weight_rur_2008)-> wmdif_men2008

mean(mapdata_men$`2009_urban`) - mean(mapdata_men$`2009_rural`)
weighted.mean(mapdata_men$`2009_urban`, w = mapdata_men$weight_urb_2009) - weighted.mean(mapdata_men$`2009_rural`, w = mapdata_men$weight_rur_2009)-> wmdif_men2009

mean(mapdata_men$`2010_urban`) - mean(mapdata_men$`2010_rural`)
weighted.mean(mapdata_men$`2010_urban`, w = mapdata_men$weight_urb_2010) - weighted.mean(mapdata_men$`2010_rural`, w = mapdata_men$weight_rur_2010)-> wmdif_men2010

mean(mapdata_men$`2011_urban`) - mean(mapdata_men$`2011_rural`)
weighted.mean(mapdata_men$`2011_urban`, w = mapdata_men$weight_urb_2011) - weighted.mean(mapdata_men$`2011_rural`, w = mapdata_men$weight_rur_2011)-> wmdif_men2011

mean(mapdata_men$`2012_urban`) - mean(mapdata_men$`2012_rural`)
weighted.mean(mapdata_men$`2012_urban`, w = mapdata_men$weight_urb_2012) - weighted.mean(mapdata_men$`2012_rural`, w = mapdata_men$weight_rur_2012)-> wmdif_men2012

mean(mapdata_men$`2013_urban`) - mean(mapdata_men$`2013_rural`)
weighted.mean(mapdata_men$`2013_urban`, w = mapdata_men$weight_urb_2013) - weighted.mean(mapdata_men$`2013_rural`, w = mapdata_men$weight_rur_2013)-> wmdif_men2013

mean(mapdata_men$`2014_urban`) - mean(mapdata_men$`2014_rural`)
weighted.mean(mapdata_men$`2014_urban`, w = mapdata_men$weight_urb_2014) - weighted.mean(mapdata_men$`2014_rural`, w = mapdata_men$weight_rur_2014)-> wmdif_men2014

mean(mapdata_men$`2015_urban`) - mean(mapdata_men$`2015_rural`)
weighted.mean(mapdata_men$`2015_urban`, w = mapdata_men$weight_urb_2015) - weighted.mean(mapdata_men$`2015_rural`, w = mapdata_men$weight_rur_2015)-> wmdif_men2015

mean(mapdata_men$`2016_urban`) - mean(mapdata_men$`2016_rural`)
weighted.mean(mapdata_men$`2016_urban`, w = mapdata_men$weight_urb_2016) - weighted.mean(mapdata_men$`2016_rural`, w = mapdata_men$weight_rur_2016)-> wmdif_men2016

mean(mapdata_men$`2017_urban`) - mean(mapdata_men$`2017_rural`)
weighted.mean(mapdata_men$`2017_urban`, w = mapdata_men$weight_urb_2017) - weighted.mean(mapdata_men$`2017_rural`, w = mapdata_men$weight_rur_2017)-> wmdif_men2017


e_men <- data.frame(Weighted_prev=c(wmdif_men2002,wmdif_men2003,wmdif_men2004,wmdif_men2005,wmdif_men2006,wmdif_men2007,wmdif_men2008,
                                    wmdif_men2009,wmdif_men2010,wmdif_men2011,wmdif_men2012,wmdif_men2013,wmdif_men2014,wmdif_men2015,
                                    wmdif_men2016,wmdif_men2017))
e_men$year <- c(seq(from=2002, to=2017, by=1))



mean(mapdata_men$national_inac_2002); weighted.mean(mapdata_men$national_inac_2002, w = mapdata_men$weight_tot_2002)
mean(mapdata_men$national_inac_2003); weighted.mean(mapdata_men$national_inac_2003, w = mapdata_men$weight_tot_2003)
mean(mapdata_men$national_inac_2004); weighted.mean(mapdata_men$national_inac_2004, w = mapdata_men$weight_tot_2004)
mean(mapdata_men$national_inac_2005); weighted.mean(mapdata_men$national_inac_2005, w = mapdata_men$weight_tot_2005)
mean(mapdata_men$national_inac_2006); weighted.mean(mapdata_men$national_inac_2006, w = mapdata_men$weight_tot_2006)
mean(mapdata_men$national_inac_2007); weighted.mean(mapdata_men$national_inac_2007, w = mapdata_men$weight_tot_2007)
mean(mapdata_men$national_inac_2008); weighted.mean(mapdata_men$national_inac_2008, w = mapdata_men$weight_tot_2008)
mean(mapdata_men$national_inac_2009); weighted.mean(mapdata_men$national_inac_2009, w = mapdata_men$weight_tot_2009)
mean(mapdata_men$national_inac_2010); weighted.mean(mapdata_men$national_inac_2010, w = mapdata_men$weight_tot_2010)
mean(mapdata_men$national_inac_2011); weighted.mean(mapdata_men$national_inac_2011, w = mapdata_men$weight_tot_2011)
mean(mapdata_men$national_inac_2012); weighted.mean(mapdata_men$national_inac_2012, w = mapdata_men$weight_tot_2012)
mean(mapdata_men$national_inac_2013); weighted.mean(mapdata_men$national_inac_2013, w = mapdata_men$weight_tot_2013)
mean(mapdata_men$national_inac_2014); weighted.mean(mapdata_men$national_inac_2014, w = mapdata_men$weight_tot_2014)
mean(mapdata_men$national_inac_2015); weighted.mean(mapdata_men$national_inac_2015, w = mapdata_men$weight_tot_2015)
mean(mapdata_men$national_inac_2016); weighted.mean(mapdata_men$national_inac_2016, w = mapdata_men$weight_tot_2016)
mean(mapdata_men$national_inac_2017); weighted.mean(mapdata_men$national_inac_2017, w = mapdata_men$weight_tot_2017)


mean(mapdata_men$Urban_pop_per_2002); weighted.mean(mapdata_men$Urban_pop_per_2002, w = mapdata_men$weight_tot_2002)
mean(mapdata_men$Urban_pop_per_2017); weighted.mean(mapdata_men$Urban_pop_per_2017, w = mapdata_men$weight_tot_2017)


mean(mapdata_men$Inac_dif_2002_urban_rural); weighted.mean(mapdata_men$Inac_dif_2002_urban_rural, w = mapdata_men$weight_tot_2002)
mean(mapdata_men$Inac_dif_2003_urban_rural); weighted.mean(mapdata_men$Inac_dif_2003_urban_rural, w = mapdata_men$weight_tot_2003)
mean(mapdata_men$Inac_dif_2004_urban_rural); weighted.mean(mapdata_men$Inac_dif_2004_urban_rural, w = mapdata_men$weight_tot_2004)
mean(mapdata_men$Inac_dif_2005_urban_rural); weighted.mean(mapdata_men$Inac_dif_2005_urban_rural, w = mapdata_men$weight_tot_2005)
mean(mapdata_men$Inac_dif_2006_urban_rural); weighted.mean(mapdata_men$Inac_dif_2006_urban_rural, w = mapdata_men$weight_tot_2006)
mean(mapdata_men$Inac_dif_2007_urban_rural); weighted.mean(mapdata_men$Inac_dif_2007_urban_rural, w = mapdata_men$weight_tot_2007)
mean(mapdata_men$Inac_dif_2008_urban_rural); weighted.mean(mapdata_men$Inac_dif_2008_urban_rural, w = mapdata_men$weight_tot_2008)
mean(mapdata_men$Inac_dif_2009_urban_rural); weighted.mean(mapdata_men$Inac_dif_2009_urban_rural, w = mapdata_men$weight_tot_2009)
mean(mapdata_men$Inac_dif_2010_urban_rural); weighted.mean(mapdata_men$Inac_dif_2010_urban_rural, w = mapdata_men$weight_tot_2010)
mean(mapdata_men$Inac_dif_2011_urban_rural); weighted.mean(mapdata_men$Inac_dif_2011_urban_rural, w = mapdata_men$weight_tot_2011)
mean(mapdata_men$Inac_dif_2012_urban_rural); weighted.mean(mapdata_men$Inac_dif_2012_urban_rural, w = mapdata_men$weight_tot_2012)
mean(mapdata_men$Inac_dif_2013_urban_rural); weighted.mean(mapdata_men$Inac_dif_2013_urban_rural, w = mapdata_men$weight_tot_2013)
mean(mapdata_men$Inac_dif_2014_urban_rural); weighted.mean(mapdata_men$Inac_dif_2014_urban_rural, w = mapdata_men$weight_tot_2014)
mean(mapdata_men$Inac_dif_2015_urban_rural); weighted.mean(mapdata_men$Inac_dif_2015_urban_rural, w = mapdata_men$weight_tot_2015)
mean(mapdata_men$Inac_dif_2016_urban_rural); weighted.mean(mapdata_men$Inac_dif_2016_urban_rural, w = mapdata_men$weight_tot_2016)
mean(mapdata_men$Inac_dif_2017_urban_rural); weighted.mean(mapdata_men$Inac_dif_2017_urban_rural, w = mapdata_men$weight_tot_2017)



mapdata_men$Pop_rur_mean_2002_2017 <- (mapdata_men$Pop_rur_2002 + mapdata_men$Pop_rur_2017)/2
PobTot_rur_mean_2002_2017 <- sum(mapdata_men$Pop_rur_mean_2002_2017)
mapdata_men$weight_Pop_rur_2002_2017_mean <- (mapdata_men$Pop_rur_mean_2002_2017)/PobTot_rur_mean_2002_2017

mapdata_men$Pop_urb_mean_2002_2017 <- (mapdata_men$Pop_urb_2002 + mapdata_men$Pop_urb_2017)/2
PobTot_urb_mean_2002_2017 <- sum(mapdata_men$Pop_urb_mean_2002_2017)
mapdata_men$weight_Pop_urb_2002_2017_mean <- (mapdata_men$Pop_urb_mean_2002_2017)/PobTot_urb_mean_2002_2017


mean(mapdata_men$Total_contribution); weighted.mean(mapdata_men$Total_contribution, w = mapdata_men$weight_Pop_2002_2017_mean)
mean(mapdata_men$Urban_contribution); weighted.mean(mapdata_men$Urban_contribution, w = mapdata_men$weight_Pop_2002_2017_mean)
mean(mapdata_men$Rural_contribution); weighted.mean(mapdata_men$Rural_contribution, w = mapdata_men$weight_Pop_2002_2017_mean)
mean(mapdata_men$Urbanization_contribution); weighted.mean(mapdata_men$Urbanization_contribution, w = mapdata_men$weight_Pop_2002_2017_mean)


(mean(mapdata_men$Urban_contribution))/(mean(mapdata_men$Total_contribution))*100; 
(mean(mapdata_men$Rural_contribution))/(mean(mapdata_men$Total_contribution))*100; 
(mean(mapdata_men$Urbanization_contribution))/(mean(mapdata_men$Total_contribution))*100; 

(weighted.mean(mapdata_men$Urban_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_men$Total_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))*100
(weighted.mean(mapdata_men$Rural_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_men$Total_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))*100
(weighted.mean(mapdata_men$Urbanization_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_men$Total_contribution, w = mapdata_men$weight_Pop_2002_2017_mean))*100


# Women

mean(mapdata_women$`2002_rural`); weighted.mean(mapdata_women$`2002_rural`, w = mapdata_women$weight_rur_2002)-> wmrural_women2002
mean(mapdata_women$`2003_rural`); weighted.mean(mapdata_women$`2003_rural`, w = mapdata_women$weight_rur_2003)-> wmrural_women2003
mean(mapdata_women$`2004_rural`); weighted.mean(mapdata_women$`2004_rural`, w = mapdata_women$weight_rur_2004)-> wmrural_women2004
mean(mapdata_women$`2005_rural`); weighted.mean(mapdata_women$`2005_rural`, w = mapdata_women$weight_rur_2005)-> wmrural_women2005
mean(mapdata_women$`2006_rural`); weighted.mean(mapdata_women$`2006_rural`, w = mapdata_women$weight_rur_2006)-> wmrural_women2006
mean(mapdata_women$`2007_rural`); weighted.mean(mapdata_women$`2007_rural`, w = mapdata_women$weight_rur_2007)-> wmrural_women2007
mean(mapdata_women$`2008_rural`); weighted.mean(mapdata_women$`2008_rural`, w = mapdata_women$weight_rur_2008)-> wmrural_women2008
mean(mapdata_women$`2009_rural`); weighted.mean(mapdata_women$`2009_rural`, w = mapdata_women$weight_rur_2009)-> wmrural_women2009
mean(mapdata_women$`2010_rural`); weighted.mean(mapdata_women$`2010_rural`, w = mapdata_women$weight_rur_2010)-> wmrural_women2010
mean(mapdata_women$`2011_rural`); weighted.mean(mapdata_women$`2011_rural`, w = mapdata_women$weight_rur_2011)-> wmrural_women2011
mean(mapdata_women$`2012_rural`); weighted.mean(mapdata_women$`2012_rural`, w = mapdata_women$weight_rur_2012)-> wmrural_women2012
mean(mapdata_women$`2013_rural`); weighted.mean(mapdata_women$`2013_rural`, w = mapdata_women$weight_rur_2013)-> wmrural_women2013
mean(mapdata_women$`2014_rural`); weighted.mean(mapdata_women$`2014_rural`, w = mapdata_women$weight_rur_2014)-> wmrural_women2014
mean(mapdata_women$`2015_rural`); weighted.mean(mapdata_women$`2015_rural`, w = mapdata_women$weight_rur_2015)-> wmrural_women2015
mean(mapdata_women$`2016_rural`); weighted.mean(mapdata_women$`2016_rural`, w = mapdata_women$weight_rur_2016)-> wmrural_women2016
mean(mapdata_women$`2017_rural`); weighted.mean(mapdata_women$`2017_rural`, w = mapdata_women$weight_rur_2017)-> wmrural_women2017


mean(mapdata_women$`2002_urban`); weighted.mean(mapdata_women$`2002_urban`, w = mapdata_women$weight_urb_2002)-> wmurban_women2002
mean(mapdata_women$`2003_urban`); weighted.mean(mapdata_women$`2003_urban`, w = mapdata_women$weight_urb_2003)-> wmurban_women2003
mean(mapdata_women$`2004_urban`); weighted.mean(mapdata_women$`2004_urban`, w = mapdata_women$weight_urb_2004)-> wmurban_women2004
mean(mapdata_women$`2005_urban`); weighted.mean(mapdata_women$`2005_urban`, w = mapdata_women$weight_urb_2005)-> wmurban_women2005
mean(mapdata_women$`2006_urban`); weighted.mean(mapdata_women$`2006_urban`, w = mapdata_women$weight_urb_2006)-> wmurban_women2006
mean(mapdata_women$`2007_urban`); weighted.mean(mapdata_women$`2007_urban`, w = mapdata_women$weight_urb_2007)-> wmurban_women2007
mean(mapdata_women$`2008_urban`); weighted.mean(mapdata_women$`2008_urban`, w = mapdata_women$weight_urb_2008)-> wmurban_women2008
mean(mapdata_women$`2009_urban`); weighted.mean(mapdata_women$`2009_urban`, w = mapdata_women$weight_urb_2009)-> wmurban_women2009
mean(mapdata_women$`2010_urban`); weighted.mean(mapdata_women$`2010_urban`, w = mapdata_women$weight_urb_2010)-> wmurban_women2010
mean(mapdata_women$`2011_urban`); weighted.mean(mapdata_women$`2011_urban`, w = mapdata_women$weight_urb_2011)-> wmurban_women2011
mean(mapdata_women$`2012_urban`); weighted.mean(mapdata_women$`2012_urban`, w = mapdata_women$weight_urb_2012)-> wmurban_women2012
mean(mapdata_women$`2013_urban`); weighted.mean(mapdata_women$`2013_urban`, w = mapdata_women$weight_urb_2013)-> wmurban_women2013
mean(mapdata_women$`2014_urban`); weighted.mean(mapdata_women$`2014_urban`, w = mapdata_women$weight_urb_2014)-> wmurban_women2014
mean(mapdata_women$`2015_urban`); weighted.mean(mapdata_women$`2015_urban`, w = mapdata_women$weight_urb_2015)-> wmurban_women2015
mean(mapdata_women$`2016_urban`); weighted.mean(mapdata_women$`2016_urban`, w = mapdata_women$weight_urb_2016)-> wmurban_women2016
mean(mapdata_women$`2017_urban`); weighted.mean(mapdata_women$`2017_urban`, w = mapdata_women$weight_urb_2017)-> wmurban_women2017

c_women <- data.frame(Weighted_prev=c(wmrural_women2002,wmrural_women2003,wmrural_women2004,wmrural_women2005,wmrural_women2006,wmrural_women2007,wmrural_women2008,
                                    wmrural_women2009,wmrural_women2010,wmrural_women2011,wmrural_women2012,wmrural_women2013,wmrural_women2014,wmrural_women2015,
                                    wmrural_women2016,wmrural_women2017))
c_women$Typeofcommunity2cat <- "Rural"
c_women$year <- c(seq(from=2002, to=2017, by=1))


d_women <- data.frame(Weighted_prev=c(wmurban_women2002,wmurban_women2003,wmurban_women2004,wmurban_women2005,wmurban_women2006,wmurban_women2007,wmurban_women2008,
                                    wmurban_women2009,wmurban_women2010,wmurban_women2011,wmurban_women2012,wmurban_women2013,wmurban_women2014,wmurban_women2015,
                                    wmurban_women2016,wmurban_women2017))
d_women$Typeofcommunity2cat <- "Urban"
d_women$year <- c(seq(from=2002, to=2017, by=1))

b <- bind_rows(c_women, d_women)


mean(mapdata_women$`2002_urban`) - mean(mapdata_women$`2002_rural`)
weighted.mean(mapdata_women$`2002_urban`, w = mapdata_women$weight_urb_2002) - weighted.mean(mapdata_women$`2002_rural`, w = mapdata_women$weight_rur_2002)-> wmdif_women2002

mean(mapdata_women$`2003_urban`) - mean(mapdata_women$`2003_rural`)
weighted.mean(mapdata_women$`2003_urban`, w = mapdata_women$weight_urb_2003) - weighted.mean(mapdata_women$`2003_rural`, w = mapdata_women$weight_rur_2003)-> wmdif_women2003

mean(mapdata_women$`2004_urban`) - mean(mapdata_women$`2004_rural`)
weighted.mean(mapdata_women$`2004_urban`, w = mapdata_women$weight_urb_2004) - weighted.mean(mapdata_women$`2004_rural`, w = mapdata_women$weight_rur_2004)-> wmdif_women2004

mean(mapdata_women$`2005_urban`) - mean(mapdata_women$`2005_rural`)
weighted.mean(mapdata_women$`2005_urban`, w = mapdata_women$weight_urb_2005) - weighted.mean(mapdata_women$`2005_rural`, w = mapdata_women$weight_rur_2005)-> wmdif_women2005

mean(mapdata_women$`2006_urban`) - mean(mapdata_women$`2006_rural`)
weighted.mean(mapdata_women$`2006_urban`, w = mapdata_women$weight_urb_2006) - weighted.mean(mapdata_women$`2006_rural`, w = mapdata_women$weight_rur_2006)-> wmdif_women2006

mean(mapdata_women$`2007_urban`) - mean(mapdata_women$`2007_rural`)
weighted.mean(mapdata_women$`2007_urban`, w = mapdata_women$weight_urb_2007) - weighted.mean(mapdata_women$`2007_rural`, w = mapdata_women$weight_rur_2007)-> wmdif_women2007

mean(mapdata_women$`2008_urban`) - mean(mapdata_women$`2008_rural`)
weighted.mean(mapdata_women$`2008_urban`, w = mapdata_women$weight_urb_2008) - weighted.mean(mapdata_women$`2008_rural`, w = mapdata_women$weight_rur_2008)-> wmdif_women2008

mean(mapdata_women$`2009_urban`) - mean(mapdata_women$`2009_rural`)
weighted.mean(mapdata_women$`2009_urban`, w = mapdata_women$weight_urb_2009) - weighted.mean(mapdata_women$`2009_rural`, w = mapdata_women$weight_rur_2009)-> wmdif_women2009

mean(mapdata_women$`2010_urban`) - mean(mapdata_women$`2010_rural`)
weighted.mean(mapdata_women$`2010_urban`, w = mapdata_women$weight_urb_2010) - weighted.mean(mapdata_women$`2010_rural`, w = mapdata_women$weight_rur_2010)-> wmdif_women2010

mean(mapdata_women$`2011_urban`) - mean(mapdata_women$`2011_rural`)
weighted.mean(mapdata_women$`2011_urban`, w = mapdata_women$weight_urb_2011) - weighted.mean(mapdata_women$`2011_rural`, w = mapdata_women$weight_rur_2011)-> wmdif_women2011

mean(mapdata_women$`2012_urban`) - mean(mapdata_women$`2012_rural`)
weighted.mean(mapdata_women$`2012_urban`, w = mapdata_women$weight_urb_2012) - weighted.mean(mapdata_women$`2012_rural`, w = mapdata_women$weight_rur_2012)-> wmdif_women2012

mean(mapdata_women$`2013_urban`) - mean(mapdata_women$`2013_rural`)
weighted.mean(mapdata_women$`2013_urban`, w = mapdata_women$weight_urb_2013) - weighted.mean(mapdata_women$`2013_rural`, w = mapdata_women$weight_rur_2013)-> wmdif_women2013

mean(mapdata_women$`2014_urban`) - mean(mapdata_women$`2014_rural`)
weighted.mean(mapdata_women$`2014_urban`, w = mapdata_women$weight_urb_2014) - weighted.mean(mapdata_women$`2014_rural`, w = mapdata_women$weight_rur_2014)-> wmdif_women2014

mean(mapdata_women$`2015_urban`) - mean(mapdata_women$`2015_rural`)
weighted.mean(mapdata_women$`2015_urban`, w = mapdata_women$weight_urb_2015) - weighted.mean(mapdata_women$`2015_rural`, w = mapdata_women$weight_rur_2015)-> wmdif_women2015

mean(mapdata_women$`2016_urban`) - mean(mapdata_women$`2016_rural`)
weighted.mean(mapdata_women$`2016_urban`, w = mapdata_women$weight_urb_2016) - weighted.mean(mapdata_women$`2016_rural`, w = mapdata_women$weight_rur_2016)-> wmdif_women2016

mean(mapdata_women$`2017_urban`) - mean(mapdata_women$`2017_rural`)
weighted.mean(mapdata_women$`2017_urban`, w = mapdata_women$weight_urb_2017) - weighted.mean(mapdata_women$`2017_rural`, w = mapdata_women$weight_rur_2017)-> wmdif_women2017


f_women <- data.frame(Weighted_prev=c(wmdif_women2002,wmdif_women2003,wmdif_women2004,wmdif_women2005,wmdif_women2006,wmdif_women2007,wmdif_women2008,
                                    wmdif_women2009,wmdif_women2010,wmdif_women2011,wmdif_women2012,wmdif_women2013,wmdif_women2014,wmdif_women2015,
                                    wmdif_women2016,wmdif_women2017))
f_women$year <- c(seq(from=2002, to=2017, by=1))

mean(mapdata_women$national_inac_2002); weighted.mean(mapdata_women$national_inac_2002, w = mapdata_women$weight_tot_2002)
mean(mapdata_women$national_inac_2003); weighted.mean(mapdata_women$national_inac_2003, w = mapdata_women$weight_tot_2003)
mean(mapdata_women$national_inac_2004); weighted.mean(mapdata_women$national_inac_2004, w = mapdata_women$weight_tot_2004)
mean(mapdata_women$national_inac_2005); weighted.mean(mapdata_women$national_inac_2005, w = mapdata_women$weight_tot_2005)
mean(mapdata_women$national_inac_2006); weighted.mean(mapdata_women$national_inac_2006, w = mapdata_women$weight_tot_2006)
mean(mapdata_women$national_inac_2007); weighted.mean(mapdata_women$national_inac_2007, w = mapdata_women$weight_tot_2007)
mean(mapdata_women$national_inac_2008); weighted.mean(mapdata_women$national_inac_2008, w = mapdata_women$weight_tot_2008)
mean(mapdata_women$national_inac_2009); weighted.mean(mapdata_women$national_inac_2009, w = mapdata_women$weight_tot_2009)
mean(mapdata_women$national_inac_2010); weighted.mean(mapdata_women$national_inac_2010, w = mapdata_women$weight_tot_2010)
mean(mapdata_women$national_inac_2011); weighted.mean(mapdata_women$national_inac_2011, w = mapdata_women$weight_tot_2011)
mean(mapdata_women$national_inac_2012); weighted.mean(mapdata_women$national_inac_2012, w = mapdata_women$weight_tot_2012)
mean(mapdata_women$national_inac_2013); weighted.mean(mapdata_women$national_inac_2013, w = mapdata_women$weight_tot_2013)
mean(mapdata_women$national_inac_2014); weighted.mean(mapdata_women$national_inac_2014, w = mapdata_women$weight_tot_2014)
mean(mapdata_women$national_inac_2015); weighted.mean(mapdata_women$national_inac_2015, w = mapdata_women$weight_tot_2015)
mean(mapdata_women$national_inac_2016); weighted.mean(mapdata_women$national_inac_2016, w = mapdata_women$weight_tot_2016)
mean(mapdata_women$national_inac_2017); weighted.mean(mapdata_women$national_inac_2017, w = mapdata_women$weight_tot_2017)


mean(mapdata_women$Urban_pop_per_2002); weighted.mean(mapdata_women$Urban_pop_per_2002, w = mapdata_women$weight_tot_2002)
mean(mapdata_women$Urban_pop_per_2017); weighted.mean(mapdata_women$Urban_pop_per_2017, w = mapdata_women$weight_tot_2017)


mean(mapdata_women$Inac_dif_2002_urban_rural); weighted.mean(mapdata_women$Inac_dif_2002_urban_rural, w = mapdata_women$weight_tot_2002)
mean(mapdata_women$Inac_dif_2003_urban_rural); weighted.mean(mapdata_women$Inac_dif_2003_urban_rural, w = mapdata_women$weight_tot_2003)
mean(mapdata_women$Inac_dif_2004_urban_rural); weighted.mean(mapdata_women$Inac_dif_2004_urban_rural, w = mapdata_women$weight_tot_2004)
mean(mapdata_women$Inac_dif_2005_urban_rural); weighted.mean(mapdata_women$Inac_dif_2005_urban_rural, w = mapdata_women$weight_tot_2005)
mean(mapdata_women$Inac_dif_2006_urban_rural); weighted.mean(mapdata_women$Inac_dif_2006_urban_rural, w = mapdata_women$weight_tot_2006)
mean(mapdata_women$Inac_dif_2007_urban_rural); weighted.mean(mapdata_women$Inac_dif_2007_urban_rural, w = mapdata_women$weight_tot_2007)
mean(mapdata_women$Inac_dif_2008_urban_rural); weighted.mean(mapdata_women$Inac_dif_2008_urban_rural, w = mapdata_women$weight_tot_2008)
mean(mapdata_women$Inac_dif_2009_urban_rural); weighted.mean(mapdata_women$Inac_dif_2009_urban_rural, w = mapdata_women$weight_tot_2009)
mean(mapdata_women$Inac_dif_2010_urban_rural); weighted.mean(mapdata_women$Inac_dif_2010_urban_rural, w = mapdata_women$weight_tot_2010)
mean(mapdata_women$Inac_dif_2011_urban_rural); weighted.mean(mapdata_women$Inac_dif_2011_urban_rural, w = mapdata_women$weight_tot_2011)
mean(mapdata_women$Inac_dif_2012_urban_rural); weighted.mean(mapdata_women$Inac_dif_2012_urban_rural, w = mapdata_women$weight_tot_2012)
mean(mapdata_women$Inac_dif_2013_urban_rural); weighted.mean(mapdata_women$Inac_dif_2013_urban_rural, w = mapdata_women$weight_tot_2013)
mean(mapdata_women$Inac_dif_2014_urban_rural); weighted.mean(mapdata_women$Inac_dif_2014_urban_rural, w = mapdata_women$weight_tot_2014)
mean(mapdata_women$Inac_dif_2015_urban_rural); weighted.mean(mapdata_women$Inac_dif_2015_urban_rural, w = mapdata_women$weight_tot_2015)
mean(mapdata_women$Inac_dif_2016_urban_rural); weighted.mean(mapdata_women$Inac_dif_2016_urban_rural, w = mapdata_women$weight_tot_2016)
mean(mapdata_women$Inac_dif_2017_urban_rural); weighted.mean(mapdata_women$Inac_dif_2017_urban_rural, w = mapdata_women$weight_tot_2017)



mapdata_women$Pop_rur_mean_2002_2017 <- (mapdata_women$Pop_rur_2002 + mapdata_women$Pop_rur_2017)/2
mapdata_women$weight_Pop_rur_2002_2017_mean <- (mapdata_women$Pop_rur_mean_2002_2017)/PobTot_rur_mean_2002_2017

mapdata_women$Pop_urb_mean_2002_2017 <- (mapdata_women$Pop_urb_2002 + mapdata_women$Pop_urb_2017)/2
mapdata_women$weight_Pop_urb_2002_2017_mean <- (mapdata_women$Pop_urb_mean_2002_2017)/PobTot_urb_mean_2002_2017


mean(mapdata_women$Total_contribution); weighted.mean(mapdata_women$Total_contribution, w = mapdata_women$weight_Pop_2002_2017_mean)
mean(mapdata_women$Urban_contribution); weighted.mean(mapdata_women$Urban_contribution, w = mapdata_women$weight_Pop_2002_2017_mean)
mean(mapdata_women$Rural_contribution); weighted.mean(mapdata_women$Rural_contribution, w = mapdata_women$weight_Pop_2002_2017_mean)
mean(mapdata_women$Urbanization_contribution); weighted.mean(mapdata_women$Urbanization_contribution, w = mapdata_women$weight_Pop_2002_2017_mean)


(mean(mapdata_women$Urban_contribution))/(mean(mapdata_women$Total_contribution))*100; 
(mean(mapdata_women$Rural_contribution))/(mean(mapdata_women$Total_contribution))*100; 
(mean(mapdata_women$Urbanization_contribution))/(mean(mapdata_women$Total_contribution))*100; 

(weighted.mean(mapdata_women$Urban_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_women$Total_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))*100
(weighted.mean(mapdata_women$Rural_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_women$Total_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))*100
(weighted.mean(mapdata_women$Urbanization_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))/(weighted.mean(mapdata_women$Total_contribution, w = mapdata_women$weight_Pop_2002_2017_mean))*100



#### MAPS ####
my_colors4 <- RColorBrewer::brewer.pal(3, "Accent")
palette_red <- RColorBrewer::brewer.pal(9, "Reds")
palette_blue <- RColorBrewer::brewer.pal(9, "Blues")


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

ggsave("map_m.tiff", width = 8, height = 6, units = "in", dpi = 600)


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

ggsave("map_w.pdf", width = 8, height = 6, units = "in", dpi = 600)


## Urban-rural differences
diff_2002_m <- ggplot(mapdata_men, aes(fill = Inac_dif_2002_urban_rural)) +  scale_fill_steps2(n.breaks = 13,limits = c(-4.5,7.5), 
        guide = guide_coloursteps(even.steps = T, ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Urban–rural difference in\ninactive prevalence (%)") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "right",
        title = element_text(size = 12, face = "bold"))

diff_2002_m


diff_2017_m <- ggplot(mapdata_men, aes(fill = Inac_dif_2017_urban_rural))  + scale_fill_steps2(n.breaks = 13, limits = c(-4.5,7.5),
        guide = guide_coloursteps(even.steps = T, ticks = T,frame.colour="black",ticks.colour="black",frame.linewidth=1.25,ticks.linewith=10, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 15)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Urban–rural difference in\ninactivity prevalence (%)") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.title = element_text(size=10),
                                title = element_text(size = 12, face = "bold"), legend.position = "none")
diff_2017_m

legend <- cowplot::get_legend(diff_2017_m)
ggdraw(ylim = c(0.1,0.9),xlim = c(0,1.25)) +
        draw_plot(diff_2002_m, x = 0, y = 0, width = .5, height = 1) +
        draw_plot(diff_2017_m, x = .5, y = 0, width = .5, height = 1) + 
        draw_plot(legend, x = 1, y = 0, width = 0.25) +
        draw_plot_label(label = c("2002", "2017"), size = 14,
                        x = c(0, 0.5), y = c(0.9, 0.9))

ggsave("map_m.pdf", width = 12, height = 6, units = "in", dpi = 600)



diff_2002_w <- ggplot(mapdata_women, aes(fill = Inac_dif_2002_urban_rural))   +  scale_fill_steps2(n.breaks = 13,limits = c(-4.5,7.5), 
                                                                                                   guide = guide_coloursteps(even.steps = T, ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Urban–rural difference in\nphysically inactive prevalence (%)") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
                                title = element_text(size = 12, face = "bold"), legend.position = "none")
diff_2002_w

diff_2017_w <- ggplot(mapdata_women, aes(fill = Inac_dif_2017_urban_rural)) +  scale_fill_steps2(n.breaks = 13,limits = c(-4.5,7.5), 
                                                                                                 guide = guide_coloursteps(even.steps = T, ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Urban–rural difference in\nphysically inactive prevalence (%)") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
                                title = element_text(size = 12, face = "bold"),  legend.position = "none")

diff_2017_w

ggdraw(ylim = c(0.1,0.9),xlim = c(0,1.25)) +
        draw_plot(diff_2002_w, x = 0, y = 0, width = .5, height = 1) +
        draw_plot(diff_2017_w, x = .5, y = 0, width = .5, height = 1) + 
        draw_plot(legend, x = 1, y = 0, width = 0.25) +
        draw_plot_label(label = c("2002", "2017"), size = 14,
                        x = c(0, 0.5), y = c(0.9, 0.9))

ggsave("map_w.pdf", width = 12, height = 6, units = "in", dpi = 600)



# Urbanization change 2002-2017
diff_urbanization <- ggplot(mapdata_men, aes(fill = Urbanization_Dif_2002_2017)) +  scale_fill_steps2(n.breaks = 10,
                                                                                                      guide = guide_coloursteps(even.steps = T, ticks = T, title.position="top", title.hjust = 0.5, barwidth = 0.5, barheight = 8)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + labs(subtitle = "", fill = "Urban–rural difference in\nphysically inactive prevalence (%)") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
                                title = element_text(size = 12, face = "bold"),  legend.title =  element_text(size = 9))
diff_urbanization

# Overall values urbanization
urbanization_2002 <- ggplot(mapdata_men, aes(fill = Urban_pop_per_2002)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

urbanization_2017 <- ggplot(mapdata_men, aes(fill = Urban_pop_per_2017)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())


# Overall values - national, urban an rural
# Men
legend2 <- cowplot::get_legend(national_2017_m)

national_2002_m <- ggplot(mapdata_men, aes(fill = national_inac_2002)) + 
        scale_fill_gradient() + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_bw() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none")
national_2002_m


national_2017_m <- ggplot(mapdata_men, aes(fill = national_inac_2017)) + 
        scale_fill_gradient(limits = c(5,55)) + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_bw() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none")
national_2017_m


urban_2002_m <- ggplot(mapdata_men, aes(fill = `2002_urban`)) + 
        scale_fill_gradient() + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none")
urban_2002_m


urban_2017_m <- ggplot(mapdata_men, aes(fill = `2017_urban`)) + 
        scale_fill_gradient() + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none")
urban_2017_m


rural_2002_m <- ggplot(mapdata_men, aes(fill = `2002_rural`)) + 
        scale_fill_gradient() + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),legend.position = "none")
rural_2002_m


rural_2017_m <- ggplot(mapdata_men, aes(fill = `2017_rural`)) + 
        scale_fill_gradient() + 
        geom_sf(color = alpha("black", 1/3)) + 
        xlim(c(-10, 35)) + ylim(c(35, 70)) + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none")
rural_2017_m


ggdraw(ylim = c(0.1,2.9), xlim = c(0,1)) +
        draw_plot(national_2002_m, x = -0.4, y = 1, width = 1.5, height = 2) +
        draw_plot(national_2017_m, x = .5, y = 1, width = 1, height = 1)

        draw_plot(urban_2002_m, x = 0, y = 1, width = 1, height = 1) +
        draw_plot(urban_2017_m, x = .5, y = 1, width = 1, height = 1) +
        draw_plot(rural_2002_m, x = 0, y = 0, width = 1, height = 1.5) +
        draw_plot(rural_2017_m, x = .5, y = 0, width = 1, height = 1.5) +
        draw_plot(legend2, x = 1, y = 1.5, width = 0.25)

ggsave("map3_m.pdf", width = 16, height = 8, units = "in", dpi = 600)



# women
national_2002_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

national_2017_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

urban_2002_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

urban_2017_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

rural_2002_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

rural_2017_w <- ggplot(mapdata_women, aes(fill = values)) + scale_fill_continuous() + 
        geom_sf(color = alpha("white", 1/3), alpha = .6) + 
        xlim(c(-12, 44)) + ylim(c(35, 70)) + labs(title = "Total fertilite rate, 2015", 
                                                  subtitle = "Avg", fill = "%") + 
        theme_classic() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())


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
ggsave("contributions.tiff", width = 9, height = 11, units = "in", dpi = 600)



#### RANKING PLOTS ####
my_colors <- RColorBrewer::brewer.pal(3, "Accent")[1]
my_colors2 <- RColorBrewer::brewer.pal(3, "Accent")[2]
my_colors3 <- RColorBrewer::brewer.pal(3, "Accent")[3]

# MEN
mapdata_men$names[mapdata_men$Country_rec == "AT - Austria"] <- "Austria"
mapdata_men$names[mapdata_men$Country_rec == "BE - Belgium"] <- "Bélgica"
mapdata_men$names[mapdata_men$Country_rec == "BG - Bulgaria"] <-"Bulgaria"
mapdata_men$names[mapdata_men$Country_rec == "CY - Cyprus (Republic)"] <- "Chipre"
mapdata_men$names[mapdata_men$Country_rec == "CZ - Czech Republic"] <- "República Checa"
mapdata_men$names[mapdata_men$Country_rec == "DK - Denmark"] <- "Dinamarca"
mapdata_men$names[mapdata_men$Country_rec == "EE - Estonia"] <- "Estonia"
mapdata_men$names[mapdata_men$Country_rec == "FI - Finland"] <- "Finlandia"
mapdata_men$names[mapdata_men$Country_rec == "FR - France"] <- "Francia" 
mapdata_men$names[mapdata_men$Country_rec == "DE Germany" ] <- "Alemania"
mapdata_men$names[mapdata_men$Country_rec == "GR - Greece"] <- "Grecia"
mapdata_men$names[mapdata_men$Country_rec == "HR - Croatia"] <- "Croacia"
mapdata_men$names[mapdata_men$Country_rec == "HU - Hungary" ] <- "Hungría"
mapdata_men$names[mapdata_men$Country_rec == "IE - Ireland"] <- "Irlanda"
mapdata_men$names[mapdata_men$Country_rec == "IT - Italy" ] <- "Italia"
mapdata_men$names[mapdata_men$Country_rec == "LV - Latvia"] <- "Letonia" 
mapdata_men$names[mapdata_men$Country_rec == "LT - Lithuania"] <- "Lituania"
mapdata_men$names[mapdata_men$Country_rec == "LU - Luxembourg"] <- "Luxemburgo"
mapdata_men$names[mapdata_men$Country_rec == "MT - Malta" ] <- "Malta"
mapdata_men$names[mapdata_men$Country_rec == "NL - The Netherlands"] <- "Países Bajos"
mapdata_men$names[mapdata_men$Country_rec == "PL - Poland" ] <- "Polonia"
mapdata_men$names[mapdata_men$Country_rec == "PT - Portugal"] <- "Portugal"
mapdata_men$names[mapdata_men$Country_rec == "RO - Romania" ] <- "Rumanía"
mapdata_men$names[mapdata_men$Country_rec == "SE - Sweden" ] <- "Suecia"
mapdata_men$names[mapdata_men$Country_rec == "SI - Slovenia"] <-  "Eslovenia"
mapdata_men$names[mapdata_men$Country_rec == "SK - Slovakia"] <- "Eslovaquia" 
mapdata_men$names[mapdata_men$Country_rec == "ES -Spain"] <- "España"
mapdata_men$names[mapdata_men$Country_rec == "UK United Kingdom"] <- "Reino Unido"


mapdata_men$mean1 <- (mapdata_men$national_inac_2002 + mapdata_men$national_inac_2017)/2

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
mapdata_women$names[mapdata_women$Country_rec == "AT - Austria"] <- "Austria"
mapdata_women$names[mapdata_women$Country_rec == "BE - Belgium"] <- "Bélgica"
mapdata_women$names[mapdata_women$Country_rec == "BG - Bulgaria"] <-"Bulgaria"
mapdata_women$names[mapdata_women$Country_rec == "CY - Cyprus (Republic)"] <- "Chipre"
mapdata_women$names[mapdata_women$Country_rec == "CZ - Czech Republic"] <- "República Checa"
mapdata_women$names[mapdata_women$Country_rec == "DK - Denmark"] <- "Dinamarca"
mapdata_women$names[mapdata_women$Country_rec == "EE - Estonia"] <- "Estonia"
mapdata_women$names[mapdata_women$Country_rec == "FI - Finland"] <- "Finlandia"
mapdata_women$names[mapdata_women$Country_rec == "FR - France"] <- "Francia" 
mapdata_women$names[mapdata_women$Country_rec == "DE Germany" ] <- "Alemania"
mapdata_women$names[mapdata_women$Country_rec == "GR - Greece"] <- "Grecia"
mapdata_women$names[mapdata_women$Country_rec == "HR - Croatia"] <- "Croacia"
mapdata_women$names[mapdata_women$Country_rec == "HU - Hungary" ] <- "Hungría"
mapdata_women$names[mapdata_women$Country_rec == "IE - Ireland"] <- "Irlanda"
mapdata_women$names[mapdata_women$Country_rec == "IT - Italy" ] <- "Italia"
mapdata_women$names[mapdata_women$Country_rec == "LV - Latvia"] <- "Letonia" 
mapdata_women$names[mapdata_women$Country_rec == "LT - Lithuania"] <- "Lituania"
mapdata_women$names[mapdata_women$Country_rec == "LU - Luxembourg"] <- "Luxemburgo"
mapdata_women$names[mapdata_women$Country_rec == "MT - Malta" ] <- "Malta"
mapdata_women$names[mapdata_women$Country_rec == "NL - The Netherlands"] <- "Países Bajos"
mapdata_women$names[mapdata_women$Country_rec == "PL - Poland" ] <- "Polonia"
mapdata_women$names[mapdata_women$Country_rec == "PT - Portugal"] <- "Portugal"
mapdata_women$names[mapdata_women$Country_rec == "RO - Romania" ] <- "Rumanía"
mapdata_women$names[mapdata_women$Country_rec == "SE - Sweden" ] <- "Suecia"
mapdata_women$names[mapdata_women$Country_rec == "SI - Slovenia"] <-  "Eslovenia"
mapdata_women$names[mapdata_women$Country_rec == "SK - Slovakia"] <- "Eslovaquia" 
mapdata_women$names[mapdata_women$Country_rec == "ES -Spain"] <- "España"
mapdata_women$names[mapdata_women$Country_rec == "UK United Kingdom"] <- "Reino Unido"

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
ggsave("rank2.tiff", width = 15, height = 6, units = "in", dpi = 600)


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

ggarrange(line_men1, line_women1, line_diff_m, line_diff_w, nrow=2, align = "v", ncol = 2, common.legend = T)
ggsave("line_diff_urb_rur.pdf", width = 13, height = 7, units = "in", dpi = 600)

# Urbanization across time
Urbanization <- melt(mapdata_men, id = c( "Country_rec"),
            measure = c("Urban_pop_per_2002", "Urban_pop_per_2003", "Urban_pop_per_2004", "Urban_pop_per_2005",
                        "Urban_pop_per_2006", "Urban_pop_per_2007", "Urban_pop_per_2008", "Urban_pop_per_2009",
                        "Urban_pop_per_2010", "Urban_pop_per_2011", "Urban_pop_per_2012", "Urban_pop_per_2013", 
                        "Urban_pop_per_2014", "Urban_pop_per_2015", "Urban_pop_per_2016", "Urban_pop_per_2017"),
            variable.name = "Year",
            value.name = "Urbanization")

Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2002"] <- 2002;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2003"] <- 2003
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2004"] <- 2004;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2005"] <- 2005
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2006"] <- 2006;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2007"] <- 2007
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2008"] <- 2008;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2009"] <- 2009
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2010"] <- 2010;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2011"] <- 2011
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2012"] <- 2012;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2013"] <- 2013
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2014"] <- 2014;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2015"] <- 2015
Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2016"] <- 2016;Urbanization$Year1[Urbanization$Year == "Urban_pop_per_2017"] <- 2017

line_urb <- ggplot(Urbanization, aes(x=Year1, y=Urbanization)) +
        geom_smooth(size=0.75, se = F)  +
        ylab("Urbanization (%)") + labs(title = "Unweighted") + coord_cartesian(ylim = c(60,90)) +
        theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_text(face = "bold", size=10),
                           legend.title = element_text(size = 10, face = "bold"))
line_urb

ggsave("line_urb.pdf", width = 4, height = 3, units = "in", dpi = 600)


## ALL COUNTRIES SEPARATELY
# MEN
my_colors5 <- RColorBrewer::brewer.pal(3, "Accent")[1:2]

line_men1 <- ggplot() +
        geom_smooth(data= ALL_PREDICTS_MEN, aes(x=year, y=predict,color=Typeofcommunity2cat, fill=Typeofcommunity2cat), size=1, se = T, alpha=0.25) +
        geom_smooth(data= a, aes(x=year, y=Weighted_prev,color=Typeofcommunity2cat, fill=Typeofcommunity2cat), size=1, se = F, linetype="dashed") +
        ylab("Prevalencia población inactiva (%)") + labs(subtitle = "Hombres", color="Lugar de residencia") + guides(fill = FALSE) +
        scale_color_manual(values=my_colors5, labels = c("Rural", "Urbano")) + 
        scale_fill_manual(values=my_colors5, labels = c("Rural", "Urbano")) + 
        scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))+
        theme_minimal() + theme(plot.subtitle = element_text(family = "Times", face = "bold"),axis.title.x = element_blank(), axis.title.y = element_text(face = "bold", size=10, family = "Times"),
                           legend.text = element_text(family = "Times"),legend.title = element_text(size = 10, face = "bold", family = "Times"),axis.text = element_text(family = "Times"), legend.position = c(0.5,0.9), legend.direction = "horizontal") + coord_cartesian(ylim = c(5,55))

line_men1

legend <- cowplot::get_legend(line_men1)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "AT - Austria") -> Austria_men
Austria_line1 <- ggplot(Austria_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Austria")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)

ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "BE - Belgium") -> Belgium_men
Belgium_line1 <- ggplot(Belgium_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Belgium")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)

ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "BG - Bulgaria") -> Bulgaria_men
Bulgaria_men2 <- Bulgaria_men
Bulgaria_men$predict [ Bulgaria_men$year == 2002] <- NA
Bulgaria_men$predict [ Bulgaria_men$year == 2003] <- NA
Bulgaria_men$predict [ Bulgaria_men$year == 2004] <- NA
Bulgaria_line1 <- ggplot(Bulgaria_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Bulgaria")+
        geom_smooth(data = Bulgaria_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "CY - Cyprus (Republic)") -> Cyprus_men
Cyprus_men2 <- Cyprus_men
Cyprus_men$predict [ Cyprus_men$year == 2002] <- NA
Cyprus_men$predict [ Cyprus_men$year == 2003] <- NA
Cyprus_men$predict [ Cyprus_men$year == 2004] <- NA
Cyprus_line1 <- ggplot(Cyprus_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Cyprus")+
        geom_smooth(data = Cyprus_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "CZ - Czech Republic") -> Czech_men
Czech_men2 <- Czech_men
Czech_men$predict [ Czech_men$year == 2002] <- NA
Czech_men$predict [ Czech_men$year == 2003] <- NA
Czech_men$predict [ Czech_men$year == 2004] <- NA
Czech_line1 <- ggplot(Czech_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Czech Republic")+
        geom_smooth(data = Czech_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "DK - Denmark") -> Denmark_men
Denmark_line1 <- ggplot(Denmark_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Denmark")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "EE - Estonia") -> Estonia_men
Estonia_men2 <- Estonia_men
Estonia_men$predict [ Estonia_men$year == 2002] <- NA
Estonia_men$predict [ Estonia_men$year == 2003] <- NA
Estonia_men$predict [ Estonia_men$year == 2004] <- NA
Estonia_line1 <- ggplot(Estonia_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Estonia")+
        geom_smooth(data = Estonia_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "ES -Spain") -> Spain_men
Spain_line1 <- ggplot(Spain_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Spain")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "FI - Finland") -> Finland_men
Finland_line1 <- ggplot(Finland_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Finland")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "FR - France") -> France_men
France_line1 <- ggplot(France_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "France")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "GR - Greece") -> Greece_men
Greece_line1 <- ggplot(Greece_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Greece")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "HR - Croatia") -> Croatia_men
Croatia_men2 <- Croatia_men
Croatia_men$predict [ Croatia_men$year == 2002] <- NA
Croatia_men$predict [ Croatia_men$year == 2003] <- NA
Croatia_men$predict [ Croatia_men$year == 2004] <- NA
Croatia_line1 <- ggplot(Croatia_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Croatia")+
        geom_smooth(data = Croatia_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "HU - Hungary") -> Hungary_men
Hungary_men2 <- Hungary_men
Hungary_men$predict [ Hungary_men$year == 2002] <- NA
Hungary_men$predict [ Hungary_men$year == 2003] <- NA
Hungary_men$predict [ Hungary_men$year == 2004] <- NA
Hungary_line1 <- ggplot(Hungary_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Hungary")+
        geom_smooth(data = Hungary_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "IE - Ireland") -> Ireland_men
Ireland_line1 <- ggplot(Ireland_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Ireland")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "IT - Italy") -> Italy_men
Italy_line1 <- ggplot(Italy_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Italy")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "LT - Lithuania") -> Lithuania_men
Lithuania_men2 <- Lithuania_men
Lithuania_men$predict [ Lithuania_men$year == 2002] <- NA
Lithuania_men$predict [ Lithuania_men$year == 2003] <- NA
Lithuania_men$predict [ Lithuania_men$year == 2004] <- NA
Lithuania_line1 <- ggplot(Lithuania_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Lithuania")+
        geom_smooth(data = Lithuania_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "LU - Luxembourg") -> Luxembourg_men
Luxembourg_line1 <- ggplot(Luxembourg_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Luxembourg")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "LV - Latvia") -> Latvia_men
Latvia_men2 <- Latvia_men
Latvia_men$predict [ Latvia_men$year == 2002] <- NA
Latvia_men$predict [ Latvia_men$year == 2003] <- NA
Latvia_men$predict [ Latvia_men$year == 2004] <- NA
Latvia_line1 <- ggplot(Latvia_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Latvia")+
        geom_smooth(data = Latvia_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "MT - Malta") -> Malta_men
Malta_men2 <- Malta_men
Malta_men$predict [ Malta_men$year == 2002] <- NA
Malta_men$predict [ Malta_men$year == 2003] <- NA
Malta_men$predict [ Malta_men$year == 2004] <- NA
Malta_line1 <- ggplot(Malta_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Malta")+
        geom_smooth(data = Malta_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "NL - The Netherlands") -> Netherlands_men
Netherlands_line1 <- ggplot(Netherlands_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "The Netherlands")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "PL - Poland") -> Poland_men
Poland_men2 <- Poland_men
Poland_men$predict [ Poland_men$year == 2002] <- NA
Poland_men$predict [ Poland_men$year == 2003] <- NA
Poland_men$predict [ Poland_men$year == 2004] <- NA
Poland_line1 <- ggplot(Poland_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Poland")+
        geom_smooth(data = Poland_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "PT - Portugal") -> Portugal_men
Portugal_line1 <- ggplot(Portugal_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Portugal")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "RO - Romania") -> Romania_men
Romania_men2 <- Romania_men
Romania_men$predict [ Romania_men$year == 2002] <- NA
Romania_men$predict [ Romania_men$year == 2003] <- NA
Romania_men$predict [ Romania_men$year == 2004] <- NA
Romania_line1 <- ggplot(Romania_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Romania")+
        geom_smooth(data = Romania_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "SE - Sweden") -> Sweden_men
Sweden_line1 <- ggplot(Sweden_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Sweden")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(),  axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "SI - Slovenia") -> Slovenia_men
Slovenia_men2 <- Slovenia_men
Slovenia_men$predict [ Slovenia_men$year == 2002] <- NA
Slovenia_men$predict [ Slovenia_men$year == 2003] <- NA
Slovenia_men$predict [ Slovenia_men$year == 2004] <- NA
Slovenia_line1 <- ggplot(Slovenia_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Slovenia")+
        geom_smooth(data = Slovenia_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "SK - Slovakia") -> Slovakia_men
Slovakia_men2 <- Slovakia_men
Slovakia_men$predict [ Slovakia_men$year == 2002] <- NA
Slovakia_men$predict [ Slovakia_men$year == 2003] <- NA
Slovakia_men$predict [ Slovakia_men$year == 2004] <- NA
Slovakia_line1 <- ggplot(Slovakia_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Slovakia")+
        geom_smooth(data = Slovakia_men2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "DE Germany") -> Germany_men
Germany_line1 <- ggplot(Germany_men, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Germany")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_MEN %>% filter(ALL_PREDICTS_MEN$Country_rec == "UK United Kingdom") -> UK_men
UK_line1 <- ggplot(UK_men, aes(x=year, y=predict)) + labs(subtitle = "United Kingdom")+
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,55))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ggdraw(ylim = c(0,8), xlim = c(-0.1,4)) +
        draw_plot(line_men1, x = -0.08, y = 5.92, width = 2.08, height = 2.08) +
        draw_plot(legend, x = 0.075, y = 6.45, width = 2.045, height = 2.08)+
        draw_plot(Austria_line1, x = 2, y = 7, width = 1, height = 1)+
        draw_plot(Belgium_line1, x = 3, y = 7, width = 1, height = 1) +
        draw_plot(Bulgaria_line1, x = 2, y = 6, width = 1, height = 1) +
        draw_plot(Croatia_line1, x = 3, y = 6, width = 1, height = 1) +
        draw_plot(Cyprus_line1, x = 0, y = 5, width = 1, height = 0.96) +
        draw_plot(Czech_line1, x = 1, y = 5, width = 1, height = 0.96)+
        draw_plot(Denmark_line1, x = 2, y = 5, width = 1, height = 0.96) +
        draw_plot(Estonia_line1, x = 3, y = 5, width = 1, height = 0.96)+
        draw_plot(Finland_line1, x = 0, y = 4, width = 1, height = 1) +
        draw_plot(France_line1, x = 1, y = 4, width = 1, height = 1) +
        draw_plot(Germany_line1, x = 2, y = 4, width = 1, height = 1)+
        draw_plot(Greece_line1, x = 3, y = 4, width = 1, height = 1) +
        draw_plot(Hungary_line1, x = 0, y = 3, width = 1, height = 1)+
        draw_plot(Ireland_line1, x = 1, y = 3, width = 1, height = 1) +
        draw_plot(Italy_line1, x = 2, y = 3, width = 1, height = 1)+
        draw_plot(Latvia_line1, x = 3, y = 3, width = 1, height = 1) +
        draw_plot(Lithuania_line1, x = 0, y = 2, width = 1, height = 1) +
        draw_plot(Luxembourg_line1, x = 1, y = 2, width = 1, height = 1) +
        draw_plot(Malta_line1, x = 2, y = 2, width = 1, height = 1) +
        draw_plot(Poland_line1, x = 3, y = 2, width = 1, height = 1)+
        draw_plot(Portugal_line1, x = 0, y = 1, width = 1, height = 1) +
        draw_plot(Romania_line1, x = 1, y = 1, width = 1, height = 1)+
        draw_plot(Slovakia_line1, x = 2, y = 1, width = 1, height = 1) +
        draw_plot(Slovenia_line1, x = 3, y = 1, width = 1, height = 1)+
        draw_plot(Spain_line1, x = 0, y = 0, width = 1, height = 1) +
        draw_plot(Sweden_line1, x = 1, y = 0, width = 1, height = 1) +
        draw_plot(Netherlands_line1, x = 2, y = 0, width = 1, height = 1)+
        draw_plot(UK_line1, x = 3, y = 0, width = 1, height = 1)

ggsave("lines_m.tiff", width = 9, height = 12, units = "in", dpi = 300)

# WOMEN

line_women1 <- ggplot() +
        geom_smooth(data= ALL_PREDICTS_WOMEN, aes(x=year, y=predict,color=Typeofcommunity2cat, fill=Typeofcommunity2cat), size=1, se = T, alpha=0.25) +
        geom_smooth(data= b, aes(x=year, y=Weighted_prev,color=Typeofcommunity2cat, fill=Typeofcommunity2cat), size=1, se = F, linetype="dashed") +
        ylab("Inactivity prevalence (%)") + labs(subtitle = "Mujeres", color="Resident place") + guides(fill = FALSE) +
        scale_color_manual(values=my_colors5) + 
        scale_fill_manual(values=my_colors5) + 
        scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))+
        theme_minimal() + theme(plot.subtitle = element_text(family = "Times", face = "bold"), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
                           legend.title = element_text(size = 10, face = "bold", family = "Times"),axis.text = element_text(family = "Times"), legend.position = c(0.5,0.9), legend.direction = "horizontal") + coord_cartesian(ylim = c(5,55))

line_women1


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "AT - Austria") -> Austria_women
Austria_line1 <- ggplot(Austria_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Austria")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)

ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "BE - Belgium") -> Belgium_women
Belgium_line1 <- ggplot(Belgium_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Belgium")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)

ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "BG - Bulgaria") -> Bulgaria_women
Bulgaria_women2 <- Bulgaria_women
Bulgaria_women$predict [ Bulgaria_women$year == 2002] <- NA
Bulgaria_women$predict [ Bulgaria_women$year == 2003] <- NA
Bulgaria_women$predict [ Bulgaria_women$year == 2004] <- NA
Bulgaria_line1 <- ggplot(Bulgaria_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Bulgaria")+
        geom_smooth(data = Bulgaria_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "CY - Cyprus (Republic)") -> Cyprus_women
Cyprus_women2 <- Cyprus_women
Cyprus_women$predict [ Cyprus_women$year == 2002] <- NA
Cyprus_women$predict [ Cyprus_women$year == 2003] <- NA
Cyprus_women$predict [ Cyprus_women$year == 2004] <- NA
Cyprus_line1 <- ggplot(Cyprus_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Cyprus")+
        geom_smooth(data = Cyprus_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "CZ - Czech Republic") -> Czech_women
Czech_women2 <- Czech_women
Czech_women$predict [ Czech_women$year == 2002] <- NA
Czech_women$predict [ Czech_women$year == 2003] <- NA
Czech_women$predict [ Czech_women$year == 2004] <- NA
Czech_line1 <- ggplot(Czech_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Czech Republic")+
        geom_smooth(data = Czech_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "DK - Denmark") -> Denmark_women
Denmark_line1 <- ggplot(Denmark_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Denmark")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "EE - Estonia") -> Estonia_women
Estonia_women2 <- Estonia_women
Estonia_women$predict [ Estonia_women$year == 2002] <- NA
Estonia_women$predict [ Estonia_women$year == 2003] <- NA
Estonia_women$predict [ Estonia_women$year == 2004] <- NA
Estonia_line1 <- ggplot(Estonia_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Estonia")+
        geom_smooth(data = Estonia_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "ES -Spain") -> Spain_women
Spain_line1 <- ggplot(Spain_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Spain")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "FI - Finland") -> Finland_women
Finland_line1 <- ggplot(Finland_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Finland")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "FR - France") -> France_women
France_line1 <- ggplot(France_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "France")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "GR - Greece") -> Greece_women
Greece_line1 <- ggplot(Greece_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Greece")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "HR - Croatia") -> Croatia_women
Croatia_women2 <- Croatia_women
Croatia_women$predict [ Croatia_women$year == 2002] <- NA
Croatia_women$predict [ Croatia_women$year == 2003] <- NA
Croatia_women$predict [ Croatia_women$year == 2004] <- NA
Croatia_line1 <- ggplot(Croatia_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Croatia")+
        geom_smooth(data = Croatia_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "HU - Hungary") -> Hungary_women
Hungary_women2 <- Hungary_women
Hungary_women$predict [ Hungary_women$year == 2002] <- NA
Hungary_women$predict [ Hungary_women$year == 2003] <- NA
Hungary_women$predict [ Hungary_women$year == 2004] <- NA
Hungary_line1 <- ggplot(Hungary_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Hungary")+
        geom_smooth(data = Hungary_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "IE - Ireland") -> Ireland_women
Ireland_line1 <- ggplot(Ireland_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Ireland")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "IT - Italy") -> Italy_women
Italy_line1 <- ggplot(Italy_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Italy")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "LT - Lithuania") -> Lithuania_women
Lithuania_women2 <- Lithuania_women
Lithuania_women$predict [ Lithuania_women$year == 2002] <- NA
Lithuania_women$predict [ Lithuania_women$year == 2003] <- NA
Lithuania_women$predict [ Lithuania_women$year == 2004] <- NA
Lithuania_line1 <- ggplot(Lithuania_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Lithuania")+
        geom_smooth(data = Lithuania_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "LU - Luxembourg") -> Luxembourg_women
Luxembourg_line1 <- ggplot(Luxembourg_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Luxembourg")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "LV - Latvia") -> Latvia_women
Latvia_women2 <- Latvia_women
Latvia_women$predict [ Latvia_women$year == 2002] <- NA
Latvia_women$predict [ Latvia_women$year == 2003] <- NA
Latvia_women$predict [ Latvia_women$year == 2004] <- NA
Latvia_line1 <- ggplot(Latvia_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Latvia")+
        geom_smooth(data = Latvia_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "MT - Malta") -> Malta_women
Malta_women2 <- Malta_women
Malta_women$predict [ Malta_women$year == 2002] <- NA
Malta_women$predict [ Malta_women$year == 2003] <- NA
Malta_women$predict [ Malta_women$year == 2004] <- NA
Malta_line1 <- ggplot(Malta_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Malta")+
        geom_smooth(data = Malta_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "NL - The Netherlands") -> Netherlands_women
Netherlands_line1 <- ggplot(Netherlands_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "The Netherlands")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "PL - Poland") -> Poland_women
Poland_women2 <- Poland_women
Poland_women$predict [ Poland_women$year == 2002] <- NA
Poland_women$predict [ Poland_women$year == 2003] <- NA
Poland_women$predict [ Poland_women$year == 2004] <- NA
Poland_line1 <- ggplot(Poland_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Poland")+
        geom_smooth(data = Poland_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "PT - Portugal") -> Portugal_women
Portugal_line1 <- ggplot(Portugal_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Portugal")+
        theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "RO - Romania") -> Romania_women
Romania_women2 <- Romania_women
Romania_women$predict [ Romania_women$year == 2002] <- NA
Romania_women$predict [ Romania_women$year == 2003] <- NA
Romania_women$predict [ Romania_women$year == 2004] <- NA
Romania_line1 <- ggplot(Romania_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Romania")+
        geom_smooth(data = Romania_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "SE - Sweden") -> Sweden_women
Sweden_line1 <- ggplot(Sweden_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Sweden")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(),  axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "SI - Slovenia") -> Slovenia_women
Slovenia_women2 <- Slovenia_women
Slovenia_women$predict [ Slovenia_women$year == 2002] <- NA
Slovenia_women$predict [ Slovenia_women$year == 2003] <- NA
Slovenia_women$predict [ Slovenia_women$year == 2004] <- NA
Slovenia_line1 <- ggplot(Slovenia_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Slovenia")+
        geom_smooth(data = Slovenia_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)


ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "SK - Slovakia") -> Slovakia_women
Slovakia_women2 <- Slovakia_women
Slovakia_women$predict [ Slovakia_women$year == 2002] <- NA
Slovakia_women$predict [ Slovakia_women$year == 2003] <- NA
Slovakia_women$predict [ Slovakia_women$year == 2004] <- NA
Slovakia_line1 <- ggplot(Slovakia_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +labs(subtitle = "Slovakia")+
        geom_smooth(data = Slovakia_women2, aes(color=Typeofcommunity2cat), size=1, se = F, linetype="dotted") +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "DE Germany") -> Germany_women
Germany_line1 <- ggplot(Germany_women, aes(x=year, y=predict)) +
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) + labs(subtitle = "Germany")+
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)



ALL_PREDICTS_WOMEN %>% filter(ALL_PREDICTS_WOMEN$Country_rec == "UK United Kingdom") -> UK_women
UK_line1 <- ggplot(UK_women, aes(x=year, y=predict)) + labs(subtitle = "United Kingdom")+
        geom_smooth(aes(color=Typeofcommunity2cat), size=1, se = F) +
        theme_bw() + theme(axis.title.x = element_blank(),axis.text.y = element_blank(), axis.title.y = element_blank(),
                           legend.position = "none") + coord_cartesian(ylim = c(5,65))+
        scale_x_continuous(breaks = c(2002, 2005,2008,2011,2014,2017))+
        scale_color_manual(values=my_colors5)





ggdraw(ylim = c(0,8), xlim = c(-0.1,4)) +
        draw_plot(line_women1, x = -0.08, y = 5.92, width = 2.08, height = 2.08) +
        draw_plot(legend, x = 0.075, y = 6.45, width = 2.045, height = 2.08)+
        draw_plot(Austria_line1, x = 2, y = 7, width = 1, height = 1)+
        draw_plot(Belgium_line1, x = 3, y = 7, width = 1, height = 1) +
        draw_plot(Bulgaria_line1, x = 2, y = 6, width = 1, height = 1) +
        draw_plot(Croatia_line1, x = 3, y = 6, width = 1, height = 1) +
        draw_plot(Cyprus_line1, x = 0, y = 5, width = 1, height = 0.96) +
        draw_plot(Czech_line1, x = 1, y = 5, width = 1, height = 0.96)+
        draw_plot(Denmark_line1, x = 2, y = 5, width = 1, height = 0.96) +
        draw_plot(Estonia_line1, x = 3, y = 5, width = 1, height = 0.96)+
        draw_plot(Finland_line1, x = 0, y = 4, width = 1, height = 1) +
        draw_plot(France_line1, x = 1, y = 4, width = 1, height = 1) +
        draw_plot(Germany_line1, x = 2, y = 4, width = 1, height = 1)+
        draw_plot(Greece_line1, x = 3, y = 4, width = 1, height = 1) +
        draw_plot(Hungary_line1, x = 0, y = 3, width = 1, height = 1)+
        draw_plot(Ireland_line1, x = 1, y = 3, width = 1, height = 1) +
        draw_plot(Italy_line1, x = 2, y = 3, width = 1, height = 1)+
        draw_plot(Latvia_line1, x = 3, y = 3, width = 1, height = 1) +
        draw_plot(Lithuania_line1, x = 0, y = 2, width = 1, height = 1) +
        draw_plot(Luxembourg_line1, x = 1, y = 2, width = 1, height = 1) +
        draw_plot(Malta_line1, x = 2, y = 2, width = 1, height = 1) +
        draw_plot(Poland_line1, x = 3, y = 2, width = 1, height = 1)+
        draw_plot(Portugal_line1, x = 0, y = 1, width = 1, height = 1) +
        draw_plot(Romania_line1, x = 1, y = 1, width = 1, height = 1)+
        draw_plot(Slovakia_line1, x = 2, y = 1, width = 1, height = 1) +
        draw_plot(Slovenia_line1, x = 3, y = 1, width = 1, height = 1)+
        draw_plot(Spain_line1, x = 0, y = 0, width = 1, height = 1) +
        draw_plot(Sweden_line1, x = 1, y = 0, width = 1, height = 1) +
        draw_plot(Netherlands_line1, x = 2, y = 0, width = 1, height = 1)+
        draw_plot(UK_line1, x = 3, y = 0, width = 1, height = 1)

ggsave("lines_w.tiff", width = 9, height = 12, units = "in", dpi = 300)


#### ANIMATE PLOT ####
library("ggplot2")
library("gganimate")
library(ggrepel)

# Men
Diff_urb_rur_men2 <- melt(mapdata_men, id = c( "Country_rec"),
                         measure = c("Pob_2002", "Pob_2003", "Pob_2004", "Pob_2005",
                                     "Pob_2006", "Pob_2007", "Pob_2008", "Pob_2009",
                                     "Pob_2010", "Pob_2011", "Pob_2012", "Pob_2013", 
                                     "Pob_2014", "Pob_2015", "Pob_2016", "Pob_2017"),
                         variable.name = "Year",
                         value.name = "Pop")

Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2002"] <- 2002;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2003"] <- 2003
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2004"] <- 2004;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2005"] <- 2005
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2006"] <- 2006;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2007"] <- 2007
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2008"] <- 2008;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2009"] <- 2009
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2010"] <- 2010;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2011"] <- 2011
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2012"] <- 2012;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2013"] <- 2013
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2014"] <- 2014;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2015"] <- 2015
Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2016"] <- 2016;Diff_urb_rur_men2$Year1[Diff_urb_rur_men2$Year == "Pob_2017"] <- 2017

Diff_urb_rur_men3 <- melt(mapdata_men, id = c( "Country_rec"),
                          measure = c("national_inac_2002", "national_inac_2003", "national_inac_2004", "national_inac_2005",
                                      "national_inac_2006", "national_inac_2007", "national_inac_2008", "national_inac_2009",
                                      "national_inac_2010", "national_inac_2011", "national_inac_2012", "national_inac_2013", 
                                      "national_inac_2014", "national_inac_2015", "national_inac_2016", "national_inac_2017"),
                          variable.name = "Year",
                          value.name = "National_Inac")

Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2002"] <- 2002;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2003"] <- 2003
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2004"] <- 2004;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2005"] <- 2005
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2006"] <- 2006;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2007"] <- 2007
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2008"] <- 2008;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2009"] <- 2009
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2010"] <- 2010;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2011"] <- 2011
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2012"] <- 2012;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2013"] <- 2013
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2014"] <- 2014;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2015"] <- 2015
Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2016"] <- 2016;Diff_urb_rur_men3$Year1[Diff_urb_rur_men3$Year == "national_inac_2017"] <- 2017

Diff_urb_rur_men <- merge(Diff_urb_rur_men, Diff_urb_rur_men2, by =  c("Country_rec", "Year1"))
Diff_urb_rur_men <- merge(Diff_urb_rur_men, Diff_urb_rur_men3, by =  c("Country_rec", "Year1"))
Diff_urb_rur_men <- Diff_urb_rur_men[-c(3,5,7)]

Diff_urb_rur_men$Gender <- "Men"

# Women
Diff_urb_rur_women2 <- melt(mapdata_women, id = c( "Country_rec"),
                          measure = c("Pob_2002", "Pob_2003", "Pob_2004", "Pob_2005",
                                      "Pob_2006", "Pob_2007", "Pob_2008", "Pob_2009",
                                      "Pob_2010", "Pob_2011", "Pob_2012", "Pob_2013", 
                                      "Pob_2014", "Pob_2015", "Pob_2016", "Pob_2017"),
                          variable.name = "Year",
                          value.name = "Pop")

Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2002"] <- 2002;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2003"] <- 2003
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2004"] <- 2004;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2005"] <- 2005
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2006"] <- 2006;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2007"] <- 2007
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2008"] <- 2008;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2009"] <- 2009
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2010"] <- 2010;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2011"] <- 2011
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2012"] <- 2012;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2013"] <- 2013
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2014"] <- 2014;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2015"] <- 2015
Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2016"] <- 2016;Diff_urb_rur_women2$Year1[Diff_urb_rur_women2$Year == "Pob_2017"] <- 2017

Diff_urb_rur_women3 <- melt(mapdata_women, id = c( "Country_rec"),
                          measure = c("national_inac_2002", "national_inac_2003", "national_inac_2004", "national_inac_2005",
                                      "national_inac_2006", "national_inac_2007", "national_inac_2008", "national_inac_2009",
                                      "national_inac_2010", "national_inac_2011", "national_inac_2012", "national_inac_2013", 
                                      "national_inac_2014", "national_inac_2015", "national_inac_2016", "national_inac_2017"),
                          variable.name = "Year",
                          value.name = "National_Inac")

Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2002"] <- 2002;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2003"] <- 2003
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2004"] <- 2004;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2005"] <- 2005
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2006"] <- 2006;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2007"] <- 2007
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2008"] <- 2008;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2009"] <- 2009
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2010"] <- 2010;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2011"] <- 2011
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2012"] <- 2012;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2013"] <- 2013
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2014"] <- 2014;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2015"] <- 2015
Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2016"] <- 2016;Diff_urb_rur_women3$Year1[Diff_urb_rur_women3$Year == "national_inac_2017"] <- 2017

Diff_urb_rur_women <- merge(Diff_urb_rur_women, Diff_urb_rur_women2, by =  c("Country_rec", "Year1"))
Diff_urb_rur_women <- merge(Diff_urb_rur_women, Diff_urb_rur_women3, by =  c("Country_rec", "Year1"))
Diff_urb_rur_women <- Diff_urb_rur_women[-c(3,5,7)]

Diff_urb_rur_women$Gender <- "Women"

Diff_urb_rur <- bind_rows(Diff_urb_rur_men, Diff_urb_rur_women)
colnames(Diff_urb_rur)[colnames(Diff_urb_rur) == "Year1"] <- "Year"
Diff_urb_rur$Year <- as.integer(Diff_urb_rur$Year)



urb_rur_time_plot <- ggplot(Diff_urb_rur, aes(x = National_Inac, y = Diff_urban_rural, size = Pop)) +
        geom_hline(yintercept=0, linetype="dashed", color = "red") +
        geom_point(alpha = 0.7, show.legend = FALSE) + 
        geom_text_repel(size = 2,aes(label=Country_rec), alpha=0.5) +
        theme_bw() + facet_wrap(Gender~.) +
        scale_size(range = c(2, 8)) + 
        labs(title = 'Year: {frame_time}', x = 'National inactive prevalence (%)', y = 'Urban-Rural differences\nin inactive prevalence') +
        transition_time(Year) +
        ease_aes('linear')

animate(urb_rur_time_plot, width = 750, height = 450)
anim_save("Urban-rural-across-time.gif")

