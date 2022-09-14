

library("foreign")

GE <- read.spss("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/04. Development of gender equality elicits women participation in physical activity_ENVIADO/TRABAJO PREVIO/An Mo Ll/MMR Gender inequality/Resultados JJOO gender eq/GII 0.1%.sav", to.data.frame = T)

attach(GE)
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
library(writexl)

loadfonts(device = "win")
windowsFonts(Helvetica=windowsFont("Helvetica"))
windowsFonts(Times=windowsFont("TT Times New Roman"))


stepsnat <- filter(GE, steps_mean_total > 0)
WHO <- filter(GE, AF_Porcen_Activos_Total > 0)

# Variables a transformar
GE$Log_GII16 <- log10(GE$GII16)
GE$Inv_GII16 <- 1/GE$GII16
GE$Log_MMR_15 <- log10(GE$MMR_2015)
GE$Inver_MMR_2015 <- 1/GE$MMR_2015
GE$Log_ABR_2016 <- log10(GE$ABR_2016)
GE$Inver_ABR_2015 <- 1/GE$ABR_2016
GE$Log_Rep_health_16 <- log10(GE$Reproductive_health_dimension16)
GE$Inver_health_16 <- 1/GE$Reproductive_health_dimension16
GE$Log_Dif_pop_educ_16 <- log10(GE$Dif_pop_educ_16)
GE$Inver_Dif_pop_educ16 <- 1/GE$Dif_pop_educ_16
GE$Log_Dif_labourforce_16 <- log10(GE$Dif_Labourforce_dimension_16)
stepsnat$Log_GII14 <- log10(stepsnat$GII14)
stepsnat$Inver_GII14 <- 1/stepsnat$GII14
stepsnat$Log_MMR_14 <- log10(stepsnat$MMR_2014)
stepsnat$Inver_MMR_2014 <- 1/stepsnat$MMR_2014
stepsnat$Log_ABR_2014 <- log10(stepsnat$ABR_2014)
stepsnat$Log_Dif_pop_educ_14 <- log10(stepsnat$Dif_pop_educ_14)
stepsnat$Inver_dif_pop_educ_14 <- 1/stepsnat$Dif_pop_educ_14
stepsnat$Log_Dif_labourforce_14 <- log10(stepsnat$Dif_Labourforce_dimension_14)
WHO$Log_AF_activos_gendergap <- log10(WHO$AF_activos_gender_gap)
WHO$Inver_AF_activos_gendergap <- 1/WHO$AF_activos_gender_gap
WHO$Log_GII16 <- log10(WHO$GII16)
WHO$Log_MMR_15 <- log10(WHO$MMR_2015)
WHO$Log_ABR_16 <- log10(WHO$ABR_2016)
WHO$Log_Rep_health_16 <- log10(WHO$Reproductive_health_dimension16)
WHO$Log_Log_dif_labourforce_16 <- log10(WHO$Dif_Labourforce_dimension_16)

# Supuestos de normalidad

library("moments")

shapiro.test(GE$Rio2016_gender_ratio)
skewness(GE$Rio2016_gender_ratio, na.rm = T)
densityPlot(GE$Rio2016_gender_ratio)
GE$log_rat_rio16 <- log10(GE$Rio2016_gender_ratio)
GE$Inver_rat_rio16 <- 1/GE$Rio2016_gender_ratio
shapiro.test(GE$log_rat_rio16)
shapiro.test(GE$Inver_rat_rio16)


shapiro.test(WHO$AF_activos_gender_ratio)
skewness(WHO$AF_activos_gender_ratio)
densityPlot(WHO$AF_activos_gender_ratio)
WHO$log10_AF_activos_ratio <- log10(WHO$AF_activos_gender_ratio)
WHO$Inver_AF_activos_ratio <- 1/WHO$AF_activos_gender_ratio
shapiro.test(WHO$log10_AF_activos_ratio)
shapiro.test(WHO$Inver_AF_activos_ratio)


shapiro.test(GE$GDP16)
skewness(GE$GDP16, na.rm = T)
densityPlot(GE$GDP16)
GE$log10_GDP16 <- log10(GE$GDP16)
GE$Inver_GDP16 <- 1/GE$GDP16
shapiro.test(GE$log10_GDP16)
shapiro.test(GE$Inver_GDP16)


shapiro.test(GE$Life_expectancy16)
skewness(GE$Life_expectancy16)  
densityPlot(GE$Life_expectancy16)
GE$logref_life_16 <- log10(84.8 - GE$Life_expectancy16) 
GE$Inverref_life_16 <- 1 / (84.8 - GE$Life_expectancy16) 
shapiro.test(GE$logref_life_16)
shapiro.test(GE$Inverref_life_16)


shapiro.test(GE$Rat_pop_educ_16)
skewness(GE$Rat_pop_educ_16)
densityPlot(GE$Rat_pop_educ_16)
GE$LOg_Rat_pop_educ_16 <- log10(GE$Rat_pop_educ_16)
GE$Inver_rat_educ_16 <- 1/GE$Rat_pop_educ_16
shapiro.test(GE$LOg_Rat_pop_educ_16)
shapiro.test(GE$Inver_rat_educ_16)



shapiro.test(GE$Rat_empower_16)
skewness(GE$Rat_empower_16)
densityPlot(GE$Rat_empower_16)
GE$log_Rat_empower_16 <- log10(GE$Rat_empower_16)
GE$Inver_rat_empower_16 <- 1/GE$Rat_empower_16
shapiro.test(GE$log_Rat_empower_16)
shapiro.test(GE$Inver_rat_empower_16)


shapiro.test(GE$Rat_labourforce_16)
skewness(GE$Rat_labourforce_16)
densityPlot(GE$Rat_labourforce_16)
GE$log_Rat_labourforce_16 <- log10(GE$Rat_labourforce_16)
GE$Inver_rat_labourforce_16 <- 1/GE$Rat_labourforce_16
shapiro.test(GE$log_Rat_labourforce_16)
shapiro.test(GE$Inver_rat_labourforce_16)


shapiro.test(stepsnat$stepsmean_gender_ratio)
skewness(stepsnat$stepsmean_gender_ratio)
densityPlot(stepsnat$stepsmean_gender_ratio)
stepsnat$log10_steps_ratio <- log10(stepsnat$stepsmean_gender_ratio)
stepsnat$Inver_steps_ratio <- 1/stepsnat$stepsmean_gender_ratio
shapiro.test(stepsnat$log10_steps_ratio)
shapiro.test(stepsnat$Inver_steps_ratio)

shapiro.test(stepsnat$GDP14)
skewness(stepsnat$GDP14)
densityPlot(stepsnat$GDP14)
stepsnat$log10_GDP14 <- log10(stepsnat$GDP14)
shapiro.test(stepsnat$log10_GDP14)
stepsnat$Inver_GDP14 <- 1/stepsnat$GDP14
shapiro.test(stepsnat$Inver_GDP14)

shapiro.test(stepsnat$Life_expectancy14)
skewness(stepsnat$Life_expectancy14) # Transformacion refleja 
densityPlot(stepsnat$Life_expectancy14)
stepsnat$logref_life_14 <- log10(84.5 - stepsnat$Life_expectancy14) 
stepsnat$Inverref_life_14 <- 1 / (84.5 - stepsnat$Life_expectancy14) 
shapiro.test(stepsnat$logref_life_14)
shapiro.test(stepsnat$Inverref_life_14)

shapiro.test(stepsnat$Rat_pop_educ_14)
skewness(stepsnat$Rat_pop_educ_14)
densityPlot(stepsnat$Rat_pop_educ_14)
stepsnat$log_Rat_pop_educ_14 <- log10(stepsnat$Rat_pop_educ_14)
stepsnat$Inver_rat_educ_14 <- 1/stepsnat$Rat_pop_educ_14
shapiro.test(stepsnat$log_Rat_pop_educ_14)
shapiro.test(stepsnat$Inver_rat_educ_14)

shapiro.test(stepsnat$Rat_empower_14)
skewness(stepsnat$Rat_empower_14)
densityPlot(stepsnat$Rat_empower_14)
stepsnat$Inver_rat_empower_14 <- 1/stepsnat$Rat_empower_14
stepsnat$log_Rat_empower_14 <- log10(stepsnat$Rat_empower_14)
shapiro.test(stepsnat$log_Rat_empower_14)
shapiro.test(stepsnat$Inver_rat_empower_14)

shapiro.test(stepsnat$Rat_labourforce_14)
skewness(stepsnat$Rat_labourforce_14)
densityPlot(stepsnat$Rat_labourforce_14) 
stepsnat$Inver_rat_labourforce_14 <- 1/stepsnat$Rat_labourforce_14
stepsnat$log_Rat_labourforce_14 <- log10(stepsnat$Rat_labourforce_14)
shapiro.test(stepsnat$log_Rat_labourforce_14)
shapiro.test(stepsnat$Inver_rat_labourforce_14)


# Estadistica de regresiones simples

WHO$Inver_rat_empower_16 <- 1/WHO$Rat_empower_16
WHO$Inver_rat_educ_16 <- 1/WHO$Rat_pop_educ_16
WHO$Inver_rat_labourforce_16 <- 1/WHO$Rat_labourforce_16
WHO$log_GDP16 <- log10(WHO$GDP16)
WHO$logref_Life_16 <- 1/(84.8 - WHO$Life_expectancy16)

Tcontrol <- lmRob.control(estim = "Final")

model1 <- rlm(WHO$GII16 ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16, method = "M")
summary(model1)
f.robftest(model1, var = "WHO$Inver_AF_activos_ratio")
f.robftest(model1, var = "WHO$log_GDP16")
f.robftest(model1, var = "WHO$logref_Life_16")
f.robftest(model1)
anova(model1)
lm.beta(model1)

summary(lm(WHO$GII16 ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16))
model1 <-lm(WHO$GII16 ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16)
WHO$rstudent1<- rstudent(model1)
shapiro.test(WHO$rstudent) # normalidad de residuos
bptest(model1) # homocedasticidad 
plot(WHO$rstudent)
dwtest(Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16 ~ GII16, alternative = "two.sided", data = WHO) # incorrelación de residuos
autoplot(model1, which = 1:6, ncol = 2, label.size = 3) # graficos


model1a <- rlm(WHO$GII16 ~ WHO$log_GDP16 + WHO$logref_Life_16)
model1b <- rlm(WHO$Inver_AF_activos_ratio ~ WHO$log_GDP16 + WHO$logref_Life_16)
resid.model1a <- residuals(model1a)
resid.model1b <- residuals(model1b)
NEWDF1 <- data.frame(RES1 = resid.model1a, RES1b = resid.model1b)
NEWDF1$RES1b <- NEWDF1$RES1b*-1

summary(lm(NEWDF1$RES1 ~ NEWDF1$RES1b))


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


summary(lmRob(stepsnat$GII14 ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14, control = Tcontrol))
summary(lm(stepsnat$Log_GII14 ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14))
model2 <- lm(stepsnat$Log_GII14 ~ stepsnat$Inver_steps_ratio  + stepsnat$log10_GDP14 + stepsnat$logref_life_14)
stepsnat$rstudent2<- rstudent(model2)
shapiro.test(stepsnat$rstudent)
bptest(model2)
plot(stepsnat$rstudent)
dwtest(Inver_steps_ratio + log10_GDP14 + logref_life_14 ~ Log_GII14, alternative = "two.sided", data = stepsnat)
autoplot(model2, which = 1:6, ncol = 2, label.size = 3)
lm.beta(model2)


model2a <- lm(stepsnat$Log_GII14 ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
model2b <- lm(stepsnat$Inver_steps_ratio ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
resid.model2a <- residuals(model2a)
resid.model2b <- residuals(model2b)
NEWDF2 <- data.frame(RES1 = resid.model2a, RES2b = resid.model2b)
NEWDF2$RES2b <- NEWDF2$RES2b*-1

summary(lm(NEWDF2$RES1 ~ NEWDF2$RES2b))


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

model3 <- rlm(GE$GII16 ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16, method = "M")
summary(model3)
f.robftest(model3, var = "GE$log_rat_rio16")
f.robftest(model3, var = "GE$log10_GDP16")
f.robftest(model3, var = "GE$logref_life_16")
f.robftest(model3)
anova(model3)
lm.beta(model3)

model3 <- lm(GE$GII16 ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16)
summary(lm(GE$GII16 ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16))
GE$rstudent3<- rstudent(model3)
shapiro.test(GE$rstudent3)
bptest(model3)
plot(GE$rstudent3)
dwtest(log_rat_rio16 + log10_GDP16 + logref_life_16 ~ GII16, alternative = "two.sided", data = GE)
autoplot(model3, which = 1:6, ncol = 2, label.size = 3)

model3a <- rlm(GE$GII16 ~ GE$log10_GDP16 + GE$logref_life_16)
model3b <- rlm(GE$log_rat_rio16 ~ GE$log10_GDP16 + GE$logref_life_16, method = "M")
resid.model3a <- residuals(model3a)
resid.model3b <- residuals(model3b)
NEWDF3 <- data.frame(RES1 = resid.model3a, RES1b = resid.model3b);

summary(lm(NEWDF3$RES1 ~ NEWDF3$RES1b))

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


# Correlaciones entre las 3 variables independientes

WHO$log_rat_rio16 <- log10(WHO$Rio2016_gender_ratio)
stepsnat$log_rat_rio16 <- log10(stepsnat$Rio2016_gender_ratio)
stepsnat$inver_rat_activos <- 1 / stepsnat$AF_activos_gender_ratio


# Analisis estadisticos mayor igualdad, mayor actividad fisica

WHO$log_rio16_total <- log10(WHO$Rio2016_n_total)
WHO$log_rio16_ratio <- log10(WHO$Rio2016_gender_ratio)
WHO$inver_steps_mean <- 1/WHO$stepsmean_gender_ratio

stepsnat$inver_AF_ratio <- 1/stepsnat$AF_activos_gender_ratio
stepsnat$log_rio_total <- log10(stepsnat$Rio2016_n_total)
stepsnat$log_rio_ratio <- log10(stepsnat$Rio2016_gender_ratio)
stepsnat$log_GDP16 <- log10(stepsnat$GDP16)
stepsnat$logref_life_16 <- log10(84.8 - stepsnat$Life_expectancy16) 


shapiro.test(GE$Rio2016_n_total)
skewness(GE$Rio2016_n_total, na.rm = T)
densityPlot(GE$Rio2016_n_total)
GE$log_rio16_total <- log10(GE$Rio2016_n_total)
GE$Inver_rio16_total <- 1/GE$Rio2016_n_total
shapiro.test(GE$log_rio16_total)
shapiro.test(GE$Inver_rio16_total)

shapiro.test(GE$Rio2016_n_particip_female)
skewness(GE$Rio2016_n_particip_female, na.rm = T)
densityPlot(GE$Rio2016_n_particip_female)
GE$log_rio16_female <- log10(GE$Rio2016_n_particip_female)
shapiro.test(GE[-9, "log_rio16_female"])
skewness(GE[-9, "log_rio16_female"])

shapiro.test(GE$Rio2016_n_particip_male)
skewness(GE$Rio2016_n_particip_male, na.rm = T)
densityPlot(GE$Rio2016_n_particip_male)
GE$log_rio16_male <- log10(GE$Rio2016_n_particip_male)
shapiro.test(GE$log_rio16_male)
skewness(GE$log_rio16_male, na.rm = T)


shapiro.test(WHO$AF_Porcen_Activos_Total)
skewness(WHO$AF_Porcen_Activos_Total, na.rm = T)
densityPlot(WHO$AF_Porcen_Activos_Total)
shapiro.test(WHO$AF_Porcen_Activos_female)
skewness(WHO$AF_Porcen_Activos_female, na.rm = T)
densityPlot(WHO$AF_Porcen_Activos_female)
shapiro.test(WHO$AF_Porcen_Activos_male)
skewness(WHO$AF_Porcen_Activos_male, na.rm = T)
densityPlot(WHO$AF_Porcen_Activos_male)


shapiro.test(stepsnat$steps_mean_total)
skewness(stepsnat$steps_mean_total, na.rm = T)
densityPlot(stepsnat$steps_mean_total)
shapiro.test(stepsnat$steps_mean_female)
skewness(stepsnat$steps_mean_female, na.rm = T)
densityPlot(stepsnat$steps_mean_female)
shapiro.test(stepsnat$steps_mean_male)
skewness(stepsnat$steps_mean_male, na.rm = T)
densityPlot(stepsnat$steps_mean_male)


      # Regresiones lineales y graficos

model10 <- rlm(WHO$AF_Porcen_Activos_Total ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16, method = "M")
summary(model10)
f.robftest(model10, var = "WHO$Inver_AF_activos_ratio")
f.robftest(model10, var = "WHO$log_GDP16")
f.robftest(model10, var = "WHO$logref_Life_16")
f.robftest(model10)
anova(model10)
lm.beta(model10)

summary(lm(WHO$AF_Porcen_Activos_Total ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16))
model10 <-lm(WHO$AF_Porcen_Activos_Total ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16)
WHO$rstudent10<- rstudent(model10)
shapiro.test(WHO$rstudent10) # normalidad de residuos
bptest(model10) # homocedasticidad 
plot(WHO$rstudent10)
dwtest(Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16 ~ AF_Porcen_Activos_Total, alternative = "two.sided", data = WHO) # incorrelación de residuos
autoplot(model10, which = 1:6, ncol = 2, label.size = 3) # graficos


model10a <- rlm(WHO$Inver_AF_activos_ratio ~ WHO$log_GDP16 + WHO$logref_Life_16)
model10b <- rlm(WHO$AF_Porcen_Activos_Total ~ WHO$log_GDP16 + WHO$logref_Life_16)
resid.model10a <- residuals(model10a)
resid.model10b <- residuals(model10b)
NEWDF10 <- data.frame(RES1 = resid.model10a, RES1b = resid.model10b)
NEWDF10$RES1 <- NEWDF10$RES1*-1

summary(lm(NEWDF10$RES1 ~ NEWDF10$RES1b))
cor.test(NEWDF10$RES1, NEWDF10$RES1b, exact = T, conf.level = .95)


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

summary(rlm(stepsnat$steps_mean_total ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14))
model20 <- lm(stepsnat$steps_mean_total ~ stepsnat$Inver_steps_ratio  + stepsnat$log10_GDP14 + stepsnat$logref_life_14)
stepsnat$rstudent20<- rstudent(model20)
shapiro.test(stepsnat$rstudent20)
bptest(model20)
plot(stepsnat$rstudent20)
dwtest(Inver_steps_ratio + log10_GDP14 + logref_life_14 ~ steps_mean_total, alternative = "two.sided", data = stepsnat)
autoplot(model20, which = 1:6, ncol = 2, label.size = 3)
lm.beta(model20)

model20 <- rlm(stepsnat$steps_mean_total ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14, method = "M")
summary(model20)
f.robftest(model20, var = "stepsnat$Inver_steps_ratio")
f.robftest(model20, var = "stepsnat$log10_GDP14")
f.robftest(model20, var = "stepsnat$logref_life_14")
f.robftest(model20)
anova(model20)
lm.beta(model20)

model20a <- rlm(stepsnat$steps_mean_total ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
model20b <- rlm(stepsnat$Inver_steps_ratio ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
resid.model20a <- residuals(model20a)
resid.model20b <- residuals(model20b)
NEWDF20 <- data.frame(RES1 = resid.model20a, RES2b = resid.model20b)
NEWDF20$RES2b <- NEWDF20$RES2b*-1

summary(lm(NEWDF20$RES1 ~ NEWDF20$RES2b))
cor.test(NEWDF20$RES1, NEWDF20$RES2b, exact = T, conf.level = .95)


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

model30 <- rlm(GE$log_rio16_total ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16, method = "M")
summary(model30)
f.robftest(model30, var = "GE$log_rat_rio16")
f.robftest(model30, var = "GE$log10_GDP16")
f.robftest(model30, var = "GE$logref_life_16")
f.robftest(model30)
anova(model30)
lm.beta(model30)

model30 <- lm(GE$log_rio16_total ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16)
summary(lm(GE$log_rio16_total ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16))
GE$rstudent30<- rstudent(model30)
shapiro.test(GE$rstudent30)
bptest(model30)
plot(GE$rstudent30)
dwtest(log_rat_rio16 + log10_GDP16 + logref_life_16 ~ log_rio16_total, alternative = "two.sided", data = GE)
autoplot(model30, which = 1:6, ncol = 2, label.size = 3)

model30a <- rlm(GE$log_rio16_total~ GE$log10_GDP16 + GE$logref_life_16)
model30b <- rlm(GE$log_rat_rio16 ~ GE$log10_GDP16 + GE$logref_life_16)
resid.model30a <- residuals(model30a)
resid.model30b <- residuals(model30b)
NEWDF30 <- data.frame(RES1 = resid.model30a, RES1b = resid.model30b);

summary(lm(NEWDF30$RES1 ~ NEWDF30$RES1b))
cor.test(NEWDF30$RES1, NEWDF30$RES1b, exact = T, conf.level = .95)


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


# Regresiones lineales Ratio AF y MAYOR AF en hombres y mujeres

      # Prevalencia AF


model10 <- rlm(WHO$AF_Porcen_Activos_female ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16, method = "M")
summary(model10)
f.robftest(model10, var = "WHO$Inver_AF_activos_ratio")
f.robftest(model10, var = "WHO$log_GDP16")
f.robftest(model10, var = "WHO$logref_Life_16")
f.robftest(model10)
anova(model10)
lm.beta(model10)

summary(lm(WHO$AF_Porcen_Activos_female ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16))
model100 <-lm(WHO$AF_Porcen_Activos_female ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16)
WHO$rstudent100<- rstudent(model100)
shapiro.test(WHO$rstudent10) # normalidad de residuos
bptest(model100) # homocedasticidad 
plot(WHO$rstudent100)
dwtest(Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16 ~ AF_Porcen_Activos_female, alternative = "two.sided", data = WHO) # incorrelación de residuos
autoplot(model100, which = 1:6, ncol = 2, label.size = 3) # graficos


model10a <- rlm(WHO$AF_Porcen_Activos_female ~ WHO$log_GDP16 + WHO$logref_Life_16)
model10b <- rlm(WHO$Inver_AF_activos_ratio ~ WHO$log_GDP16 + WHO$logref_Life_16)
resid.model10a <- residuals(model10a)
resid.model10b <- residuals(model10b)
NEWDF10 <- data.frame(RES1 = resid.model10a, RES1b = resid.model10b)
NEWDF10$RES1b <- NEWDF10$RES1b*-1

summary(lm(NEWDF10$RES1 ~ NEWDF10$RES1b))
cor.test(NEWDF10$RES1, NEWDF10$RES1b, exact = T, conf.level = .95)


p100 <- ggscatterstats(marginal = F, data = NEWDF10, x = RES1b, point.size = 0.1,k=3,
                      y = RES1,  xlab = "Total PA prevalence (%)", ylab = "Gender PA Ratio",
                      messages = FALSE, bf.message = F, line.color = "blue3") +
  geom_smooth(method=lm, color="blue3", fill = "blue2", alpha=0.2) + 
  geom_point(shape=16, color="blue1") + 
  theme_classic2() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=10, face="bold")) +
  theme(element_line(size = 1), plot.subtitle = element_text(hjust = 0.5))

p100


model101 <- rlm(WHO$AF_Porcen_Activos_male ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16, method = "M")
summary(model101)
f.robftest(model101, var = "WHO$Inver_AF_activos_ratio")
f.robftest(model101, var = "WHO$log_GDP16")
f.robftest(model101, var = "WHO$logref_Life_16")
f.robftest(model101)
anova(model101)
lm.beta(model101)

summary(lm(WHO$AF_Porcen_Activos_male ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16))
model101 <-lm(WHO$AF_Porcen_Activos_male ~ WHO$Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16)
WHO$rstudent101<- rstudent(model101)
shapiro.test(WHO$rstudent101) # normalidad de residuos
bptest(model101) # homocedasticidad 
plot(WHO$rstudent101)
dwtest(Inver_AF_activos_ratio + WHO$log_GDP16 + WHO$logref_Life_16 ~ AF_Porcen_Activos_male, alternative = "two.sided", data = WHO) # incorrelación de residuos
autoplot(model101, which = 1:6, ncol = 2, label.size = 3) # graficos


model10a <- rlm(WHO$AF_Porcen_Activos_male ~ WHO$log_GDP16 + WHO$logref_Life_16)
model10b <- rlm(WHO$Inver_AF_activos_ratio ~ WHO$log_GDP16 + WHO$logref_Life_16)
resid.model10a <- residuals(model10a)
resid.model10b <- residuals(model10b)
NEWDF101 <- data.frame(RES1 = resid.model10a, RES1b = resid.model10b)
NEWDF101$RES1b <- NEWDF101$RES1b*-1

summary(lm(NEWDF101$RES1 ~ NEWDF101$RES1b))
cor.test(NEWDF101$RES1, NEWDF101$RES1b, exact = T, conf.level = .95)


inactive <- merge(NEWDF10, NEWDF101, by =  "RES1b")
inactive$RES1c <- inactive$RES1b

colnames(inactive)[colnames(inactive)=="RES1b"] <- "Gender_PA_Ratio_hombres"
colnames(inactive)[colnames(inactive)=="RES1c"] <- "Gender_PA_Ratio_mujeres"
colnames(inactive)[colnames(inactive)=="RES1.x"] <- "Total_prev_mujeres"
colnames(inactive)[colnames(inactive)=="RES1.y"] <- "Total_prev_hombres"

write_xlsx(inactive, 'inactive.xlsx')
Inactive_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/Inactive.csv",
                                header = T, sep = ";", dec = ",")


p10 <- ggplot(data = Inactive_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
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

p10


legend <- cowplot::get_legend(p10)



      # STEPS NATURE


summary(rlm(stepsnat$steps_mean_male ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14))
model200 <- rlm(stepsnat$steps_mean_male ~ stepsnat$Inver_steps_ratio  + stepsnat$log10_GDP14 + stepsnat$logref_life_14)
stepsnat$rstudent200<- rstudent(model200)
shapiro.test(stepsnat$rstudent200)
bptest(model200)
plot(stepsnat$rstudent200)
dwtest(Inver_steps_ratio + log10_GDP14 + logref_life_14 ~ steps_mean_total, alternative = "two.sided", data = stepsnat)
autoplot(model200, which = 1:6, ncol = 2, label.size = 3)
lm.beta(model200)

model200 <- rlm(stepsnat$steps_mean_male ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14, method = "M")
summary(model200)
f.robftest(model200, var = "stepsnat$Inver_steps_ratio")
f.robftest(model200, var = "stepsnat$log10_GDP14")
f.robftest(model200, var = "stepsnat$logref_life_14")
f.robftest(model200)
anova(model200)
lm.beta(model200)

model20a <- rlm(stepsnat$steps_mean_male ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
model20b <- rlm(stepsnat$Inver_steps_ratio ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
resid.model20a <- residuals(model20a)
resid.model20b <- residuals(model20b)
NEWDF20 <- data.frame(RES1 = resid.model20a, RES2b = resid.model20b)
NEWDF20$RES2b <- NEWDF20$RES2b*-1

summary(lm(NEWDF20$RES1 ~ NEWDF20$RES2b))
cor.test(NEWDF20$RES1, NEWDF20$RES2b, exact = T, conf.level = .95)


p200 <- ggscatterstats(marginal = F, data = NEWDF20, x = RES2b, point.size = 0.1,k=3,
                     y = RES1,  xlab = "Total Average Daily Steps", ylab = "Gender Average Steps Ratio\n(Inverse)",
                     messages = FALSE, bf.message = F,
                     line.color = "firebrick3") +
  geom_smooth(method=lm, color="firebrick3", fill = "firebrick2", alpha=0.2, se = T) + 
  geom_text_repel(size = 2,aes(label=stepsnat$Abr), alpha=0.6) +
  geom_point(shape=16, color="firebrick1") + 
  theme_classic2() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=10, face="bold")) +
  theme(element_line(size = 1), plot.subtitle = element_text(hjust = 0.5))
p200


summary(rlm(stepsnat$steps_mean_female ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14))
model201 <- rlm(stepsnat$steps_mean_female ~ stepsnat$Inver_steps_ratio  + stepsnat$log10_GDP14 + stepsnat$logref_life_14)
stepsnat$rstudent201<- rstudent(model201)
shapiro.test(stepsnat$rstudent201)
bptest(model201)
plot(stepsnat$rstudent201)
dwtest(Inver_steps_ratio + log10_GDP14 + logref_life_14 ~ steps_mean_female, alternative = "two.sided", data = stepsnat)
autoplot(model201, which = 1:6, ncol = 2, label.size = 3)
lm.beta(model201)

model201 <- rlm(stepsnat$steps_mean_total ~ stepsnat$Inver_steps_ratio + stepsnat$log10_GDP14 + stepsnat$logref_life_14, method = "M")
summary(model201)
f.robftest(model201, var = "stepsnat$Inver_steps_ratio")
f.robftest(model201, var = "stepsnat$log10_GDP14")
f.robftest(model201, var = "stepsnat$logref_life_14")
f.robftest(model201)
anova(model201)
lm.beta(model201)

model20a <- rlm(stepsnat$steps_mean_female ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
model20b <- rlm(stepsnat$Inver_steps_ratio ~ stepsnat$log10_GDP14 + stepsnat$logref_life_14)
resid.model20a <- residuals(model20a)
resid.model20b <- residuals(model20b)
NEWDF201 <- data.frame(RES1 = resid.model20a, RES2b = resid.model20b)
NEWDF201$RES2b <- NEWDF201$RES2b*-1

summary(lm(NEWDF201$RES1 ~ NEWDF201$RES2b))
cor.test(NEWDF201$RES1, NEWDF201$RES2b, exact = T, conf.level = .95)

steps <- merge(NEWDF20, NEWDF201, by =  "RES2b")
steps$RES2c <- steps$RES2b

colnames(steps)[colnames(steps)=="RES2b"] <- "Gender_PA_Ratio_hombres"
colnames(steps)[colnames(steps)=="RES2c"] <- "Gender_PA_Ratio_mujeres"
colnames(steps)[colnames(steps)=="RES1.x"] <- "Total_prev_mujeres"
colnames(steps)[colnames(steps)=="RES1.y"] <- "Total_prev_hombres"

write_xlsx(steps, 'steps.xlsx')
steps_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/steps.csv",
                       header = T, sep = ";", dec = ",")


p20 <- ggplot(data = steps_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
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

p20






p2 <- ggplot(data = NEWDF20, x = RES2b, y = RES1,  xlab = "Total Average Daily Steps", ylab = "Gender Average Steps Ratio\n(Inverse)") +
  geom_smooth(method=lm, color="firebrick3", fill = "firebrick2", alpha=0.2, se = T) + 
  geom_text_repel(size = 2,aes(label=stepsnat$Abr), alpha=0.6) +
  geom_point(shape=16, color="firebrick1") + 
  theme_classic2() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=10, face="bold")) +
  theme(element_line(size = 1), plot.subtitle = element_text(hjust = 0.5))
p2





      # RIO 2016

model300 <- rlm(GE$log_rio16_male ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16, method = "M")
summary(model300)
f.robftest(model300, var = "GE$log_rat_rio16")
f.robftest(model300, var = "GE$log10_GDP16")
f.robftest(model300, var = "GE$logref_life_16")
f.robftest(model300)
anova(model300)
lm.beta(model300)

model300 <- lm(GE$log_rio16_male ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16)
summary(lm(GE$log_rio16_male ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16))
GE$rstudent300<- rstudent(model300)
shapiro.test(GE$rstudent300)
bptest(model300)
plot(GE$rstudent300)
dwtest(log_rat_rio16 + log10_GDP16 + logref_life_16 ~ log_rio16_male, alternative = "two.sided", data = GE)
autoplot(model300, which = 1:6, ncol = 2, label.size = 3)

model30a <- rlm(GE$log_rio16_male~ GE$log10_GDP16 + GE$logref_life_16)
model30b <- rlm(GE$log_rat_rio16 ~ GE$log10_GDP16 + GE$logref_life_16)
resid.model30a <- residuals(model30a)
resid.model30b <- residuals(model30b)
NEWDF30 <- data.frame(RES1 = resid.model30a, RES1b = resid.model30b);

summary(lm(NEWDF30$RES1 ~ NEWDF30$RES1b))
cor.test(NEWDF30$RES1, NEWDF30$RES1b, exact = T, conf.level = .95)


p3 <- ggscatterstats(marginal = F, data = NEWDF30, x = RES1b, point.size = 0.1,k=3,
                     y = RES1,  xlab = "Total Olympic Participation\n(Log10)", ylab = "Gender Olympic Participation Ratio\n(Log10)",
                     messages = FALSE, bf.message = F,
                     line.color = "chartreuse3") +
  geom_smooth(method=lm, color="chartreuse3", fill = "chartreuse2", alpha=0.2, se = T) + 
  geom_text_repel(size = 2,aes(label=GE$Abr), alpha=0.6) +
  geom_point(shape=16, color="chartreuse1") + 
  theme_classic2() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=10, face="bold")) +
  theme(element_line(size = 1), plot.subtitle = element_text(hjust = 0.5))
p3


model301 <- rlm(GEIraq$log_rio16_female ~ GEIraq$log_rat_rio16 + GEIraq$log10_GDP16 + GEIraq$logref_life_16, method = "M")
summary(model301)
f.robftest(model301, var = "GEIraq$log_rat_rio16")
f.robftest(model301, var = "GEIraq$log10_GDP16")
f.robftest(model301, var = "GEIraq$logref_life_16")
f.robftest(model301)
anova(model301)
lm.beta(model301)

GEIraq <- GE[-62, ]

model301 <- lm(GEIraq$log_rio16_female ~ GEIraq$log_rat_rio16+ GEIraq$log10_GDP16 + GEIraq$logref_life_16)
summary(lm(GE$log_rio16_female ~ GE$log_rat_rio16 + GE$log10_GDP16 + GE$logref_life_16))
GEIraq$rstudent301<- rstudent(model301)
shapiro.test(GEIraq$rstudent301)
bptest(model301)
plot(GEIraq$rstudent301)
dwtest(log_rat_rio16 + log10_GDP16 + logref_life_16 ~ log_rio16_female, alternative = "two.sided", data = GEIraq)
autoplot(model301, which = 1:6, ncol = 2, label.size = 3)

model30a <- rlm(GEIraq$log_rio16_female~ GEIraq$log10_GDP16 + GEIraq$logref_life_16)
model30b <- rlm(GEIraq$log_rat_rio16 ~ GEIraq$log10_GDP16 + GEIraq$logref_life_16)
resid.model30a <- residuals(model30a)
resid.model30b <- residuals(model30b)
NEWDF301 <- data.frame(RES1 = resid.model30a, RES1b = resid.model30b);

summary(lm(NEWDF301$RES1 ~ NEWDF301$RES1b))
cor.test(NEWDF301$RES1, NEWDF301$RES1b, exact = T, conf.level = .95)

NEWDF301$id <- seq(1:88)
NEWDF30$id <- seq(1:89)

rio <- merge(NEWDF30, NEWDF301, by =  "id")

colnames(rio)[colnames(rio)=="RES1b.y"] <- "Gender_PA_Ratio_hombres"
colnames(rio)[colnames(rio)=="RES1b.x"] <- "Gender_PA_Ratio_mujeres"
colnames(rio)[colnames(rio)=="RES1.x"] <- "Total_prev_mujeres"
colnames(rio)[colnames(rio)=="RES1.y"] <- "Total_prev_hombres"

write_xlsx(rio, 'rio.xlsx')
rio_2 <- read.csv("C:/Users/anton/Desktop/Umubox/SECONDARY DATA ANALYSIS/Manuscritos ANTONIO/Tesis Doctoral/Tesis figuras/Archivos datos/rio.csv",
                    header = T, sep = ";", dec = ",")


p30 <- ggplot(data = rio_2, aes(y = Total_prev, x = Gender_PA_Ratio)) + 
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

p30





p3 <- ggscatterstats(marginal = F, data = NEWDF30, x = RES1b, point.size = 0.1,k=3,
                     y = RES1,  xlab = "Total Olympic Participation\n(Log10)", ylab = "Gender Olympic Participation Ratio\n(Log10)",
                     messages = FALSE, bf.message = F,
                     line.color = "chartreuse3") +
  geom_smooth(method=lm, color="chartreuse3", fill = "chartreuse2", alpha=0.2, se = T) + 
  geom_text_repel(size = 2,aes(label=GEIraq$Abr), alpha=0.6) +
  geom_point(shape=16, color="chartreuse1") + 
  theme_classic2() +
  theme(plot.title = element_text(size=14, face="bold", hjust = 0.5), 
        axis.title.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(size=10, face="bold")) +
  theme(element_line(size = 1), plot.subtitle = element_text(hjust = 0.5))
p3

# Unión de los 3 gráficos de regresión  (gender ratio)

ggsave("p2_GII.tiff", width = 12, height = 6, units = "in", dpi = 600)
ggdraw(xlim = c(-0.01, 1)) +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = -0.008, y = 0, width = 1, height = 0.5)

ggsave("p3_GII.pdf", width = 12, height = 6, units = "in", dpi = 600)
ggdraw(xlim = c(-0.01, 1)) +
  draw_plot(p10, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p20, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p30, x = -0.008, y = 0, width = 1, height = 0.5)

ggsave("p4_GII_2.tiff", width = 12, height = 6, units = "in", dpi = 600)
ggdraw(xlim = c(-0.01, 1)) +
  draw_plot(p10, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p20, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p30, x = -0.008, y = 0, width = 1, height = 0.5)+
  draw_plot(legend, x = 0.02, y = 0.45)
  


