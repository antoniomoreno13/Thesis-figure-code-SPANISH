
# Densiogramas según GII 16 Y 14 (Stepsnature)

# WHO

dens1 <- density(WHO$AF_activos_gender_gap[which(GII16_TERCILES == "Alta Desigualdad")], na.rm = T)
df1 <- data.frame(dens1$x, dens1$y)
names(df1)[1:2] <- c("x","y")
df1$tercile <- "Alta Desigualdad"

dens2 <- density(WHO$AF_activos_gender_gap[which(GII16_TERCILES == "Media Igualdad")], na.rm = T)
df2 <- data.frame(dens2$x, dens2$y)
names(df2)[1:2] <- c("x","y")
df2$tercile <- "Media Igualdad"

dens3 <- density(WHO$AF_activos_gender_gap[which(GII16_TERCILES == "Alta Igualdad")], na.rm = T)
df3 <- data.frame(dens3$x, dens3$y)
names(df3)[1:2] <- c("x","y")
df3$tercile <- "Alta Igualdad"

df4 <- bind_rows(df3, df2, df1)
df4$tercile <- factor(df4$tercile)
df4$tercile <- ordered(df4$tercile, levels = c("Alta Igualdad","Media Igualdad",
                                                 "Alta Desigualdad"), 
                        labels = c("Alta Igualdad", 
                                   "Media Igualdad", "Alta Desigualdad"))

dplot1 <- ggplot() + geom_line(data = df4, aes(x, y, color=tercile), size=1.75 ,na.rm = T) + 
  scale_color_manual(values = c("violetred4", "violetred3", "lightpink")) +
  scale_linetype_manual(values=c("dotdash", "dotted", "solid")) + theme_classic() +
  labs( x = "PA prevalence WHO", y = "Probablity density", color = "GII Terciles") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=10, face="bold"), 
        axis.title.x = element_text(size = 12, face = "bold"), legend.title = element_text(face = "bold"))

dplot1

# scale_colour_brewer(palette="Set1") 

# STEPS

dens11 <- density(stepsnat$steps_gender_gap[which(GII14_TERCILES == "Alta Desigualdad")], na.rm = T)
df11 <- data.frame(dens11$x, dens11$y)
names(df11)[1:2] <- c("x","y")
df11$tercile <- "Alta Desigualdad"

dens31 <- density(stepsnat$steps_gender_gap[which(GII14_TERCILES == "Media Igualdad")], na.rm = T)
df31 <- data.frame(dens31$x, dens31$y)
names(df31)[1:2] <- c("x","y")
df31$tercile <- "Media Igualdad"

dens41 <- density(stepsnat$steps_gender_gap[which(GII14_TERCILES == "Alta Igualdad")], na.rm = T)
df41 <- data.frame(dens41$x, dens41$y)
names(df41)[1:2] <- c("x","y")
df41$tercile <- "Alta Igualdad"

df51 <- bind_rows(df11, df31, df41)
df51$tercile <- factor(df51$tercile)
df51$tercile <- ordered(df51$tercile, levels = c("Alta Igualdad","Media Igualdad",
                                                   "Alta Desigualdad"), 
                         labels = c("Alta Igualdad", 
                                    "Media Igualdad",
                                    "Alta Desigualdad"))

dplot2 <- ggplot() + geom_line(data = df51, aes(x, y, color=tercile), size=1.75 ,na.rm = T) + 
  scale_color_manual(values = c("violetred4", "violetred3", "lightpink")) +
  scale_linetype_manual(values=c("dotdash", "dotted", "solid")) + theme_classic() +
  labs( x = "Average Daily Steps", y = "Probablity density", color = "GII Terciles") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=10, face="bold"), 
        axis.title.x = element_text(size = 12, face = "bold"), legend.title = element_text(face = "bold"))

dplot2

# RIO 2016

dens12 <- density(GE$Rio2016_porcen_gendergap[which(GII16_TERCILES == "Alta Desigualdad")], na.rm = T)
df12 <- data.frame(dens12$x, dens12$y)
names(df12)[1:2] <- c("x","y")
df12$tercile <- "Alta Desigualdad"

dens32 <- density(GE$Rio2016_porcen_gendergap[which(GII16_TERCILES == "Media Igualdad")], na.rm = T)
df32 <- data.frame(dens32$x, dens32$y)
names(df32)[1:2] <- c("x","y")
df32$tercile <- "Media Igualdad"

dens42 <- density(GE$Rio2016_porcen_gendergap[which(GII16_TERCILES == "Alta Igualdad")], na.rm = T)
df42 <- data.frame(dens42$x, dens42$y)
names(df42)[1:2] <- c("x","y")
df42$tercile <- "Alta Igualdad"

df52 <- bind_rows(df12, df32, df42)
df52$tercile <- factor(df5$tercile)
df52$tercile <- ordered(df52$tercile, levels = c("Alta Igualdad","Media Igualdad",
                                                  "Alta Desigualdad"), 
                         labels = c("Alta Igualdad", 
                                    "Media Igualdad",
                                    "Alta Desigualdad"))


dplot3 <- ggplot() + geom_line(data = df52, aes(x, y, color=tercile), size=1.75 ,na.rm = T) + 
  scale_color_manual(values = c("violetred4", "violetred3", "lightpink")) +
  scale_linetype_manual(values=c("dotdash", "dotted", "solid")) + theme_classic() +
  labs( x = "Participation in Rio 2016", y = "Probablity density", color = "GII Terciles") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=10, face="bold"), 
        axis.title.x = element_text(size = 12, face = "bold"), legend.title = element_text(face = "bold"))



dplot3

tiff("plot2.tiff", width = 7, height = 10, units = "in", res = 600)
ggarrange(dplot1, dplot2, dplot3, nrow=3, labels = c("A", "B", "C"),
          align = "hv", ncol = 1, common.legend = T, legend = "right")
dev.off()


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
