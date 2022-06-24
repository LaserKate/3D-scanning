library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(cowplot)

df_magic<-read.csv("Test_MagicScanner_final.csv")

my_comparisions.magic<-list( c("Acuta", "Tenuis"), 
                             c("Acuta", "daedalea"), 
                             c("Tenuis", "daedalea"))

##1) SPEED----
df_magic %>%
  ggboxplot(x = "Class", y = "Time.per.Scan",group="Species",
            add = "jitter") + facet_grid(.~Species)
  stat_compare_means(comparisons=my_comparisions.magic, method = "wilcox.test",aes(label=..p.adj..))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(x="Reef", y=expression("sd.y of ts_"*~degree*C))

##species, size-----
species_fig<-df_magic %>%
    ggboxplot(x = "Species", y = "Time.per.Scan",group="Species",
              color="Species",
              add = "jitter") +
  stat_compare_means(comparisons=my_comparisions.magic, method = "wilcox.test",aes(label=..p.adj..))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    labs(x="Reef", y="Time per scan (minutes)")+theme_classic(24)+
    theme(legend.position="top") +
    scale_color_brewer(palette="Accent", direction=-1)
  
  ##live or dead----
  my_comparisions.type<-list( c("live", "skeleton"))
live.dead.fig<- df_magic %>% 
    ggboxplot(x = "Type", y = "Time.per.Scan",group="Type",
              add = "jitter") + #facet_grid(.~Species)+
    stat_compare_means(comparisons=my_comparisions.type, method = "wilcox.test",aes(label=..p.adj..))+
    theme(panel.grid.minor = element_blank(), axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    labs(x="Reef", y="Time per scan (minutes)")

Figure1<-plot_grid(species_fig+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
                   live.dead.fig+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
            labels = c("a", "b"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))
  
##2) VOLUME---  
Volume.fig<-df_magic %>% 
    ggboxplot(x = "Species", y = "volume",group="Individual", colour="Individual",
              add = "jitter") + facet_grid(Class~., scales="free")+
    stat_compare_means(comparisons=my_comparisions.type, method = "wilcox.test",aes(label=..p.adj..))+
    theme(panel.grid.minor = element_blank(), axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    labs(x="Reef", y="Volume (mm3)")

df_summary_magic<-summarySE(df_magic, measurevar="volume", groupvars=c("Species", "Class"), na.rm=TRUE)
df_summary_magic.allSpecies<-summarySE(df_magic, measurevar="volume", groupvars=c("Individual"), na.rm=TRUE)
df_summary_magic2<-summarySE(df_magic, measurevar="volume", groupvars=c("Species", "Individual", "Class"), na.rm=TRUE)

df_summary_magic.allSpecies %>% 
  ggboxplot(x = "Individual", y = "sd",
            add = "jitter") + #facet_grid(Individual~., scales="free")+
  stat_compare_means(comparisons=my_comparisions.type, method = "wilcox.test",aes(label=..p.adj..))+
  theme(panel.grid.minor = element_blank(), axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(x="Reef", y="SD of Volume (mm3)")

ggplot(df_summary_magic2, aes(x = Species, y = sd, color=as.factor(Individual))) +
      geom_point(alpha=0.75)+facet_grid(.~Class)

sd.volume<-ggplot(df_summary_magic2, aes(x = log(volume), y = log(sd), color=as.factor(Species))) +#theme_bw()+#theme_pubr(base_size = 8)+
  geom_point(alpha=0.75)+geom_vline(xintercept = 5.5)#+facet_grid(.~Class) #5.5 is big juvenile

Volume.fig<-plot_grid(Volume.fig+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          sd.volume+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))
#volume log log----
log.vl<-
ggplot(df_summary_magic2, aes(x = log(volume), y = log(sd))) +
  geom_point(alpha=0.75)+geom_vline(xintercept = 5.5)+
  stat_cor(label.x = 3)+
  geom_smooth(method = "lm", se=TRUE)+theme_classic(24)+theme(legend.position="top")

#Estimated Error - volume-----
df_summary_magic2.new<-dplyr::rename(df_summary_magic2, Estimated.Error=se)
df_summary_magic2.new.est<-summarySE(df_summary_magic2.new, measurevar="Estimated.Error", groupvars=c("Species", "Class"), na.rm=TRUE)

Estimtederror.vol<-ggplot(df_summary_magic2.new.est, aes(x = Estimated.Error, y = Species, color=as.factor(Class))) +#+facet_grid(.~Class)
  geom_linerange(colour = "grey90", aes(xmin=Estimated.Error-ci, xmax=Estimated.Error+ci), size=2,alpha=0.8) +
  geom_point(size=3)+theme_classic(24)+
  theme(legend.position="top") + labs(y="Species", x="Estimated Error -Volume (mm)")+
  scale_color_brewer(palette="Accent", direction=-1)#,position=position_dodge(.2))

#CoV Volume----
#Coefficient of variation = (Standard Deviation / Mean) * 100. In symbols: CV = (SD/x̄) * 100. Multiplying the coefficient by 100 is an optional step to get a percentage, as opposed to a decimal.
CoV_Volume<- df_summary_magic2 %>% mutate(CoV_Vol.val = (sd/volume) * 100)

CoV.Vol<-ggplot(CoV_Volume, aes(x = CoV_Vol.val, y = Species, color=as.factor(Class))) +#+facet_grid(.~Class)
  #geom_linerange(colour = "grey90", aes(xmin=Estimated.Error-ci, xmax=Estimated.Error+ci), size=2,alpha=0.8) +
  geom_point(size=3)+theme_classic(24)+
  theme(legend.position="top") + labs(y="Species", x="CoV - volume (mm)")+
  scale_color_brewer(palette="Accent", direction=-1)#,position=position_dodge(.2))

#mean COV for SA----
summarySE(CoV_Volume, measurevar="CoV_Vol.val", groupvars=c("Species", "Class"), na.rm=TRUE)
summarySE(CoV_Volume, measurevar="CoV_Vol.val", groupvars=c("Class"), na.rm=TRUE)
summarySE(CoV_Volume, measurevar="CoV_Vol.val", na.rm=TRUE)

##3) SURFACE AREA----
Surface.Area.fig<-df_magic %>% 
  ggboxplot(x = "Species", y = "surfacearea",group="Individual", colour="Individual",
            add = "jitter") + facet_grid(Class~., scales="free")

df_summary_magic_SA<-summarySE(df_magic, measurevar="surfacearea", groupvars=c("Species", "Individual", "Class"), na.rm=TRUE)

sd.SA<-ggplot(df_summary_magic_SA, aes(x = log(surfacearea), y = log(sd))) +
  geom_point(alpha=0.75, aes(color=as.factor(Species)), add = "reg.line")+geom_vline(xintercept = 5.5)+
  stat_cor(aes(color = Species), label.x = 3)+
  geom_smooth(method = "lm", se=FALSE, aes(color=Species))#+facet_grid(.~Class) #5.5 is big juvneile
#log log SA----
log.SA<-ggplot(df_summary_magic_SA, aes(x = log(surfacearea), y = log(sd))) +
  geom_point(alpha=0.75)+geom_vline(xintercept = 5.5)+
  stat_cor(label.x = 3)+
  geom_smooth(method = "lm", se=TRUE)+theme_classic(24)+theme(legend.position="top")


Surface.Area_fig<-plot_grid(Surface.Area.fig+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          sd.SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))

###

ddply(df_magic, .(Species, Class), summarise, med = median(volume))

ggscatter(df_summary_magic_SA, x = "surfacearea", y = "sd", color= "Species", add = "reg.line") +
  stat_cor(label.y = 60, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 50)#+facet_grid(.~Class)

ggscatter(df_summary_magic_SA, x = "surfacearea", y = "sd", add = "reg.line") +
  stat_cor(label.y = 20, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 30)#+facet_grid(.~Class)

ggscatter(df_summary_magic2, x = "volume", y = "sd", add = "reg.line") +
  stat_cor(label.y = 20, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 30)#+facet_grid(.~Class)

ggscatter(df_summary_magic_SA, x = "surfacearea", y = "sd",
          add = "reg.line",                         # Add regression line
          color = "Species", palette = "jco",           # Color by groups "cyl",                            # Change point shape by groups "cyl"
          fullrange = TRUE,                         # Extending the regression line
          rug = TRUE                                # Add marginal rug
)+stat_cor(aes(color = Species), label.x = 3)           # Add correlation coefficient

#Estimated Error - surface area----
df_summary_magic_SA.new<-dplyr::rename(df_summary_magic_SA, Estimated.Error=se)
df_summary_magic_SA.new.est<-summarySE(df_summary_magic_SA.new, measurevar="Estimated.Error", groupvars=c("Species", "Class"), na.rm=TRUE)

#CoV SA----
#Coefficient of variation = (Standard Deviation / Mean) * 100. In symbols: CV = (SD/x̄) * 100. Multiplying the coefficient by 100 is an optional step to get a percentage, as opposed to a decimal.
CoV_SA<- df_summary_magic_SA %>% mutate(CoV_SA.val = (sd/surfacearea) * 100)

Estimtederror.SA<-ggplot(df_summary_magic_SA.new.est, aes(x = Estimated.Error, y = Species, color=as.factor(Class))) +#+facet_grid(.~Class)
  geom_linerange(colour = "grey90", aes(xmin=Estimated.Error-ci, xmax=Estimated.Error+ci), size=2,alpha=0.8) +
  geom_point(size=3)+theme_classic(24)+
  theme(legend.position="top") + labs(y="Species", x="Estimated Error -Surface Area (mm)")+
  scale_color_brewer(palette="Accent", direction=-1)#,position=position_dodge(.2))

CoV.SA<-ggplot(CoV_SA, aes(x = CoV_SA.val, y = Species, color=as.factor(Class))) +#+facet_grid(.~Class)
  #geom_linerange(colour = "grey90", aes(xmin=Estimated.Error-ci, xmax=Estimated.Error+ci), size=2,alpha=0.8) +
  geom_point(size=3)+theme_classic(24)+
  theme(legend.position="top") + labs(y="Species", x="CoV -Surface Area (mm)")+
  scale_color_brewer(palette="Accent", direction=-1)#,position=position_dodge(.2))

#mean COV for SA----
summarySE(CoV_SA, measurevar="CoV_SA.val", groupvars=c("Species", "Class"), na.rm=TRUE)
summarySE(CoV_SA, measurevar="CoV_SA.val", groupvars=c("Class"), na.rm=TRUE)
summarySE(CoV_SA, measurevar="CoV_SA.val", na.rm=TRUE)

#log10 (Error, microm)

#Dumbbell plots----
#https://datavizpyr.com/dumbbell-plot-in-r-with-ggplot2/
Dumbell_SA<-df_magic %>% 
  ggplot(aes(x= surfacearea, y= reorder(Individual,surfacearea))) +
  geom_line(aes(group = Individual),color="grey")+
  geom_point(aes(colour=Species), size=3) + geom_point(colour = "grey90", size = 1.5)+
  labs(y="Individuals")+
  theme_classic(24)+
  theme(legend.position="top") +
  scale_color_brewer(palette="Accent", direction=-1)

Dumbell_volume<-df_magic %>% 
  ggplot(aes(x= volume, y= reorder(Individual,volume))) +
  geom_line(aes(group = Individual),color="grey")+
  geom_point(aes(colour=Species), size=3) + geom_point(colour = "grey90", size = 1.5)+
  labs(y="Individuals")+
  theme_classic(24)+
  theme(legend.position="top") +
  scale_color_brewer(palette="Accent", direction=-1)

plot_grid(Dumbell_volume+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          Estimtederror.vol+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))

plot_grid(Dumbell_SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          Estimtederror.SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))

plot_grid(Dumbell_volume+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          Estimtederror.vol+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          Dumbell_SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          Estimtederror.SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b", "c", "d"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))

plot_grid(CoV.SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          CoV.Vol+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b", "c", "d"), label_size = 18, align = 'v', nrow=2, rel_heights = c(0.5,0.5))

plot_grid(log.vl+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          log.SA+theme(legend.margin = margin(rep(-5,4)), legend.key.size = unit(5,'mm')), 
          labels = c("a", "b"), label_size = 18, align = 'v', nrow=1, rel_heights = c(0.5,0.5))