# Making figures for results section
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
#install.packages('ghibli')
library(ghibli)
library(reshape2)

# read in data
dat<-read.csv("pgls_final_data.csv")
dat$nitro_range<-dat$nitro_maxquant-dat$nitro_minquant
dat$mutualism<-ifelse(dat$EFN==1 | dat$Domatia==1 | dat$fixer==1, "1", "0")


# Finding basic summary stats
sum(dat$EFN==0)
sum(dat$EFN==1)


sum(dat$Domatia==0)
sum(dat$Domatia==1)

sum(dat$fixer==0)
sum(dat$fixer==1)

mutualism<-filter(dat, EFN==1 | fixer==1 | Domatia==1)
no_mutualism<-filter(dat, EFN==0 & fixer==0 & Domatia==0)

### Add mutualism column to df
dat$mutualism<-ifelse(dat$EFN==1 | dat$fixer==1 | dat$Domatia==1, 1, 0)
dat$mutualism<-as.factor(dat$mutualism)


# popsicle plot for occurrence number vs species

mutualism_plot<-dat %>%
#arrange(mutualism, desc(n), .by_group = TRUE) %>% 
subset(mutualism=="0") %>% 
#mutate(species = fct_reorder(species, rank(mutualism))) %>%
ggplot() +
aes(x=fct_reorder(species, n), y=n, colour=mutualism) +
geom_point(size=1.5)+
geom_segment(aes(x=species, y=0, yend=n))+
#scale_x_discrete()+
theme(axis.text.x=element_blank())+
ylab("number of occurrences after \n thinning and cleaning")+
xlab("Species not \n engaged in mutualism")+
scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
labs(color="mutualism")+
ylim(0, 2e+05)+
theme(legend.position="none")


no_mutualism_plot<-dat %>%
  subset(mutualism=="1") %>% 
  ggplot() +
  aes(x=fct_reorder(species, n), y=n, colour=mutualism) +
  geom_point(size=1.5)+
  geom_segment(aes(x=species, y=0, yend=n))+
  #scale_x_discrete()+
  theme(axis.text.x=element_blank(), axis.title.y = element_blank())+
  ylab("number of occurrences after \n thinning and cleaning")+
  xlab("Species \n engaged in mutualism")+
  scale_colour_manual(values=c("#DCCA2CFF"))+
  ylim(0, 2e+05)+
  #scale_colour_ghibli_d("LaputaMedium", direction = -1)+
  labs(color="mutualism")+
  theme(legend.position="none")


cowplot::plot_grid(mutualism_plot, no_mutualism_plot, nrow=1, ncol=2)

ggplot(dat, aes(x=))


### Making boxplots for max/min precipitation

data_short<-dat %>% dplyr::select(species, precip_maxquant,
                                 precip_minquant, temp_maxquant,
                                 temp_minquant, nitro_maxquant,
                                 nitro_minquant,
                                 EFN, Domatia,
                                 fixer, mutualism)

# use melt to make the dataframe longer  
data_melt<-reshape2::melt(data_short, id.vars=c("species", "EFN", "Domatia", "fixer", "mutualism"),
                           measure.vars=c("precip_maxquant",
                                          "precip_minquant", "temp_maxquant",
                                          "temp_minquant", "nitro_maxquant",
                                          "nitro_minquant"))

data_melt$EFN<-as.factor(data_melt$EFN)
data_melt$Domatia<-as.factor(data_melt$Domatia)
data_melt$fixer<-as.factor(data_melt$fixer)
data_melt$variable<-as.factor(data_melt$variable)

EFN_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual temperature \n (Celsius)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())
  


EFN_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Annual precipitation \n (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())

EFN_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, EFN) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=EFN, group=EFN)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")
  #theme(axis.title.x=element_blank())

# Domatia

Domatia_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())



Domatia_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Annual precipitation (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

Domatia_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, Domatia) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=Domatia, group=Domatia)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none")
  #theme(axis.title.x=element_blank())




# Fixer

fixer_temp<-data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("average annual temperature")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())



fixer_precip<-data_melt %>% 
  subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Annual precipitation (mm)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("not present", "present"))+
  #theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
 # guides(fill=guide_legend(title="Mutualism"))
  labs(colour = "Mutualism") 

fixer_nitro<-data_melt %>% 
  subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
  group_by(variable, fixer) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, colour=fixer, group=fixer)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none")
  #theme(axis.title.x=element_blank())
  

cowplot::plot_grid(EFN_precip, Domatia_precip, fixer_precip,
                   EFN_temp, Domatia_temp, fixer_temp,
                   EFN_nitro, Domatia_nitro, fixer_nitro, nrow=3, ncol=3)



### Using the melted dataframe to make a plot showing the data


data_melt_ID<-reshape2::melt(data_short, id.vars=c("species"),
                          measure.vars=c("EFN", "Domatia", "fixer"))

data_melt_ID$value<-as.factor(data_melt_ID$value)

ggplot(data_melt_ID, aes(x = variable, fill = value)) + 
  geom_bar()+
  theme_classic()+
  xlab("Mutualism type")+
  ylab("Number of species")+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1, labels = c("Does not have trait", "Has trait"))+
  scale_x_discrete(labels=c("EFN", "domatia", "fixer"))+
  labs(fill=element_blank())
 
dev.copy2pdf(file="data_description_fig.pdf", width = 7, height = 5)
  


