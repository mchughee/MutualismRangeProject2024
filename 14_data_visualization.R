# Making figures for results section
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
#install.packages('ghibli')
library(ghibli)
library(reshape2)

# read in data
dat<-read.csv("pgls_but_now_with_model_fit.csv")
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

### Make sure the categorical variables are being read as categorical by R
dat$Domatia<-as.factor(dat$Domatia)
dat$EFN<-as.factor(dat$EFN)
dat$fixer<-as.factor(dat$fixer)


### Making data usable for boxplots and reaction norm plots!

# shorter version of dataframe with just what we need
data_short<-dat %>% dplyr::select(species, precip_maxquant,
                                 precip_minquant, temp_maxquant,
                                 temp_minquant, nitro_maxquant,
                                 nitro_minquant,
                                 EFN, Domatia,
                                 fixer, mutualism,
                                 precip_predict,
                                 temp_predict,
                                 nitro_predict)

# use melt to make the dataframe longer (this way, can graph with
# multiple measures on same axes)
data_melt<-reshape2::melt(data_short, id.vars=c("species", "EFN", "Domatia", "fixer", "mutualism"),
                           measure.vars=c("precip_maxquant",
                                          "precip_minquant", "temp_maxquant",
                                          "temp_minquant", "nitro_maxquant",
                                          "nitro_minquant"))

data_melt$EFN<-as.factor(data_melt$EFN)
data_melt$Domatia<-as.factor(data_melt$Domatia)
data_melt$fixer<-as.factor(data_melt$fixer)
data_melt$variable<-as.factor(data_melt$variable)

#### Making reaction norms for max/min precipitation

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
  theme(axis.title.x=element_blank(), axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", y=22.5, x=2, size = 7, label = "NS/*")
  


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
  theme(axis.title.x=element_blank(), axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", x=2, y=1900, label = "*/NS", size = 7)

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
  theme(legend.position="none", axis.title=element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", x=2, y=350, label = "***/NS", size = 7)

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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=26, size = 7)



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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=3000, size = 7)

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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none", axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=350, size = 7)




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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  annotate("text", label = "*/NS", x=2, y=24, size = 7)



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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1, labels=c("not present", "present"))+
  #theme(legend.position="none")+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.title=element_text(size=16), axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
 # guides(fill=guide_legend(title="Mutualism"))
  theme(legend.position="none")+
  labs(colour = "Mutualism")+
  annotate("text", label = "***/*", x=2, y=2000, size = 7)

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
  #scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none", axis.title=element_text(size=16), 
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))
  #annotate("text", label = "NS/NS", x=2, y=300, size = 7)
  

plot<-cowplot::plot_grid(EFN_precip, Domatia_precip, fixer_precip,
                   EFN_temp, Domatia_temp, fixer_temp,
                   EFN_nitro, Domatia_nitro, fixer_nitro, nrow=3, ncol=3,
                   labels = c('A', 'D', 'H', 'B', 'E', 'I', 'C', 'F', 'J'),
                   label_size = 14,
                   label_x = c(0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05))
plot

jpeg("maxmin.jpeg", width=700, height=600)

plot

dev.off()


### Using the melted dataframe to make plots showing hte 
### distribution of the data


data_melt_ID<-reshape2::melt(data_short, id.vars=c("species"),
                          measure.vars=c("EFN", "Domatia", "fixer"))

data_melt_ID$value<-as.factor(data_melt_ID$value)

mutualism_plot<-ggplot(data_melt_ID, aes(x = variable, fill = value)) + 
  geom_bar()+
  theme_classic()+
  xlab("Mutualism type")+
  ylab("Number of species")+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1, labels = c("Does not have mutualism", "Has mutualism"))+
  scale_x_discrete(labels=c("EFN", "domatia", "fixer"))+
  labs(fill=element_blank())

### Plot showing invasive vs native
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")
str(points)
head(points)
# The "warning" message here is about the dateIdentified not being in the 
# correct format, which like, I don't care about, so I'm ignoring
points$species<-as.factor(points$species)
levels(unique(points$species))
# 2771 species in dataset

# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

# Group by species and invasive status (0 or 1) and get summarizing!
# we have several measures of niche, latitude, etc.
summary_df<-points_1 %>% 
  group_by(species, intrdcd) %>% 
  reframe(n=n(),
          precip_maxquant=quantile(precip, 0.95), 
          precip_minquant=quantile(precip, 0.05),
          precip_mean=mean(precip),
          precip_median=median(precip),
          nitro_maxquant=quantile(nitrogen, 0.95),
          nitro_minquant=quantile(nitrogen, 0.05),
          nitro_mean=mean(nitrogen),
          nitro_median=median(nitrogen),
          temp_maxquant=quantile(temp, 0.95),
          temp_minquant=quantile(temp, 0.05),
          temp_mean=mean(temp),
          temp_median=median(temp),
          max_lat=max(Y),
          min_lat=min(Y),
          mean_lat=mean(Y),
          median_lat=median(Y),
          quant95=quantile(Y, 0.95),
          quant005=quantile(Y, 0.05)
  )

summary_df<-summary_df %>% filter(n>=25)
n_distinct(unique(summary_df$species))
# There are now 2656 species in the dataset

## separate the summary_df into native and invasive range dataframes
native_ranges<-summary_df %>% subset(intrdcd=="0")
intro_ranges<-summary_df %>% subset(intrdcd=="1")

colnames(intro_ranges) <- paste0('intro_', colnames(intro_ranges))
intro_niche<-left_join(native_ranges, intro_ranges, join_by(species==intro_species), multiple="any")

intro_niche$bothranges<-ifelse(intro_niche$intrdcd=="0" & intro_niche$intro_intrdcd=="1", "1", "0")
intro_niche$bothranges[is.na(intro_niche$bothranges)] <- "0"


invasive<-ggplot(data=intro_niche, aes(x=bothranges, fill=bothranges))+
  geom_bar()+
  theme_classic()+
  scale_x_discrete(labels= c("native range only", "both native and introduced"))+
  ylab("Number of species")+
  scale_fill_manual(values=c("#CD4F38FF","#E48C2AFF"), labels = c("Does not have trait", "Has trait"))+
  labs(fill=element_blank())+
  theme(legend.position="none")
# Jesus, that was a lot of work for one plot! Onwards and upwards I guess

### absolute median latitude plot

dat$abs_med_lat<-abs(dat$median_lat)

latitude_EFN<-ggplot(data=dat, aes(x=abs_med_lat, fill=EFN))+
  geom_histogram()+
  theme_classic()+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_text(size=18),
        title.text.y = element_text(size=15), axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
  guides(fill=guide_legend("EFN"))


latitude_domatia<-ggplot(data=dat, aes(x=abs_med_lat, fill=Domatia))+
  geom_histogram()+
  theme_classic()+
  scale_fill_manual(values=c("#26432FFF", "#92BBD9FF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        title.text.y = element_text(size=15))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15),
        axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  guides(fill=guide_legend("Domatia"))

latitude_fixer<-ggplot(data=dat, aes(x=abs_med_lat, fill=fixer))+
  geom_histogram()+
  theme_classic()+
  scale_fill_manual(values=c("#403369FF", "#AE93BEFF"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
        title.text.y = element_text(size=15), axis.text.y=element_text(size=13),
        axis.text.x=element_text(size=13))+
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
  guides(fill=guide_legend("Rhizobia"))


p<-cowplot::plot_grid(latitude_EFN, latitude_domatia, latitude_fixer, nrow=1, ncol=3,
                      labels = c('A', 'B', 'C'),
                      label_x = c(0, -0.05, -0.05),
                      label_size = 18)

p <- add_sub(p, "absolute median latitude", hjust = 0.4, size=18)

plot(p) 
  



 





