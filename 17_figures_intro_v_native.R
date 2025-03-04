### making native vs. introduced figures

# library
library(ggplot2)
library(tidyverse)
library(ghibli)

# read in csv files

intro<-read_csv("introduced_ranges_data.csv")
native<-read_csv("native_ranges_data.csv")


# add nat_ to each column in native df
colnames(native) <- paste0('nat_', colnames(native))

# combine dataframes
combine<-left_join(native, intro_new, join_by(nat_species==species), multiple="any")


# shorter version of dataframe with just what we need
data_short<-combine %>% dplyr::select(nat_species, introduced_precip_niche,
                                      introduced_temp_niche, introduced_nitro_niche,
                                      nat_n, nat_precip_range, nat_temp_range,
                                      nat_nitro_range, nat_EFN, nat_Domatia, nat_fixer)

# use melt to make the dataframe longer (this way, can graph with
# multiple measures on same axes)
data_melt<-reshape2::melt(data_short, id.vars=c("nat_species", "nat_EFN", "nat_Domatia", "nat_fixer"),
                          measure.vars=c("introduced_precip_niche",
                                         "introduced_temp_niche", "introduced_nitro_niche",
                                         "nat_n", "nat_precip_range", "nat_temp_range",
                                         "nat_nitro_range"))

data_melt$nat_EFN<-as.factor(data_melt$nat_EFN)
data_melt$nat_Domatia<-as.factor(data_melt$nat_Domatia)
data_melt$nat_fixer<-as.factor(data_melt$nat_fixer)
data_melt$variable<-as.factor(data_melt$variable)

############################################################################
# EFN

EFN_temp<-data_melt %>% 
  subset(variable=="introduced_temp_niche" | variable=="nat_temp_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual \n temp range (\u00B0C)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
 # theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(fill='EFN') 
  #annotate("text", y=22.5, x=2, size = 7, label = "NS/*")


EFN_precip<-data_melt %>% 
  subset(variable=="introduced_precip_niche" | variable=="nat_precip_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Annual precipitation \n range (mm)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none", axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
#annotate("text", y=22.5, x=2, size = 7, label = "NS/*")

EFN_nitro<-data_melt %>% 
  subset(variable=="introduced_nitro_niche" | variable=="nat_nitro_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen range \n (cg/kg)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none", axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

##############################################################################

fixer_temp<-data_melt %>% 
  subset(variable=="introduced_temp_niche" | variable=="nat_temp_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Average annual temperature range (Celsius)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(fill='rhizobia') 

fixer_precip<-data_melt %>% 
  subset(variable=="introduced_precip_niche" | variable=="nat_precip_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Annual precipitation range (mm)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

fixer_nitro<-data_melt %>% 
  subset(variable=="introduced_nitro_niche" | variable=="nat_nitro_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen range (cg/kg)")+
  scale_x_discrete(labels=c("introduced", "native"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

P<-cowplot::plot_grid(EFN_temp,  fixer_temp,
                      EFN_precip, fixer_precip,
                      EFN_nitro, fixer_nitro,
                      ncol=2, nrow=3,
                      labels = c('A', 'D', 'B', 'E', 'C', 'F'),
                      label_size = 14,
                      label_x = c(0.05, -0.05,  0.05, -0.05,  0.05, -0.05))

plot(P)






