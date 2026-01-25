### making native vs. introduced figures

# library
library(ggplot2)
library(tidyverse)
library(ghibli)
library(ggpubr)

# read in csv files

intro<-read_csv("introduced_ranges_data.csv")
native<-read_csv("native_ranges_data.csv")
total<-read_csv("total_ranges_data.csv")


# add nat_ to each column in native df
colnames(native) <- paste0('nat_', colnames(native))
colnames(total)<-paste0('tot_', colnames(total))

# combine dataframes
combine<-left_join(native, intro, join_by(nat_species==species), multiple="any")
combine1<-left_join(combine, total, join_by(nat_species==tot_species), multiple="any")


# shorter version of dataframe with just what we need
data_short<-combine1 %>% dplyr::select(nat_species, precip_range,
                                      temp_range, nitro_range,
                                      nat_precip_range, nat_temp_range,
                                      nat_nitro_range, nat_EFN, nat_Domatia, nat_fixer,
                                      tot_precip_range, tot_temp_range, tot_nitro_range)

# use melt to make the dataframe longer (this way, can graph with
# multiple measures on same axes)
data_melt<-reshape2::melt(data_short, id.vars=c("nat_species", "nat_EFN", "nat_Domatia", "nat_fixer"),
                          measure.vars=c("precip_range",
                                         "temp_range", "nitro_range",
                                         "nat_precip_range", "nat_temp_range",
                                         "nat_nitro_range", "tot_precip_range",
                                         "tot_nitro_range", "tot_temp_range"))

data_melt$nat_EFN<-as.factor(data_melt$nat_EFN)
data_melt$nat_Domatia<-as.factor(data_melt$nat_Domatia)
data_melt$nat_fixer<-as.factor(data_melt$nat_fixer)
data_melt$variable<-as.factor(data_melt$variable)

################################################################################
# EFN

EFN_temp<-data_melt %>% 
  subset(variable=="temp_range" | variable=="nat_temp_range" | variable=="tot_temp_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual \n temp range (\u00B0C)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
 # theme(legend.position="none")+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(fill='EFN') 
  #annotate("text", y=22.5, x=2, size = 7, label = "NS/*")


EFN_precip<-data_melt %>% 
  subset(variable=="precip_range" | variable=="nat_precip_range" | variable=="tot_precip_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Annual precipitation \n range (mm)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  theme(legend.position="none", axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
#annotate("text", y=22.5, x=2, size = 7, label = "NS/*")

EFN_nitro<-data_melt %>% 
  subset(variable=="nitro_range" | variable=="nat_nitro_range" | variable=="tot_nitro_range") %>% 
  group_by(nat_EFN) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_EFN)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("EFN")+
  ylab("Nitrogen range \n (cg/kg)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  theme(legend.position="none", axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  guides(fill = guide_legend(nrow = 1))

##############################################################################

fixer_temp<-data_melt %>% 
  subset(variable=="temp_range" | variable=="nat_temp_range" | variable=="tot_temp_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Average annual temperature range (Celsius)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(fill='rhizobia')

fixer_precip<-data_melt %>% 
  subset(variable=="precip_range" | variable=="nat_precip_range" | variable=="tot_precip_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Annual precipitation range (mm)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

fixer_nitro<-data_melt %>% 
  subset(variable=="nitro_range" | variable=="nat_nitro_range" | variable=="tot_nitro_range") %>% 
  group_by(nat_fixer) %>%
  #summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=value, fill=nat_fixer)+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  xlab("Fixer")+
  ylab("Nitrogen range (cg/kg)")+
  scale_x_discrete(labels=c("introduced", "native", "total"))+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"))+
  theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank())+
  theme(axis.title.x=element_blank(), axis.title=element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  guides(fill = guide_legend(nrow = 1))


legend_efn <- cowplot::get_legend(EFN_temp)
legend_fixer<-cowplot::get_legend(fixer_temp)
legends<-cowplot::plot_grid(legend_efn, legend_fixer, ncol=1, nrow=2)



P<-cowplot::plot_grid(EFN_temp+ theme(legend.position="none"),  
                      fixer_temp+ theme(legend.position="none"),
                      legends,
                      EFN_precip, fixer_precip, NA,
                      EFN_nitro, fixer_nitro, NA,
                      ncol=3, nrow=3,
                      labels = c('A', 'D', '', 'B', 'E', '', 'C', 'F', ''),
                      label_size = 14,
                      label_x = c(0.05, -0.05, 0, 0.05, -0.05, 0, 0.05, -0.05, 0))

plot(P)
###############################################################################
## Making plot with number of human uses 





