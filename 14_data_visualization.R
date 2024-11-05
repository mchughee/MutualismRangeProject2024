# Making figures for results section

library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)

# read in data
dat<-read.csv("pgls_final_data.csv")
dat$nitro_range<-dat$nitro_maxquant-dat$nitro_minquant
dat$mutualism<-ifelse(dat$EFN==1 | dat$Domatia==1 | dat$fixer==1, "1", "0")

# make list of factors we want to loop over to make plots-- make a plot for each
# mutualism type
factor_list<-c("EFN", "Domatia", "fixer")

# order data by precip_maxquant

for (i in factor_list){
  dat[,i]<-as.factor(dat[,i])
  dat %>%
    mutate(species = fct_reorder(species, precip_maxquant)) %>% #rank(dat[, i]))) %>%
    ggplot() +
    aes(x=species, y=precip_maxquant, colour=dat[,i], group=dat[, 1])+
    geom_point()+
    geom_segment(aes(x=species, y=precip_minquant, yend=precip_maxquant))+
    scale_x_discrete()+
    theme(axis.text.x=element_blank())+
    ylab("precipitation niche breadth (mm/year)")+
    scale_colour_manual(values=c("#2A788EFF", "#7AD151FF"))+
    labs(color=i)
  ggsave(filename = str_c("data_viz/","precipmax", "_", i, "_", "nichebreadth", ".pdf"), width = 14, height = 6)}



for (i in factor_list){
  dat[,i]<-as.factor(dat[,i])
    #mutate(species = fct_reorder(species, rank(dat[, i]))) %>%
    dat$species<-as.factor(dat$species)
    dat %>% arrange(species, temp_range) %>% 
    ggplot() +
    aes(x=species, y=temp_range, colour=dat[,i]) +
    geom_point()+
    geom_segment(aes(x=species, y=0, yend=temp_range))+
    scale_x_discrete()+
    theme(axis.text.x=element_blank())+
    ylab("temperature niche breadth (degrees)")+
    scale_colour_manual(values=c("#2A788EFF", "#7AD151FF"))+
    labs(color=i)
  ggsave(filename = str_c("data_viz/","temp", "_", i, "_", "nichebreadth", ".pdf"), width = 14, height = 6)}



for (i in factor_list){
  dat[,i]<-as.factor(dat[,i])
  dat %>%
    mutate(species = fct_reorder(species, rank(dat[, i]))) %>%
    ggplot() +
    aes(x=species, y=nitro_range, colour=dat[,i]) +
    geom_point()+
    geom_segment(aes(x=species, y=0, yend=nitro_range))+
    scale_x_discrete()+
    theme(axis.text.x=element_blank())+
    ylab("nitrogen niche breadth (cg/kg)")+
    scale_colour_manual(values=c("#2A788EFF", "#7AD151FF"))+
    labs(color=i)
  ggsave(filename = str_c("data_viz/","nitro", "_", i, "_", "nichebreadth", ".pdf"), width = 14, height = 6)}



  dat %>%
    #mutate(species = fct_reorder(species, rank(dat[, i]))) %>%
    ggplot() +
    #aes(x=species, y=nitro_range, colour=dat[,i]) +
    (aes(fct_reorder(species, nitro_range, .desc = TRUE), nitro_range, colour = fixer)) +
    geom_point()+
    geom_segment(aes(x=species, y=0, yend=nitro_range))+
    scale_x_discrete()+
    theme(axis.text.x=element_blank())+
    ylab("nitrogen niche breadth (cg/kg)")+
    scale_colour_manual(values=c("#2A788EFF", "#7AD151FF"))+
    labs(color=i)



# popsicle plot for occurrence number vs species

mutualism_plot<-dat %>%
  mutate(species = fct_reorder(species, rank(mutualism))) %>%
  ggplot() +
  aes(x=species, y=n, colour=mutualism) +
  geom_point()+
  geom_segment(aes(x=species, y=0, yend=n))+
  scale_x_discrete()+
  theme(axis.text.x=element_blank())+
  ylab("number of occurrences after thinning and cleaning")+
  scale_colour_manual(values=c("#39568CFF", "#FDE725FF"), labels=c("no mutualism", "mutualism"))+
  labs(color="mutualism")
ggsave(plot=last_plot(), filename = str_c("data_viz/","occs_per_species.pdf"), width = 14, height = 6)


# Let's make some boxplots
dat$EFN<-as.factor(dat$EFN)
dat$Domatia<-as.factor(dat$Domatia)
dat$fixer<-as.factor(dat$fixer)


EFN<-ggplot(dat, aes(x=EFN, y=precip_range, fill=EFN))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Extrafloral nectaries")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())
  
  
Domatia<-ggplot(dat, aes(x=Domatia, y=precip_range, fill=Domatia))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())

fixer<-ggplot(dat, aes(x=fixer, y=precip_range, fill=fixer))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Nitrogen-fixing bacteria")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())

# the same thing but for temperature niche
EFN_temp<-ggplot(dat, aes(x=EFN, y=temp_range, fill=EFN))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Extrafloral nectaries")+
  ylab("Temperature breadth \n (degrees)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())


Domatia_temp<-ggplot(dat, aes(x=Domatia, y=temp_range, fill=Domatia))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Temperature breadth \n (degrees)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())

fixer_temp<-ggplot(dat, aes(x=fixer, y=temp_range, fill=fixer))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Nitrogen-fixing bacteria")+
  ylab("Temperature breadth \n (degrees)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())

# For nitrogen
EFN_nitro<-ggplot(dat, aes(x=EFN, y=nitro_range, fill=EFN))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Extrafloral nectaries")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)


Domatia_nitro<-ggplot(dat, aes(x=Domatia, y=nitro_range, fill=Domatia))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Domatia")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.y=element_blank())

fixer_nitro<-ggplot(dat, aes(x=fixer, y=nitro_range, fill=fixer))+
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  xlab("Nitrogen-fixing bacteria")+
  ylab("Nitrogen breadth \n (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))+
  scale_fill_viridis(discrete=TRUE)+
  theme(axis.title.y=element_blank())





cowplot::plot_grid(EFN, Domatia, fixer, 
                   EFN_temp, Domatia_temp, fixer_temp, 
                   EFN_nitro, Domatia_nitro, fixer_nitro,
                   nrow=3, ncol=3, hjust=0.3,
                   labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"))


# What about plotting max vs min temperature for the species


  
