# Calculating difference between native and invasive niche breadth

library(ape)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ghibli)
library(nlme)

# Read in my data
points<-read_csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

# Group by species only to get total niche breadth
total_summary_df<-points_1 %>% 
  group_by(species) %>% 
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

# Calculate niche breadth indicators for total niche
total_summary_df$precip_range<-total_summary_df$precip_maxquant-total_summary_df$precip_minquant
total_summary_df$temp_range<-total_summary_df$temp_maxquant-total_summary_df$temp_minquant
total_summary_df$nitro_range<-total_summary_df$nitro_maxquant-total_summary_df$nitro_minquant

# calculate absolute median latitude
total_summary_df$abs_med_lat<-abs(total_summary_df$median_lat)

# Group by species and invasive status (0 or 1) and get summarizing!
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

# calculate niche breadth
summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# calculate absolute median latitude
summary_df$abs_med_lat<-abs(summary_df$median_lat)

# drop species with less than 25 occurrences
# Yes, I did this for PGLS and now I'm doing it for separate native
# and invasive ranges, but I think I need at least 25 occurrences for
# both native and invasive

summary_df<-summary_df %>% filter(n>=25)

sp_diff <- setdiff(total_summary_df$species, summary_df$species)
ID <- total_summary_df$species %in% sp_diff
total_summary_df <- total_summary_df[!ID,]

## separate by introduced status
native_ranges<-summary_df %>% subset(intrdcd=="0")
intro_ranges<-summary_df %>% subset(intrdcd=="1")

# remove extra species that only have native ranges and no introduced range
native_thin_ranges<-native_ranges %>% filter(species %in% intro_ranges$species)
total_summary_df<-total_summary_df %>% filter(species %in% intro_ranges$species)

# why are there more species in the intro than native now?
differences<-setdiff(intro_ranges$species, native_thin_ranges$species)
differences

# remove those pesky species and look into them later
intro_ranges <- intro_ranges[!intro_ranges$species %in% differences,]

# now, remove those 11 species from total dataframe
total_summary_df<- total_summary_df[!total_summary_df$species %in% differences,]

# add "intro" to all introduced column names and make a species column that will
# allow us to bind the total, introduced, and native niche data together
colnames(intro_ranges) <- paste0('intro_', colnames(intro_ranges))
intro_ranges$species<-intro_ranges$intro_species

# And add "tot" to all total niche column names (in total_summary_df)
colnames(total_summary_df) <- paste0('tot_', colnames(total_summary_df))
total_summary_df$species<-total_summary_df$tot_species

# merge dataframes by species

all_ranges<-left_join(intro_ranges, native_thin_ranges, join_by(species==species), multiple="any")
master_df<-left_join(all_ranges, total_summary_df, join_by(species==species), multiple="any")

# calculating native and invasive parts of niche
master_df$intro_temp_breadth<-master_df$tot_temp_range-master_df$temp_range
master_df$nat_temp_breadth<-master_df$tot_temp_range-master_df$intro_temp_range

master_df$intro_precip_breadth<-master_df$tot_precip_range-master_df$precip_range
master_df$nat_precip_breadth<-master_df$tot_precip_range-master_df$intro_precip_range

master_df$intro_nitro_breadth<-master_df$tot_nitro_range-master_df$nitro_range
master_df$nat_nitro_breadth<-master_df$tot_nitro_range-master_df$intro_nitro_range

# t-test for native vs invasive ranges

hist(log(master_legume$intro_precip_range))
hist(log(master_legume$precip_range))
t.test(log(master_legume$intro_precip_range), log(master_legume$precip_range), paired=TRUE)

hist(master_legume$intro_temp_range)
hist(master_legume$temp_range)
t.test(master_legume$intro_temp_range, master_legume$temp_range, paired=TRUE)

hist(log(master_legume$intro_nitro_range))
hist(log(master_legume$nitro_range))
t.test(master_legume$intro_nitro_range, master_legume$nitro_range, paired=TRUE)

### Set factors as such
master_legume$EFN<-as.factor(master_legume$EFN)
master_legume$Domatia<-as.factor(master_legume$Domatia)
master_legume$fixer<-as.factor(master_legume$fixer)


# To do further analysis, let's smoosh this dataframe with the traits dataframe
traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy
traits$species<-gsub(" ", "_", traits$species)

traits<-traits %>% filter(species %in% native_thin_ranges$species)

master_legume<-left_join(master_df, traits, join_by(species==species), multiple="any")

### Time to plot freakin 95th and 5th percentiles

### Make boxplots for max/min
# Make short data first

data_short<- master_legume %>% dplyr::select(species, precip_maxquant,
                                  precip_minquant, temp_maxquant,
                                  temp_minquant, nitro_maxquant,
                                  nitro_minquant,
                                  tot_precip_maxquant,
                                  tot_precip_minquant, tot_temp_maxquant,
                                  tot_temp_minquant, tot_nitro_maxquant,
                                  tot_nitro_minquant,
                                  intro_precip_maxquant,
                                  intro_precip_minquant, intro_temp_maxquant,
                                  intro_temp_minquant, intro_nitro_maxquant,
                                  intro_nitro_minquant, EFN, Domatia, fixer)

# Make sure R is reading mutualism data as a factor and not an integer!
data_short$EFN<-as.factor(data_short$EFN)
data_short$Domatia<-as.factor(data_short$Domatia)
data_short$fixer<-as.factor(data_short$fixer)

# use melt to make the dataframe longer  
data_melt<-reshape2::melt(data_short, id.vars=c("species","EFN", "Domatia", "fixer"),
                          measure.vars=c("precip_maxquant",
                                         "precip_minquant", "temp_maxquant",
                                         "temp_minquant", "nitro_maxquant",
                                         "nitro_minquant",
                                         "tot_precip_maxquant",
                                         "tot_precip_minquant", "tot_temp_maxquant",
                                         "tot_temp_minquant", "tot_nitro_maxquant",
                                         "tot_nitro_minquant",
                                         "intro_precip_maxquant",
                                         "intro_precip_minquant", "intro_temp_maxquant",
                                         "intro_temp_minquant", "intro_nitro_maxquant",
                                         "intro_nitro_minquant"))



data_melt$variable<-as.character(data_melt$variable)

data_melt$status<-ifelse(startsWith(data_melt$variable, "intro"), "introduced", 
                         ifelse(startsWith(data_melt$variable, "tot"), "total", "native"))

# must rename variable levels so that I can plot on same x-axis points

data_melt$variable<-gsub("intro_", "", data_melt$variable)
data_melt$variable<-gsub("tot_", "", data_melt$variable)

# Make these freaking graphs I guess

 temp<- data_melt %>% 
  subset(variable=="temp_minquant" | variable=="temp_maxquant") %>% 
  group_by(variable, status) %>%
  summarise(average = mean(value)) %>% 
  ggplot()+
  aes(x=variable, y=average, group=status, colour=status)+
  geom_point(size=5)+
  geom_path(size=1.5)+
  theme_classic()+
  #xlab()+
  ylab("Average annual temperature \n (Celsius)")+
  scale_x_discrete(labels=c("maximum", "minimum"))+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())
  
  
  precip<-data_melt %>% 
    subset(variable=="precip_minquant" | variable=="precip_maxquant") %>% 
    group_by(variable, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot()+
    aes(x=variable, y=average, group=status, colour=status)+
    geom_point(size=5)+
    geom_path(size=1.5)+
    theme_classic()+
    #xlab()+
    ylab("Annual precipitation (mm)")+
    scale_x_discrete(labels=c("maximum", "minimum"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")+
    theme(axis.title.x=element_blank())

  
  nitro<-data_melt %>% 
    subset(variable=="nitro_minquant" | variable=="nitro_maxquant") %>% 
    group_by(variable, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot()+
    aes(x=variable, y=average, group=status, colour=status)+
    geom_point(size=5)+
    geom_path(size=1.5)+
    theme_classic()+
    #xlab()+
    ylab("Average soil nitrogen (cg/kg)")+
    scale_x_discrete(labels=c("maximum", "minimum"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    #theme(legend.position="none")+
    theme(axis.title.x=element_blank())
  

  cowplot::plot_grid(precip, temp, nitro, ncol=3)
  
  
  #### graphs for each mutualism x status
  data_melt$EFN<-as.factor(data_melt$EFN)
  data_melt$Domatia<-as.factor(data_melt$Domatia)
  data_melt$fixer<-as.factor(data_melt$fixer)
  data_melt$variable<-as.factor(data_melt$variable)
  
  EFN_temp_min<-data_melt %>% 
    subset(variable=="temp_minquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Temp")+
    scale_x_discrete(labels=c("no EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #theme(axis.title.x=element_blank())
  
  EFN_temp_max<-data_melt %>% 
    subset(variable=="temp_minquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Temp")+
    scale_x_discrete(labels=c("no EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
  #theme(axis.title.x=element_blank())
  
  EFN_precip_min<-data_melt %>% 
    subset(variable=="precip_minquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Precip")+
    scale_x_discrete(labels=c("No EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #theme(axis.title.x=element_blank())
  
  
  EFN_precip_max<-data_melt %>% 
    subset(variable=="precip_maxquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Precip")+
    scale_x_discrete(labels=c("No EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
  #theme(axis.title.x=element_blank())
  
  
  EFN_nitro_min<-data_melt %>% 
    subset(variable=="nitro_minquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Nitro")+
    scale_x_discrete(labels=c("No EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
   # theme(axis.title.x=element_blank())
  
  
  EFN_nitro_max<-data_melt %>% 
    subset(variable=="nitro_maxquant") %>% 
    group_by(variable, EFN, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=EFN, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=EFN))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Nitro")+
    scale_x_discrete(labels=c("No EFN", "EFN"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #geom_jitter()
  # theme(axis.title.x=element_blank())
  
  
  # fixer 
  fixer_temp_min<-data_melt %>% 
    subset(variable=="temp_minquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Temp")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #theme(axis.title.x=element_blank())
  
  fixer_temp_max<-data_melt %>% 
    subset(variable=="temp_maxquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Temp")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
  #theme(axis.title.x=element_blank())
  
  fixer_precip_min<-data_melt %>% 
    subset(variable=="precip_minquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=factor(status)))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Precip")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #theme(axis.title.x=element_blank())
  
  fixer_precip_max<-data_melt %>% 
    subset(variable=="precip_maxquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Precip")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
  #theme(axis.title.x=element_blank())
  
  fixer_nitro_min<-data_melt %>% 
    subset(variable=="nitro_minquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Min")+
    ylab("Nitro")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
    theme(legend.position="none")
    #theme(axis.title.x=element_blank())
  
  fixer_nitro_max<-data_melt %>% 
    subset(variable=="nitro_maxquant") %>% 
    group_by(variable, fixer, status) %>%
    summarise(average = mean(value)) %>% 
    ggplot(aes(x=fixer, y=average, colour=status))+
    geom_jitter(size=4, aes(shape=fixer))+
    #geom_line(aes(group=EFN_interaction)) +
    #geom_path(size=1.5, aes(group=EFN_interaction))+
    theme_classic()+
    xlab("Max")+
    ylab("Nitro")+
    scale_x_discrete(labels=c("No fixer", "Fixer"))+
    scale_colour_ghibli_d("YesterdayMedium", direction = -1)
  # theme(legend.position="none")+
  #theme(axis.title.x=element_blank())
  
  
  cowplot::plot_grid(EFN_precip_min, EFN_precip_max, 
                     EFN_temp_min, EFN_temp_max,
                     EFN_nitro_min, EFN_nitro_max,
                     fixer_precip_min, fixer_precip_max,
                    fixer_temp_min, fixer_temp_max,
                    fixer_nitro_min, fixer_nitro_max)
  
  

  
