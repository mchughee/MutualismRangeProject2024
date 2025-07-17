# Making df with summarized niche breadth measures

library(tidyverse)
library(cowplot)


# Read in my data
points<-read_csv("invasiveclass_thinnedoccs_soil_clim_biome.csv")
n_distinct(points$species)


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
n_distinct(points_1$species)
points_1<-points_1 %>% drop_na(intrdcd)
n_distinct(points_1$species)


names(which.max(table(points_1$BIOME)))

summary_biome<-points_1 %>% 
  group_by(species) %>% 
  reframe(biome=names(table(BIOME)))

summary_biome<-points_1 %>% 
  group_by(species) %>% 
  reframe(biome=names(which.max(table(BIOME))),
        n_max=n(which.max(table(BIOME))))



# Group by species and get summarizing!
summary_df<-points_1 %>% 
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
          median_long=median(X),
          quant95=quantile(Y, 0.95),
          quant005=quantile(Y, 0.05),
          biome=names(which.max(table(BIOME)))
          
  )

print(duplicated(summary_df$species))


summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# Now read in traits

traits<-read.csv("data_files/legume_range_traits.csv")
traits$species<-traits$Phy

# Let us smoosh the traits together with the summary_df.
# The key here is going to be getting each trait row to match up with it's (potentially multiple) partner
# in summary_df

traits$Phy<-gsub(" ", "_", traits$Phy)
master_legume<-left_join(summary_df, traits, join_by(species==Phy), multiple="any")
# Yay! Smooshed data!

dropped_sp<-filter(master_legume, n<25)

master_thin<-master_legume %>% filter(n>=25)

master_thin$biome<-as.factor(master_thin$biome)
hist(master_thin$biome)

class(master_thin$EFN)
master_thin$EFN<-as.factor(master_thin$EFN)

EFN_list<-master_thin %>% 
  subset(EFN=="1")

summary(EFN_list$biome)

master_thin %>% 
  subset(EFN=="1") %>% 
  ggplot(aes(x = biome))+
  geom_histogram(stat="count", fill="#446590FF")+
  scale_x_discrete(breaks = seq(0, 13, 1))+
  theme_classic()+
  ylab("EFN count")+
  scale_x_discrete(labels=c('Tropical/subtrop. moist broadleaf frst',
                            #'Tropical/subtrop. dry broadleaf frst', 
                            #'Tropical/suptrop. coniferous frst', 
                            'Temp. broadleaf + mixed frst',
                            'Temp. Coniferous frst',
                            'Boreal forest',
                            'Tropical/subtrop. grsslnd',
                            'Temp. grasslands',
                            'Flooded grasslands',
                            #'Montane grasslands/shrubs',
                            'Tundra',
                            'Mediterranean'
                            #'Deserts/xeric shrub'
                            ))+
  theme(axis.text.x = element_text(angle = 90))

master_thin %>% 
  subset(fixer=="1") %>% 
  ggplot(aes(x = biome))+
  geom_histogram(stat="count", fill="#3E6248FF")+
  #scale_x_discrete(breaks = seq(0, 13, 1))+
  theme_classic()+
  ylab("fixer count")+
  scale_x_discrete(labels=c('Tropical/subtrop. moist broadleaf forests',
                            'Tropical/subtrop. dry broadleaf forests', 
                            'Tropical/suptrop. coniferous forests', 
                            'Temp. broadleaf and mixed forests',
                            'Temp. Coniferous Forest',
                            'Boreal forest',
                            'Tropical/subtrop. grasslands',
                            'Temp. grasslands',
                            'Flooded grasslands',
                            'Montane grasslands/shrubs',
                            'Tundra',
                            'Mediterranean',
                            'Deserts/xeric shrub'))+
  theme(axis.text.x = element_text(angle = 90))




write.csv(master_thin, "data_files/pgls_summary_data_long_added_biome_added.csv")





