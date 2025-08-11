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

# make sure biome is a factor
class(points_1$BIOME)
levels(points_1$BIOME)
points_1$BIOME<-as.factor(points_1$BIOME)
points_1$species<-as.factor(points_1$species)

# let's see what biome looks like. Where are most points found?

levels(points_1$BIOME)
summary(points_1$BIOME)
summary(points_1$BIOME)
# as could be anticipated, most points are falling into tropical and temperate
# forests, and tropical grasslands

points_1 %>% group_by(species) %>% reframe(num_biome=length(unique(BIOME)))

# common sense check that the code works
(points_1 %>% filter(species=="Acacia_acuminata"))$BIOME

(points_1 %>% filter(species=="Abrus_precatorius"))$BIOME

# common sense check-- species shouldn't be found in more than 16 biomes
# (this is the number of biome levels in the occurrence dataset-- remember,
# we have 98 and 99 in there)

# Group by species and get summarizing!
summary_df<-points_1 %>% 
  group_by(species) %>% 
  filter(BIOME!="98" & BIOME!="99") %>% 
  droplevels() %>% 
  # this line of code removes the 98 and 99 biomes (lakes and ice) from
  # the dataset. Please remove if not running this code specifically for
  # biome-related analyses
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
          num_biome=length(unique(na.omit(BIOME))),
          biome=names(which.max(table(na.omit(BIOME))))
          
          
  )


# common sense check-- species shouldn't be found in more than 14 biomes

summary(summary_df$num_biome)
# Nice! Everything is as it should be


print(duplicated(summary_df$species))
summary_df[duplicated(summary_df$species),]
# no duplicates

# add in our niche breadth measures
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

# write our new dataframe into a csv
#write.csv(master_thin, "data_files/pgls_summary_data_long_added_biome_added.csv")


###############################################################################
# Making figures to explore the effects of biome
# Bringing in phylogeny to thin to just the species in our final dataset
# (remember, we took some species not in the phylogeny out of our analysis)
library(ape)
library(ggtree)
tree<-read.tree("phylogeny/polytomy_removed.tre")


setdiff(master_thin$species, tree$tip.label)
master_thin<-filter(master_thin, master_thin$species %in% tree$tip.label)

########################
# Do species engaged in mutualisms occur in more biomes than non-mutualistic species?
master_thin$EFN<-as.factor(master_thin$EFN)
master_thin$fixer<-as.factor(master_thin$fixer)

efn_biome<-master_thin %>%
  ggplot(aes(x=EFN, y=num_biome, fill=EFN))+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  scale_fill_manual(values=c("#92BBD9FF", "#B50A2AFF"), labels=c("no", "yes"))+
  scale_x_discrete(labels= c("no", "yes"))+
  ylab("biome number")

rhiz_biome<-master_thin %>%
  ggplot(aes(x=fixer, y=num_biome, fill=fixer))+
  geom_boxplot(stat="boxplot")+
  theme_classic()+
  scale_fill_manual(values=c("#92BBD9FF", "#26432FFF"), labels=c("no", "yes"), name="Rhizobia")+
  scale_x_discrete(labels= c("no", "yes"))+
  xlab("Rhizobia")+
  theme(axis.title.y=element_blank())

cowplot::plot_grid(efn_biome, rhiz_biome)

########################################
# Are the means different?
mean(master_thin[master_thin$EFN=="0",]$num_biome)
mean(master_thin[master_thin$EFN=="1",]$num_biome)

mean(master_thin[master_thin$fixer=="0",]$num_biome)
mean(master_thin[master_thin$fixer=="1",]$num_biome)

# t-test to check if difference is significant
t.test(num_biome ~ EFN, data = master_thin, var.equal = TRUE)
t.test(num_biome ~ fixer, data = master_thin, var.equal = TRUE)
