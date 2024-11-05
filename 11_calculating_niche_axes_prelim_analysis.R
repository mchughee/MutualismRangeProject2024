# Bringing in phylogeny and doing preliminary analysis

library(ape)
library(geiger)
library(caper)
library(tidyverse)
library(sf)
#install.packages("RRphylo")
library(RRphylo)
library(cowplot)
#install.packages("nodiv")
library(nodiv)
install.packages("TreeTools")
library(TreeTools)


# Read in my data
points<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

# Group by species and invasive status (0 or 1) and get summarizing!
summary_df<-points_1 %>% 
  group_by(species, intrdcd) %>% 
  reframe(precip_maxquant=quantile(precip, 0.95), 
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

summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant

# Now read in traits

traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy

# Let us smoosh the traits together with the summary_df.
# The key here is going to be getting each trait row to match up with it's (potentially multiple) partner
# in summary_df

traits$Phy<-gsub(" ", "_", traits$Phy)
master_legume<-left_join(summary_df, traits, join_by(species==Phy), multiple="any")
# Yay! Smooshed data!


# Okay, now time to get moving with this freaking tree!! A la 
# https://nhcooper123.github.io/macro-module-2020/phylogenetic-generalised-least-squares-pgls-in-r.html#phylogenetic-generalized-least-squares-models-pgls

ggplot(master_legume, aes(x = fixer, 
                   y = precip_range, 
                   colour = intrdcd)) +
  geom_point() +
  theme_bw()

# Let's generate some cutie patootie graphs and anovas so I actually have something to show in meeting today

# let's just make sure our binary-coded variables are factors and not numbers haha!!
master_legume$intrdcd<-as.factor(master_legume$intrdcd)
master_legume$fixer<-as.factor(master_legume$fixer)


# Let's look at the effect of mutualism on niche breadth for native and invasive points for precipitation

native_precip<-ggplot(master_legume[master_legume$intrdcd==0,], aes(x=fixer, y=precip_range))+
  geom_boxplot()+
  theme_classic()

intro_precip<-ggplot(master_legume[master_legume$intrdcd==1,], aes(x=fixer, y=precip_range))+
  geom_boxplot()+
  theme_classic()


plot_grid(native_precip, intro_precip, labels = c('species in their native range', 'species in their introduced range'))

# Let's look at the effect of mutualism on niche breadth for native and invasive points for temperature

native_temp<-ggplot(master_legume[master_legume$intrdcd==0,], aes(x=fixer, y=temp_range))+
  geom_boxplot()+
  theme_classic()

intro_temp<-ggplot(master_legume[master_legume$intrdcd==1,], aes(x=fixer, y=temp_range))+
  geom_boxplot()+
  theme_classic()


cowplot::plot_grid(native_temp, intro_temp, labels = c('species in their native range', 'species in their introduced range'))

# Make new master dataset with species grouped, not grouped by native or invasive
summary_df_species<-points_1 %>% 
  group_by(species) %>% 
  reframe(precip_maxquant=quantile(precip, 0.95), 
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

summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant

master_legume_species<-left_join(summary_df_, traits, join_by(species==Phy), multiple="any")

# Back to PGLS
# Let's chop off our big pizza slice of polytomy pain

# Read in the tree
myTree <- ape::read.tree("phylogeny_all_buildnodes.tre")
plot(myTree)

# drop tips from tree that are not in our dataset

phydiff <- setdiff(myTree$tip.label, $Phy2)

# make a dataframe with the dataset and the phylogeny



