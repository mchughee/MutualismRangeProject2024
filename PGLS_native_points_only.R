### PGLS, native points only

library(ape)
library(tidyverse)
library(nlme)
library(cowplot)


# Read in my data
points<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)

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

summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# drop rows where species are invasive (intrdcd==0)
summary_native<-summary_df[!(summary_df$intrdcd %in% "1"),]

# drop species with fewer than 25 obsersvations
summary_native1<-summary_native %>% filter(n>=25)

# Now read in traits

traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy

# Let us smoosh the traits together with the summary_df.
# The key here is going to be getting each trait row to match up with it's (potentially multiple) partner
# in summary_df

traits$Phy<-gsub(" ", "_", traits$Phy)
master_legume<-left_join(summary_native1, traits, join_by(species==Phy), multiple="any")
# Yay! Smooshed data!

# Bring in phylogeny
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")

# drop extra species from the tree
dropped_species<- setdiff(mytree$tip.label, master_legume$species)
tree_pruned <- drop.tip(mytree, dropped_species)


# make rows in data match rows in tree
master_legume <- master_legume[match(tree_pruned$tip.label,master_legume$species),]

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(master_legume$temp_range)

hist(log(master_legume$temp_range))

hist(scale(master_legume$temp_range))

#summary_native1$log_temp_range<-(log(summary_native1$temp_range))
#summary_native1$scale_temp_range<-(scale(summary_native1$temp_range))

# precip data
hist(master_legume$precip_range)
hist(log(master_legume$precip_range))
master_legume$log_precip_range<-(log(master_legume$precip_range))


# distribution of nitrogen
hist(master_legume$nitro_range)
hist(log(master_legume$nitro_range))
min(master_legume$nitro_range)
master_legume$log_nitro_range<-(log(master_legume$nitro_range))


# number of occurrences
hist(master_legume$n)
hist(log(master_legume$n))
master_legume$log_n<-log(master_legume$n)

# Run PGLS

precip_range <- gls(log_precip_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                    data=master_legume, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)


# pgls for nitro range

nitro_range <- gls(log_nitro_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + log_n + median_lat,
                   
                   data=master_legume, 
                   
                   correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)



# pgls for temp range

temp_range <- gls(temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + log_n + median_lat,
                  data=master_legume, 
                  correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

plot(temp_range)
