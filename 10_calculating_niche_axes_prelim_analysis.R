# Bringing in phylogeny and doing preliminary analysis

library(ape)
library(tidyverse)
library(cowplot)


# Read in my data
points<-read.csv("invasiveclass_thindat_climadd_soilgridsadd.csv")


# Drop points that have NA values for the niche axes and the intrdcd status
points_1<-points %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)
points_1<-points_1 %>% drop_na(intrdcd)


# Group by species and invasive status (0 or 1) and get summarizing!
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
          quant95=quantile(Y, 0.95),
          quant005=quantile(Y, 0.05)
  )

duplicated(summary_df$species)


summary_df$precip_range<-summary_df$precip_maxquant-summary_df$precip_minquant
summary_df$temp_range<-summary_df$temp_maxquant-summary_df$temp_minquant
summary_df$nitro_range<-summary_df$nitro_maxquant-summary_df$nitro_minquant

# Now read in traits

traits<-read.csv("legume_range_traits.csv")
traits$species<-traits$Phy

# Let us smoosh the traits together with the summary_df.
# The key here is going to be getting each trait row to match up with it's (potentially multiple) partner
# in summary_df

traits$Phy<-gsub(" ", "_", traits$Phy)
master_legume<-left_join(summary_df, traits, join_by(species==Phy), multiple="any")
# Yay! Smooshed data!

master_thin<-master_legume %>% filter(n>=25)




write.csv(master_thin, "pgls_final_data.csv")





