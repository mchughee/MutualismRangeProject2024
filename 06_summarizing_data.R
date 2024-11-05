### Read in required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Make summary dataframe

data<-read.csv("thindat_climadd_soilgridsadd.csv")

# summarize data

data1<-data %>% drop_na(precip) %>% drop_na(temp) %>%  drop_na(nitrogen)

summary_df<-data1 %>% 
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


# write into csv file
write.csv(summary_df, "summary_df_soilsgridsadd_sept2024.csv")

# smoosh with range traits dataset
traits<-read.csv("legume_range_traits.csv")
traits$Phy<-gsub(" ", "_", traits$Phy)

traits$Phy<-as.factor(traits$Phy)
data1$species<-as.factor(data1$species)
traits<-dplyr::filter(traits, traits$Phy %in% summary_df$species)

# remove the duplicates
traits$duplicate<-duplicated(traits$Phy)
subset(traits, traits$duplicate=="TRUE")
traits<-traits[-67,]

# Merge the datasets
master<-cbind(summary_df, traits)

# Make column with niche breadth
master$precip_breadth<-master$precip_maxquant-master$precip_minquant
master$temp_breadth<-master$temp_maxquant-master$temp_minquant
master$nitro_breadth<-master$nitro_maxquant-master$nitro_minquant

# Plot niche breadth for EFN
master$EFN<-as.factor(master$EFN)
ggplot(master, aes(x=EFN, y=nitro_breadth))+
  geom_boxplot(stat="boxplot")+
  theme_classic()

# For rhizobia
master$fixer<-as.factor(master$fixer)
ggplot(master, aes(x=fixer, y=nitro_breadth))+
  geom_boxplot(stat="boxplot")+
  theme_classic()
# wow...a lot of outliers! Maybe there is something there

# For domatia
master$Domatia<-as.factor(master$Domatia)
ggplot(master, aes(x=Domatia, y=nitro_breadth))+
  geom_boxplot(stat="boxplot")+
  theme_classic()

# For fungus
master$fungus<-ifelse(master$AM=="Y" | master$EM=="Y" , "1", "0")
master$fungus<-as.factor(master$fungus)
ggplot(master, aes(x=fungus, y=nitro_breadth))+
  geom_boxplot(stat="boxplot")+
  theme_classic()
# okay lots of outliers

# mrmrmrmrmrmrmmr seems to suggest some broader spread?
lm_efn<-lm(nitro_breadth~EFN, master)
anova(lm_efn)

t.test(nitro_breadth ~ EFN, data = master, var.equal = FALSE)
# significant

t.test(nitro_breadth ~ Domatia, data = master, var.equal = FALSE)
#not significant

t.test(nitro_breadth ~ fixer, data = master, var.equal = FALSE)
#significant

t.test(nitro_breadth ~ fungus, data = master, var.equal = FALSE)
#not significant