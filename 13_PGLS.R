# Running PGLS
library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(emmeans)
library(ghibli)
library(ggplot2)

# Removing these weird species with 0-little niche breadth at all, and writing 
# it into a new file that I can read in at any time (very exciting stuff):
#data_2<-data_1 %>% filter(n>=25)
#write.csv(data_2, "pgls_final_data.csv")
# we will read this in next

# Read back in PGLS dataframe
data<-read.csv("pgls_final_data.csv")
#data$mutualism<-ifelse(data$EFN==1 | data$Domatia==1 | data$fixer==1, "1", "0")


# Bring in tree-- this tree has the polytomy removed,
# so we also need to trim down the dataset to only include
# species in the tree!
mytree<-read.tree("polytomy_removed.tre")

# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, dropped_species)

# here, we'll trim down
data1<-filter(data, data$species %in% tree_pruned$tip.label)
# don't worry about the fact that it's 133 species dropped, not 139
# when dropping species without occurrences, I think we ended up dropping
# some species in the polytomy


# make rows in data match rows in tree
data_1 <- data1[match(tree_pruned$tip.label,data1$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(data_1$temp_range)
hist(log(data_1$temp_range))


# precip data
hist(data_1$precip_range)
hist(log(data_1$precip_range))


# distribution of nitrogen
hist(data_1$nitro_range)
hist(log(data_1$nitro_range))
min(data_1$nitro_range)



# make sure R is reading our factors as factors!!!
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

# precip first

precip_range <- gls(log(precip_range) ~ EFN + Domatia + fixer + woody
                    + uses_num_uses + annual + n + poly(abs_med_lat, 2)+EFN*poly(abs_med_lat, 2)+
                      Domatia*poly(abs_med_lat, 2)+fixer*poly(abs_med_lat, 2),
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))





# pgls for temp range

temp_range <- gls(temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + n + poly(abs_med_lat, 2)+EFN*poly(abs_med_lat, 2)+
                    Domatia*poly(abs_med_lat, 2)+fixer*poly(abs_med_lat, 2),
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)

data_1$temp_predict<-predict(temp_range)


#EFN_temp_pred <- data.frame(EFN = c("0", "1"))
#predict(temp_range, newdata=temp_range_pred)



#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + Domatia + fixer + woody + uses_num_uses
                   + annual + n + poly(abs_med_lat, 2)+EFN*poly(abs_med_lat, 2)+
                     Domatia*poly(abs_med_lat, 2)+fixer*poly(abs_med_lat, 2),

                   data=data_1, 

                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))

data_1$nitro_predict<-predict(nitro_range)

########################################

# write new dataframe with the columns containing model fit from pgls output

write.csv(data_1, "pgls_but_now_with_model_fit.csv")


#######################################
# Make latitude figure

# EFN temp
EFN_temp <- ggplot(data=data_1)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=EFN), alpha=0.3)+
  #geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=EFN))+
  geom_line(aes(y=temp_predict, x=abs(median_lat)))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("average annual temperature \n range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.x=element_blank(), text = element_text(size = 10), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "**", x=65, y=22.5, size = 8)


# EFN precip
EFN_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=EFN), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("annual precipitation \n range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "***", x=65, y=4500, size = 8)


# EFN nitro
EFN_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=EFN), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=EFN))+
  theme_cowplot()+
  scale_colour_ghibli_d("YesterdayMedium", direction = -1)+
  ylab("nitrogen \n range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none", axis.title.x = element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))+
  geom_text(label = "***", x=65, y=1500, size = 8)

### Domatia time

# Domatia temp
domatia_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  #scale_colour_ghibli_d("MarnieMedium2")+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# domatia precip
domatia_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# domatia nitro
domatia_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=Domatia), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=Domatia))+
  theme_cowplot()+
  scale_colour_manual(values=c("#26432FFF", "#92BBD9FF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))



### Rhizobia o'clock

# fixer temp
fixer_temp <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=temp_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=temp_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  #scale_colour_ghibli_d("YesterdayMedium", direction=1)+
  ylab("Average annual temperature range (Celsius)")+
  xlab("absolute median latitude")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# fixer precip
fixer_precip <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=precip_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=precip_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("annual precipitation range (mm)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))


# fixer nitro
fixer_nitro <- ggplot(data=dat)+
  geom_point(aes(x=abs(median_lat), y=nitro_range, color=fixer), alpha=0.3)+
  geom_smooth(method="lm", aes(x=abs(median_lat), y=nitro_range, color=fixer))+
  theme_cowplot()+
  scale_colour_manual(values=c("#403369FF", "#AE93BEFF"))+
  ylab("nitrogen range (cg/kg)")+
  xlab("absolute median latitude")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), text = element_text(size = 10),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=9))

P<-cowplot::plot_grid(EFN_temp, domatia_temp, fixer_temp,
                      EFN_precip, domatia_precip, fixer_precip,
                      EFN_nitro, domatia_nitro, fixer_nitro,
                      labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                      label_size = 11,
                      label_x = c(0, -0.05, -0.05, 0, -0.05, -0.05, 0, -0.05, -0.05))

P <- add_sub(P, "absolute median latitude", hjust = 0.4, size=12)

plot(P)

#dev.copy2pdf(file="latitude_v_breadth_fig.pdf", width = 7.5, height = 7.5)

jpeg("latitudefig.jpg", width=900, height=900)

plot(P)

dev.off()


