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
data$mutualism<-ifelse(data$EFN==1 | data$Domatia==1 | data$fixer==1, "1", "0")


# Bring in tree
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")


# drop tips not in dataset (remember, we dropped species that had less than
# 25 occurrences after cleaning and thinning!)
dropped_species<- setdiff(mytree$tip.label, data$species)
tree_pruned <- drop.tip(mytree, dropped_species)


# make rows in data match rows in tree
data_1 <- data[match(tree_pruned$tip.label,data$species),]

# Add in nitrogen range-- we didn't calculate this in the dataset
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# calculate absolute median latitude
data_1$abs_med_lat<-abs(data_1$median_lat)

# Does our data meet the assumptions of a GLS?
# and if not, let's transform the variables!

# temp data
hist(data_1$temp_range)

hist(log(data_1$temp_range))

hist(sqrt(data_1$temp_range))

hist(scale(data_1$temp_range, scale=TRUE))


# precip data
hist(data_1$precip_range)
hist(log(data_1$precip_range))
#data_1$log_precip_range<-(log(data_1$precip_range))
shapiro.test(data_1$log_precip_range)


# distribution of nitrogen
hist(data_1$nitro_range)
hist(log(data_1$nitro_range))
min(data_1$nitro_range)
data_1$log_nitro_range<-(log(data_1$nitro_range))
shapiro.test(data_1$log_precip_range)
shapiro.test(data_1$precip_range)


### Looking at predictor variables:
# First, number of human uses
hist(data_1$uses_num_uses)
hist(log(data_1$uses_num_uses))
hist(scale(data_1$uses_num_uses))

# median latitude
hist(sqrt(data_1$abs_med_lat))
hist(data_1$abs_med_lat)

# number of occurrences
hist(data_1$n)
hist(log(data_1$n))
data_1$log_n<-log(data_1$n)

# Let's run a gls model for precip range
data_1$EFN<-as.factor(data_1$EFN)
data_1$Domatia<-as.factor(data_1$Domatia)
data_1$fixer<-as.factor(data_1$fixer)

# First check that the residuals do not, in fact, have equal variance

precip_range <- gls(log(precip_range) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range)

plot(precip_range)

qqnorm(precip_range, abline = c(0,1))

hist(residuals(precip_range))

precip_range_means_EFN<-data.frame(emmeans(precip_range, ~EFN))
precip_range_means_EFN$emmeans_unlog<-exp(precip_range_means_EFN$emmean)
precip_range_means_EFN$clup_unlog<-exp(precip_range_means_EFN$upper.CL)
precip_range_means_EFN$cldown_unlog<-exp(precip_range_means_EFN$lower.CL)

precip_range_means_Domatia<-data.frame(emmeans(precip_range, ~Domatia))
precip_range_means_Domatia$emmeans_unlog<-exp(precip_range_means_Domatia$emmean)
precip_range_means_Domatia$clup_unlog<-exp(precip_range_means_Domatia$upper.CL)
precip_range_means_Domatia$cldown_unlog<-exp(precip_range_means_Domatia$lower.CL)

precip_range_means_fixer<-data.frame(emmeans(precip_range, ~fixer))
precip_range_means_fixer$emmeans_unlog<-exp(precip_range_means_fixer$emmean)
precip_range_means_fixer$clup_unlog<-exp(precip_range_means_fixer$upper.CL)
precip_range_means_fixer$cldown_unlog<-exp(precip_range_means_fixer$lower.CL)

EFN_precip_plot<-ggplot(precip_range_means_EFN, aes(x=EFN, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  theme_classic()+
  xlab("EFN")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())

Domatia_precip_plot<-ggplot(precip_range_means_Domatia, aes(x=Domatia, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  xlab("Domatia")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())


fixer_precip_plot<-ggplot(precip_range_means_fixer, aes(x=fixer, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  xlab("Fixer")+
  ylab("Precipitation breadth \n (mm/year)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())



# pgls for temp range

temp_range <- gls(temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses
                  + annual + n + abs_med_lat,
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

hist(residuals(temp_range))

plot(temp_range)


temp_range_means_EFN<-data.frame(emmeans(temp_range, ~EFN))
temp_range_means_EFN$emmeans_unsq<-(temp_range_means_EFN$emmean)
temp_range_means_EFN$clup_unsq<-(temp_range_means_EFN$upper.CL)
temp_range_means_EFN$cldown_unsq<-(temp_range_means_EFN$lower.CL)

temp_range_means_Domatia<-data.frame(emmeans(temp_range, ~Domatia))
temp_range_means_Domatia$emmeans_unsq<-(temp_range_means_Domatia$emmean)
temp_range_means_Domatia$clup_unsq<-(temp_range_means_Domatia$upper.CL)
temp_range_means_Domatia$cldown_unsq<-(temp_range_means_Domatia$lower.CL)

temp_range_means_fixer<-data.frame(emmeans(temp_range, ~fixer))
temp_range_means_fixer$emmeans_unsq<-(temp_range_means_fixer$emmean)
temp_range_means_fixer$clup_unsq<-(temp_range_means_fixer$upper.CL)
temp_range_means_fixer$cldown_unsq<-(temp_range_means_fixer$lower.CL)

EFN_temp_plot<-ggplot(temp_range_means_EFN, aes(x=EFN, y=emmeans_unsq))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  geom_errorbar(aes(ymin = cldown_unsq, ymax = clup_unsq), width=.1)+
  theme_classic()+
  xlab("EFN")+
  ylab("Average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
 # theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())

Domatia_temp_plot<-ggplot(temp_range_means_Domatia, aes(x=Domatia, y=emmeans_unsq))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unsq, ymax = clup_unsq), width=.1)+
  xlab("Domatia")+
  ylab("Average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())


fixer_temp_plot<-ggplot(temp_range_means_fixer, aes(x=fixer, y=emmeans_unsq))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unsq, ymax = clup_unsq), width=.1)+
  xlab("Fixer")+
  ylab("Average annual temperature")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())



#### pgls for nitro range

nitro_range <- gls(log(nitro_range) ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n + abs_med_lat,

                   data=data_1, 

                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range)

plot(nitro_range)

hist(residuals(nitro_range))

qqnorm(temp_range, abline = c(0,1))

# pulling model estimates and making boxplots!
nitro_range_means_EFN<-data.frame(emmeans(nitro_range, ~EFN))
nitro_range_means_EFN$emmeans_unlog<-exp(nitro_range_means_EFN$emmean)
nitro_range_means_EFN$clup_unlog<-exp(nitro_range_means_EFN$upper.CL)
nitro_range_means_EFN$cldown_unlog<-exp(nitro_range_means_EFN$lower.CL)

nitro_range_means_Domatia<-data.frame(emmeans(nitro_range, ~Domatia))
nitro_range_means_Domatia$emmeans_unlog<-exp(nitro_range_means_Domatia$emmean)
nitro_range_means_Domatia$clup_unlog<-exp(nitro_range_means_Domatia$upper.CL)
nitro_range_means_Domatia$cldown_unlog<-exp(nitro_range_means_Domatia$lower.CL)

nitro_range_means_fixer<-data.frame(emmeans(nitro_range, ~fixer))
nitro_range_means_fixer$emmeans_unlog<-exp(nitro_range_means_fixer$emmean)
nitro_range_means_fixer$clup_unlog<-exp(nitro_range_means_fixer$upper.CL)
nitro_range_means_fixer$cldown_unlog<-exp(nitro_range_means_fixer$lower.CL)

EFN_nitro_plot<-ggplot(nitro_range_means_EFN, aes(x=EFN, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  theme_classic()+
  xlab("EFN")+
  ylab("Mean soil nitrogen content (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())

Domatia_nitro_plot<-ggplot(nitro_range_means_Domatia, aes(x=Domatia, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  xlab("Domatia")+
  ylab("Mean soil nitrogen content (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
 # theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())


fixer_nitro_plot<-ggplot(nitro_range_means_fixer, aes(x=fixer, y=emmeans_unlog))+
  geom_point(size=6)+
  geom_line(group=1)+ 
  theme_classic()+
  geom_errorbar(aes(ymin = cldown_unlog, ymax = clup_unlog), width=.1)+
  xlab("Fixer")+
  ylab("Mean soil nitrogen content (cg/kg)")+
  scale_x_discrete(labels=c("no", "yes"))
  #scale_fill_ghibli_d("LaputaMedium", direction = -1)+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())


cowplot::plot_grid(EFN_precip_plot, Domatia_precip_plot, fixer_precip_plot,
                   EFN_temp_plot, Domatia_temp_plot, fixer_temp_plot,
                   EFN_nitro_plot, Domatia_nitro_plot, fixer_nitro_plot, nrow=3, ncol=3)



### Running PGLS models, but just using mutualism as a factor

precip_range_m <- gls(log(precip_range) ~ mutualism + woody + uses_num_uses + annual + n + abs_med_lat,
                    data=data_1, 
                    correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(precip_range_m)

plot(precip_range_m)

qqnorm(precip_range_m, abline = c(0,1))

hist(residuals(precip_range_m))


temp_range_m <- gls(temp_range ~ mutualism + woody + uses_num_uses
                  + annual + n + abs_med_lat,
                  data=data_1, 
                  correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(temp_range_m)

qqnorm(temp_range_m, abline = c(0,1))

hist(residuals(temp_range_m))

plot(temp_range_m)


nitro_range_m <- gls(log(nitro_range) ~ mutualism + woody + uses_num_uses + annual + n + abs_med_lat,
                   
                   data=data_1, 
                   
                   correlation=corPagel(1, tree_pruned, form=~species), method="ML")

summary(nitro_range_m)

plot(nitro_range_m)

hist(residuals(nitro_range_m))




