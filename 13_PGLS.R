# Running PGLS

library(ape)
library(phytools)
library(nlme)
library(tidyverse)
#remotes::install_github("pbreheny/visreg")
library(visreg)
library(MASS)

# Read in PGLS dataframe

data<-read.csv("legume_data_for_PGLS.csv")


# Bring in tree
mytree<-read.tree("phylogeny_buildnodes1_droppedspecies.tre")

# make rows in data match rows in tree
data_1 <- data[match(mytree$tip.label,data$species),]

# Add in nitrogen range
data_1$nitro_range<-data_1$nitro_maxquant-data_1$nitro_minquant

# Removing these four weird species with 0 niche breadth at all- something is going on here
# I fear: 
data_2<-data_1 %>% filter(n>=25)

write.csv(data_2, "pgls_final_data.csv")

# Does our data meet the assumptions of a GLS?

# precip_range is not normally distributed-- is left-skewed
hist(data_2$temp_range)
hist(log(data_2$temp_range))
data_2$log_temp_range<-(log(data_2$temp_range))


# What if we try logging the data?
hist(data_2$precip_range)
hist(log(data_2$precip_range))
data_2$log_precip_range<-(log(data_2$precip_range))


# distribution of nitrogen
hist(data_2$nitro_range)
hist(log(data_2$nitro_range))
min(data_2$nitro_range)

data_2$log_nitro_range<-(log(data_2$nitro_range))


# Let's run a gls model for precip range
# First check that the residuals do not, in fact, have equal variance
lmchecking<-

precip_range <- gls(log_precip_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n,
         data=data_2, 
         correlation=corPagel(1, mytree, form=~species), method="ML")

summary(precip_range)

# plot the residuals

qqnorm(precip_range, abline = c(0,1))

plot(z)


# pgls for temp range

temp_range <- gls(log_temp_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n,
                    data=data_2, 
                    correlation=corPagel(1, mytree, form=~species), method="ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))

plot(temp_range)


# pgls for temp range

nitro_range <- gls(log_nitro_range ~ EFN + Domatia + fixer + woody + uses_num_uses + annual + n,
                  data=data_2, 
                  correlation=corPagel(1, mytree, form=~species), method="ML")

summary(nitro_range)

qqnorm(temp_range, abline = c(0,1))

plot(temp_range)





