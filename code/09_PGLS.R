# Running PGLS

library(ape)
library(phytools)
library(nlme)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggeffects)


# Read in summary dataframe
data <- read_csv("data/pgls_species_data_poly_dropped.csv")

# Bring in tree
mytree <- read.tree("phylogeny/phylogeny_polytomy_removed.tre")


# make rows in data match rows in tree
data_1 <- data[match(mytree$tip.label,data$species),]

# calculate absolute median latitude
data_1$abs_med_lat <- abs(data_1$median_lat)

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
data_1$EFN <- as.factor(data_1$EFN)
data_1$Domatia <- as.factor(data_1$Domatia)
data_1$fixer <- as.factor(data_1$fixer)


# PGLS for precip range ----

precip_range <- gls(log(precip_range) ~ EFN*abs_med_lat + fixer*abs_med_lat + 
                      woody + uses_num_uses + annual,
                    data = data_1, 
                    correlation = corPagel(1, mytree, form=~species), method = "ML")

summary(precip_range)

plot(precip_range)
qqnorm(precip_range, abline = c(0,1))
hist(residuals(precip_range))


# Save as RDS file
write_rds(precip_range, "model_fits/precip_niche_breadth.rds")
# Read in RDS file (if coming back to code)
precip_range <- read_rds("model_fits/precip_niche_breadth.rds")

# Save model output
precip_df <- data.frame(coef(summary(precip_range))) %>% format(scientific = F)
precip_df$p.value <- as.numeric(precip_df$p.value) %>% round(4)
write.csv(precip_df, "tables/precip_breadth_output_table.csv")


## Pull predicted means for EFN and fixer ----

EFN_precip_means <- ggpredict(precip_range, terms = c("abs_med_lat [all]", "EFN [all]"), type = "fixed")
plot(EFN_precip_means)

fixer_precip_means <- ggpredict(precip_range, terms = c("abs_med_lat [all]", "fixer [all]"), type = "fixed")
plot(fixer_precip_means)


## Make plots for EFN and rhizobia separately ----

p1 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = precip_range, shape = EFN, colour = EFN), alpha = 0.2) +
  theme_cowplot() +
  scale_y_log10() +
  scale_shape_manual(values = c(21, 19), guide = "none") +
  scale_colour_manual(values = c("#0E84B4FF", "#B50A2AFF"), labels = c("no", "yes"), name = "EFN") +
  ylab("Annual precip.\nrange (mm)") +
  xlab("Absolute median latitude") +
  theme(axis.title.x = element_blank()) +
  geom_line(data = EFN_precip_means %>% filter(!(group == "1" & x > 55)), aes(x = x, y = predicted, colour = group), linewidth = 1.4) +
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  annotate("text", label="EFN: **\n  Int.: NS", x=50, y=2000, lineheight = .75, hjust=0); p1

p2 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = precip_range, color = fixer, shape = fixer), alpha = 0.05) +
  theme_cowplot() +
  scale_y_log10() +
  scale_shape_manual(values = c(21, 19), guide = "none") +
  scale_colour_manual(values = c("#0E84B4FF", "#26432FFF"), labels = c("no", "yes")) +
  ylab("Annual precip.\nrange (mm)") +
  xlab("Absolute median latitude") +
  labs(colour = "Rhizobia") +
  theme(axis.title.x = element_blank()) +
  geom_line(data = fixer_precip_means %>% filter(!(group=="0" & x > 45)), aes(x = x, y = predicted, colour = group), linewidth = 1.4) +
  scale_fill_manual(values = c("#0E84B4FF", "#26432FFF")) +
  annotate("text", label = "Rhizobia: NS\n         Int.: **", x = 42, y = 2000, lineheight = 0.75, hjust = 0); p2


# PGLS for temp range ----

temp_range <- gls(temp_range ~ EFN*abs_med_lat + fixer*abs_med_lat
                  + woody + uses_num_uses + annual,
                  data = data_1, 
                  correlation = corPagel(1, mytree, form = ~species), method = "ML")

summary(temp_range)

qqnorm(temp_range, abline = c(0,1))
hist(residuals(temp_range))
plot(temp_range)


# Save as RDS file

write_rds(temp_range, "model_fits/temp_niche_breadth.rds")

# Read in RDS file (if coming back to code)
temp_range <- read_rds("model_fits/temp_niche_breadth.rds")

# save model output!
temp_df <- data.frame(coef(summary(temp_range))) %>% format(scientific = F)
temp_df$p.value <- as.numeric(temp_df$p.value) %>% round(4)
write.csv(temp_df, "temp_niche_breadth.csv")


## Pull predicted means for EFN and fixer ----

EFN_temp_means <- ggpredict(temp_range, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_temp_means)


fixer_temp_means <- ggpredict(temp_range, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_temp_means)


## Make plots for EFN and rhizobia separately ----

p3 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = temp_range, colour = EFN, shape = EFN), alpha = 0.2) +
  theme_cowplot() +
  scale_shape_manual(values = c(21, 19), guide="none") +
  scale_colour_manual(values = c("#0E84B4FF", "#B50A2AFF"), labels = c("no", "yes")) +
  ylab("Mean annual\ntemp. range (\u00B0C)") +
  xlab("Absolute median latitude") +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(vjust = 5)) +
  geom_line(data = EFN_temp_means %>% filter(!(group == "1" & x > 55)), aes(x = x, y = predicted, colour = group), linewidth = 1.2) +
  scale_fill_manual(values = c("#0E84B4FF", "#B50A2AFF")) +
  annotate("text", label = "EFN: *\n  Int.: NS", x = 50, y = 20, lineheight = 0.75, hjust = 0); p3



p4 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = temp_range, color = fixer, shape = fixer), alpha = 0.05) +
  theme_cowplot() +
  scale_colour_manual(values = c("#0E84B4FF", "#26432FFF"), labels = c("no", "yes")) +
  scale_shape_manual(values = c(21, 19), guide = "none") +
  ylab("Mean annual temp.\nrange (\u00B0C)") +
  xlab("Absolute median latitude") +
  labs(colour = "Rhizobia") +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(vjust = 5)) +
  geom_line(data = fixer_temp_means %>% filter(!(group == "0" & x > 45)), aes(x = x, y = predicted, colour = group), linewidth = 1.4) +
  scale_fill_manual(values = c("#0E84B4FF", "#26432FFF"))+
  annotate("text", label = "Rhizobia: ***\n         Int.: ***", x = 44, y = 20, lineheight = 0.75, hjust = 0); p4


# PGLS for nitro range ----

nitro_range <- gls(log(nitro_range) ~ EFN*abs_med_lat + fixer*abs_med_lat +
                   woody + uses_num_uses + annual,
                   data = data_1, 
                   correlation = corPagel(1, mytree, form =. ~species), method = "ML")

summary(nitro_range)

plot(nitro_range)
hist(residuals(nitro_range))
qqnorm(temp_range, abline = c(0,1))

# Save as RDS file
write_rds(nitro_range, "model_fits/nitro_niche_breadth.rds")

# Read in RDS file (if coming back to code)
nitro_range <- read_rds("model_fits/nitro_niche_breadth.rds")

# save model output
nitro_df <- data.frame(coef(summary(nitro_range))) %>% format(scientific = F)
nitro_df$p.value <- as.numeric(nitro_df$p.value) %>% round(4)
write.csv(nitro_df, "tables/nitro_breadth_output_table.csv")


## Pull predicted means for EFN and fixer ----

EFN_nitro_means <- ggpredict(nitro_range, terms = c("abs_med_lat [all]", "EFN [all]"), type = "fixed")
plot(EFN_nitro_means)

fixer_nitro_means <- ggpredict(nitro_range, terms = c("abs_med_lat [all]", "fixer [all]"), type = "fixed")
plot(fixer_nitro_means)


# Make plots of nitroegen niche breadth

p5 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = nitro_range, colour = EFN, shape = EFN), alpha = 0.2) +
  theme_cowplot() +
  scale_y_log10() +
  scale_shape_manual(values = c(21, 19), guide = "none") +
  scale_colour_manual(values = c("#0E84B4FF", "#B50A2AFF"), labels = c("no", "yes")) +
  ylab("Soil nitrogen\nrange (cg/kg)") +
  xlab("Absolute median latitude") +
  theme(axis.title.x=element_blank()) +
  geom_line(data = EFN_nitro_means %>% filter(!(group == "1" & x > 55)), aes(x = x, y = predicted, colour = group), linewidth = 1.4) +
  scale_fill_manual(values = c("#0E84B4FF", "#B50A2AFF")) +
  annotate("text", label = "EFN: **\n  Int.: NS", x = 50,  y = 800, lineheight = 0.75, hjust = 0); p5


p6 <- ggplot() +
  geom_point(data = data_1, aes(x = abs_med_lat, y = nitro_range, color = fixer, shape = fixer),alpha = 0.05) +
  theme_cowplot() +
  scale_y_log10() +
  scale_shape_manual(values = c(21, 19), guide = "none") +
  scale_colour_manual(values = c("#0E84B4FF", "#26432FFF"), labels = c("no", "yes")) +
  ylab("Soil nitrogen\nrange (cg/kg)") +
  xlab("Absolute median latitude") +
  labs(colour="Rhizobia") +
  theme(axis.title.x = element_blank()) +
  geom_line(data = fixer_nitro_means %>% filter(!(group == "0" & x > 45)), aes(x = x, y = predicted, colour = group), linewidth = 1.4) +
  scale_fill_manual(values = c("#0E84B4FF", "#26432FFF")) +
  annotate("text", label = "Rhizobia: NS\n         Int.: *", x = 42, y = 800, lineheight = 0.75, hjust = 0); p6


leg_fixer <- get_legend(p2)
efn_fixer <- get_legend(p1)
comp_leg <- plot_grid(leg_fixer, efn_fixer, ncol=1, nrow=2); comp_leg

p <- cowplot::plot_grid(p1 + theme(legend.position = "none"), p2 + theme(legend.position = "none", axis.title.y=element_blank()), 
                        comp_leg,
                        p3 + theme(legend.position = "none"), p4 + theme(legend.position="none", axis.title.y=element_blank()), NA,
                        p5 + theme(legend.position="none"), p6 + theme(legend.position="none", axis.title.y=element_blank()), NA,
                        ncol = 3, nrow = 3, labels = c("A", "D", "", "B", "E", "", "C", "F", ""), axis = "l", align = "v", 
                        rel_widths = c(1, 1, 0.5),
                        label_x = c(0, 0, 0, 0, -0.035, 0, 0, 0, 0)); p

p <- add_sub(p, "Absolute median latitude", hjust = 0.5, size = 14, x = 0.44)

save_plot("figures/niche_breadth.jpg", p, base_height = 10, base_width = 10)
save_plot("figures/niche_breadth.pdf", p, base_height = 10, base_width = 10)


# PGLS with biome as response variable ----

biome_number <- gls(num_biome ~ EFN*abs_med_lat + fixer*abs_med_lat
                  + woody + uses_num_uses
                  + annual,
                  data = data_1, 
                  correlation=corPagel(1, mytree, form=~species), method="ML")

summary(biome_number)

plot(biome_number)
hist(residuals(biome_number))
qqnorm(biome_number, abline = c(0,1))


# save model output
biome_number_df<-data.frame(coef(summary(biome_number))) %>% format(scientific=F)
biome_number_df$p.value<-as.numeric(biome_number_df$p.value) %>% round(4)
write.csv(biome_number_df, "tables/biome_number_output_table.csv")


## Pull predicted means for EFN and fixer ----

EFN_biome_means<-ggpredict(biome_number, terms=c("abs_med_lat [all]", "EFN [all]"), type="fixed")
plot(EFN_biome_means)

fixer_biome_means<-ggpredict(biome_number, terms=c("abs_med_lat [all]", "fixer [all]"), type="fixed")
plot(fixer_biome_means)

efn_biome_plot <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=num_biome, colour=EFN, shape=EFN), alpha=0.2)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#B50A2AFF"), labels=c("no", "yes"))+
  ylab("Biome count")+
  xlab("Absolute median latitude")+
  theme(axis.title.x=element_blank())+
  geom_line(data=EFN_biome_means %>% filter(!(group=="1" & x>55)), aes(x=x, y=predicted, colour=group), linewidth=1.4)+
  scale_fill_manual(values=c("#0E84B4FF", "#B50A2AFF"))+
  annotate("text", label="EFN: ***\n  Int.:  NS", x=55, y=12, lineheight = .75, hjust=0); efn_biome_plot


fixer_biome_plot <- ggplot()+
  geom_point(data=data_1, aes(x=abs_med_lat, y=num_biome, color=fixer, shape=fixer),alpha=0.05)+
  theme_cowplot()+
  scale_shape_manual(values = c(21,19), guide="none")+
  scale_colour_manual(values=c("#0E84B4FF", "#26432FFF"), labels=c("no", "yes"))+
  ylab("Biome count")+
  xlab("Absolute median latitude")+
  labs(colour="Rhizobia")+
  geom_line(data=fixer_biome_means %>% filter(!(group=="0" & x>45)), aes(x=x, y=predicted, colour=group), linewidth=1.4)+
  scale_fill_manual(values=c("#0E84B4FF", "#26432FFF"))+
  annotate("text", label="Rhizobia: **\n         Int.: NS", x=47, y=12, lineheight = .75, hjust=0); fixer_biome_plot

biome_together = cowplot::plot_grid(efn_biome_plot, fixer_biome_plot, ncol = 1, align = "v", axis = "lr"); biome_together
ggsave("figures/biome_number.jpg", height = 8, width = 5)
ggsave("figures/biome_number.pdf", height = 8, width = 5)


# Plots for presentations ---- 

plot1<-cowplot::plot_grid(p1+ theme(legend.position="none"), p3+ theme(legend.position="none"), p5+ theme(legend.position="none"), efn_fixer,
                          ncol=4, nrow=1)
plot1<-add_sub(plot1, "absolute median latitude", hjust = 0.5, size=12)
plot(plot1)

# save_plot("figures/niche_breadth_efn_fig.jpeg", plot1, base_height=5, base_width=12)


plot2<-cowplot::plot_grid(p2+ theme(legend.position="none"), p4+ theme(legend.position="none"),
                          p6+ theme(legend.position="none"), leg_fixer, ncol=4, nrow=1)

plot2<-add_sub(plot2, "absolute median latitude", hjust = 1, size=12)

# save_plot("figures/niche_breadth_fixer_fig.jpeg", plot2, base_height=5, base_width=13)
