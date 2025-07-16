###################################################################
# File: fig4_lactose_gene.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df_bgl_all.csv
#          df_bgl_CH.csv
#
# Output:  fig4.png
#
#
###################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggsignif)
library(ggpubr)

df_bgl_all_reformat<- read.csv("CDRF_metagenome/data/df_bgl_all.csv", header=T) 
df_bgl_CH_reformat<- read.csv("CDRF_metagenome/data/df_bgl_CH.csv", header=T) 

bgl_all_plot <- ggplot(data = df_bgl_all_reformat, aes(x=LNP, y=sum_gene_abd.bgl_sensitive, fill = LNP))+
  geom_boxplot()+ylab("Microbial beta-galactosidase gene relative abundance") + 
  xlab("Host lactase genotype\n(All subjects)") + 
  scale_x_discrete(limits=c("LNP", "LP")) +
  scale_fill_grey() + stat_boxplot(geom="errorbar", width=0.15) +
  geom_signif(comparisons = list(c("LNP", "LP")), map_signif_level = TRUE, annotation = "p-adj = 0.038")+ 
  theme_bw()

bgl_CH_plot <- ggplot(data = df_bgl_CH_reformat, aes(x=LNP, y=sum_gene_abd.bgl_sensitive, fill = LNP))+
  geom_boxplot()+ylab("Microbial beta-galactosidase gene relative abundance") + 
  xlab("Host lactase genotype\n(Caucasians and Hispanics)") + 
  scale_x_discrete(limits=c("LNP", "LP")) +
  scale_fill_grey() + stat_boxplot(geom="errorbar", width=0.15) +
  geom_signif(comparisons = list(c("LNP", "LP")), map_signif_level = TRUE, annotation = "p-adj > 0.05")+ 
  theme_bw()

# range plots for result write-up
ggarrange(bgl_all_plot,bgl_CH_plot, labels = "AUTO", ncol = 2, common.legend = TRUE, legend="right")
ggsave("fig4.png", width = 8, height = 6, units =  "in", dpi = 300)