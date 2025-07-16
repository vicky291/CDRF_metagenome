###################################################################
# File: figS4_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df_CH.csv
#
# Output:  figS4.png
#
#
###################################################################

library(tidyr)
library(dplyr)
library(bestNormalize)
library(ggplot2)
library(ggpubr) #stat_compare_means()
library(ggeffects)

df_CH<- read.csv("CDRF_metagenome/data/df_CH.csv", header=T) 

## rel_abund_diff_acetate
model_asaFiber_acetRatio_CH <- lm(rel_abund_diff_acetate ~ LNP + sex + bmi_final + age + fiber_total.asa, data = df_CH)
shapiro.test(residuals(model_asaFiber_acetRatio_CH))
anova(model_asaFiber_acetRatio_CH)

# model_asaFiber_acetRatio_CH
stat.model_asaFiber_acetRatio_CH <- data.frame(group1 = "LNP", group2 = "LP", 
                                               p.adj= 0.024, p.signif = "**", y.position=0.03)
plot_asaFiber_acetRatio_CH <- plot( predict_response(model_asaFiber_acetRatio_CH, terms = "LNP") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Relative to expected ratio of acetate") + 
  xlab("Lactase genotype in Caucasians and Hispanics") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asaFiber_acetRatio_CH , tip.length = FALSE, 
                     hide.ns = "p.adj", label = "p-adj = {p.adj}") + 
  scale_x_discrete("Lactase genotype in Caucasians and Hispanics")


## rel_abund_diff_propionate
df_CH$rel_abund_diff_propionate_yeo <- bestNormalize::yeojohnson(df_CH$rel_abund_diff_propionate)$x.t

model_asaFiber_propRatio_CH <- lm(rel_abund_diff_propionate_yeo ~ LNP + sex + bmi_final + age + fiber_total.asa, data = df_CH)
shapiro.test(residuals(model_asaFiber_propRatio_CH))
anova(model_asaFiber_propRatio_CH)
# model_asaFiber_propRatio_CH
stat.model_asaFiber_acetRatio_CH <- data.frame(group1 = "LNP", group2 = "LP", 
                                               p.adj= 0.036, p.signif = "**", y.position=0.6)
plot_asaFiber_propRatio_CH <- plot( predict_response(model_asaFiber_propRatio_CH, terms = "LNP") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Yeo-johnson transformed relative to\n expected ratio of propionate") + 
  xlab("Lactase genotype in Caucasians and Hispanics") + 
  labs(title = NULL) + 
  stat_pvalue_manual(stat.model_asaFiber_acetRatio_CH , tip.length = FALSE, 
                     hide.ns = "p.adj", label = "p-adj = {p.adj}") + 
  scale_x_discrete("Lactase genotype in Caucasians and Hispanics")

ggarrange( plot_asaFiber_acetRatio_CH, plot_asaFiber_propRatio_CH, ncol = 2, labels = "AUTO")
ggsave("fig_S4.png", width = 8, height = 4, units =  "in", dpi = 300)