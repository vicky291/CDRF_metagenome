###################################################################
# File: fig5_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250714
#
# Inputs:  df.csv
#          df_CH.csv
#
# Output:  fig5.png
#
#
###################################################################

library(tidyr)
library(dplyr)
library(bestNormalize)
library(ggplot2)
library(ggpubr) #stat_compare_means()
library(ggeffects)

df<- read.csv("CDRF_metagenome/data/df.csv", header=T) 
df_CH<- read.csv("CDRF_metagenome/data/df_CH.csv", header=T) 

# model testing
## model_asaFiber_prop_ALL
df$propionate_log <- bestNormalize::log_x(df$propionate, a=0, b=10)$x.t
model_asaFiber_prop_ALL <- lm(propionate_log ~ LNP + sex + bmi_final + age + 
                                fiber_total.asa, data = df)
shapiro.test(residuals(model_asaFiber_prop_ALL))
anova(model_asaFiber_prop_ALL)

## model_asaFiber_buty_CH
df_CH$butyrate_trans<- bestNormalize::arcsinh_x(df_CH$butyrate)$x.t
model_asaFiber_buty_CH <- lm(butyrate_trans ~ LNP + sex + bmi_final + age + 
                               fiber_total.asa, data = df_CH)
shapiro.test(residuals(model_asaFiber_buty_CH))
anova(model_asaFiber_buty_CH)

## model_asaFiber_prop_CH
df_CH$propionate_log <- bestNormalize::log_x(df_CH$propionate, a=0, b=10)$x.t
model_asaFiber_prop_CH <- lm(propionate_log ~ LNP + sex + bmi_final + age + 
                               fiber_total.asa, data = df_CH)
shapiro.test(residuals(model_asaFiber_prop_CH))
anova(model_asaFiber_prop_CH)

## model_asaFiber_total_CH
df_CH$total_fscfa_trans <- bestNormalize::boxcox(df_CH$total_fscfa)$x.t
model_asaFiber_total_CH <- lm(total_fscfa_trans ~ LNP + sex + bmi_final + age + 
                                fiber_total.asa, data = df_CH)
shapiro.test(residuals(model_asaFiber_total_CH))
anova(model_asaFiber_total_CH)

# Multiple testing correction
procedures = c("BH")
rawp <- c(0.0298335, 0.4549, 0.00388)
adjusted <- mt.rawp2adjp(rawp, proc = procedures) 
adjusted$adjp[order(adjusted$index), ]


## visualization
# model_asaFiber_prop_ALL
stat.model_asaFiber_prop_ALL <- data.frame(group1 = "LNP", group2 = "LP", 
                                           p.adj= 0.034, p.signif = "**", y.position=0.5)
plot_asaFiber_prop_ALL <- plot( predict_response(model_asaFiber_prop_ALL, terms = "LNP") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Log10 (fecal propionate nmol/mg)") + 
  xlab("Lactase genotype in all subjects") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asaFiber_prop_ALL , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("In all subjects")

# model_asaFiber_buty_CH
stat.model_asaFiber_buty_CH <- data.frame(group1 = "LNP", group2 = "LP", 
                                          p.adj= 0.039, p.signif = "**", y.position=0.8)
plot_asaFiber_buty_CH <- plot( predict_response(model_asaFiber_buty_CH, terms = "LNP") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Arcsinh transformed (fecal butyrate nmol/mg)") + 
  xlab("Lactase genotype in Caucasians and Hispanics") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asaFiber_buty_CH , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("In Caucasians and Hispanics")

# model_asaFiber_prop_CH
stat.model_asaFiber_prop_CH <- data.frame(group1 = "LNP", group2 = "LP", 
                                          p.adj= 0.023, p.signif = "**", y.position=0.8)
plot_asaFiber_prop_CH <- plot( predict_response(model_asaFiber_prop_CH, terms = "LNP") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Log10 (fecal propionate nmol/mg)") + 
  xlab("Lactase genotype in Caucasians and Hispanics") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asaFiber_prop_CH , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("In Caucasians and Hispanics")

# model_asaFiber_total_CH
# use ggeffect pacakge
stat.model_asaFiber_total_CH <- data.frame(group1 = "LNP", group2 = "LP", 
                                           p.adj= 0.039, p.signif = "**", y.position=0.7)
plot_asaFiber_total_CH <- plot( predict_response(model_asaFiber_total_CH, terms = "LNP") ) +
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Box-cox transformed (total fecal SCFA nmol/mg)") + 
  xlab("Lactase genotype in Caucasians and Hispanics") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asaFiber_total_CH , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("In Caucasians and Hispanics")


ggarrange(plot_asaFiber_prop_ALL, plot_asaFiber_buty_CH, plot_asaFiber_prop_CH, plot_asaFiber_total_CH, ncol = 4, nrow=1, labels = "AUTO")
ggsave("fig5.png", width = 10, height = 4, units =  "in", dpi = 300)