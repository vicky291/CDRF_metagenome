###################################################################
# File: figS3_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df.csv
#
# Output:  figS3.png
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

## butyrate
df$butyrate_trans<- bestNormalize::arcsinh_x(df$butyrate)$x.t

model_asaFiber_buty_ALL <- lm(butyrate_trans ~ LNP + sex + bmi_final + age + fiber_total.asa, data = df)
shapiro.test(residuals(model_asaFiber_buty_ALL))
anova(model_asaFiber_buty_ALL)
# model_asaFiber_buty_ALL
plot_asaFiber_buty_ALL <- plot( predict_response(model_asaFiber_buty_ALL, terms = "LNP") ) + theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + ylab("Arcsinh transformed (fecal butyrate nmol/mg)") + xlab("In all subjects") + labs(title = NULL) 


## acetate
df$acetate_log <- bestNormalize::log_x(df$acetate, a= 0, b=10)$x.t

model_asaFiber_acet_ALL <- lm(acetate_log ~ LNP + sex + bmi_final + age + fiber_total.asa, data = df)
shapiro.test(residuals(model_asaFiber_acet_ALL))
anova(model_asaFiber_acet_ALL)
# model_asaFiber_acet_ALL
plot_asaFiber_acet_ALL <- plot( predict_response(model_asaFiber_acet_ALL, terms = "LNP") ) + theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + ylab("Log10 (fecal acetate nmol/mg)") + xlab("In all subjects") + labs(title = NULL) 


## total_fscfa
df$total_fscfa_trans <- bestNormalize::boxcox(df$total_fscfa)$x.t
model_asaFiber_total_ALL <- lm(total_fscfa_trans ~ LNP + sex + bmi_final + age + fiber_total.asa, data = df)
shapiro.test(residuals(model_asaFiber_total_ALL))
anova(model_asaFiber_total_ALL)
# model_asaFiber_total_ALL
plot_asaFiber_total_ALL <- plot( predict_response(model_asaFiber_total_ALL, terms = "LNP") ) + theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + ylab("Boxcox (total fecal SCFA nmol/mg)") + xlab("In all subjects") + labs(title = NULL) 

ggarrange(plot_asaFiber_buty_ALL, plot_asaFiber_acet_ALL, plot_asaFiber_total_ALL, ncol = 3, nrow=1, labels = "AUTO")
ggsave("fig_S3.png", width = 8, height = 4, units =  "in", dpi = 300)