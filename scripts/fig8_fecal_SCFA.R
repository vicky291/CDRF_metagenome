###################################################################
# File: fig8_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df.csv
#
# Output:  fig8.png
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

df_altermilk <- df
df_altermilk<- df_altermilk %>% mutate (altermilk_asa_status = case_when(
  (df$altmilk_servings.asa == 0) & !is.na(df$altmilk_servings.asa) ~ "NonConsumer",
  (df$altmilk_servings.asa > 0) & !is.na(df$altmilk_servings.asa) ~ "Consumer"
))
df_altermilk$altermilk_asa_status <- as.factor(df_altermilk$altermilk_asa_status)


## acetate
model_asa_acet_ALL <- lm(acetate ~ altermilk_asa_status + sex + bmi_final + age + fiber_total.asa, data = df_altermilk)
plot(model_asa_acet_ALL, add.smooth = FALSE, which = 1)
plot(model_asa_acet_ALL, which = 2)
anova(model_asa_acet_ALL)

# model_asa_acet_ALL
stat.model_asa_acet_ALL <- data.frame(group1 = "NonConsumer", group2 = "Consumer", 
                                      p.adj= 0.0045, p.signif = "**", y.position=33)
plot_asa_acet_ALL <- plot( predict_response(model_asa_acet_ALL, terms = "altermilk_asa_status") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Fecal acetate nmol/mg") + 
  xlab("Recent alternative milk non-consumer vs consumer") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asa_acet_ALL , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("Recent alternative milk\nin all subjects")

## butyrate
model_asa_buty_ALL <- lm(butyrate ~ altermilk_asa_status + sex + bmi_final + age +fiber_total.asa, data = df_altermilk)
plot(model_asa_buty_ALL, add.smooth = FALSE, which = 1)
plot(model_asa_buty_ALL, which = 2)
anova(model_asa_buty_ALL)

# model_asa_buty_ALL
stat.model_asa_buty_ALL <- data.frame(group1 = "NonConsumer", group2 = "Consumer", 
                                      p.adj= 0.0045, p.signif = "**", y.position=12)
plot_asa_buty_ALL <- plot( predict_response(model_asa_buty_ALL, terms = "altermilk_asa_status") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Fecal butyrate nmol/mg") + 
  xlab("Recent alternative milk non-consumer vs consumer") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asa_buty_ALL , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("Recent alternative milk\nin all subjects")


## propionate
model_asa_prop_ALL <- lm(propionate ~ altermilk_asa_status + sex + bmi_final + age +fiber_total.asa, data = df_altermilk)
plot(model_asa_prop_ALL, add.smooth = FALSE, which = 1)
plot(model_asa_prop_ALL, which = 2)
anova(model_asa_prop_ALL)
# model_asa_prop_ALL
stat.model_asa_prop_ALL <- data.frame(group1 = "NonConsumer", group2 = "Consumer", 
                                      p.adj= 0.0045, p.signif = "**", y.position=10.5)
plot_asa_prop_ALL <- plot( predict_response(model_asa_prop_ALL, terms = "altermilk_asa_status") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Fecal propionate nmol/mg") + 
  xlab("Recent alternative milk non-consumer vs consumer") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asa_prop_ALL , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("Recent alternative milk\nin all subjects")


## total_fscfa
model_asa_total_ALL <- lm(total_fscfa ~ altermilk_asa_status + sex + bmi_final + age + fiber_total.asa, data = df_altermilk)
plot(model_asa_total_ALL, add.smooth = FALSE, which = 1)
plot(model_asa_total_ALL, which = 2)
anova(model_asa_total_ALL)

# model_asa_total_ALL
stat.model_asa_total_ALL <- data.frame(group1 = "NonConsumer", group2 = "Consumer", 
                                       p.adj= 0.0043, p.signif = "**", y.position=56)
plot_asa_total_ALL <- plot( predict_response(model_asa_total_ALL, terms = "altermilk_asa_status") ) + 
  theme(  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank()) + 
  ylab("Total fecal SCFA nmol/mg") + 
  xlab("Recent alternative milk non-consumer vs consumer") + 
  labs(title = NULL)  + 
  stat_pvalue_manual(stat.model_asa_total_ALL , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  scale_x_discrete("Recent alternative milk\nin all subjects")

ggarrange(plot_asa_acet_ALL, plot_asa_buty_ALL, plot_asa_prop_ALL, plot_asa_total_ALL, ncol = 4, nrow=1, labels = "AUTO")

ggsave("fig8.png", width = 10, height = 4, units =  "in", dpi = 300)