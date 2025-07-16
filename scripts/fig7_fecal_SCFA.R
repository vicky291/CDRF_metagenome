###################################################################
# File: fig7_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df_CH.csv
#
# Output:  fig7.png
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

# rel_abund_diff_acetate
model_lacsFFQtercile_acteRatio_CH <- lm(rel_abund_diff_acetate ~ LP_lacsFFQtercile + sex + bmi_final + age + fiber_total.ffq, data = df_CH_LP_lacsFFQtercile4group)
shapiro.test(residuals(model_lacsFFQtercile_acteRatio_CH))
anova(model_lacsFFQtercile_acteRatio_CH)

post_hoc_test_1<- tukey_hsd(model_lacsFFQtercile_acteRatio_CH, "LP_lacsFFQtercile")

effect_1 <-effect("LP_lacsFFQtercile", model_lacsFFQtercile_acteRatio_CH)
effect_1_df <- data.frame(effect_1)

# add y.position parameter
post_hoc_test_1 <- post_hoc_test_1 %>%
  mutate(y.position = c(0,0,0.04, 0, 0,0))

FFQtercile_acetRatio_CH <-  effect_1_df %>%
  ggplot(aes(x=LP_lacsFFQtercile, y = fit)) +geom_bar(stat="identity",fill="#619CFF")  + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se,), width=.1,col="black") + 
  xlab("Lactase genotype-lactose intake group") + 
  ylab("The relative to expected ratio of fecal acetate")+ 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_point(aes(x=LP_lacsFFQtercile,y=lower),size=1.5,shape=21)+ 
  geom_point(aes(x=LP_lacsFFQtercile,y=upper),size=1.5,shape=21)  + 
  stat_pvalue_manual(post_hoc_test_1 , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


# rel_abund_diff_propionate
model_lacsFFQtercile_propRatio_CH <- lm(rel_abund_diff_propionate ~ LP_lacsFFQtercile + sex + bmi_final + age + fiber_total.ffq, data = df_CH_LP_lacsFFQtercile4group)
shapiro.test(residuals(model_lacsFFQtercile_propRatio_CH))
anova(model_lacsFFQtercile_propRatio_CH)

post_hoc_test_2<- tukey_hsd(model_lacsFFQtercile_propRatio_CH, "LP_lacsFFQtercile")

effect_2 <-effect("LP_lacsFFQtercile", model_lacsFFQtercile_propRatio_CH)
effect_2_df <- data.frame(effect_2)

# add y.position parameter
post_hoc_test_2 <- post_hoc_test_2 %>%
  mutate(y.position = c(0,0.015,0.02, 0, 0,0))

FFQtercile_propRatio_CH <- effect_2_df %>%
  ggplot(aes(x=LP_lacsFFQtercile, y = fit)) +geom_bar(stat="identity",fill="#619CFF")  + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se,), width=.1,col="black") + 
  xlab("Lactase genotype-lactose intake group") + 
  ylab("The relative to expected ratio of fecal propionate")+ 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  geom_point(aes(x=LP_lacsFFQtercile,y=lower),size=1.5,shape=21)+ 
  geom_point(aes(x=LP_lacsFFQtercile,y=upper),size=1.5,shape=21)  + 
  stat_pvalue_manual(post_hoc_test_2 , tip.length = FALSE, hide.ns = "p.adj", 
                     label = "p-adj = {p.adj}") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

# range plots for result write-up
fig7 <- ggarrange(FFQtercile_acetRatio_CH,FFQtercile_propRatio_CH, labels = "AUTO", ncol = 2)

annotate_figure(fig7, bottom = text_grob("LNP-low: low lactose intake (< 6.1g/day).   LNP-high: high lactose intake (>13.0g/day)\nLP-low: low lactose intake (< 6.1g/day).   LP-high: high lactose intake (>13.0g/day).", size = 11, face = "bold"))

ggsave("fig7.png", width = 8, height = 5, units =  "in", dpi = 300)