###################################################################
# File: fig6_fecal_SCFA.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df.csv
#
# Output:  fig6.png
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

## LP_lacsASAtercile
# remove "NA" & "-med" in LP_lacsASAtercile
df_LP_lacsASAtercile4group <- df[!grepl("NA", df$LP_lacsASAtercile) & !grepl("-med", df$LP_lacsASAtercile) & complete.cases(df$LP_lacsASAtercile),] #185 subjects left
df_LP_lacsASAtercile4group$LP_lacsASAtercile <- as.factor(df_LP_lacsASAtercile4group$LP_lacsASAtercile)
df_LP_lacsASAtercile4group$LP_lacsFFQtercile <- as.factor(df_LP_lacsASAtercile4group$LP_lacsFFQtercile)

# butyrate
df_LP_lacsASAtercile4group$butyrate_trans <- bestNormalize::boxcox(df_LP_lacsASAtercile4group$butyrate)$x.t
model_lacsASAtercile_buty_ALL <- lm(butyrate_trans ~ LP_lacsASAtercile + sex + bmi_final + age + fiber_total.asa, data = df_LP_lacsASAtercile4group)
shapiro.test(residuals(model_lacsASAtercile_buty_ALL))
anova(model_lacsASAtercile_buty_ALL)

effect_3 <-effect("LP_lacsASAtercile", model_lacsASAtercile_buty_ALL)

effect_3_df <- data.frame(effect_3)

# add y.position parameter
post_hoc_test_3 <- post_hoc_test_3 %>%
  mutate(y.position = c(0,0, 0.8,0, 0,0))

graph_label <- c(LNP.1 = "LNP-low: low lactose intake (< 3.2g/day)",
                 LNP.3 = "LNP-high: high lactose intake (>10.0g/day)", 
                 LP.1 = "LP-low: low lactose intake (< 3.2g/day)", 
                 LP.3 = "LP-high: high lactose intake (>10.0g/day)")

ASAtercile_buty_ALL <-effect_3_df %>%
  ggplot(aes(x=LP_lacsASAtercile, y = fit)) +geom_bar(stat="identity",fill="#619CFF")  + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se,), width=.1,col="black") + 
  xlab("Lactase genotype-recent lactose intake group") + 
  ylab("Box-cox transformed fecal butyrate level")+ 
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  geom_point(aes(x=LP_lacsASAtercile,y=lower),size=1.5,shape=21)+ 
  geom_point(aes(x=LP_lacsASAtercile,y=upper),size=1.5,shape=21) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  + 
  stat_pvalue_manual(post_hoc_test_3 , tip.length = FALSE, hide.ns = "p.adj", label = "p-adj = {p_adj}")


# total_fscfa
df_LP_lacsASAtercile4group$total_fscfa_trans <- bestNormalize::boxcox(df_LP_lacsASAtercile4group$total_fscfa)$x.t
model_lacsASAtercile_total_ALL <- lm(total_fscfa_trans ~ LP_lacsASAtercile + sex + bmi_final + age + fiber_total.asa, data = df_LP_lacsASAtercile4group)
shapiro.test(residuals(model_lacsASAtercile_total_ALL))
anova(model_lacsASAtercile_total_ALL)

effect_4 <-effect("LP_lacsASAtercile", model_lacsASAtercile_total_ALL)
effect_4_df <- data.frame(effect_4)

# add y.position parameter
post_hoc_test_4 <- post_hoc_test_4 %>%
  mutate(y.position = c(0,0,0, 0, 0.6,0))


ASAtercile_total_ALL<- effect_4_df %>% ggplot(aes(x=LP_lacsASAtercile, y = fit)) +
  geom_bar(stat="identity",fill="#619CFF")  + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se,), width=.1,col="black") + 
  xlab("Lactase genotype-recent lactose intake group") + 
  ylab("Box-cox transformed total fecal SCFA level")+ 
  geom_jitter(shape=16, position=position_jitter(0.2))  + 
  geom_point(aes(x=LP_lacsASAtercile,y=lower),size=1.5,shape=21)+ 
  geom_point(aes(x=LP_lacsASAtercile,y=upper),size=1.5,shape=21)  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  + 
  stat_pvalue_manual(post_hoc_test_4 , tip.length = FALSE, hide.ns = "p.adj", label = "p-adj = {p_adj}")  

# range plots for result write-up
fig6 <- ggarrange(ASAtercile_buty_ALL,ASAtercile_total_ALL, labels = "AUTO", ncol = 2)
# annotate the figure by adding a common label
annotate_figure(fig6, bottom = text_grob("LNP-low: low lactose intake (< 3.2g/day).   LNP-high: high lactose intake (>10.0g/day).\nLP-low: low lactose intake (< 3.2g/day).   LP-high: high lactose intake (>10.0g/day).", size = 11, face = "bold"))

ggsave("fig6.png", width = 8, height = 5, units =  "in", dpi = 300)