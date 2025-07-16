###################################################################
# File: figS5_yogurt.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df.csv
#          df_CH.csv
#.         merged_metaphlan_v4-0-6.txt
#
# Output:  figS5.png
#
#
###################################################################

library(tidyr)
library(dplyr)
library(Maaslin2)

df<- read.csv("CDRF_metagenome/data/df.csv", header=T) 
df_CH<- read.csv("CDRF_metagenome/data/df_CH.csv", header=T) 
df_input_taxa <- data.frame(data.table::fread(file = 'CDRF_metagenome/data/merged_metaphlan_v4-0-6.txt', header = TRUE, sep = "\t"), row.names = 1)

names(df_input_taxa) <-substring(names(df_input_gene), 1, 5)

# Extract yogurt-associated microbes relative abundance from `df_input_taxa` (yogurt-associated microbes refer to the two microbes that must be present in yogurt, _Lactobacillus delbrueckii subsp. Bulgaricus (L. bulgaricus)_ and _Streptococcus thermophilus (S. thermophilus)_):
df_input_taxa_yogurt <- df_input_taxa[grepl("thermophilus", rownames(df_input_taxa)) | grepl("delbrueckii", rownames(df_input_taxa)),]
df_input_taxa_yogurt <- df_input_taxa_yogurt[c(1,3),]
# Modify yogurt-associated microbes taxonomic names to more readable names
rownames(df_input_taxa_yogurt)<- c("s__Streptococcus_thermophilus", "s__Lactobacillus_delbrueckii")


df$sex<- as.factor(df$sex)
df$Ethnicity<-as.factor(df$Ethnicity)
df$LCT<-as.factor(df$LCT)
df$LNP<-as.factor(df$LNP)
df$age_cat <- as.factor(df$age_cat)
df$bmi_cat <- as.factor(df$bmi_cat)

# Reformat metadata file for MaAsLin2:
# make SubjectID column rownames
df_reformat <- data.frame(df[, -1], row.names = df[,1])
df_CH_reformat <- data.frame(df_CH[, -1], row.names = df_CH[,1])



## All subjects
S_thermophilus_yogurtASA <- Maaslin2(input_data = df_input_taxa_yogurt, 
                                     input_metadata = df_reformat, 
                                     min_prevalence = 0,
                                     max_significance = 0.05,
                                     normalization  = "NONE",
                                     output         = "no_interactions/S_thermophilus_yogurtASA_all", 
                                     fixed_effects  = c("d_yogurt.asa", "sex", "age", "bmi_final", "fiber_total.asa"))
# random_effects = c("batch_var")

S_thermophilus_yogurtFFQ <- Maaslin2(input_data = df_input_taxa_yogurt, 
                                     input_metadata = df_reformat, 
                                     min_prevalence = 0,
                                     max_significance = 0.05,
                                     normalization  = "NONE",
                                     output         = "no_interactions/S_thermophilus_yogurtFFQ_all", 
                                     fixed_effects  = c("d_yogurt.ffq", "sex", "age", "bmi_final", "dt_fiber.ffq"))
## CH subjects
S_thermophilus_yogurtASA_CH <- Maaslin2(input_data = df_input_taxa_yogurt, 
                                        input_metadata = df_CH_reformat, 
                                        min_prevalence = 0,
                                        max_significance = 0.05,
                                        normalization  = "NONE",
                                        output         = "no_interactions/S_thermophilus_yogurtASA_CH", 
                                        fixed_effects  = c("d_yogurt.asa",  "sex", "age", "bmi_final", "fiber_total.asa"))

S_thermophilus_yogurtFFQ_CH <- Maaslin2(input_data = df_input_taxa_yogurt, 
                                        input_metadata = df_CH_reformat, 
                                        min_prevalence = 0,
                                        max_significance = 0.05,
                                        normalization  = "NONE",
                                        output         = "no_interactions/S_thermophilus_yogurtFFQ_CH", 
                                        fixed_effects  = c("d_yogurt.ffq", "sex", "age", "bmi_final", "dt_fiber.ffq"))
