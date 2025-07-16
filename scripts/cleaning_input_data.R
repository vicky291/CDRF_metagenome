###################################################################
# File: cleaning_input_data.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  03_ffq_lactose_dtotal_lct_fscfa.csv
#          04_ffq_dairyproduct_lct_fscfa.csv
#          02_asa24_lactose_dtotal_no_powders_lct.csv
#          03_asa24_dairyproduct_lct_fscfa.csv
#          SCFA_ratio_Andrew.csv
#          FL100_ASA24_avgs_cleaned_all_dt.csv
#          FL100_FFQ_cleaned_all_dt.csv
#
# Outputs: df.csv
#          df_CH.csv
#
#
###################################################################

library(tidyr)
library(dplyr)

## Read dietary input file and fscfa variables.
ffq<- read.csv("CDRF_metagenome/data/03_ffq_lactose_dtotal_lct_fscfa.csv", header=T) # 393 subjects
ffq_dairy <- read.csv("CDRF_metagenome/data/04_ffq_dairyproduct_lct_fscfa.csv", header=T) # 393 subjects
asa<- read.csv("CDRF_metagenome/data/02_asa24_lactose_dtotal_no_powders_lct.csv", header=T) # 393 subjects
asa_dairy <- read.csv("CDRF_metagenome/data/03_asa24_dairyproduct_lct_fscfa.csv", header=T) # 393 subjects
f_ratio <- read.csv("CDRF_metagenome/data/SCFA_ratio_Andrew.csv", header = TRUE) # 363 subjects
# fiber intake as covariate
asa_clean<- read.csv("CDRF_metagenome/data/FL100_ASA24_avgs_cleaned_all_dt.csv", header = TRUE) # 350 x 145
ffq_clean <- read.csv("CDRF_metagenome/data/FL100_FFQ_cleaned_all_dt.csv", header = TRUE)

## Edit dietary variable names
# ffq
colnames(ffq)[which(names(ffq) == "d_total")] <- "d_total.ffq"
colnames(ffq)[which(names(ffq) == "lacs")] <- "lacs.ffq"
colnames(ffq)[which(names(ffq) == "dt_kcal")] <- "dt_kcal.ffq"
# ffq_dairy
colnames(ffq_dairy)[which(names(ffq_dairy) == "d_cheese")] <- "d_cheese.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "d_milk")] <- "d_milk.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "d_yogurt")] <- "d_yogurt.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "total_milk")] <- "total_milk.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "total_dairy")] <- "total_dairy.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "fluidmilk_servings")] <- "fluidmilk_servings.ffq"
colnames(ffq_dairy)[which(names(ffq_dairy) == "altmilk_servings")] <- "altmilk_servings.ffq"
ffq_dairy <- ffq_dairy %>% select(SubjectID: bmi_cat, d_cheese.ffq:altmilk_servings.ffq)
# asa
colnames(asa)[which(names(asa) == "Lactose.consumed")] <- "lacs.asa"
colnames(asa)[which(names(asa) == "D_TOTAL")] <- "d_total.asa"
colnames(asa)[which(names(asa) == "KCAL")] <- "kcal.asa"
asa <- asa %>% select(SubjectID: bmi_cat, lacs.asa:kcal.asa)
# asa_dairy
colnames(asa_dairy)[which(names(asa_dairy) == "d_cheese")] <- "d_cheese.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "d_milk")] <- "d_milk.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "d_yogurt")] <- "d_yogurt.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "total_milk")] <- "total_milk.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "total_dairy")] <- "total_dairy.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "fluidmilk_servings")] <- "fluidmilk_servings.asa"
colnames(asa_dairy)[which(names(asa_dairy) == "altmilk_servings")] <- "altmilk_servings.asa"
asa_dairy <- asa_dairy %>% select(SubjectID: bmi_cat, d_cheese.asa:altmilk_servings.asa)
# fiber
colnames(asa_clean)[which(names(asa_clean)== "fibe_total")] <- "fiber_total.asa"
colnames(ffq_clean)[which(names(ffq_clean)== "dt_fibe")] <- "dt_fiber.ffq"
colnames(ffq_clean)[which(names(ffq_clean)== "dt_fiber_insol")] <- "dt_fiber_insol.ffq"
colnames(ffq_clean)[which(names(ffq_clean)== "dt_fiber_sol")] <- "dt_fiber_sol.ffq"
colnames(ffq_clean)[which(names(ffq_clean)== "total_fiber")] <- "fiber_total.ffq"


## Merge dataframes
df<- merge(ffq, select(ffq_dairy, c("SubjectID", "d_cheese.ffq", "d_milk.ffq","d_yogurt.ffq", "total_milk.ffq","total_dairy.ffq" ,"altmilk_servings.ffq" )), by="SubjectID", all=T)
df<- merge(df, select(asa, c("SubjectID", "lacs.asa", "d_total.asa")), by="SubjectID", all=T)
df <- merge(df, select(asa_dairy, c("SubjectID", "d_cheese.asa", "d_milk.asa", "d_yogurt.asa", "altmilk_servings.asa")), by="SubjectID", all=T)
# fiber
df <- merge(df, select(asa_clean, c('subject_id', 'fiber_total.asa')), by.y = "subject_id", by.x = "SubjectID", all = T)
df <- merge(df, select(ffq_clean, c('subject_id', 'dt_fiber.ffq', 'dt_fiber_insol.ffq', 'dt_fiber_sol.ffq', 'fiber_total.ffq')), by.y = "subject_id", by.x = "SubjectID", all = T)
f_ratio <- f_ratio %>% select(subject_id, rel_abund_diff_acetate, rel_abund_diff_propionate: rel_abund_diff_new_butyrate)
df <- merge(df, f_ratio, by.y = "subject_id", by.x = "SubjectID", all = T)
# Total SCFA
df$total_fscfa <- round(df$acetate + df$butyrate + df$propionate + df$isobutyrate, digits = 2)


## Remove African American subjects who are GG
af_gg<- df %>% filter(Ethnicity == "African.American" & LCT == "GG") #8 subjects
'%nin%'<-Negate('%in%')
df_filt<- df %>% filter(SubjectID %nin% af_gg$SubjectID) %>% filter(SubjectID != 6083 & SubjectID != 8015)
df <- df_filt

## Only keep the Caucasian and Hispanic subjects
df_CH <- df %>% filter(Ethnicity == "Caucasian" | Ethnicity == "Hispanic") # 291 subjects

## Add lactose tercile information.
df <- mutate(df, laca.asa.tercile = ntile(df$lacs.asa, 3), laca.ffq.tercile = ntile(df$lacs.ffq, 3))
# Add a column combining lactase.persistance and group information
df <- df %>% mutate(laca.asa.tercile_label = case_when(
  laca.asa.tercile == 1 ~ "low",
  laca.asa.tercile == 2 ~ "med",
  laca.asa.tercile == 3 ~ "high"
), laca.ffq.tercile_label = case_when(
  laca.ffq.tercile == 1 ~ "low",
  laca.ffq.tercile == 2 ~ "med",
  laca.ffq.tercile == 3 ~ "high"
))
df <- mutate(df, LP_lacsASAtercile = paste(df$LNP, df$laca.asa.tercile_label, sep = "-"), 
             LP_lacsFFQtercile = paste(df$LNP, df$laca.ffq.tercile_label, sep = "-"))
df$laca.asa.tercile <- as.factor(df$laca.asa.tercile)
df$laca.ffq.tercile <- as.factor(df$laca.ffq.tercile)

## Caucasian and Hispanic
df_CH <- mutate(df_CH, laca.asa.tercile = ntile(df_CH$lacs.asa, 3), laca.ffq.tercile = ntile(df_CH$lacs.ffq, 3))
# Add a column combining lactase.persistance and group information
df_CH <- df_CH %>% mutate(laca.asa.tercile_label = case_when(
  laca.asa.tercile == 1 ~ "low",
  laca.asa.tercile == 2 ~ "med",
  laca.asa.tercile == 3 ~ "high"
), laca.ffq.tercile_label = case_when(
  laca.ffq.tercile == 1 ~ "low",
  laca.ffq.tercile == 2 ~ "med",
  laca.ffq.tercile == 3 ~ "high"
))
df_CH <- mutate(df_CH, LP_lacsASAtercile = paste(df_CH$LNP, df_CH$laca.asa.tercile_label, sep = "-"), 
                LP_lacsFFQtercile = paste(df_CH$LNP, df_CH$laca.ffq.tercile_label, sep = "-"))
df_CH$laca.asa.tercile <- as.factor(df_CH$laca.asa.tercile)
df_CH$laca.ffq.tercile <- as.factor(df_CH$laca.ffq.tercile)

write.csv(df, "CDRF_metagenome/data/df.csv", row.names = FALSE)
write.csv(df_CH, "CDRF_metagenome/data/df_CH.csv", row.names = FALSE)

