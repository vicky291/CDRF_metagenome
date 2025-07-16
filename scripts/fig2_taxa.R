###################################################################
# File: fig2_taxa.R
#
# Purpose: 
#
# Author: Yirui Tang
# Date: 20250716
#
# Inputs:  df.csv
#          df_CH.csv
#
# Output:  fig2.png
#
#
###################################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggeffects)

df_input_taxa <- data.frame(data.table::fread(file = 'CDRF_metagenome/data/merged_metaphlan_v4-0-6.txt', header = TRUE, sep = "\t"), row.names = 1) # metaphlan4 relative abundance;  6627*330

### prepare input data for ggplot
df_input_taxa$Taxon <- rownames(df_input_taxa)
rownames(df_input_taxa) <- c(1: length(rownames(df_input_taxa)))
rownames(df_input_taxa) <- paste("Otu", rownames(df_input_taxa), sep = "")

df_input_taxa_expanded <- df_input_taxa %>% 
  separate(col=Taxon, into  = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Strain"), sep = "\\|", fill = "right", extra = "merge")

### top positive log2FoldChange values
df_3 <-df_input_taxa_expanded %>% filter(grepl('g__Veillonella|g__Lactobacillus|g__Lacticaseibacillus|g__Lactococcus', Genus) & is.na(Species) & !grepl('_unclassified', Genus))

df_3_long <-df_3 %>% pivot_longer(cols = X5001:X9067,
                                  names_to = "SubjectID",
                                  values_to = "rel_ab")
df_3_long_merged<- merge(x = df_3_long, y= df_for_ggplot[, c("SubjectID", "LNP", "LP_lacsASAtercile")], by= "SubjectID", all.y = TRUE)

## remove "NA" & "-med" in LP_lacsASAtercile
df_3_long_merged_LP_lacsASAtercilenoNA <- df_3_long_merged[!grepl("NA", df_3_long_merged$LP_lacsASAtercile) & !grepl("-med", df_3_long_merged$LP_lacsASAtercile) & complete.cases(df_3_long_merged$LP_lacsASAtercile),] %>% drop_na(Genus) # 1169 rows

posG_ASA_ALL <- ggplot(df_3_long_merged_LP_lacsASAtercilenoNA[which(df_3_long_merged_LP_lacsASAtercilenoNA$rel_ab<0.4),], aes(x=LP_lacsASAtercile, y=rel_ab, fill=LP_lacsASAtercile)) + 
  geom_boxplot()+ ylab("Relative abundance %") + 
  facet_wrap(~Genus, scales="free_y", ncol = 4)+ 
  scale_fill_discrete(name = NULL, labels = c("LNP-high: high lactose intake (>10.0g/day)", 
                                              "LNP-low: low lactose intake (< 3.2g/day)", 
                                              "LP-high: high lactose intake (>10.0g/day)", 
                                              "LP-low: low lactose intake (< 3.2g/day)"))+ 
  guides(fill=guide_legend(nrow=2))  +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.title.x = element_blank()) + 
  theme(strip.text.x = element_text(size = 4.5)) 

### prepare input data for ggplot
### top negative log2FoldChange values
df_3_1 <-df_input_taxa_expanded %>% filter(grepl('g__Erysipelatoclostridium|g__Holdemanella|g__Anaerotignum', Genus) & is.na(Species) & !grepl('_unclassified', Genus))

df_3_long_1 <-df_3_1 %>% pivot_longer(cols = X5001:X9067,
                                      names_to = "SubjectID",
                                      values_to = "rel_ab")
df_3_long_merged_1<- merge(x = df_3_long_1, y= df_for_ggplot[, c("SubjectID", "LNP", "LP_lacsASAtercile")], by= "SubjectID", all.y = TRUE)

## remove "NA" & "-med" in LP_lacsASAtercile
df_3_long_merged_LP_lacsASAtercilenoNA_1 <- df_3_long_merged_1[!grepl("NA", df_3_long_merged_1$LP_lacsASAtercile) & !grepl("-med", df_3_long_merged_1$LP_lacsASAtercile) & complete.cases(df_3_long_merged_1$LP_lacsASAtercile),] %>% drop_na(Genus) # 1169 rows

negG_ASA_ALL <- ggplot(df_3_long_merged_LP_lacsASAtercilenoNA_1, 
                       aes(x=LP_lacsASAtercile, y=rel_ab, fill=LP_lacsASAtercile)) + 
  geom_boxplot()  + ylab("Relative abundance %") + 
  facet_wrap(~Genus, scales="free_y")+ 
  scale_fill_discrete(name = NULL, labels = c("LNP-high: high lactose intake (>10.0g/day)", 
                                              "LNP-low: low lactose intake (< 3.2g/day)", 
                                              "LP-high: high lactose intake (>10.0g/day)", 
                                              "LP-low: low lactose intake (< 3.2g/day)")) + 
  guides(fill=guide_legend(nrow=2))  +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.title.x = element_blank()) + 
  theme(strip.text.x = element_text(size = 5.5)) 


ggarrange(posG_ASA_ALL, negG_ASA_ALL, labels = "AUTO", ncol = 2, common.legend = TRUE, legend="bottom") 
ggsave("fig2.png", width = 8, height = 4, units =  "in", dpi = 300)