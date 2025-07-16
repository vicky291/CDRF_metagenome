# READ ME  
Yirui Tang, July 2025
## Summary  
This repository contains scripts for fecal microbiome taxa and function analyses and fecal short-chain fatty acid analyses presented in the following study:
* **Tang, Y.**, Oliver, A., Alkan, Z., Korf, I., Huang, L., Kable, M., & Lemay, D. (Under review). Association of lactose intake and lactase persistence genotype with microbial taxa and function in healthy multi-ethnic U.S. adults. *Food & Function*. 

## Required Software
* R 4.2.2 (or newer)
* RStudio '2022.12.0.353' (or newer)

## Required Files
**Data Availability**: Dietary intake data and fecal SCFA data that are already publicly available are provided in this GitHub repository. Metagenomic reads for 330 individuals are deposited in the NCBI Sequence Read Archive under two accession numbers: SRP354271 and SRP497208. Requests for non-metagenomic data from the USDA ARS WHNRC Nutritional Phenotyping Study used in this analysis should be made via email to the senior WHNRC author on the publication of interest. Requests will be reviewed quarterly by a committee consisting of the study investigators.

## Description of Scripts
Scripts in each set are intended to be run separately.
1. **Data Cleaning**. This script merges ASA24 Recall Data, Food Frequency Questionnaire Data, as well as Lactase Persistent Genotype Data, modifies variable names, and saves the data frame for differential abundance analyses and ANCOVA analyses.
   * cleaning_input_data.R

3. **Taxa**. These scripts produce the figures for the microbial taxa abundance after Deseq2 analysis.
   * fig2_taxa.R
   * fig3_taxa.R

5. **Gene**. This script includes analyses of microbial beta-galactosidase gene abundance with host lactase genotype and produces Figure 4. 
   * fig4_lactose_gene.R

7. **Short-Chain Fatty Acids**. The section includes scripts for the analyses of fecal SCFA levels and host lactase genotype, with or without lactose/dairy intake data.
   * fig5_fecal_SCFA.R
   * fig6_fecal_SCFA.R
   * fig7_fecal_SCFA.R
   * fig8_fecal_SCFA.R
   * figS3_fecal_SCFA.R
   * figS4_fecal_SCFA.R

## Metagenomic analysis pipeline script
