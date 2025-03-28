# Pharmacokinetics Analysis Using Non-Compartmental Techniques in R

![Image (1)](https://github.com/user-attachments/assets/350278f3-0ded-4193-b21d-8d475f01e24e)
*<small>AI Generated photo</small> 

This repository contains R scripts and code to perform pharmacokinetic (PK) data analysis on a dataset with one treatment administered at three different dosage regimens using non-compartmental analysis (NCA) techniques and data visualization. The analysis focuses on estimating key pharmacokinetic parameters, including clearance, volume of distribution, and area under the curve (AUC), from individual concentration-time data. Additionally, the analysis includes stratified visualizations by gender and dose to explore trends and variability in the dataset.

## Key Objectives
Perform exploratory data analysis (EDA) of pharmacokinetic datasets.
Calculate essential pharmacokinetic parameters (e.g., CL, Vd, auclast, cmax, tmax, etc.).
Visualize time-concentration profiles stratified by key factors like Dose and Gender.
Add non-compartmental analysis results (e.g., clearance, volume of distribution) to the data.
Summarize and visualize pharmacokinetic data to identify trends and provide insight into dose-response and gender-related differences.

## Dataset : 

The primary dataset used in this analysis is a sample pharmacokinetic dataset (sample_data.csv) that contains time-concentration profiles and subject-specific details. Below is a description of the columns:
| Column Name | Description |
| :-- | :-- |
| ID | Subject ID |
| Time | Time of concentration measurement (e.g., hours) |
| Conc | Measured drug concentration (e.g., ng/mL) |
| Dose | Dose amount administered to the subject (e.g., mg) |
| Gender | Gender of the subject (e.g., Male or Female) |
| Age | Age of the subject (e.g., years) |
| Weight | Subject's weight (e.g., kg) |
| Race | Race of the subject |
| AGECAT | Age category assigned to the subject |


## Requirements
The analysis scripts require the following R packages:
| Column Name | Description |
| :-- | :-- |
|dplyr:| Data manipulation |
|ggplot2:| Data visualization |
|tidyr:| Data wrangling |
|PKNCA:| Non-compartmental analysis |

## To install the required packages:

```{r}
install.packages(c("dplyr", "ggplot2", "tidyr", "PKNCA", "gridExtra"))


## Contact Information:
This repository is maintained by Grzegorz Sterkowski. Please feel free to reach out with any questions, feedback, or issues.

### Author: grzegorzsterkowski@gmail.com 
