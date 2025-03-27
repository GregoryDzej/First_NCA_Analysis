Pharmacokinetics Analysis Using Non-Compartmental Techniques in R
This repository contains R scripts and code to perform pharmacokinetic (PK) data analysis using non-compartmental analysis (NCA) techniques and data visualization. The analysis is focused on estimating key pharmacokinetic parameters such as clearance, volume of distribution, and AUC from individual concentration-time data, along with stratified visualizations by gender and dose.

Key Objectives
Perform exploratory data analysis (EDA) of pharmacokinetic datasets.
Calculate essential pharmacokinetic parameters (e.g., CL, Vd, auclast, cmax, tmax, etc.).
Visualize time-concentration profiles stratified by key factors like Dose and Gender.
Add non-compartmental analysis results (e.g., clearance, volume of distribution) to the data.
Summarize and visualize pharmacokinetic data to identify trends and provide insight into dose-response and gender-related differences.

Dataset : 

The script reads a dataset from one the consulting companies in area of pharmaceutical industry of time-concentration data from a CSV file (sample_data.csv) containing the following columns:
ID (Subject ID)
Time (Time at which concentration was measured)
Conc (Measured drug concentration)
Dose (Amount of administrated dose)
Gender (Sex of the Subject ID)
Age    (Age of the Subject ID)
Weight (Weight of the Subject ID)
Race  (Race of the Subject ID)
Dose  (Dose value per row)
AGECAT Age category 