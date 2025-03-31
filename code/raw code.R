## load required R packages 

library(ggplot2)
library(dplyr)
#install.packages("PKNCA")
library(PKNCA)
#install.packages('pander')
library(pander)
library(tidyr)

#load the sample_data dataset

data <- read.csv("C:\\Users\\Grzegorz_Sterkowski\\Documents\\Pharma\\my_NCA_repo\\dataset\\sample_data.csv")

head(data)
###
##ID  Time  Amt       Conc Age Weight Gender      Race Dose AGECAT
##1    1  0.00 5000   0.000000  56     94   Male  Hispanic 5000      0
##2    1  0.25    0   8.612809  56     94   Male  Hispanic 5000      0
##3    1  0.50    0  19.436818  56     94   Male  Hispanic 5000      0
##4    1  1.00    0  34.006699  56     94   Male  Hispanic 5000      0
##5    1  2.00    0  30.228800  56     94   Male  Hispanic 5000      0
##6    1  3.00    0  31.299610  56     94   Male  Hispanic 5000      0
##7    1  4.00    0  24.979117  56     94   Male  Hispanic 5000      0

#summarising the data 
summary(data)

#number of subject in the study
n_distinct(data$ID)


######visualisation of the data

ggplot(data, aes(x=Time, y=Conc)) +
  geom_point(size=3)
ggsave("images/0_Simple_Conc_vs_Time_without_logtransformation_plot.png", width= 8, height= 6, dpi= 300)

# plot of data with log transformation
ggplot(data %>% filter(!is.na(Conc), Conc > 0), aes(x=Time, y=Conc)) +
  geom_point(size=3)+
  scale_y_log10() 
  ggsave("images/1_Simple_Conc_vs_Time_plot.png", width= 8, height= 6, dpi= 300)
 
# plot with the lines by Subject ID 

ggplot(data %>% filter(!is.na(Conc), Conc > 0),aes(Time,Conc) )+ geom_point()+
  geom_line(aes(group=ID))+
  scale_y_log10() 
ggsave("images/2_Simple_Conc_vs_Time_plot_by_Subject_ID.png", width = 8, height = 6, dpi = 300)


# create 3 plots split by Dose 
ggplot(data %>% filter(!is.na(Conc), Conc > 0),aes(Time,Conc,colour=as.factor(Dose)))+
  geom_line(aes(group=ID))+
  facet_grid(~Dose )+ 
  scale_y_log10()
  ggsave("images/3_Conc_vs_Time_split_by_Dose_plot.png", width= 8, height= 6, dpi= 300)
  
 
# create split by Dose 
ggplot(data %>% filter(!is.na(Conc), Conc > 0),aes(Time,Conc,colour=as.factor(Dose)))+
  geom_line(aes(group=ID))+
  scale_y_log10()
  ggsave("images/4_Line_plot_dose_color.png", width= 8, height= 6, dpi= 300)  
  

# histograms of quantitative variables 
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) # Define plotting grid layout
hist(data$Conc, main = "Histogram of Concentration", xlab = "Concentration", col = "blue")
hist(data$Age, main = "Histogram of Age", xlab = "Age", col = "green")
hist(data$Weight, main = "Histogram of Weight", xlab = "Weight", col = "orange")
dev.copy(png, filename = "images/5_Histograms_quantitative_variables.png", width = 800, height = 600)
dev.off()
 
# median and  0.25 and 0.75 quantile of log(concentration) vs time  
ggplot(data,aes(Time,Conc))+
   geom_line(aes(group=ID),color="black",alpha=0.2)+
   geom_ribbon(mapping = aes(x = Time, y = Conc,),
               stat = "summary",
               fun.ymin = function(y) {quantile(y,0.25)},
               fun.ymax = function(y) {quantile(y,0.75)},
               fun.y = median, alpha =0.3)+
   geom_line(data= data[!is.na(data$Conc),], mapping = aes(x = Time, y = Conc),
             stat = "summary",
             fun.y = median, alpha =1,size=2 ) +
   scale_y_log10()+ ## log10 on y axis
   theme_bw()
  ggsave("images/6_Median_0.25_and_0.75_quantile_of_log(concentration)_vs_time.png", width=8, height=6, dpi=300)
             
# median and  0.25 and 0.75 quantile of log(concentration) vs time  by doses      
       
ggplot(data,aes(Time,Conc))+
  geom_line(aes(group=ID),color="black",alpha=0.2)+
  geom_ribbon(mapping = aes(x = Time, y = Conc,fill=as.factor(Dose)),
              stat = "summary",
              fun.ymin = function(y) {quantile(y,0.25)},
              fun.ymax = function(y) {quantile(y,0.75)},
              fun.y = median, alpha =0.3)+
  geom_line(data= data[!is.na(data$Conc),], mapping = aes(x = Time, y = Conc,linetype=as.factor(Dose)),
            stat = "summary",
            fun.y = median, alpha =1,size=2 ) +
  scale_y_log10()+ ## log10 on y axis
  theme_bw()   
ggsave("images/7_median_and_0.25_and_0.75_quantile_of_log(concentration)_vs_time_by_doses.png", width=8, height=6, dpi=300)

# median and  0.25-and_0.75_quantile_of_log(concentration)_vs_time_grouped_by_dose_by_gender
##################
ggplot(data,aes(Time,Conc))+
  geom_line(aes(group=ID),color="black",alpha=0.2)+
  geom_ribbon(mapping = aes(x = Time, y = Conc,fill=Gender, group = paste(Gender,Dose)),
              stat = "summary",
              fun.ymin = function(y) {quantile(y,0.25)},
              fun.ymax = function(y) {quantile(y,0.75)},
              fun.y = median, alpha =0.3)+
  geom_line(data= data[!is.na(data$Conc),], mapping = aes(x = Time, y = Conc,
                                                                        colour=Gender,
                                                                        linetype=Gender),
            stat = "summary",
            fun.y = median, alpha =1,size=2 ) +
  scale_y_log10()+
  facet_grid(~Dose )+ # try (~ Race ) here 
  theme_bw()
ggsave("images/8_median_and_0.25_and_0.75_quantile_of_log_concentration_vs_time_by_dose_by_gender.png", width=8, height=6, dpi=300)
)

######################################################
#### noncomparmental analysis considering only Subject 


## obtaining dose data from original dataset
dose_data <- data %>%
  filter(Time == 0) %>%  # Only keep rows where Time == 0 (corresponding to dosing)
  group_by(ID) %>%       # Group by each subject ID
  summarize(
    Subject = first(ID), # Unique subject identifier
    Time = first(Time),  # Dosing time (usually 0)
    Dose = first(Dose)   # Extract the actual dose per subject
  )
print(dose_data)
#creating Dose object
dose_obj <- PKNCAdose(
  dose_data,                         # Raw dosing dataset
  Dose ~ Time | Subject                   # Formula: Dose depends on Time, grouped by ID
)

# obtaining concentration-time data from original dataset
conc_data <- data.frame(
  Subject = data$ID,
  Time = data$Time,
  Concentration = data$Conc
) 

#creating Concentration object
conc_obj <- PKNCAconc(
  conc_data[!is.na(conc_data$Concentration), ],  # Filter rows where concentration values are not NA
  Concentration ~ Time | Subject                 # Formula: Concentration as a function of Time, grouped by Subject
)

#creation of the PK object
PK_object <- PKNCAdata(
  data.conc = conc_obj,  # Concentration-time object
  data.dose = dose_obj          # Dosing object
)

#obtaining overal results for PK_object as AUC last, Cmax, tmax, halflife
results<-pk.nca(PK_object)
pander::pander(summary(results))

# Obtaining subject-level NCA results
param_table <- as.data.frame(results)

head(param_table)

#aggregating the data from original dataset to add 'Gender' and 'Dose' level to the subject-level NCA results 
data_grouped <- data %>%
  group_by(ID) %>%
  summarize(Dose = first(Dose), Gender = first(Gender))

# left join aggregated data to param table by = c('Subject' = 'ID'))
param_table <- param_table %>% 
  left_join(data_grouped %>% select(ID, Dose, Gender), by = c('Subject' = 'ID'))

pander::pander(param_table)

head(param_table)


#pivot PPTESTCD and PPORRES columns 

data_wide <- param_table %>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

#adding the Clerance and Volume parameters to NCA subject level parameter table

data_wide <- data_wide %>%
  mutate(
    CL = Dose / aucinf.obs,            # Clearance
    Vd = CL / lambda.z                 # Volume of distribution
  )

# View the reshaped dataset
head(data_wide)


# Summary statistics for AUCLAST, CMAX, TMAX, and other NCA parameters by Dose and Gender
summary_stats <- data_wide %>%
  group_by(Dose, Gender) %>%
  summarize(
    median_auclast = median(auclast, na.rm = TRUE),           # Median AUCLAST
    q1_auclast = quantile(auclast, probs = 0.25, na.rm = TRUE), # 25th Percentile AUCLAST
    q3_auclast = quantile(auclast, probs = 0.75, na.rm = TRUE), # 75th Percentile AUCLAST
    median_cmax = median(cmax, na.rm = TRUE),                 # Median CMAX
    median_tmax = median(tmax, na.rm = TRUE),                 # Median TMAX
    median_tlast = median(tlast, na.rm = TRUE),                 # Median TLAST
    median_half_life = median(half.life, na.rm = TRUE),         # Median Half-Life
    median_CL = median(CL, na.rm = TRUE),                     # Median Clearance
    median_Vd = median(Vd, na.rm = TRUE),                     # Median Volume of Distribution
    n = n_distinct(Subject)                                   # Number of Distinct Subjects
  )


print(summary_stats)


#Dose Gender median_auclast q1_auclast q3_auclast median_cmax median_tmax median_tlast
#<int> <chr>           <dbl>      <dbl>      <dbl>       <dbl>       <dbl>        <dbl>
#1  5000 Female           581.       476.      1060.        51.6           2           24
#2  5000 Male             576.       415.       774.        58.5           3           24
#3 10000 Female          1145.       979.      2093.       105.            3           24
#4 10000 Male            1152.       828.      1575.       114.            3           24
#5 20000 Female          1718.      1477.      3166.       157.            3           24
#6 20000 Male            1693.      1236.      2355.       173.            3           2


#BOX PLOT of median AUCLast by Gender 
ggplot(data_wide, aes(x=factor(Dose), y=auclast, fill=Gender)) +
  geom_boxplot() +
  labs(title="AUC Last by Dose and Gender", x="Dose", y="AUC Last") +
  theme_minimal()
ggsave("images/9_BOX_PLOT_of_median_AUCLast_by_Gender.png", width=8, height=6, dpi=300)
  
#Violin PLOT of median Cmax by Gender MEC and MTC lines
min_therapeutic_concentration <- 40  # theoretical value for MEC
max_toxic_concentration <- 250  # theoretical value for  MTC

ggplot(data_wide, aes(x = factor(Dose), y = cmax, fill = Gender)) +
  geom_violin(trim=FALSE) +
  labs(
    title = "Cmax by Dose and Gender",
    x = "Dose",
    y = "Cmax"
  ) +
  theme_minimal() +
  geom_hline(yintercept = min_therapeutic_concentration, color = "blue", linetype = "dashed", size = 1.0) +
  geom_hline(yintercept = max_toxic_concentration, color = "red", linetype = "dashed", size = 1.0) +
  annotate("text", x = 1, y = min_therapeutic_concentration + 0.5, 
           label = "Min Therapeutic Concentration", color = "blue", size = 4, hjust = 0) +
  annotate("text", x = 1, y = max_toxic_concentration + 0.5, 
           label = "Max Toxic Concentration", color = "red", size = 4, hjust = 0)
ggsave("images/10_Violin_PLOT_of_median_Cmax_by_Gender_MEC_and_MTC_lines.png", width=8, height=6, dpi=300)

#scatterplot of Cmax vs Tmax by Gender 
ggplot(data_wide, aes(x=tmax, y=cmax, color=Gender)) +
  geom_point() +
  geom_smooth(method="loess",formula = y ~ splines::bs(x, 3), se=TRUE) +# have chosen the loees fitting , and splines 
  labs(title="Cmax vs Tmax by Gender", x="Tmax", y="Cmax") +
  theme_minimal()
ggsave("images/11_scatterplot_of_Cmax_vs_Tmax_by_Gender.png", width=8, height=6, dpi=300)
  
############# NCA analysis considering also Age and Gender
  
# dose data with Age 
dose_data_Age <- data %>%
  filter(Time == 0) %>%  # Only keep rows where Time == 0 (corresponding to dosing)
  group_by(ID) %>%       # Group by each subject ID
  summarize(
    Subject = first(ID), # Unique subject identifier
    Time = first(Time),  # Dosing time (usually 0)
    Dose = first(Dose),
    Gender = first(Gender),
    Age = first(Age)
  )
    
print(dose_data_Age_Gender)


#creating Dose object
dose_data_Age_obj <- PKNCAdose(
  dose_data_Age_Gender,                         
  Dose ~ Time | Age + Subject                
)

# Concentaration data with Age 

Conc_data_Age <- data %>%
  select(ID, Time, Conc, Age, Gender) %>%
  rename(
    Subject = ID,
    Concentration = Conc
  )
head(Conc_data_Age)

#creating Concentration object
Conc_data_Age_obj <- PKNCAconc(
  Conc_data_Age[!is.na(Conc_data_Age_Gender$Concentration), ],  # Filter rows where concentration values are not NA
  Concentration ~ Time | Age  + Subject                # Formula: Concentration as a function of Time, grouped by Subject
)

#creation of the PK object
PK_object_Age <- PKNCAdata(
  data.conc = Conc_data_Age_obj,  # Concentration-time object
  data.dose = dose_data_Age_obj          # Dosing object
)

#obtaining overal results for PK_object as AUC last, Cmax, tmax, halflife
results_Age<-pk.nca(PK_object_Age)
pander::pander(summary(results_Age))

# Obtaining subject-level NCA results
param_table_Age <- as.data.frame(results_Age)

head(param_table_Age)

#it seems that adding Age to the Dose and ConcentraTION AS GROUPING factor did not affect calculation of AUC and other NCa param

# left join aggregated data to param table by = c('Subject' = 'ID')) to add Dose
param_table_Age <- param_table_Age %>% 
  left_join(data_grouped %>% select(ID, Dose), by = c('Subject' = 'ID'))

#pivot PPTESTCD and PPORRES columns 

data_wide_Age <- param_table_Age %>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

#adding the Clerance and Volume parameters to NCA subject level parameter table

data_wide_Age <- data_wide_Age %>%
  mutate(
    CL = Dose / aucinf.obs,            # Clearance
    Vd = CL / lambda.z                 # Volume of distribution
  )


# Add Age Range to Dataset
data_wide_Age <- data_wide_Age %>%
  mutate(
    Age_Range = cut(
      Age,
      breaks = c(0, 40, 50, 60, Inf),  # Define intervals (e.g., 0-40, 40-50, etc.)
      labels = c("<40", "40-50", "51-60", ">60"),  # Labels for ranges
      right = FALSE  # Include left endpoint, exclude right endpoint
    )
  )
# create summary table that has the same column as previous table but with Age grouping
summary_stats_Age <- data_wide_Age %>%
  group_by(Dose, Age_Range) %>%
  summarize(
    median_auclast = median(auclast, na.rm = TRUE),           # Median AUCLAST
    q1_auclast = quantile(auclast, probs = 0.25, na.rm = TRUE), # 25th Percentile AUCLAST
    q3_auclast = quantile(auclast, probs = 0.75, na.rm = TRUE), # 75th Percentile AUCLAST
    median_cmax = median(cmax, na.rm = TRUE),                 # Median CMAX
    median_tmax = median(tmax, na.rm = TRUE),                 # Median TMAX
    median_tlast = median(tlast, na.rm = TRUE),                 # Median TLAST
    median_half_life = median(half.life, na.rm = TRUE),         # Median Half-Life
    median_CL = median(CL, na.rm = TRUE),                     # Median Clearance
    median_Vd = median(Vd, na.rm = TRUE),                     # Median Volume of Distribution
    n = n_distinct(Subject)                                   # Number of Distinct Subjects
  )

print(summary_stats_Age)
# the computation of 

#Median AUCLAST by Dose and Age Range with SE(0.25,0,75)
ggplot(summary_stats_Age, aes(x = factor(Dose), y = median_auclast, fill = Age_Range)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = q1_auclast, ymax = q3_auclast), 
                position = position_dodge(0.9), width = 0.25) + # Error bars
  labs(
    title = "Median AUCLAST by Dose and Age Range with SE(0.25,0,75)",
    x = "Dose (mg)",
    y = "Median AUCLAST",
    fill = "Age Range"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    legend.position = "top"                           # Move legend to the top for better layout
  )
ggsave("images/12_Median_AUCLAST_by_Dose_Age_Range_SE.png", width=8, height=6, dpi=300)


#Median Cmax by Dose and Age Range with SE(0.25,0,75)
ggplot(summary_stats_Age, aes(x = factor(Dose), y = median_cmax, fill = Age_Range)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Median Cmax by Dose and Age Range with SE(0.25,0,75)",
    x = "Dose (mg)",
    y = "Median Cmax",
    fill = "Age Range"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    legend.position = "top"                           # Move legend to the top for better layout
  )
ggsave("images/13_Median_Cmax_Dose_Age_.png", width=8, height=6, dpi=300)



