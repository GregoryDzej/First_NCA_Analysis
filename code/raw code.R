## load required R packages 

library(ggplot2)
library(dplyr)
#install.packages("PKNCA")
library(PKNCA)
#install.packages('pander')
library(pander)
library(tidyr)
library(nlme)
library(mrgsolve)
library(lattice)

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

######visualisation of the concentration of 1 to 20  subject 

subset_data20 <- subset(data, ID %in% unique(data$ID)[1:20])

print(
  xyplot(Conc ~ Time | ID ,
         data = subset_data20,
         main = 'Subjects 1 - 20 Concentration vs. Time',
         xlab = 'Time',
         ylab = "'Concentration (ng / mL)"
  )
)

######visualisation of the concentration of 21 to 40  subject 

subset_data40 <- subset(data, ID %in% unique(data$ID)[21:40])

print(
  xyplot(Conc ~ Time | ID ,
         data = subset_data40,
         main = 'Subjects 21 - 40 Concentration vs. Time',
         xlab = 'Time',
         ylab = "'Concentration (ng / mL)"
  )
)


######visualisation of the subjects intersecting 

ggplot(data, aes(x=Time, y=Conc)) +
  geom_point(size=3)
ggsave("images/0_Simple_Conc_vs_Time_without_logtransformation_plot.png", width= 8, height= 6, dpi= 300)


ggplot(data,aes(Time,Conc))+
  geom_line(aes(group=ID),color="black",alpha=0.2)+
  theme_bw()

# Linear Concentration profile
ggplot(data, aes(Time, Conc)) +
  # Individual data lines with legend entry
  geom_line(aes(group = ID, color = "Observations"), alpha = 0.2) +
  
  # Ribbon for the interquartile range
  geom_ribbon(mapping = aes(x = Time, ymin = stat(ymin), ymax = stat(ymax)),
              stat = "summary",
              fun.ymin = function(y) { quantile(y, 0.25, na.rm = TRUE) },
              fun.ymax = function(y) { quantile(y, 0.75, na.rm = TRUE) },
              fun.y = median, alpha = 0.3, fill = "darkgrey") +
  
  # Plot median line from data (with legend entry)
  geom_line(mapping = aes(y = Conc, color = "Median"), 
            stat = "summary", fun.y = median, alpha = 1, size = 2) +
  
  # Customize legend and theme
  scale_color_manual(name = "Legend", 
                     values = c("Observations" = "grey", "Median" = "black")) +
  labs(y = "Concentration", x = "Time")  # Add axis labels as needed
 



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
print(data_wide, n = 30)


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
  
############# NCA analysis considering also Age 
  
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
    
print(dose_data_Age)


#creating Dose object
dose_data_Age_obj <- PKNCAdose(
  dose_data_Age,                         
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
  Conc_data_Age[!is.na(Conc_data_Age$Concentration), ],  # Filter rows where concentration values are not NA
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
median(summary_stats$median_Vd)
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

# calculation of MRT (Mean Residence Time) 
# for oral absorption we determine that the MRT = (AUMC/ AUC) 
#  

# 1 calculate AUMC0 - last 
data <- data %>%
  mutate (Cprime = data$Time * data$Conc,
          delta_time = lead(Time) - Time,                                 # Calculate ΔTime (difference in Time)
          AUMC_t1_t2 = 0.5 * delta_time * (Cprime + lead(Cprime))         # Apply AUMC formula
  ) 
head(data)

head(data_wide)

str(data_wide$Subject)
str(data$ID)

# Summarize the data by ID
summarized_data <- data %>% 
  group_by(ID) %>% 
  summarize(total_AUMC_t1_t2 = sum(AUMC_t1_t2, na.rm = TRUE), .groups = 'drop')

# Perform the right_join
data_wide <- data_wide %>% 
  right_join(summarized_data, by = c('Subject' = 'ID'), relationship = "many-to-many")

# calculate AUMC(t- inf.)

data_wide <- data_wide %>%
  mutate(AUMC_t_inf. = (tlast * clast.obs / lambda.z) + (clast.obs / (lambda.z^2)))

#Sum(AUMCAUMC_t1_t2) + AUMC_t_inf.

data_wide <- data_wide %>% 
  mutate(AUMC_all = AUMC_t_inf. + total_AUMC_t1_t2)

# MRT 
data_wide <-data_wide %>%  
  mutate( MRT = AUMC_all/ aucinf.obs )



########
head (data)

pivot_data_summary <- data %>%
  mutate(ID = ID,
         Time = Time, 
         Conc = Conc)%>%
  pivot_wider(names_from = Time, values_from = Conc)



head (pivot_data_summary)

#####NONEM data set transformation 

# Create NONMEM-compatible dataset
nonmem_data <- data %>%
  dplyr::mutate(
    # EVID = 1 for doses, 0 for observations
    EVID = ifelse(Amt > 0, 1, 0),
    
    # DV (dependent variable) = concentration for observations, missing (.) for doses
    DV = ifelse(EVID == 0, Conc, NA),
    
    # MDV = 1 for doses (missing DV), 0 for observations
    MDV = ifelse(EVID == 1, 1, 0),
    
    # CMT = compartment; 1 for dosing, 2 for measurements
    CMT = ifelse(EVID == 1, 1, 2),
    
    TIME = Time,
    AMT = Amt,
    AGE = Age,
    WT = Weight,
    SEX = Gender,
    RACE = Race
    
  ) %>%
  # Select and order columns for NONMEM format
  dplyr::select(ID, TIME ,DV ,AMT ,CMT , EVID, MDV, AGE, SEX,RACE, WT  )

# View the NONMEM-formatted dataset
print(nonmem_data)

str(nonmem_data)



#load PK model 

mod <- modlib("pk1")

param(mod)


theta <- log(c(CL = 1, V = 100))

names(theta)

obj <- function(p, theta, nonmem_data, dv ="DV", pred = FALSE) {
  
  names(p) <- names(theta)
  
  p <- lapply(p,exp)
  

  mod <- param(mod, p)
  
  out <- mrgsim_q(mod, nonmem_data, output="df")
  
  if(pred) return(out)
  
  sqr <- (out[["CP"]] - nonmem_data[[dv]])^2
  
  sum(sqr, na.rm=TRUE)
}

#Fit with one-compartment mode

obj(theta,theta,nonmem_data)

#RRS = 5635409

#Nelder-Mead optimization

fit <- optim(par = theta, fn=obj, theta = theta, nonmem_data=nonmem_data)

optimized_params <- exp(fit$par) #exponated params 
names(optimized_params) <- names(theta)
print(optimized_params)

#generate predictions 
pred <- obj(fit$par, theta, nonmem_data, pred = TRUE)

nonmem_data$pred <- pred$CP

head(nonmem_data)

ggplot(data = nonmem_data) + 
  geom_point(aes(TIME,DV)) + 
  scale_y_log10() + 
  geom_line(aes(TIME,pred),col="firebrick", lwd=1)

####   2 compartment model 



mod2 <- modlib("pk2")
see(mod2)
param (mod2)



theta2 <- log(c(CL = 7.517, V2 = 62 , Q = 15, V3 = 31, KA = 0.87))

obj(theta2, theta2, nonmem_data)   

obj2 <- function(p, theta2, nonmem_data, dv = "DV", pred = FALSE) {
  
  # Assign parameter names and convert log parameters back to linear scale
  names(p) <- names(theta2)
  p <- lapply(p, exp)
 
  # Update the model parameters using mrgsolve
  mod <- param(mod2, p)
  
  # Simulate the model
  out <- mrgsim_q(mod, nonmem_data, output = "df")
  
  # If pred = TRUE, return the simulated output
  if (pred) return(out)
  
  # Calculate residual sum of squares (RSS) between observed and predicted
  sqr <- (out[["CP"]] - nonmem_data[[dv]])^2
  sum(sqr, na.rm = TRUE)  # Return RSS
}

fit2 <- optim(par = theta2, fn=obj2, theta = theta2, nonmem_data=nonmem_data)

print(exp(fit2$par))

pred2 <- obj2(fit2$par, theta2, nonmem_data, pred = TRUE)

nonmem_data$pred2 <- pred2$CP

ggplot(data = nonmem_data) + 
  geom_point(aes(TIME, DV), alpha = 0.6) + 
  scale_y_log10() + 
  geom_line(aes(TIME, pred2), col = "firebrick", lwd = 1) +
  theme_minimal() +
  labs(title = "Model Fit: Two-Compartment PK", 
       x = "Time (hr)", y = "Concentration (log scale)")

#weighted least squares

# Weighted least squares objective function
obj3 <- function(p, theta, nonmem_data, wt, pred = FALSE) {
  names(p) <- names(theta)
  p <- lapply(p, exp)  # Convert parameters back from log scale
  out <- mod %>% param(p) %>% mrgsim_q(nonmem_data, output = "df")  # Simulate with new parameters
  if(pred) return(out)  # Return simulated output if pred is TRUE
  return(sum(((out$CP - nonmem_data[["DV"]]) * wt)^2, na.rm = TRUE))  # Return weighted RSS
}

# Assume nonmem_data exists and has a DV column
dv <- nonmem_data[["DV"]]  # Extract DV for further calculations

# Fitting the model with weights using newuoa
fit_wt <- minqa::newuoa(par = theta, fn = obj3, theta = theta, nonmem_data = nonmem_data, wt = 1/dv)

predw <- obj3(fit_wt$par, theta, nonmem_data, wt = 1/dv, pred = TRUE)  # Call obj3 for predictions

nonmem_data$predw <- predw$CP  # Add weighted predictions to nonmem_data


head(nonmem_data)

predx <- distinct(nonmem_data, TIME, .keep_all = TRUE)



ggplot(data = nonmem_data) + 
  geom_point(aes(TIME, DV), alpha = 0.6) + 
  scale_y_log10() + 
  geom_line(aes(TIME, pred, color = "1 Compartmental model"), lwd = 1, alpha = 0.6) +
  geom_line(aes(TIME, pred2, color = "2 Compartmental model"), lwd = 1) + 
  geom_line(aes(TIME, predw, color = "Weighted Least Squares model"), lwd = 1) +
  theme_minimal() +
  labs(title = "Comparison of 1, 2 Compartmental and Weighted Least Squares", 
       x = "Time (minutes)", 
       y = "Concentration (log scale)", 
       color = "Models") +  # Change the legend title for clarification
  scale_color_manual(values = c("1 Compartmental model" = "black", 
                                "2 Compartmental model" = "darkgreen", 
                                "Weighted Least Squares model" = "firebrick"))+
  theme(legend.position = "bottom")
  



RSS_1C <- sum((nonmem_data$DV - nonmem_data$pred)^2, na.rm = TRUE)
RSS_2C <- sum((nonmem_data$DV - nonmem_data$pred2)^2, na.rm = TRUE)
RSS_3C <- sum((nonmem_data$DV - nonmem_data$predw)^2, na.rm = TRUE)
AIC_1C <- length(theta) + length(nonmem_data$DV) * log(RSS_1C)
AIC_2C <- length(theta2) + length(nonmem_data$DV) * log(RSS_2C)
AIC_3C <- length(theta) + length(nonmem_data$DV) * log(RSS_3C)
print(AIC_1C)
print(AIC_2C)
print(AIC_3C)


