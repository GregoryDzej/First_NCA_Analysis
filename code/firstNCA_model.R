

######visualisation the data 
library(ggplot2)
data <- read.csv("sample_data.csv")

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


summary(data)
n_distinct(data$ID)

ggplot(data, aes(x=Time, y=Conc)) +
  geom_point(size=3) 
  ggsave("images/Simple Conc vs Time plot.png", width=8, height=6, dpi=300)
 
# plot with the lines by Subject ID 

ggplot(data,aes(Time,Conc) )+ geom_point()+
  geom_line(aes(group=ID))
ggsave("images/Simple Conc vs Time plot by Subject ID.png", width=8, height=6, dpi=300)


# create 3 plots spit by Dose 
ggplot(data,aes(Time,Conc,colour=as.factor(Dose)))+
  geom_line(aes(group=ID))+
  facet_grid(~Dose )
  ggsave("images/ Conc vs Time split by Dose plot.png", width=8, height=6, dpi=300)



# plot of the patients concentriation colorised by Dose 
par(mfrow=c(1,1))
plot(data[,"Time"],data[,"Conc"],type="n",xlab="Time (h)",ylab="Concentrations")
lines(data[data$Dose==5000 ,"Time"],data[data$Dose==5000 ,"Conc"],col = "yellow")
lines(data[data$Dose==10000 ,"Time"],data[data$Dose==10000 ,"Conc"],col= "orange")
lines(data[data$Dose==20000 ,"Time"],data[data$Dose==20000 ,"Conc"],col= "red")
dev.copy(png, filename = "images/line_plot_dose_color.png", width = 800, height = 600)
dev.off()


# histograms of quantitative variables 
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)) # Define plotting grid layout
hist(data$Conc, main = "Histogram of Concentration", xlab = "Concentration", col = "blue")
hist(data$Age, main = "Histogram of Age", xlab = "Age", col = "green")
hist(data$Weight, main = "Histogram of Weight", xlab = "Weight", col = "orange")
dev.copy(png, filename = "images/histograms_quantitative_variables.png", width = 800, height = 600)
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
  ggsave("images/ median and  0.25 and 0.75 quantile of log(concentration) vs time.png", width=8, height=6, dpi=300)
             
       
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
ggsave("images/ median and  0.25 and 0.75 quantile of log(concentration) vs time by doses.png", width=8, height=6, dpi=300)

library(dplyr)
# median and  0.25 and 0.75 quantile of log(concentration) vs time grouped by dose by gender
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
ggsave("images/ median and  0.25 and 0.75 quantile of log(concentration) vs time by dose by gender.png", width=8, height=6, dpi=300)
)

######################################################
#### noncomparmental analysis 


install.packages("PKNCA")
library(PKNCA)
install.packages('pander')
library(pander)
## obtain dose data 

dose_data <- data.frame(
  Subject = unique(data$ID),
  Time = 0,
  Dose = 5000 # The dose values (from your dataset)
)

# Combine concentration-time data with dosing info
conc_data <- data.frame(
  Subject = data$ID,
  Time = data$Time,
  Concentration = data$Conc
) 

conc_obj <- PKNCAconc(
  conc_data[!is.na(conc_data$Concentration), ],  # Filter rows where concentration values are not NA
  Concentration ~ Time | Subject                 # Formula: Concentration as a function of Time, grouped by Subject
)

dose_obj <- PKNCAdose(
  dose_data,                         # Raw dosing dataset
  Dose ~ Time | Subject                   # Formula: Dose depends on Time, grouped by ID
)


PK_object <- PKNCAdata(
  data.conc = conc_obj,  # Concentration-time object
  data.dose = dose_obj          # Dosing object
)

results<-pk.nca(PK_object)
pander::pander(summary(results))

# Convert results into a standard data frame
param_table <- as.data.frame(results)
pander::pander(param_table)
head(param_table)

#grouping the data to be joined
data_grouped <- data %>%
  group_by(ID) %>%
  summarize(Dose = first(Dose), Gender = first(Gender))

# left join param table with data 
param_table <- param_table %>% 
  left_join(data_grouped %>% select(ID, Dose, Gender), by = c('Subject' = 'ID'))

pander::pander(param_table)

head(param_table)

library(tidyr)
#pivot some columns 

data_wide <- param_table %>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)

#adding the clerance and volume of distribution to our data

data_wide <- data_wide %>%
  mutate(
    CL = Dose / aucinf.obs,            # Clearance
    Vd = CL / lambda.z                 # Volume of distribution
  )

# View the reshaped dataset
head(data_wide)


#summary statistics for AUCLAST, CMAX, TMAX
summary_stats <- data_wide %>%
  group_by(Dose, Gender) %>%
  summarize(
    median_auclast = median(auclast, na.rm = TRUE),
    median_cmax = median(cmax, na.rm = TRUE),
    median_tmax = median(tmax, na.rm = TRUE),
    q1_auclast = quantile(auclast,probs = 0.25, na.rm = TRUE),
    q3_auclast = quantile(auclast,probs = 0.75, na.rm = TRUE),
    tlast = max(tlast, na.rm = TRUE),
    madian_half_life = median(half.life , na.rm = TRUE),
    median_CL = median(CL, na.rm = TRUE),
    median_Vd = median(Vd, na.rm = TRUE),
    n = n_distinct(Subject)
  )

print(summary_stats)
<<<<<<< Updated upstream
##
=======
#Dose Gender median_auclast median_cmax median_tmax q1_auclast q3_auclast tlast madian_half_life
#<int> <chr>           <dbl>       <dbl>       <dbl>      <dbl>      <dbl> <dbl>            <dbl>
#1  5000 Female           581.        51.6           2       476.      1060.    24             7.30
#2  5000 Male             576.        58.5           3       415.       774.    24             7.56
#3 10000 Female          1145.       105.            3       979.      2093.    24             8.26
#4 10000 Male            1152.       114.            3       828.      1575.    24             6.98
#5 20000 Female          1718.       157.            3      1477.      3166.    24             7.93
>>>>>>> Stashed changes

#BOX PLOT of median AUCLast by Gender 
ggplot(data_wide, aes(x=factor(Dose), y=auclast, fill=Gender)) +
  geom_boxplot() +
  labs(title="AUC Last by Dose and Gender", x="Dose", y="AUC Last") +
  theme_minimal()
ggsave("images/ BOX PLOT of median AUCLast by Gender.png", width=8, height=6, dpi=300)
  

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
ggsave("images/ Violin PLOT of median Cmax by Gender MEC and MTC lines.png", width=8, height=6, dpi=300)

#scatterplot of Cmax vs Tmax by Gender 
ggplot(data_wide, aes(x=tmax, y=cmax, color=Gender)) +
  geom_point() +
  geom_smooth(method="loess",formula = y ~ splines::bs(x, 3), se=TRUE) +# have chosen the loees fitting , and splines 
  labs(title="Cmax vs Tmax by Gender", x="Tmax", y="Cmax") +
  theme_minimal()
ggsave("images/ scatterplot of Cmax vs Tmax by Gender .png", width=8, height=6, dpi=300)
  
  
  
