#check from line 89

data=read.csv("D:/Water_Quality_Monitoring_Sonde_Data_20241024.csv")#read data here
dim(data)
colnames(data)
head(data)
data = subset(data, select = c("Parameter","Numeric.Result"))
dim(data)
length(which(data$Parameter=='pH'))
length(which(data$Parameter=="Water Temperature"))

length(which(data$Parameter=="Depth"))
data1=data[1:1000000,]
dim(data1)
length(which(data1$Parameter=='pH'))
length(which(data1$Parameter=="Water Temperature"))

length(which(data1$Parameter=="Depth"))

#model= lm(data$Numeric.Result ~ data$Parameter + data$Sample.Site) 
#summary(model)

sum(is.na(data))
summary(data)
dim(data)


unique_parameters = unique(data$Parameter)
print(unique_parameters)
library(tidyr)
library(dplyr)
datax <- data %>%
  group_by(Parameter) %>%
  mutate(row = row_number()) %>%  
  pivot_wider(names_from = Parameter, values_from = Numeric.Result)
print(datax)

dim(datax)

colnames(datax)
model = lm(pH ~ Depth + `Dissolved Oxygen Concentration` + `Specific Conductivity`+Turbidity+`Water Temperature`+Chlorophyll,data=datax )
summary(model)

library(ggplot2)
library(reshape2)
cm = cor(datax[, c("pH", "Depth", "Dissolved Oxygen Concentration", "Specific Conductivity", "Turbidity", "Water Temperature", "Chlorophyll")], use = "complete.obs")
cmm = melt(cm)

ggplot(data = cmm, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Heatmaap", x = "para", y = "para")


print(unique(datax$pH))
#print(distinct(data$pH))
sum(is.na(datax))
dim(datax)


#alvin told to drop data -.-
print(sum(is.na(datax)))
datay <- na.omit(datax)
print(sum(is.na(datay)))
model = lm(pH ~ Depth + `Dissolved Oxygen Concentration` + `Specific Conductivity`+Turbidity+`Water Temperature`+Chlorophyll,data=datay )
summary(model)
dim(datay)


# the order and arrangement of the columns are messed up the pivot function is not wokring properly
#try : 3
colnames(datax)
dataz = subset(datax, select = c('pH',"Depth","Dissolved Oxygen Concentration","Specific Conductivity","Turbidity","Water Temperature"))
dataz

dim(dataz)
sum(is.na(dataz))
dataz<- na.omit(dataz)
dim(dataz)
model_maybe=lm(pH ~Depth + `Dissolved Oxygen Concentration` + `Specific Conductivity`+Turbidity+`Water Temperature`,data=dataz)
summary(model_maybe)
#holy shit someho wwwworked -.-


#####---from here


# 1. Library Import ----------------------------------------------------------
library(tidyr)
library(dplyr)


# 2. Data Import -------------------------------------------------------------
data=read.csv("D:/Water_Quality_Monitoring_Sonde_Data_20241024.csv")

# Verify
dim(data)
colnames(data)
head(data)


# 3. Data Cleaning and Wrangling -----------------------------------------------------------
# Filter non-related parameters. Keep only depth, oxygen, conductivity, turbidity, water temperature, ph
cleaned_data <- data %>%
  filter(Parameter %in% c(
    "Depth",
    "Dissolved Oxygen Concentration",
    "Specific Conductivity",
    "Turbidity",
    "Water Temperature",
    "pH"
  )) %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result)


# Verify filter result
cleaned_data %>%
  distinct(Parameter) %>%
  arrange(Parameter)


# Pivot the data
cleaned_data <- cleaned_data %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result) %>%
  group_by(Sample.Site, Sample.Date, Parameter) %>%
  summarise(
    Numeric.Result = mean(Numeric.Result, na.rm = TRUE),
    .groups = "drop" # dropping groupping, treat all data as one complete dataset
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Numeric.Result,
    names_prefix = "param_"
  )

# Standardize column names
cleaned_data <- cleaned_data %>%
  rename(
    sample_site = `Sample.Site`,
    sample_date = `Sample.Date`,
    param_ph = `param_pH`,
    param_depth = `param_Depth`,
    param_conductivity = `param_Specific Conductivity`,
    param_oxgen = `param_Dissolved Oxygen Concentration`,
    param_turbidity = `param_Turbidity`,
    param_water_temp = `param_Water Temperature`
  )

# Verify
dim(cleaned_data)
colnames(cleaned_data)
head(cleaned_data)


# Remove Incomplete Records (i.e. param. value is non-numeric)
cleaned_data <- cleaned_data %>%
  # Convert all param_ columns to numeric. Non-numeric values will be converted to NA
  mutate_at(vars(starts_with("param_")), as.numeric) %>%    
  # Filter rows if not all parameters are numeric
  filter(if_all(starts_with("param_"), function(x) !is.na(x) & is.numeric(x)))


# Verify
dim(cleaned_data)
colnames(cleaned_data)
head(cleaned_data)


# Add a season column
# season definition https://nrc.canada.ca/en/certifications-evaluations-standards/canadas-official-time/3-when-do-seasons-start
cleaned_data <- cleaned_data %>%
  mutate(sample_date = as.POSIXct(sample_date, format = "%m/%d/%Y %I:%M:%S %p")) # sapmle date: 12/25/2023 10:30:45 AM

cleaned_data <- cleaned_data %>%
  mutate(
    month = as.numeric(format(sample_date, "%m")),
    day = as.numeric(format(sample_date, "%d")),
    season = case_when(
      # Winter: December 22 to March 19
      (month == 12 & day >= 22) | (month == 1 | month == 2) | (month == 3 & day < 20) ~ "Winter",
      
      # Spring: March 20 to June 20
      (month == 3 & day >= 20) | (month == 4 | month == 5) | (month == 6 & day < 21) ~ "Spring",
      
      # Summer: June 21 to September 22
      (month == 6 & day >= 21) | (month == 7 | month == 8) | (month == 9 & day < 23) ~ "Summer",
      
      # Autumn: September 23 to December 21
      (month == 9 & day >= 23) | (month == 10 | month == 11) | (month == 12 & day < 22) ~ "Autumn"
    )
  )

# Drop the temporary helper columns (month, day)
cleaned_data <- cleaned_data %>%
  select(-month, -day)

# Move season to the second position
cleaned_data <- cleaned_data %>%
  select(sample_site, season, everything())

# Reorder the records based on date
cleaned_data <- cleaned_data %>%
  arrange(sample_date)

# Verify
dim(cleaned_data)
colnames(cleaned_data)
head(cleaned_data)


#outleirs cehck
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x < lower_bound | x > upper_bound
}

#this takes 15-20 mins to run depending on ur machine

data_with_outliers <- cleaned_data %>%
  mutate(across(where(is.numeric), find_outliers, .names = "outlier_{col}")) %>%
  rowwise() %>%
  mutate(is_outlier = any(c_across(starts_with("outlier_")), na.rm = TRUE)) %>%
  ungroup()

# Separate tables
outliers_table <- filter(data_with_outliers, is_outlier)
non_outliers_table <- filter(data_with_outliers, !is_outlier)

# View tables
print("Outliers:")
print(outliers_table)

print("Non-Outliers:")
print(non_outliers_table)


#if u want to contine with outliers use  cleaned_data ---> (the original data )
# if u want to continue with outliers use non_outliers_table

#colnames(non_outliers_table)

model1=lm(param_ph ~ sample_site+season+param_depth+param_oxgen+param_conductivity+param_water_temp+param_turbidity,data=cleaned_data)
summary(model1)


model55=lm(param_ph~param_oxgen+param_conductivity+param_water_temp+param_turbidity+param_depth,data=cleaned_data)
summary(model55)



#lose almost 10% accuracy when u remove outlier

#VIF

library(mctest) #for VIF
imcdiag(model1, method="VIF")
#no colinerity [ except in sample site]

#Testing for Homoscedasticity
library(lmtest)
bptest(model1)
#Here we reject the null hypothesis (p-value < 0.05)
#so we conclude we do have heteroscedasticity


#normality check
shapiro.test(residuals(model1))
#another way for normality
shapiro_test <- shapiro.test(cleaned_data$param_ph)
print(shapiro_test)


#anderson
# Install the nortest package if not already installed
install.packages("nortest")
library(nortest)

# Anderson-Darling Test
colnames(cleaned_data)
ad_test <- ad.test(cleaned_data$param_depth)
print(ad_test)
ad_test <- ad.test(cleaned_data$param_oxgen)
print(ad_test)
ad_test <- ad.test(cleaned_data$param_conductivity)
print(ad_test)
ad_test <- ad.test(cleaned_data$param_water_temp)
print(ad_test)
ad_test <- ad.test(cleaned_data$param_turbidity)
print(ad_test)
ad_test <- ad.test(cleaned_data$param_ph)
print(ad_test)

#kolmogrov normality test
column_data <- cleaned_data$param_depth
column_mean <- mean(column_data, na.rm = TRUE)
column_sd <- sd(column_data, na.rm = TRUE)
ks_test_result <- ks.test(column_data, "pnorm", mean = column_mean, sd = column_sd)
print(ks_test_result)



library(MASS) #for the boxcox()function
bc=boxcox(model1,lambda=seq(-1,1))



# form here-----------------------------------------------------
#alvin ta cleaning panna sollu

rest=read.csv("C:\\Users\\lalit\\Downloads\\Water_Quality_Monitoring_Sonde_Data_20241128.csv")


cleaned_data_test <- rest %>%
  filter(Parameter %in% c(
    "Depth",
    "Dissolved Oxygen Concentration",
    "Specific Conductivity",
    "Turbidity",
    "Water Temperature",
    "pH"
  )) %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result)


# Verify filter result
cleaned_data_test %>%
  distinct(Parameter) %>%
  arrange(Parameter)


# Pivot the data
cleaned_data_test <- cleaned_data_test %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result) %>%
  group_by(Sample.Site, Sample.Date, Parameter) %>%
  summarise(
    Numeric.Result = mean(Numeric.Result, na.rm = TRUE),
    .groups = "drop" # dropping groupping, treat all data as one complete dataset
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Numeric.Result,
    names_prefix = "param_"
  )

# Standardize column names
cleaned_data_test <- cleaned_data_test %>%
  rename(
    sample_site = `Sample.Site`,
    sample_date = `Sample.Date`,
    param_ph = `param_pH`,
    param_depth = `param_Depth`,
    param_conductivity = `param_Specific Conductivity`,
    param_oxgen = `param_Dissolved Oxygen Concentration`,
    param_turbidity = `param_Turbidity`,
    param_water_temp = `param_Water Temperature`
  )

# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)


# Remove Incomplete Records (i.e. param. value is non-numeric)
cleaned_data_test <- cleaned_data_test %>%
  # Convert all param_ columns to numeric. Non-numeric values will be converted to NA
  mutate_at(vars(starts_with("param_")), as.numeric) %>%    
  # Filter rows if not all parameters are numeric
  filter(if_all(starts_with("param_"), function(x) !is.na(x) & is.numeric(x)))


# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)


# Add a season column
# season definition https://nrc.canada.ca/en/certifications-evaluations-standards/canadas-official-time/3-when-do-seasons-start
cleaned_data_test <- cleaned_data_test %>%
  mutate(sample_date = as.POSIXct(sample_date, format = "%m/%d/%Y %I:%M:%S %p")) # sapmle date: 12/25/2023 10:30:45 AM

cleaned_data_test <- cleaned_data_test %>%
  mutate(
    month = as.numeric(format(sample_date, "%m")),
    day = as.numeric(format(sample_date, "%d")),
    season = case_when(
      # Winter: December 22 to March 19
      (month == 12 & day >= 22) | (month == 1 | month == 2) | (month == 3 & day < 20) ~ "Winter",
      
      # Spring: March 20 to June 20
      (month == 3 & day >= 20) | (month == 4 | month == 5) | (month == 6 & day < 21) ~ "Spring",
      
      # Summer: June 21 to September 22
      (month == 6 & day >= 21) | (month == 7 | month == 8) | (month == 9 & day < 23) ~ "Summer",
      
      # Autumn: September 23 to December 21
      (month == 9 & day >= 23) | (month == 10 | month == 11) | (month == 12 & day < 22) ~ "Autumn"
    )
  )

# Drop the temporary helper columns (month, day)
cleaned_data_test <- cleaned_data_test %>%
  select(-month, -day)

# Move season to the second position
cleaned_data_test <- cleaned_data_test %>%
  select(sample_site, season, everything())

# Reorder the records based on date
cleaned_data_test <- cleaned_data_test %>%
  arrange(sample_date)

# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)



#-----------------------------------------run maps

test_data= cleaned_data_test
  
predictions = predict(model1, newdata = test_data)

plot_data = data.frame(
  Observed = test_data$param_ph,
  Predicted = predictions
)
ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_line(color = "blue", size = 1.2, linetype = "solid") + 
  geom_point(color = "red", size = 2) +
  labs(
    title = "act vs pred pH vals",
    x = "observed pH",
    y = "predicted pH",
    color = "Legend"
  )
print(test_data$param_ph)
print(predictions)

#two_pred=predictions*2
#two_pred

plot(test_data$param_ph,type='l',col='blue')
lines(predictions,type='l',col='red')

length(predictions)
length(test_data$param_ph)


test_data



plot(test_data$param_ph, 
     type = 'l', 
     col = 'blue', 
     xlab = 'Index', 
     ylab = 'pH', 
     main = 'Actual vs Predicted pH Values')

lines(predictions, 
      type = 'l', 
      col = 'red')

legend("topright", 
       legend = c("Actual pH", "Predicted pH"), 
       col = c("blue", "red"), 
       lty = 1,
       lwd = 3,
       x.intersp = 0.1,
       cex = 1.2, # Larger text
       bty = "n", # Keeps the box
       inset = c(-0.5, -0.05)
       )
  



#plot(two_pred[1:10000],type='l',col='red')
#lines(test_data$param_ph[1:200],type='l',col='blue')


seasonal_pH <- cleaned_data %>%
  group_by(season) %>%
  summarise(mean_pH = mean(param_ph))


ggplot(seasonal_pH, aes(x = season, y = mean_pH, fill = season)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(title = "Seasonal Variations in pH Levels", 
       x = "Season", 
       y = "Average pH") +
  theme_minimal() +
  scale_fill_manual(values = c("Summer" = "orange", "Autumn" = "brown", "Spring" = "green", "Winter" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#-----lead----------------------------------

rest=read.csv("prediction_data.csv")


cleaned_data_test <- rest %>%
  filter(Parameter %in% c(
    "Depth",
    "Dissolved Oxygen Concentration",
    "Specific Conductivity",
    "Turbidity",
    "Water Temperature",
    "pH"
  )) %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result)


# Verify filter result
cleaned_data_test %>%
  distinct(Parameter) %>%
  arrange(Parameter)


# Pivot the data
cleaned_data_test <- cleaned_data_test %>%
  select(Sample.Site, Sample.Date, Parameter, Numeric.Result) %>%
  group_by(Sample.Site, Sample.Date, Parameter) %>%
  summarise(
    Numeric.Result = mean(Numeric.Result, na.rm = TRUE),
    .groups = "drop" # dropping groupping, treat all data as one complete dataset
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Numeric.Result,
    names_prefix = "param_"
  )
colnames(cleaned_data_test)
# Standardize column names
cleaned_data_test <- cleaned_data_test %>%
  rename(
    sample_site = Sample.Site,
    sample_date = Sample.Date,
    param_ph = param_pH,
    param_depth = param_Depth,
    param_conductivity = `param_Specific Conductivity`,
    param_oxgen = `param_Dissolved Oxygen Concentration`,
    param_turbidity = param_Turbidity,
    param_water_temp = `param_Water Temperature`
  )

# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)


# Remove Incomplete Records (i.e. param. value is non-numeric)
cleaned_data_test <- cleaned_data_test %>%
  # Convert all param_ columns to numeric. Non-numeric values will be converted to NA
  mutate_at(vars(starts_with("param_")), as.numeric) %>%    
  # Filter rows if not all parameters are numeric
  filter(if_all(starts_with("param_"), function(x) !is.na(x) & is.numeric(x)))


# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)


# Add a season column
# season definition https://nrc.canada.ca/en/certifications-evaluations-standards/canadas-official-time/3-when-do-seasons-start
cleaned_data_test <- cleaned_data_test %>%
  mutate(sample_date = as.POSIXct(sample_date, format = "%m/%d/%Y %I:%M:%S %p")) # sapmle date: 12/25/2023 10:30:45 AM

cleaned_data_test <- cleaned_data_test %>%
  mutate(
    month = as.numeric(format(sample_date, "%m")),
    day = as.numeric(format(sample_date, "%d")),
    season = case_when(
      # Winter: December 22 to March 19
      (month == 12 & day >= 22) | (month == 1 | month == 2) | (month == 3 & day < 20) ~ "Winter",
      
      # Spring: March 20 to June 20
      (month == 3 & day >= 20) | (month == 4 | month == 5) | (month == 6 & day < 21) ~ "Spring",
      
      # Summer: June 21 to September 22
      (month == 6 & day >= 21) | (month == 7 | month == 8) | (month == 9 & day < 23) ~ "Summer",
      
      # Autumn: September 23 to December 21
      (month == 9 & day >= 23) | (month == 10 | month == 11) | (month == 12 & day < 22) ~ "Autumn"
    )
  )

# Drop the temporary helper columns (month, day)
cleaned_data_test <- cleaned_data_test %>%
  select(-month, -day)

# Move season to the second position
cleaned_data_test <- cleaned_data_test %>%
  select(sample_site, season, everything())

# Reorder the records based on date
cleaned_data_test <- cleaned_data_test %>%
  arrange(sample_date)

# Verify
dim(cleaned_data_test)
colnames(cleaned_data_test)
head(cleaned_data_test)



#-----------------------------------------run maps


#changeing cleaned_data_test col names to fit the model - by aomine

cleaned_data_test <- cleaned_data_test %>%
  rename(
    ph=param_ph,
    water_temp=param_water_temp,
    conductivity=param_conductivity,
    turbidity=param_turbidity,
    oxygen=param_oxgen,
    season=season
    
  )

cleaned_data_test<- cleaned_data_test%>%select(ph,water_temp,conductivity,turbidity,oxygen,season)
colnames(cleaned_data_test)

#end of change made by aomine
#colnames(cleaned_data_test)

test_data= cleaned_data_test

predictions = predict(interaction_model, newdata = test_data)
summary(interaction_model)

plot_data = data.frame(
  Observed = test_data$ph,
  Predicted = predictions
)
#ggplot(plot_data, aes(x = Observed, y = Predicted)) +
# geom_line(color = "blue", size = 1.2, linetype = "solid") + 
#geom_point(color = "red", size = 2) +
#labs(
# title = "act vs pred pH vals",
#x = "observed pH",
#y = "predicted pH",
#color = "Legend"
#)
print(test_data$ph)
print(predictions)

#two_pred=predictions*2
#two_pred

#plot(test_data$ph,type='l',col='blue')
#lines(predictions,type='l',col='red')

length(predictions)
length(test_data$ph)





plot(test_data$ph, 
     type = 'l', 
     col = 'blue', 
     xlab = 'Index', 
     ylab = 'pH', 
     main = 'Actual vs Predicted pH Values')

lines(predictions, 
      type = 'l', 
      col = 'red')

legend("topright", 
       legend = c("Actual pH", "Predicted pH"), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)





