#######################################################################################
## Final Project: Diana Rendon - Divara Cendana - Kangjun Zou
## Title: Identifying Pivotal Factors for Level of Salary
##
## 1. Dataset 1 - Salary
##	Open and Review Set
##	Color Palettes
##	Data Cleaning
##	Plots
## 2. Dataset 2 - Income
##	Open and Review dataset
##	Bar Plot for Income Distribution
## 3. Dataset 3 - Union Affliation
##	Read File
##	Dot Charts
##	Box Plots
##	Histograms
##	Scatterplot Matrices
##	Cross Affliation with another data set for relevance
## 4. Dataset 4 - 6	
##	Setting up Working Directory and Importing Data
##	Data Cleaning
##	Plot 1
##	Plot 2
##	Plot 3
##	Regression Model
##	Extract Coefficients, P-Values and R-Squared
##	Creating Data Frames
##	Tabulated Information
##
##
########################################################################################



#################  Dataset 1 - Salary  #############################

### Open and Review dataset###
read.csv("salary_data_cleaned.csv")
salary <- read.csv("salary_data_cleaned.csv")
View(salary)
summary(salary)

### Color Palettes ###
install.packages("vangogh")
library("vangogh")
vangoghcolor <- vangogh_palette("CafeTerrace")
vangoghcolor2 <- vangogh_palette("Chaise")

### Data Cleaning ###
# Removing row with a non-state under a state variable #
row_to_remove <- 127
salary_filtered <- salary[-row_to_remove, ]

## Removing row with invalid data ##
row_to_remove2 <- 582
salary_filtered <- salary[-row_to_remove2, ]

### Plots ###
# Box Plot for Average Salary by State #
boxplot(salary_filtered$avg_salary ~ salary_filtered$job_state, main = 'Average Salary by State', xlab = 'States', ylab = 'Average Salary (in thousands)', cex.axis = 1, col = vangoghcolor, las = 2)

# Box Plot for Average Salary by Different Types of Ownership #
boxplot(salary_filtered$avg_salary ~ salary_filtered$Type.of.ownership, main = 'Average Salary by Different Types of Ownership', xlab = 'Types of Ownership', ylab = 'Average Salary (in thousands)', cex.axis = 0.5, col = vangoghcolor2)

# Bar Plot for Average Salary for R users and non-R users #
avg_salary_by_R <- tapply(salary_filtered$avg_salary, salary_filtered$R_yn, mean)
barplot(avg_salary_by_R, names.arg = c('R users', 'non-R users'), col = c('darkolivegreen3', 'coral2'), main = 'Average Salary by Knowledge of R', ylab = 'Average Salary (in thousands)', ylim = c(0, max(avg_salary_by_R) + 5))

# Bar Plot for Average Salary for Python users and non-Python users #
avg_salary_by_Python <- tapply(salary_filtered$avg_salary, salary_filtered$python_yn, mean)avg_salary_by_Python <- tapply(salary_filtered$avg_salary, salary_filtered$python_yn, mean)
barplot(avg_salary_by_Python, names.arg = c('non-Python users', 'Python users'), col = c('darkolivegreen3', 'coral2'), main = 'Average Salary by Knowledge of Python', ylab = 'Average Salary (in thousands)', ylim = c(0, max(avg_salary_by_Python) + 5))

#################  Dataset 2 - Income  #############################

## Open and Review dataset ##
read.csv("income.csv")
income <- read.csv("income.csv")
View(income)
summary(income)


## Bar Plot for Income Distribution for Education Level ##

educationdata <- table(income$income, income$education)
barplot(educationdata, beside = TRUE, col = c("darkblue", "darkgreen"), main = "Income Distribution per Education Level", xlab = "Level of Education",ylab = "Count",legend.text = rownames(educationdata),args.legend = list(x = "topleft"), cex.names = 0.7)

#################  Dataset 3 - Union Affiliation  #############################

### Read File ###

union <- read.csv("cpsaat43_clean2.csv", header=T)
summary(union)

### Plots  and Summary Statistics ###

### Dot Chart###

dotchart(union$Members.of.unions, labels=union$Occupation.and.industry, cex=0.8,
	main= "Members of Union and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

dotchart(union$Represented.by.unions, labels=union$Occupation.and.industry, cex=0.8,
	main= "Represented by Unions and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

dotchart(union$Non.union, labels=union$Occupation.and.industry, cex=0.8,
	main= "Non Union and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

### Boxplot ###

par(mfrow = c(1,3))	
boxplot(union$Members.of.unions, data= union, col="#c44536",
	main="Members of Union and their Salary", cex=0.4, ylab="Salary")
boxplot(union$Represented.by.unions, data=union, col="#197278", 
	main="Represented by Unions and their Salary", cex=0.4, ylab="Salary")
boxplot(union$Non.union, data=union, col= "#5f0f40",
	main="Non Union and their Salary", cex=0.4, ylab="Salary")

### Histogram ###

hist(union$Members.of.unions, col=rgb(1,0,0,0.25),
	ylim = c(0,50),main = "Frequency of Salaries by Union Affliation",				
	xlab = "Salary", xlim = c(300,2500))
hist(union$Represented.by.unions, col=rgb(0,0,1,0.25),add=T)
hist(union$Non.union, col=rgb(0,1,0,0.25),add=T)

legend("topright", c("Members of Unions", "Represented by Unions", "Non-union"), 
col=c(rgb(1,0,0,0.25),rgb(0,0,1,0.25), rgb(0,1,0,0.25)), lwd=10)

### Scatterplot Matrice ###

pairs(~Members.of.unions+Represented.by.unions+Non.union, data=union,
	main="Union Affliation Scatterplot Matrice")

### Cross Affliation with another data set for relevance###

salarycross <- read.csv("salary_data_cross.csv")
summary(salarycross)

#Create a subset for Private
private_data <- subset(salarycross, Type.of.ownership == "Company - Private")

#Display summary statistics for SalaryLow and SalaryHigh within Private type of ownership
summary(private_data$SalaryLow)
summary(private_data$SalaryHigh)

#Create a subset for Public
public_data <- subset(salarycross, Type.of.ownership == "Company - Public")

# Display summary statistics for SalaryLow and SalaryHigh within Public type of ownership
summary(public_data$SalaryLow)
summary(public_data$SalaryHigh)

# Create a subset for private sector in the Union dataset
private_union <-subset(union, Occupation.and.industry =="Private sector")


# Create a subset for public sector in the Union dataset
public_union <-subset(union, Occupation.and.industry =="Public sector")

summary(private_union)
summary(public_union)

# Box Plot for Minimum Salary by Different Types of Ownership #
boxplot(salarycross$SalaryLow ~ salarycross$Type.of.ownership, main = 'Minimum Salary by Different Types of Ownership', xlab = 'Types of Ownership', ylab = 'Average Salary (in thousands)', cex.axis = 0.5, col = vangoghcolor2)

# Box Plot for Maximum Salary by Different Types of Ownership #
boxplot(salarycross$SalaryHigh ~ salarycross$Type.of.ownership, main = 'Maximum Salary by Different Types of Ownership', xlab = 'Types of Ownership', ylab = 'Average Salary (in thousands)', cex.axis = 0.5, col = vangoghcolor2)


#################  Dataset 4 -6  #############################

### Data Cleaning ###

## set the working directory and import data sets
setwd("C:/Users/kj_zo/Desktop/Data science course work/Exploratory data analysis and visulization/Group project")
library(readr)
degrees_that_pay_back <- read_csv("degrees-that-pay-back.csv")
salaries_by_college_type <- read_csv("salaries-by-college-type.csv")
salaries_by_region <- read_csv("salaries-by-region.csv")

summary(degrees_that_pay_back) ## clean

summary(salaries_by_college_type) ##  wrong data type: column 5 and 8
columns_to_convert <- c(5,8) 
salaries_by_college_type[, columns_to_convert] <- sapply( ## fix the data type
  salaries_by_college_type[, columns_to_convert],
  function(x) as.numeric(x)
)

# Replace NAs with mean in col 5 and col 8
salaries_by_college_type$`Mid-Career 10th Percentile Salary`[is.na(salaries_by_college_type$`Mid-Career 10th Percentile Salary`)] <- mean(salaries_by_college_type$`Mid-Career 10th Percentile Salary`, na.rm = TRUE)
salaries_by_college_type$`Mid-Career 90th Percentile Salary`[is.na(salaries_by_college_type$`Mid-Career 90th Percentile Salary`)] <- mean(salaries_by_college_type$`Mid-Career 90th Percentile Salary`, na.rm = TRUE)

summary(salaries_by_region)
salaries_by_region$Region<-as.factor(salaries_by_region$Region)
columns_to_convert <- c(5,8) 
salaries_by_region[, columns_to_convert] <- sapply( ## fix the data type
  salaries_by_region[, columns_to_convert],
  function(x) as.numeric(x)
)

# Replace NAs with mean in col 5 and col 8
salaries_by_region$`Mid-Career 10th Percentile Salary`[is.na(salaries_by_region$`Mid-Career 10th Percentile Salary`)] <- mean(salaries_by_region$`Mid-Career 10th Percentile Salary`, na.rm = TRUE)
salaries_by_region$`Mid-Career 90th Percentile Salary`[is.na(salaries_by_region$`Mid-Career 90th Percentile Salary`)] <- mean(salaries_by_region$`Mid-Career 90th Percentile Salary`, na.rm = TRUE)

### Plot 1 ###

library(dplyr)
library(ggplot2)
degrees_that_pay_back <- degrees_that_pay_back %>%
  mutate(`Undergraduate Major` = reorder(`Undergraduate Major`, `Starting Median Salary`))

degrees_that_pay_back$percentage_change_trans<-degrees_that_pay_back$`Percent change from Starting to Mid-Career Salary`*1000
ggplot(degrees_that_pay_back, aes(x = `Undergraduate Major`)) +
  geom_line(aes(y = `Starting Median Salary`, group = 1, color = "Starting Median Salary")) +
  geom_line(aes(y = `Mid-Career Median Salary`, group = 1, color = "Mid-Career Median Salary")) +
  geom_point(aes(y = `Starting Median Salary`, color = "Starting Median Salary")) +
  geom_point(aes(y = `Mid-Career Median Salary`, color = "Mid-Career Median Salary")) +
  # Add another line on the second y-axis
  geom_line(aes(y = percentage_change_trans, group = 2, color = "Percent change from Starting to Mid-Career Salary")) +
  
  # Points for the new line
  geom_point(aes(y = percentage_change_trans, color = "Percent change from Starting to Mid-Career Salary")) +
  
  scale_y_continuous(
    
    name = "Salaries in thousands",labels = scales::comma_format(scale = 1e-3, suffix = ""),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="Percentage change \n %")
  ) + 
  scale_color_manual(
    name = "Legend",
    values = c("Starting Median Salary" = "blue", "Mid-Career Median Salary" = "red", "Percent change from Starting to Mid-Career Salary" = "green"),
    breaks = c("Starting Median Salary", "Mid-Career Median Salary", "Percent change from Starting to Mid-Career Salary")
  ) +
    labs(title = "Median Salary (Starting vs Mid-career vs Percentage change)",
       fill = "Legend",
       color = "Legend") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_hline(yintercept = 55000, linetype = "dashed", color = "blue") + ## Add horizontal lines at both 55000 and 90000 levels
  geom_hline(yintercept = 90000, linetype = "dashed", color = "red")


### Plot 2 ###

# Assuming salaries_by_college_type is your data frame
ggplot(salaries_by_college_type, aes(x = `Starting Median Salary`, y = `Mid-Career Median Salary`, shape = `School Type`, color = `School Type`)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 18, 19,20)) +  # Set different shapes for categories
  labs(title = "Starting vs. Mid-Career Median Salary by School Type",
       x = "Starting Median Salary",
       y = "Mid-Career Median Salary")+
  theme_minimal()

### Plot 3 ###

ggplot(salaries_by_region, aes(x = `Starting Median Salary`, y = `Mid-Career Median Salary`, shape = `Region`, color = `Region`)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 18, 19,20)) +  # Set different shapes for categories
  labs(title = "Starting vs. Mid-Career Median Salary by Region",
       x = "Starting Median Salary",
       y = "Mid-Career Median Salary")+
  theme_minimal()

### Regression Model ###

summary(salaries_by_college_type)
summary(salaries_by_region)
merged<-merge(salaries_by_college_type,salaries_by_region,by="School Name")

## fit the linear regression model
my_model<-lm(`Mid-Career Median Salary.x`~merged$`School Type`+merged$Region+merged$`Starting Median Salary.x`,data = merged)

summary_data <- summary(my_model)

### Extract Coefficients, P-Values and R-Squared ###

# Extract coefficients, p-values, and R-squared
coefficients <- coef(summary_data)
pvalues <- coefficients[, "Pr(>|t|)"]
r_squared <- summary_data$r.squared

# Round all numbers to the third decimal place
coefficients <- round(coefficients, 2)
pvalues <- round(pvalues, 2)
r_squared <- round(r_squared, 2)

### Creating Data Frames ###

# Create a data frame for coefficients and p-values
coeff_pvalue_df <- data.frame(
  Coefficients = coefficients[, 1],
  P_Values = pvalues,
  Significance = ifelse(pvalues <= 0.001, '***', ifelse(pvalues <= 0.01, '**', ifelse(pvalues <= 0.05, '*', '.')))
)

# Create a data frame for R-squared
r_squared_df <- data.frame(
  R_Squared = r_squared
)

### Tabulated Information ###

# Display the tabulated information using knitr::kable
cat("Coefficients and P-Values:\n")
knitr::kable(coeff_pvalue_df, format = "markdown")

cat("\nR-Squared:\n")
knitr::kable(r_squared_df, format = "markdown")
