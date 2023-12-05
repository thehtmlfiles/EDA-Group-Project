## Dataset 1 - Salary ##

## Open and Review dataset##
read.csv("salary_data_cleaned.csv")
salary <- read.csv("salary_data_cleaned.csv")
View(salary)
summary(salary)

## Color Palettes ##
install.packages("vangogh")
library("vangogh")
vangoghcolor <- vangogh_palette("CafeTerrace")
vangoghcolor2 <- vangogh_palette("Chaise")

## Data Cleaning ##
# Removing row with a non-state under a state variable #
row_to_remove <- 127
salary_filtered <- salary[-row_to_remove, ]

# Removing row with invalid data #
row_to_remove2 <- 582
salary_filtered <- salary[-row_to_remove2, ]

## Plots ##
# Box Plot for Average Salary by State #
boxplot(salary_filtered$avg_salary ~ salary_filtered$job_state, main = 'Average Salary by State', xlab = 'States', ylab = 'Average Salary (in thousands)', cex.axis = 0.6, col = vangoghcolor)

# Box Plot for Average Salary by Different Types of Ownership #
boxplot(salary_filtered$avg_salary ~ salary_filtered$Type.of.ownership, main = 'Average Salary by Different Types of Ownership', xlab = 'Types of Ownership', ylab = 'Average Salary (in thousands)', cex.axis = 0.5, col = vangoghcolor2)

# Bar Plot for Average Salary for R users and non-R users #
avg_salary_by_R <- tapply(salary_filtered$avg_salary, salary_filtered$R_yn, mean)
barplot(avg_salary_by_R, names.arg = c('R users', 'non-R users'), col = c('darkolivegreen3', 'coral2'), main = 'Average Salary by Knowledge of R', ylab = 'Average Salary (in thousands)', ylim = c(0, max(avg_salary_by_R) + 5))

# Bar Plot for Average Salary for Python users and non-Python users #
avg_salary_by_Python <- tapply(salary_filtered$avg_salary, salary_filtered$python_yn, mean)avg_salary_by_Python <- tapply(salary_filtered$avg_salary, salary_filtered$python_yn, mean)
barplot(avg_salary_by_Python, names.arg = c('non-Python users', 'Python users'), col = c('darkolivegreen3', 'coral2'), main = 'Average Salary by Knowledge of Python', ylab = 'Average Salary (in thousands)', ylim = c(0, max(avg_salary_by_Python) + 5))

#########################################################

## Dataset 2 - Income ##

## Open and Review dataset ##
read.csv("income.csv")
income <- read.csv("income.csv")
View(income)
summary(income)


# Bar Plot for Income Distribution for Education Level #

educationdata <- table(income$income, income$education)
barplot(educationdata, beside = TRUE, col = c("darkblue", "darkgreen"), main = "Income Distribution per Education Level", xlab = "Level of Education",ylab = "Count",legend.text = rownames(educationdata),args.legend = list(x = "topleft"), cex.names = 0.7)







