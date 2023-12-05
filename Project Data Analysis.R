#################################################
## Project
## 
## 1. Summary Statistics
## 2. 
##
#################################################

##### Read File #####

dir()
getwd()
setwd("C:/Users/odesi/OneDrive/Desktop/Project")

##### Summary Statistics #####

## Unions as a Factor that Affects Salary ##

union <- read.csv("cpsaat43_clean2.csv", header=T)
summary(union)

# Dot Chart

dotchart(union$Members.of.unions, labels=union$Occupation.and.industry, cex=0.8,
	main= "Members of Union and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

dotchart(union$Represented.by.unions, labels=union$Occupation.and.industry, cex=0.8,
	main= "Represented by Unions and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

dotchart(union$Non.union, labels=union$Occupation.and.industry, cex=0.8,
	main= "Non Union and their Salary by Occupation and Industry",xlab= "Weekly Wage in U.S Dollars",
	ylab= "Occupation and Industry", pch = 19, col=c("#c44536", "#197278", "#5f0f40"))

# Boxplot

par(mfrow = c(1,3))	
boxplot(union$Members.of.unions, data= union, col="#c44536",
	main="Members of Union and their Salary", cex=0.4, ylab="Salary")
boxplot(union$Represented.by.unions, data=union, col="#197278", 
	main="Represented by Unions and their Salary", cex=0.4, ylab="Salary")
boxplot(union$Non.union, data=union, col= "#5f0f40",
	main="Non Union and their Salary", cex=0.4, ylab="Salary")

#Histogram

hist(union$Members.of.unions, col=rgb(1,0,0,0.25),
	ylim = c(0,50),main = "Frequency of Salaries by Union Affliation",				
	xlab = "Salary", xlim = c(300,2500))
hist(union$Represented.by.unions, col=rgb(0,0,1,0.25),add=T)
hist(union$Non.union, col=rgb(0,1,0,0.25),add=T)

legend("topright", c("Members of Unions", "Represented by Unions", "Non-union"), 
col=c(rgb(1,0,0,0.25),rgb(0,0,1,0.25), rgb(0,1,0,0.25)), lwd=10)

# Scatterplot Matrice

pairs(~Members.of.unions+Represented.by.unions+Non.union, data=union,
	main="Union Affliation Scatterplot Matrice")


