## Load Packages 
# NOTE : Run base. R if these commands return an error 
library(readr)
library(dplyr)
library(haven)


# set working directory 
setwd("H:/STA215")


# if data 
raw_data <- read_delim("raw_data.csv")
data <- na.omit(raw_data)                  

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################

table(data$grades_offered)
table(data$absent_problem)


summary(data$state_aid)
sd(data$students_per_district)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
boxplot(data$students_per_district ~ data$absent_problem, data = data)
anova <- aov(students_per_district ~ absent_problem, data = data)
summary(anova)



##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$students_per_district, data$state_aid)
print(linear_plot)

# add x line and y line for means
meanx <- mean(data$students_per_district)
meany <- mean(data$state_aid)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(data$state_aid ~ students_per_district, data = data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$students_per_district, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$grades_offered,data$absent_problem)
chisq.test(data$grades_offered,data$absent_problem)








