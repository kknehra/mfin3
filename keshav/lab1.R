#### SETUP ####

library(data.table) # Enhanced data frame support
library(readxl) # Excel file handling
library(ggplot2) # Data visualisation

# Prints out the data type of each column in a data.table
options(datatable.print.class = TRUE)

# Root path for the lab
# Change this for the root *FOLDER* where your data is located
root <- 'path_to_my_root_folder/'

# Specify the path to the data
ceosal1_path <- paste0(root, 'ceosal1.xlsx')

# Load the data
ceosal1_dt <- data.table(read_xlsx(ceosal1_path))

#### Exercise 6: Simple Linear Regression ####

## (i) Review summary statistics for the two series salary and roe
summary(ceosal1_dt[, SALARY])
summary(ceosal1_dt[, ROE])

## (ii) Generate a scatter plot of salary against roe and review
ggplot(ceosal1_dt, aes(x = ROE, y = SALARY)) + geom_point()

## (iii) Regress salary on roe
model <- lm(SALARY ~ ROE, data = ceosal1_dt)

## (iv) Write down the estimated model
summary(model)

# SALARY = 963.19 + 18.50 ROE

## (v) Interpret the intercept and slope estimates

# When ROE is zero, SALARY is $963,190 (as measured in USD thousands)
# The slope of ROE (which is mesured in percent) means that for each 1% increase in ROE, salary increases $18,501

## (vi) What is the predicted salary at ROE = 30

# SALARY = 963.19 + 18.50 * 30 = 1518.19
# Therefore salary will be $1,518,190
# This figure is the conditional mean salary, conditioned on ROE = 30

## (vii) How much of the variation in salary is explained by roe ? Does the model have a good fit?

# The R-Squared is 0.01319 which is 1.319% which means that the ROE only explains that percentage of the variation of the salary which is a poor fit. In this sample of 209 CEOs this shows very poor explanatory power and 98.7% of the variation is left unexplained

# (viii) Overlay the regression line on the scatter plot of salary against roe

# Plot ROE on the x axis and SALARY on the y axis
ggplot(ceosal1_dt, aes(x = ROE, y = SALARY)) +
	# Scatter point
	geom_point() +
	# Linear regression line
	geom_smooth(method = 'lm')




