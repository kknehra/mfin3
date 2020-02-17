#### SETUP ####

library(data.table) # Enhanced data frame support
library(readxl) # Excel file handling
library(ggplot2) # Data visualisation
library(moments) # Adds probability generating function moments - i.e. skewness and kurtosis
library(sandwich) # Robust statistics library
library(lmtest) # Linear model methods
library(aod) # Linear model testing (contains the wald.test method we use)
library(GGally) # Pairwise scatter plots

# Prints out the data type of each column in a data.table
options(datatable.print.class = TRUE)

# Root path for the lab
# Change this for the root *FOLDER* where your data is located
root <- 'path_to_my_root_folder/'

# Specify the path to the data
ceosal2_path <- paste0(root, 'ceosal2.xlsx')

# Load the data
ceosal2_dt <- data.table(read_xlsx(ceosal2_path))

#### Exercise 7: Multiple Linear Regression ####

## (i) To begin, review the descriptive statistics for salary, sales, mktval, and ceoten. Review the histograms for these variables.

for (var in c('SALARY', 'SALES', 'MKTVAL', 'CEOTEN')) {
	
	# Print summary statistics
	print(summary(ceosal2_dt[, get(var)]))
	
	# Compute a plot with a normal-density overlay
	ggp <- ggplot(ceosal2_dt, aes_string(x = var)) +
		# Data histogram
		geom_histogram(aes(y=..density..), fill = 'cyan', color = 'black', alpha = 0.3) +
		# Parametised normal distribution on top!
		stat_function(fun = dnorm, color = 'red', args = list(mean = mean(ceosal2_dt[, get(var)]), sd = sd(ceosal2_dt[, get(var)])))
	
	# Print the plot
	print(ggp)
}

## (ii) Generate scatterplots for every variable against every other
ggpairs(ceosal2_dt)

## (iii) Estimate a model relating annual salary to firm sales (always include the constant, c). Interpret the slope parameter, and test if it is significant (significantly different from zero).

model_iii <- lm(SALARY ~ SALES, data = ceosal2_dt)

# An increase of $1 million in sales will increase salary by $37
# P-Value approx zero so significant at 1% level
summary(model_iii)

## (iv) Re-specify the model, now considering mktval a second additional regressor. Estimate and compare results with the results of your model in (i). How do you interpret this model?

model_iv <- lm(SALARY ~ SALES + MKTVAL, data = ceosal2_dt)
	
# An increase of $1 million in sales will increase salary by $17 ceteris paribus
# P-Value of SALES not significant at the 5% level (it's 10.5%)
# An increase of $1 million in market value will increase salary by $25 ceteris paribus
# P-Value of MKTVAL is significant at 1% level
summary(model_iv)

# When sales and market value is zero (the intercept) this is generally considered to be meaningless

# We can see that the correlation (check the pair plot) between sales and market value is 0.755 so this explains why the regressor of SALES moves down quite a bit in the multiple regression - in the single regression, sales are sucking in some of the effect of the market value This works vice-versa too

## (v) How much of the variation in CEO salaries is explained by these two firm performance variables employed as regressors?

# The R^2 is 0.1777 which means that 17.8% of the variation of the salary is explained by these variables Therefore 82.2% of the variation is left unexplained
summary(model_iv)

## (vi) Add the variable ceoten as a third regressor to the model in part (ii). What is the estimated change in CEO salary for an additional year of CEO tenure, holding other regressors fixed?

model_iv <- lm(SALARY ~ SALES + MKTVAL + CEOTEN, data = ceosal2_dt)

# Holding other variables fixed, having an extra year as CEO increases salary by $12,700 ceteris paribus
summary(model_iv)

## (vii) What is the estimated increase in CEO salary for additional sales of $1 million, ceteris paribus?

# Holding other variables fixed, salary increases $19 per $1 million of sales, ceteris paribus
summary(model_iv)

# (viii) Find the expected salary of a CEO with tenure of 10 years, working in a company with sales of $1 billion, and market value of $2 billion.

# Equation is SALARY = 613.4 + 0.01902 SALES + 0.02340 MKTVAL + 12.70 CEOTEN
# Therefore SALARY = 1000 * (613.4 + 0.01902 * 1000 + 0.02340 * 2000 + 12.70 * 10) = $806,220
summary(model_iv)

#### Exercise 8: Heteroskedasticity-robust standard errors ####

## (i) Regress salary on sales, mktval and ceoten, including a constant, and using heteroskedasticityrobust standard errors. Review any difference between the output and the output with ordinary (homoscedasticity based) covariance method.

# Construct a standard linear model
model_8 <- lm(SALARY ~ SALES + MKTVAL + CEOTEN, data = ceosal2_dt)

# Use Heteroscedasticity-consistent standard errors - i.e. HC1 for the covariance matrix - this will change use robust estimation of the dependent variables and therefore change significance levels
model_8_vcv <- vcovHC(model_8, type = 'HC1')

# The model with standard errors
summary(model_8)

# The model with robust errors
coeftest(model_8, vcov. = model_8_vcv)

# Sales is a significant variable under robust error and not significant otherwise
# Becuase the standard errors are smaller in the robust estimate, this indicates that the dispersion is highest around the mean of the each of the independent variables where this is the case

#### Exercise 9: Heteroskedasticity-robust standard errors ####

## (i) Use the model you have estimated robustly for annual CEO salary in terms of firm sales, firm market value and CEO tenure. Are the coefficient estimates significant?

coeftest(model_8, vcov. = model_8_vcv)

# We reject NULL hypothesis for all variables, SALES at 5%, MKTVAL at 1%, CEOTEN at 5%

## (ii) Explore different ways of testing the hypothesis that the (population) slope coefficient on sales equals zero at the 10% significance level.

coeftest(model_8, vcov. = model_8_vcv)

# Sales passes at 0.01152 < 0.1 therefore significant at the 10% level

## (iii) Test the hypothesis that the (population) slope coefficient on CEO tenure is equal to 10

wald.test(Sigma = model_8_vcv, model_8$coefficients, Terms = 4, H0 = 10, df = model_8$df.residual)

## (iv) Calculate the 95% confidence interval for the slopes of sales, mktval, ceoten.

# The range of the coefficient with the requisite percentage change of being in said range
coefci(model_8, vcov = model_8_vcv, level = 0.90)
coefci(model_8, vcov = model_8_vcv, level = 0.95)
coefci(model_8, vcov = model_8_vcv, level = 0.99)













