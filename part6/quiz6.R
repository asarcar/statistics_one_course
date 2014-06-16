# Table Format
# Outcome  Predictor Variables
# -------  ---------------------------------------------------
# USD #    #        #      Categorical (teacher/lawyer/doctor)
# salary   years    courses  profession
# 67483    5.8      13       "teacher"

# Load packages
library(psych)

# Read data into a dataframe called PS (Professional Salary)
PS <- read.table("quiz6.txt", header = T)

PS$ID <- factor(PS$ID)

attributes(PS)

# Summary statistics
describe(PS) 
# Correlation analysis 
cor(PS[2:4]) 

# Question 1: In a model predicting salary, what is the
# unstandardized regression coefficient for years,
# assuming years is the only predictor variable in the model?
mY <- lm(PS$salary ~ PS$years)
summary(mY)

# Question 2: In a model predicting salary, what is the 95%
# confidence interval for the unstandardized regression
# coefficient for years, assuming years is the only predictor
# variable in the model?
round(confint(mY), 2)

# Question 3: In a model predicting salary, what is the
# unstandardized regression coefficient for years,
# assuming years and courses are both included as predictor
# variables in the model?
mYC <- lm(PS$salary ~ PS$years + PS$courses)
summary(mYC)

# Question 4: In a model predicting salary, what is the
# 95% confidence interval for the unstandardized regression
# coefficient for years, assuming years and courses are
# both included as predictor variables in the model?
round(confint(mYC), 2)

# Question 5: What is the predicted difference in salary
# between Doctors and Lawyers assuming an equal and
# average number of years and courses?
#
# Include the categorical variable: Profession
# We use contrasts
p.code <- C(PS$profession, treatment)

mYCP <- lm(PS$salary ~ PS$years + PS$courses + (p.code))
summary(mYCP)
confint(mYCP)
#
# Observe the difference in the estimate (Doctors is 0 as it
# has been referenced accordingly by the Contrast (C) function

# Question 6: Is the predicted difference between Doctors and
# Lawyers statistically significant?
#
# Observe the t-value and Pr(>|t|) and make a judgment call

# Question 7: What is the predicted difference in salary between
# Doctors and Teachers assuming an equal and average number of
# years and courses?
#
# Observe the t-value and Pr(>|t|) and make a judgment call

# Question 8: Is the predicted difference between Doctors and
# Teachers statistically significant?
#
# Observe the t-value and Pr(>|t|) and make a judgment call

# Question 9: What is the actual difference in mean salary
# between Doctors and Teachers?
round(tapply(PS$salary, PS$profession, mean), 2)

# Question 10: What combination of predictors represents the
# best model in terms of predicting salary?
# Years and courses, Years and profession, Courses and profession
# Years, courses, and profession
#
# We do a linear Model of all of those presented and see which one
# comes out best using the anova model as well to ensure the
# difference is statistically significant
# We have mY, mYC, mYCP
mYP <- lm(PS$salary ~ PS$years + (p.code))
mCP <- lm(PS$salary ~ PS$courses + (p.code))

summary(mYC)
summary(mYP)
summary(mCP)
summary(mYCP)

# YC
# (Intercept) 31362.90    2460.46  12.747  < 2e-16 ***
# PS$years     4806.65     337.86  14.227  < 2e-16 ***
# PS$courses    435.62      59.09   7.373 4.52e-12 ***
# Residual standard error: 6619 on 197 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6476 
# F-statistic: 183.8 on 2 and 197 DF,  p-value: < 2.2e-16
# YP
# (Intercept)    63937.5     2866.4  22.305  < 2e-16 ***
# PS$years        2694.7      321.7   8.376 1.04e-14 ***
# p.codelawyer  -10535.0      938.1 -11.230  < 2e-16 ***
# p.codeteacher -17971.5     1258.3 -14.282  < 2e-16 ***
# Residual standard error: 5182 on 196 degrees of freedom
# Multiple R-squared:  0.7872,	Adjusted R-squared:  0.7839 
# F-statistic: 241.7 on 3 and 196 DF,  p-value: < 2.2e-16
# CP
# PS$courses       234.10      55.56   4.213 3.84e-05 ***
# p.codelawyer  -12310.44    1017.14 -12.103  < 2e-16 ***
# p.codeteacher -22168.80    1234.97 -17.951  < 2e-16 ***
# Residual standard error: 5783 on 196 degrees of freedom
# Multiple R-squared:  0.735,	Adjusted R-squared:  0.731 
# F-statistic: 181.2 on 3 and 196 DF,  p-value: < 2.2e-16
# YCP
# (Intercept)    59573.76    2901.53  20.532  < 2e-16 ***
# PS$years        2627.15     307.33   8.548 3.60e-15 ***
# PS$courses       214.26      47.57   4.504 1.14e-05 ***
# p.codelawyer   -9204.13     942.59  -9.765  < 2e-16 ***
# p.codeteacher -15902.77    1285.47 -12.371  < 2e-16 ***
# Residual standard error: 4945 on 195 degrees of freedom
# Multiple R-squared:  0.8073,	Adjusted R-squared:  0.8033 
# F-statistic: 204.2 on 4 and 195 DF,  p-value: < 2.2e-16
#
# Data Shows Years, Courses, and Profession is the "best"
# indicator:
# 1. Highest R2
# 2. Least Residual Standard Error
# 3. All contributing factors have very low Pr (>|t|)
#    We can ignore the NULL hypothesis
#
# We can cross check with anova
anova(mYC, mYCP)
anova(mYP, mYCP)
anova(mCP, mYCP)

# mYC vs mYCP
# F 78.976 Pr < 2.2e-16 ***
#
# mYP vs mYCP
# F 20.29 Pr 1.145e-05 ***
# mCP vs mYCP
# F 73.071 Pr 3.605e-15 ***

