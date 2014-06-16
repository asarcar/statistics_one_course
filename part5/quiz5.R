# Statistics One, 2013, Lab 5

#     "ID" "salary" "years" "courses"
# "2"  2    77204    7.4     18
# Example
#   A correlational study investigating predictors of salary in professionals
#     Outcome variable (Y) is salary
#     Predictors (X) are # of "years" and # of "courses" taken by the professional
#     Initial analyses assume a sample size of N = 200 
#     Analyses are then repeated with a sample size of N = 20

# Load packages
library(psych)
library(ggplot2)

# Read data into a dataframe called SFT (Salary Factor Table)
SFT <- read.table("quiz5.txt", header = T)

# Ory
SFT$ID <- factor(SFT$ID)

# Summary statistics
describe(SFT)
round(cor(SFT[2:4]), 2)
#         salary years courses
# salary    1.00  0.74    0.54
# years     0.74  1.00    0.33
# courses   0.54  0.33    1.00

# Q1: Run a regression model with salary as the outcome variable and
# years of experience as the predictor variable. What is the 95%
# confidence interval for the regression coefficient? Type your answer
# exactly as it appears in R but include only two decimal places
# (e.g., if the 95% confidence interval is -1 to +1 then type -1.00 1.00)
# linear fit of salary (outcome) against years of experience (predictor)
mod.s.y <- lm(SFT$salary ~ SFT$years) 
mod.s.y
sum.mod.s.y <- summary(mod.s.y)
sum.mod.s.y
# R2 = 0.5549; RSE: 7457 on 198 DF; F-statistic: 246.8 on 1 and 198 DF
int.mod.s.y <- round(confint(mod.s.y), 2)
int.mod.s.y
ggplot(SFT, aes(x = years, y = salary)) + geom_smooth(method = "lm") + geom_point()

# Q2: Run a regression model with salary as the outcome variable and
# courses as the predictor variable. What is the 95% confidence interval
# for the regression coefficient?
# linear fit of salary (outcome) against years of experience (predictor)
mod.s.c <- lm(SFT$salary ~ SFT$courses) 
mod.s.c
sum.mod.s.c <- summary(mod.s.c)
sum.mod.s.c
# R2 = 0.2927; RSE: 9400 on 198 DF; F-statistic: 81.94 on 1 and 198 DF
int.mod.s.c <- round(confint(mod.s.c), 2)
int.mod.s.c
ggplot(SFT, aes(x = courses, y = salary)) + geom_smooth(method = "lm") + geom_point()

# Q3: Run a multiple regression model with both predictors and compare it
# with both the model from Question 1 and the model from Question 2.
# linear fit of salary (outcome) against years of experience (predictor)
mod.s.y.c <- lm(SFT$salary ~ SFT$years + SFT$courses) 
mod.s.y.c
sum.mod.s.y.c <- summary(mod.s.y.c)
sum.mod.s.y.c
# R2 = .6511; RSE: 6619 on 197 DF; F-Statistic: 183.8 on 2 and 197 DF
int.mod.s.y.c <- round(confint(mod.s.y.c), 2)
int.mod.s.y.c
ggplot(SFT, aes(x = years + courses, y = salary)) + geom_smooth(method = "lm") + geom_point()

# NHST: 1. Model (salary = fn(years)) == Model (salary = fn(years, courses)
# Shows difference between model based on salary and years is statistically
# compared to model based on salary and (years as well as courses)
anova(mod.s.y, mod.s.y.c)
# NHST: 2. Model (salary = fn(courses)) == Model (salary = fn(years, courses)
# Shows difference between model based on salary and courses is statistically
# compared to model based on salary and (years as well as courses)
anova(mod.s.c, mod.s.y.c)

# See how the prediction of salary is fitting the salary data
SFT$pred.s.y.c <- fitted(mod.s.y.c) 
ggplot(SFT, aes(x = pred.s.y.c, y = salary)) + geom_smooth(method = "lm") + geom_point()

# Q4: Run a standardized multiple regression model with both predictors.
# Do the confidence interval values differ from the corresponding
# unstandardized model?
mod.std.s.y.c <- lm(scale(SFT$salary) ~ scale(SFT$years) + scale(SFT$courses))
mod.std.s.y.c
sum.mod.std.s.y.c <- summary(mod.std.s.y.c)
sum.mod.std.s.y.c
# R2 = .6511; RSE: .5936 on 197 DF; F-Statistic: 183.8 on 2 and 197 DF
int.mod.std.s.y.c <- round(confint(mod.std.s.y.c), 2)
int.mod.std.s.y.c

# Q5: What function could you use to take a random subset of the data?
# A: sample()

# Q6: Run the following command in R: set.seed(1). Now take a random subset of
# the original data so that N=15. Is the correlation coefficient between
# salary and years of experience in this sample higher or lower than in
# the whole data set?
set.seed(1)
SFT.15 <- SFT[sample(nrow(SFT), 15), ]
describe(SFT.15) 
# Correlation analysis 
round(cor(SFT.15[2:4]), 2) # Round to 2 decimal places 
# 
#         salary years courses
# salary    1.00  0.59    0.56
# years     0.59  1.00   -0.03
# courses   0.56 -0.03    1.00

# Q7: Take a subset of the original data from row 51 to 70. What is the %
# of variance explained by a multiple regression model with both predictors
# (Provide your result with no decimal place)
SFT.sub <- SFT[51:70, ]
SFT.sub
describe(SFT.sub)
mod2.s.y.c <- lm(SFT.sub$salary ~ SFT.sub$years + SFT.sub$courses)
mod2.s.y.c
sum.mod2.s.y.c <- summary(mod2.s.y.c)
sum.mod2.s.y.c
# R2 = .8509; RSE: 5584 on 17 DF; F-statistic: 48.49 on 2 and 17 DF, p-value: 9.451e-08

# Q8: Using model comparison, which model provides the best fit for the
# subsetted data from Question 7?
# 1. Salary from Years
mod2.s.y <- lm(SFT.sub$salary ~ SFT.sub$years)
mod2.s.y
sum.mod2.s.y <- summary(mod2.s.y)
sum.mod2.s.y
# R2 = .7198; RSE: 7438 on 18 DF; F-statistic: 46.25 on 1 and 18 DF; p-value: 2.281e-06
# 2. Salary from Courses
mod2.s.c <- lm(SFT.sub$salary ~ SFT.sub$courses)
mod2.s.c
sum.mod2.s.c <- summary(mod2.s.c)
sum.mod2.s.c
# R2 = .6245; RSE: 8611 on 18 DF; F-statistic: 29.94 on 1 and 18 DF; p-value: 3.386e-05

# Q9: What is the correlation between the salary values predicted by the multiple
# regression model and the actual salary scores in the subsetted data?
# (Provide your result rounded to 2 decimal places)
SFT.sub$pred.s.y.c <- fitted(mod2.s.y.c)
round(cor(SFT.sub$salary, SFT.sub$pred.s.y.c), 2)
round(cor(SFT.sub[2:7]), 2)

# Q10: For the subset data, compute the correlation between the scores predicted by
# the multiple regression model and the residuals from the same model. Is the
# correlation statistically significant?
SFT.sub$e.pred.s.y.c <- resid(mod2.s.y.c)
SFT.sub
cor.test(SFT.sub$salary, SFT.sub$e.pred.s.y.c)
# PPM C = 0.39; t = 1.78, df = 18, p-value = .09
# 95% confidence interval -.07 < cor < 0.71


