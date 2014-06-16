# Statistics One, 2013, Lab 4

# Example
# Salary (Outcome Variable) can be influenced by many variables.
# Among these, years of professional
# experience (Predictor 1) and total courses completed (Predictor 2)
# in college are critical.
# This week we
# test this hypothesis with a simulated dataset including an outcome variable,
# salary, and two predictors, years of experience and courses completed.
# Sample Size: 200
# Here are a few questions based on what was covered in the lectures and the lab. Have fun!

# Load packages
library(psych)

# Read data into a dataframe called SE (salary estimate)
SE <- read.table("quiz4.txt", header = T)

SE$ID <- factor(SE$ID)

# Summary statistics
describe(SE)

# Q1: What is the correlation between salary and years of experience?
# Q2: What is the correlation between salary and courses completed?
round(cor(SE[2:4]), 2) # Round to 2 decimal places 

# Q3: What is the percentage of variance explained in a regression
# model with salary as the outcome variable and professional experience
# as the predictor variable?
# A: Observe R2: % of variance explained
model.years <- lm(scale(SE$salary) ~ scale(SE$years))
summary(model.years)

# Q4: Compared to the model from Question 3, would a regression
# model predicting salary from the number of courses be considered
# a better fit to the data?
model.courses <- lm(scale(SE$salary) ~ scale(SE$courses))
summary(model.courses)

# Q5: Now let's include both predictors (years of professional
# experience and courses completed) in a regression model with
# salary as the outcome. Now what is the percentage of variance explained?
model.both <- lm(scale(SE$salary) ~ scale(SE$years) + scale(SE$courses))
summary(model.both)

# Q6: What is the standardized regression coefficient for years
# of professional experience, predicting salary?
# Q7: What is the standardized regression coefficient for courses
# completed, predicting salary?
# A: Should be same as correlation coefficient: Refer to data in A3/4

# Q8: What is the mean of the salary distribution predicted by the model
# including both years of professional experience and courses completed
# as predictors? (with 0 decimal places)
model.unstandardized.both <- lm(SE$salary ~ SE$years + SE$courses)
summary(model.unstandardized.both)

SE$predicted.salary <- fitted(model.unstandardized.both)
describe(SE$predicted.salary)

# Plot the graph to just validate
layout(matrix(c(1,1), 1, 1, byrow = TRUE))
plot(SE$salary ~ SE$predicted.salary, main = "Scatterplot", ylab = "Salary", xlab = "Predicted Salary")
abline(lm(SE$salary ~ SE$predicted.salary), col="blue")

# Q9: What is the mean of the residual distribution for the model
# predicting salary from both years of professional experience and
# courses completed? (with 0 decimal places)
SE$error.predicted.salary <- resid(model.unstandardized.both)
describe(SE$error.predicted.salary)

# Q10: Are the residuals from the regression model with both predictors
# normally distributed?
hist(SE$error.predicted.salary)
plot(density(SE$error.predicted.salary), xlab = "Error In Predicting Salary", main = "Prediction Error Histogram")
plot(SE$predicted.salary ~ SE$error.predicted.salary, main = "Predicted Salary vs Error in Prediction", xlab = "Error in Prediction", ylab = "Predicted Salary")
abline(lm(SE$predicted.salary ~ SE$error.predicted.salary), col="blue")


