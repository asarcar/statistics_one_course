# Load packages
library(psych)
library(aod)
library(QuantPsyc)

# Read the data into a dataframe called GW: Global Warming
GW <- read.table("quiz10.txt", header = T)

# Summary statistics
describe(GW)
describeBy(GW$age, GW$change=="1")

#
# Table:
# DV: Change (1 = act now, 0 = wait and see)
# IV:
# age:  Median Age
# educ: Education Index
# gdp:  Gross Domestic Product
# co2:  Carbon Dioxide Emissions
# ---
#     "country" "change" "age" "educ" "gdp" "co2"
# "1" "England" 1        40.5  0.65   2.4   522
#


# Question 1: What is the median population age for the countries
# which voted to take action against global warming?
# (round to 2 decimal places)
GW.actnow <- subset(GW, GW$change == "1")
round(median(GW.actnow$age), 2)

# Question 2: Run a logistic regression including all predictor
# variables. Which predictors are significant in this model?
# educ and age; educ, age, gdp; gdp, age, co2; all of them
# Binary logistic regression
gw.logit <- glm(GW$change ~ GW$age + GW$educ + GW$gdp + GW$co2, family = binomial)
summary(gw.logit)

# Question 3: What does the negative value for the estimate
# of educ means?
# Countries with a lower education index score are more likely to chose to act now
# Countries with a higher education index score are more likely to chose to wait and see
# Educ and change are negatively correlated
# All of the above

# Question 4: What is the confidence interval for educ, using profiled
# log-likelihood? (round to 2 decimal places, and give the lower bound
# first and the upper bound second, separated by a space)
# CIs using profiled log-likelihood (default for logistic models)
round(confint(gw.logit), 2) 
 
# Question 5: What is the confidence interval for age, using standard
# errors? (round to 2 decimal places, and give the lower bound first
# and the upper bound second, separated by a space)
# CIs using standard errors
round(confint.default(gw.logit), 2)

# Question 6: Compare the present model with a null model. What is the
# difference in deviance for the two models? (round to 2 decimal places)
# Model fit: difference in deviance for the two models
round(with(gw.logit, null.deviance - deviance), 2) 

# Question 7: How many degrees of freedom are there for the difference
# between the two models?
# df for the difference between the two models
round(with(gw.logit, df.null - df.residual), 2)

# Question 8: Is the p-value for the difference between the two models
# significant? Yes, No?
with(gw.logit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) 

# Question 9: Do chi-squared values differ significantly if you drop
# educ as a predictor in the model? Yes or No 
# Wald tests:
# "country" "change" "age" "educ" "gdp" "co2"
wald.test(b = coef(gw.logit), Sigma = vcov(gw.logit), Terms = 3) # educ

# wald.test(b = coef(gw.logit), Sigma = vcov(gw.logit), Terms = 2) # age
# wald.test(b = coef(gw.logit), Sigma = vcov(gw.logit), Terms = 4) # gdp
# wald.test(b = coef(gw.logit), Sigma = vcov(gw.logit), Terms = 5) # co2
## Odds ratios: exponentiated coefficients
# exp(coef(gw.logit)) 

# Question 10: What is the percentage of cases that can be classified
# correctly based on our model?
# Classification table
ClassLog(gw.logit, GW$change)
