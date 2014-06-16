# Load packages
# Basic Utilities: describe
library(psych)
# Scatter plot & linear model regression line plot
library(ggplot2)
# multilevel: sobel test for indirect effect
library(multilevel)

# Read data into a dataframe called DHE: Diversity, Happiness, Extraversion
DHE <- read.table("quiz7.txt", header = T)

# TABLE
# Predictors:
# 1. Extraversion
# 2. Diversity of Life Experiences
# Outcome: Happiness
#
# diverse	happy	 extra
# 2 (1..4)      1 (1..5) 3
# Note that all are mapped to non-nominal (akin to continuous)
# variables as many of the attributes have been ranked in an order
#

# Question 1: What is the correlation between extraversion and happiness?
round(cor(DHE[1:3]), 2)
      
# Question 2: What is the correlation between extraversion and diversity
# of life experience?

# Question 3: What is the correlation between diversity of life
# experience and happiness?

# Question 4: What percentage of variance in happiness is explained
# by extraversion?
modelH.E <- lm(DHE$happy ~ DHE$extra)
summary(modelH.E)
round(confint(modelH.E), 2)

# Question 5: What percentage of variance in happiness is explained
# by a model with both extraversion and diversity of life experience
# as predictors?
modelH.DE <- lm(DHE$happy ~ DHE$extra + DHE$diverse)
summary(modelH.DE)
round(confint(modelH.DE), 2)

# Question 6: What is the 95% confidence interval for the regression
# coefficient for extraversion when it is the only predictor of happiness?

# Question 7: What is the 95% confidence interval for the regression
# coefficient for extraversion when it and diversity of life experience
# are both predictors of happiness?

# Question 8: What is the unstandardized regression estimate of the
# indirect effect?
#
# Assuming the Extraversion leads to Diversity and Diversity leads
# to happiness: let us also calculate the correlation between
# Diversity and Extraversion
modelD.E <- lm(DHE$diverse ~ DHE$extra)
summary(modelD.E)
round(confint(modelD.E), 2)

# Now run a sobel test: gives Indirect effect & z estimate
model.ALL <- sobel(DHE$extra, DHE$diverse, DHE$happy) 
model.ALL

# Question 9: What is the z-value of the Sobel test?

# Question 10: Do these analyses suggest full mediation, partial mediation,
# or no mediation?

