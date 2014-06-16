#!/usr/bin/env Rscript
# Statistics Quiz 2 

# Load packages
library(psych)
library(sm)

# Read data into a dataframe called impact
cogimp <- read.table("quiz2.txt", header = T) 

# Get the dimensions of the dataframe
dim(cogimp)
# Object types
class(cogimp)
names(cogimp) 
 
class(cogimp$subject)
class(cogimp$condition)
class(cogimp$time)
class(cogimp$SR)

# Convert subject to class factor: categorical values
cogimp$subject <- factor(cogimp$subject)
class(cogimp$subject)

# Summary statistics: mean of SR
mean(cogimp$SR) 
sd(cogimp$SR)

describe(cogimp)

# Describe when categorized by time (pre/post)
describeBy(cogimp, cogimp$time)

# Subsetting: Based on test
pretest <- subset(cogimp, cogimp$time=="pre")
describe(pretest)

# Subset Pre Tables Further: Based on Condition WM/PE/DS
wmpretest <- subset(pretest, pretest$condition=="WM")
pepretest <- subset(pretest, pretest$condition=="PE")
dspretest <- subset(pretest, pretest$condition=="DS")
describe(wmpretest)
describe(pepretest)
describe(dspretest)
# Pre Stats Grouped by Condition: compare with previous table to ensure correctness
describeBy(pretest, pretest$condition)

posttest <- subset(cogimp, cogimp$time=="post")
describe(posttest)

# Subset Post Tables Further: Based on Condition WM/PE/DS
wmposttest <- subset(posttest, posttest$condition=="WM")
peposttest <- subset(posttest, posttest$condition=="PE")
dsposttest <- subset(posttest, posttest$condition=="DS")
describe(wmposttest)
describe(peposttest)
describe(dsposttest)
# Post Stats Grouped by Condition: compare with previous table to ensure correctness
describeBy(posttest, posttest$condition)

# Which Group best approximates a normal distribution?
par(mfrow = c(2,3)) # To view pre/post (2 rows) for WM/PE/DS (3 cols)
plot(density(wmpretest$SR), xlab = "WM Pre Test", main = "")
plot(density(pepretest$SR), xlab = "PE Pre Test", main = "")
plot(density(dspretest$SR), xlab = "DS Pre Test", main = "")
plot(density(wmposttest$SR), xlab = "WM Post Test", main = "")
plot(density(peposttest$SR), xlab = "PE Post Test", main = "")
plot(density(dsposttest$SR), xlab = "DS Post Test", main = "")

# Plot in histogram to observe better: Moral Histograms provide finer granularity of observation
par(mfrow = c(2,3)) # To view pre/post (2 rows) for WM/PE/DS (3 cols)
hist(wmpretest$SR, xlab = "WM Pre Test", main = "")
hist(pepretest$SR, xlab = "PE Pre Test", main = "")
hist(dspretest$SR, xlab = "DS Pre Test", main = "")
hist(wmposttest$SR, xlab = "WM Post Test", main = "")
hist(peposttest$SR, xlab = "PE Post Test", main = "")
hist(dsposttest$SR, xlab = "DS Post Test", main = "")

# Which Group showed the biggest gains in SR?
# Subset Tables Based on Condition WM/PE/DS
wmtest <- subset(cogimp, cogimp$condition=="WM")
petest <- subset(cogimp, cogimp$condition=="PE")
dstest <- subset(cogimp, cogimp$condition=="DS")
describe(wmtest)
describe(petest)
describe(dstest)
# Cognition Test Stats Grouped by Condition: compare with previous table to ensure correctness
describeBy(cogimp, cogimp$condition)


# Compare density plots
par(mfrow = c(3,1))
sm.density.compare(wmtest$SR, wmtest$time, xlab = "WM Pre/Post SR")               
sm.density.compare(petest$SR, petest$time, xlab = "PE Pre/Post SR")               
sm.density.compare(dstest$SR, dstest$time, xlab = "DS Pre/Post SR")               



