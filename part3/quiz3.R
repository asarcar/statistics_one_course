#!/usr/bin/env Rscript

# Quiz 3: Arijit Ory Sarcar
# Header: id	cond	S1.pre	S2.pre	S1.post	S2.post	V1.pre	V2.pre	V1.post	V2.post
# Columns:
# 1.    Id:             Factor
# 2.    cond:           aer (Aerobic Training), des (Designed Sport Training)
# 3-6:  S1/2-pre/post:  Spatial Reasoning Before/After Training - TestType 1/2
# 7-10: V1/2-pre/post:  Verbal Reasoning Before/After Training - TestType 1/2

# Load packages
library(psych)
library(gclus)
library(rgl)

# Read data into a dataframe called impact
cogtraining <- read.table("quiz3.txt", header = T) 

# If you want to view the data
edit(cogtraining)

# Summary statistics
describe(cogtraining) 

describeBy(cogtraining, cogtraining$cond)

# Q1: What is the correlation between S1 and S2 pre-training?
round(cor(cogtraining[3:4]), 2)

# Q2: What is the correlation between V1 and V2 pre-training?
round(cor(cogtraining[7:8]), 2) 

# Q3: With respect to the measurement of two distinct constructs,
# spatial reasoning and verbal reasoning, the pattern of correlations
# pre-training reveals:
# Find correlation of S1 vs V1/2 & S2 vs V1/2
# Convergent Validity: S1 or V1 should be correlated to S2 or V2 respectively
# Divergent Validity: S1/2 should have close to zero correlation with V1/2
# Get the correlation matrix: Columns 3 to 10 contain the 8 baseline measures
round(cor(cogtraining[3:10]), 2)

# Q4: Correlations from the control group could be used to estimate
# test/retest reliability. If so, which test is most reliable?
# Test/ReTest: The correlation between X1 and X2 is an estimate of reliability
# pre/post are approximated as test/retest?

# 1. Create two subsets, aercog and descog
aercog <- subset(cogtraining, cogtraining$cond =="aer")
descog <- subset(cogtraining, cogtraining$cond =="des")

# 2. Find with S1/2 or V1/2 pre/post has the maximum correlation
round(cor(aercog$S1.pre, aercog$S1.post), 2)
round(cor(aercog$S2.pre, aercog$S2.post), 2)
round(cor(aercog$V1.pre, aercog$V1.post), 2)
round(cor(aercog$V2.pre, aercog$V2.post), 2)
# Choose the one with maximum correlation

# Q5: Does there appear to be a correlation between spatial
# reasoning before training and the amount of improvement in
# spatial reasoning?
# 1. Find correlation of S1-pre with S1-imp
cogtraining$S1.imp <- (cogtraining$S1.post - cogtraining$S1.pre)
round(cor(cogtraining$S1.imp, cogtraining$S1.pre), 2)

# 2. Find correlation of S2-pre with S2-imp
cogtraining$S2.imp <- (cogtraining$S2.post - cogtraining$S2.pre)
round(cor(cogtraining$S2.imp, cogtraining$S2.pre), 2)

# 3. Find correlation of avg(S1/2-pre) with avg S1/2 imp
cogtraining$S.imp <- (cogtraining$S1.imp + cogtraining$S2.imp)/2
# Find average spatial reasoning score before training
cogtraining$S.pre <- (cogtraining$S1.pre + cogtraining$S2.pre)/2
round(cor(cogtraining$S.imp, cogtraining$S.pre), 2)

# Q6: Does there appear to be a correlation between verbal reasoning
# before training and the amount of improvement in verbal reasoning?
# 1. Find correlation of V1-pre and V1-imp
cogtraining$V1.imp <- (cogtraining$V1.post - cogtraining$V1.pre)
round(cor(cogtraining$V1.imp, cogtraining$V1.pre), 2)

# 2. Find correlation of V2-pre and V2-imp
cogtraining$V2.imp <- (cogtraining$V2.post - cogtraining$V2.pre)
round(cor(cogtraining$V2.imp, cogtraining$V2.pre), 2)

# 3. Find correlation of avg(V1/2-pre) with avg S1/2 imp
cogtraining$V.imp <- (cogtraining$V1.imp + cogtraining$V2.imp)/2
# Find average verbal reasoning score before training
cogtraining$V.pre <- (cogtraining$V1.pre + cogtraining$V2.pre)/2
round(cor(cogtraining$V.imp, cogtraining$V.pre), 2)

# Q7: Which group exhibited more improvement in spatial reasoning?
mean(aercog$S.imp)
var(aercog$S.imp)
mean(descog$S.imp)
var(descog$S.imp)

# Q8: Create a color scatterplot matrix for all 4 measures at pre-test.
# Do the scatterplots suggest two reliable and valid constructs?
# Color scatterplot matrix, colored and ordered by magnitude of r
premeasures <- data.frame(cogtraining$S1.pre, cogtraining$S2.pre, cogtraining$V1.pre, cogtraining$V2.pre)
premeasures.r <- abs(cor(premeasures))
premeasures.color <- dmat.color(premeasures.r)
premeasures.order <- order.single(premeasures.r) 
cpairs(premeasures, premeasures.order, panel.colors = premeasures.color, gap = .5,
       main = "Pre-Test Variables Correlation")

# Q9: Create a color scatterplot matrix for all 4 measures at post-test.
# Do the scatterplots suggest two reliable and valid constructs?
postmeasures <- data.frame(cogtraining$S1.post, cogtraining$S2.post, cogtraining$V1.post, cogtraining$V2.post)
postmeasures.r <- abs(cor(postmeasures))
postmeasures.color <- dmat.color(postmeasures.r)
postmeasures.order <- order.single(postmeasures.r) 
cpairs(postmeasures, postmeasures.order, panel.colors = postmeasures.color, gap = .5,
       main = "Post-Test Variables Correlation")

# Q10: What is the major change from pre-test to post-test visible on
# the color matrix?
# Using Table: Ory Added
pairs(premeasures, labels = c("S1-Pre", "S2-Pre", "V1-Pre", "V2-Pre"), main = "Correlation of Pre Ability", upper.panel = NULL)
pairs(postmeasures, labels = c("S1-Post", "S2-Post", "V1-Post", "V2-Post"), main = "Correlation of Post Ability", upper.panel = NULL)


