# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

#
# TABLE FORMAT
#         WM,PE,DS  pre/post
# subject condition time      SR  
# 1	  WM	    pre       11  
#
# Objective: Study the impact of training conditions & pre/post training
# on Spatial Reasoning.
# IV: 
# A:  Training Conditions
# 1. Working Memory training (WM)
# 2. Physical Exercise, and  (PE)
# 3. Designed Sport          (DS)
# B:  Time When Measured (Pre/Post Training)
# DV: Spatial Reasoning (SR)
# 
# Read data into a dataframe called SR (spatial reasoning)
SR = read.table("quiz8.txt", header = T)

# Separate out pre and post
SR.pre = subset(SR, time == "pre")
SR.pre
SR.post = subset(SR, time == "post")
SR.post
SR.both <- SR.pre
SR.both$SR2 <- SR.post$SR
SR.both$gain <- SR.both$SR2 - SR.both$SR
SR.both

# If you want to view the data
View(SR.both)

# Summary statistics by all groups (WM, PE, DS)
# (Working Memory training, Physical Exercise, and Designed Sport)
describeBy(SR.both, SR.both$condition)

# Question 1: Using a dependent t-test, is the difference between
# pre and post-test scores significant?
#
# Conduct t test and cohen's D for dependent t-test
t.test(SR.both$SR2, SR.both$SR, paired = T)
# t.test(SR$SR, SR$time, paired=T) # does not work
# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores
cohensD(SR.both$SR2, SR.both$SR, method="paired")

# Question 2: Create subsets for each training condition.
# Which group shows no difference between pre and post-test scores?
# WM, PE, DS, or None
SR.both.wm <- subset(SR.both, condition == "WM")
SR.both.wm
SR.both.pe <- subset(SR.both, condition == "PE")
SR.both.pe
SR.both.ds <- subset(SR.both, condition == "DS")
SR.both.ds

# t-test and CohensD test for WM/PE/DS
t.test(SR.both.wm$SR2, SR.both.wm$SR, paired = T)
cohensD(SR.both.wm$SR2, SR.both.wm$SR, method="paired")
t.test(SR.both.pe$SR2, SR.both.pe$SR, paired = T)
cohensD(SR.both.pe$SR2, SR.both.pe$SR, method="paired")
t.test(SR.both.ds$SR2, SR.both.ds$SR, paired = T)
cohensD(SR.both.ds$SR2, SR.both.ds$SR, method="paired")

# Question 3: Which training group shows the largest effect size
# for the difference pre-test to post-test?
# WM, PE, DS
# Examine cohensD values

# Question 4: Reshape the data into a wide format, and create a
# new variable for gain score. Now subset the new dataframe
# based on the training conditions. Which comparison between
# training conditions does not show a significant difference?
# WM - PE, PE - DS, DS - WM, None
#
# Independent t-test
#
SR.both.wm.pe <- subset(SR.both, condition %in% c("WM", "PE"))
SR.both.wm.pe
SR.both.pe.ds <- subset(SR.both, condition %in% c("PE", "DS"))
SR.both.pe.ds
SR.both.ds.wm <- subset(SR.both, condition %in% c("DS", "WM"))
SR.both.ds.wm
t.test(SR.both.wm.pe$gain ~ SR.both.wm.pe$condition, var.equal = T)
S
t.test(SR.both.pe.ds$gain ~ SR.both.pe.ds$condition, var.equal = T)
t.test(SR.both.ds.wm$gain ~ SR.both.ds.wm$condition, var.equal = T)

# Question 5: To compare the gain scores across all groups,
# we now turn to ANOVA. Is the homogeneity of variance
# assumption violated?
# Yes, No
#
# Conduct leveneTest
leveneTest(SR.both$gain, SR.both$condition, center="mean")
leveneTest(SR.both$gain, SR.both$condition)

# Question 6: Run an ANOVA model on the gain scores as a
# function of training condition. Is the effect of
# condition significant?
# Yes, No

aov.model = aov(SR.both$gain ~ SR.both$condition)
summary(aov.model)

# Save results in a table to illustrate calculation of effect size
aov.table = summary(aov.model)
aov.table

# Question 7: What is the corresponding eta-squared value?
# (round to 2 decimal places) Answer for Question 7
#
# Effect size for ANOVA
# How much of the variance in gain is explained by training manipulation
round(etaSquared(aov.model, anova=T), 2)

# Question 8: Are the eta-squared and partial eta-squared
# value different in this case?
# Yes, No

# Question 9: Let's now run post-hoc comparisons (Tukey HSD).
# Which two groups do not significantly differ from one
# another when considering gain scores?
# WM and PE, PE and DS, DS and WM
#
# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)

# Question 10: Based on these data, which training condition
# should you choose to target some improvements in spatial reasoning?
