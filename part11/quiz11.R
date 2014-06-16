# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

#
# Recent theories in the field of cognitive enhancement suggest
# that people's belief about whether or not cognitive abilities
# can be improved influences the outcome of a training program.
# In this week's assignment, we take a look at a dataset
# including two different kinds of feedback given to participants
# in a cognitive training program, either fixed (cognitive
# abilities are innate and cannot be improved) or malleable
# (cognitive abilities are largely driven by experiences).
# DVs include verbal, spatial, and intelligence measures,
# provided before and after training.
#
# Table
# id	cond	verbal.pre	verbal.post	spatial.pre	spatial.post	intel.pre	intel.post
# 1     fixed   21              25              7               19              1               4
#

# Read data into a dataframe called cognitive enhancement
ce = read.table("quiz11.txt", header = T)
ce$pre <- ce$verbal.pre + ce$spatial.pre + ce$intel.pre
ce$post <- ce$verbal.post + ce$spatial.post + ce$intel.post
ce$gain <- ce$post - ce$pre
ce$verbal.gain <- ce$verbal.post - ce$verbal.pre
ce$spatial.gain <- ce$spatial.post - ce$spatial.pre
ce$intel.gain <- ce$intel.post - ce$intel.pre
edit(ce)
describeBy(ce, ce$cond)

# ---------------------
# Question 1: Using a t-test, compare verbal scores before and
# after training in the fixed condition. Is the difference pre-test
# to post-test significant?

ce.f = subset(ce, ce$cond == "fixed")
ce.m = subset(ce, ce$cond == "malleable")

ce.f.out = describe(ce.f)
ce.m.out = describe(ce.m)

t.test(ce.f$verbal.pre, ce.f$verbal.post, paired = T)

# Question 2: What are the degrees of freedom for the comparison between
# pre-test and post-test for the spatial scores?
t.test(ce.f$spatial.pre, ce.f$spatial.post, paired = T)

# Question 3: Run a Wilcoxon test for the same comparison (pre-test to
# post-test on spatial scores, fixed condition). Which of the two tests
# gives the highest p-value for the comparison?
wilcox.test(ce.f$spatial.pre, ce.f$spatial.post, paired = T)

# Question 4: What is the effect size (Cohens d) for the difference
# between pre-test and post-test spatial scores for the fixec condition?
# (round to two decimal places)
round(cohensD(ce.f$spatial.post, ce.f$spatial.pre, method="paired"), 2)

# Question 5: Which of the three tasks shows the largest improvements from
# pre-test to post-test, in the fixed condition?
# cohensD used to compare "largest"
round(cohensD(ce.f$verbal.post, ce.f$verbal.pre, method="paired"), 2)
round(cohensD(ce.f$spatial.post, ce.f$spatial.pre, method="paired"), 2)
round(cohensD(ce.f$intel.post, ce.f$intel.pre, method="paired"), 2)

# Question 6: Which of the three tasks shows the largest improvements from
# pre-test to post-test, in the malleable condition?
# cohensD used to compare "largest"
round(cohensD(ce.m$verbal.post, ce.m$verbal.pre, method="paired"), 2)
round(cohensD(ce.m$spatial.post, ce.m$spatial.pre, method="paired"), 2)
round(cohensD(ce.m$intel.post, ce.m$intel.pre, method="paired"), 2)

# Question 7: Conduct Mann-Whitney comparisons between all tasks at
# pre-test. Which task(s) differ significantly from the other two in
# pre-test scores?
wilcox.test(ce$spatial.pre, ce$verbal.pre, paired = F)
wilcox.test(ce$intel.pre, ce$spatial.pre, paired = F)
wilcox.test(ce$verbal.pre, ce$intel.pre, paired = F)
# Verbal when compared to both has lowest NHST probability of being same

# Question 8: Which feedback condition led to the largest improvements
# overall?
# cohensD used to compare "largest"
summary(ce.f$gain)
summary(ce.m$gain)
round(cohensD(ce.f$post, ce.f$pre, method="paired"), 2)
round(cohensD(ce.m$post, ce.m$pre, method="paired"), 2)

# Question 9: Which task is largely driving this effect?
# cohensD used to compare "largest"
summary(ce.f$verbal.gain)
summary(ce.f$spatial.gain)
summary(ce.f$intel.gain)
summary(ce.m$verbal.gain)
summary(ce.m$spatial.gain)
summary(ce.m$intel.gain)
round(cohensD(ce.f$verbal.post, ce.f$verbal.pre, method="paired"), 2)
round(cohensD(ce.f$spatial.post, ce.f$spatial.pre, method="paired"), 2)
round(cohensD(ce.f$intel.post, ce.f$intel.pre, method="paired"), 2)
round(cohensD(ce.m$verbal.post, ce.m$verbal.pre, method="paired"), 2)
round(cohensD(ce.m$spatial.post, ce.m$spatial.pre, method="paired"), 2)
round(cohensD(ce.m$intel.post, ce.m$intel.pre, method="paired"), 2)

# Question 10: Based on the present data, are you convinced that
# malleable feedback is beneficial to performance when engaging
# in a cognitive training program?
# ---------------------

