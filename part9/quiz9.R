library(psych)
library(car)
library(lsr)

#
# Table
#                  Early=1
#       ProHelp=1  OnTim=2
#       OrgEffe=2  Late =3
# PID	Prime	   Haste	Helping
# 1	1          1 	        4.38
#

# HC: Help Chance
HC <- read.table("quiz9.txt", header = T)
edit(HC)

describe(HC)

# Question 1: What is the class of Haste and Prime in R?
class(HC$PID)
class(HC$Haste)
class(HC$Prime)

HC$PID <- factor(HC$PID)
HC$Prime <- factor(HC$Prime)
HC$Haste <- factor(HC$Haste)

describeBy(HC, HC$Haste)
describeBy(HC, HC$Prime)

# Test the homogeneity of variance assumption
leveneTest(HC$Helping ~ HC$Haste * HC$Prime)

# Question 2: After converting Haste and Prime to factors,
# run an ANOVA with both Haste and Prime as independent variables.
# Is the effect of Haste significant?
aov.HasteAndPrime<- aov(HC$Helping ~ HC$Haste * HC$Prime)
summary(aov.HasteAndPrime)

# Question 3: Is the effect of Prime significant?

# Question 4: Is the interaction significant?

# Question 5: Save the ANOVA summary in a table and run
# Tukey's pairwise comparison on all group means.
# Do each level of Haste significantly differ from one another?
#                  Early=1
#       ProHelp=1  OnTim=2
#       OrgEffe=2  Late =3
# PID	Prime	   Haste	Helping
TukeyHSD(aov.HasteAndPrime)

# Question 6: What is the partial eta-squared value for the
# effect of Haste? (round to 2 decimal places).
round(etaSquared(aov.HasteAndPrime, anova = T), 2)

# Question 7: What is the partial eta-squared value for the
# interaction? (round to 2 decimal places).

# Question 8: Let's now run simple effects of Prime at each
# level of Haste. At which level of Haste is the effect of
# Prime significant?
# All of the above, On time, Late, Early
HC.Early <- subset(HC, HC$Haste == "1")
HC.OnTime <- subset(HC, HC$Haste == "2")
HC.Late <- subset(HC, HC$Haste == "3")
aov.Early <- aov(HC.Early$Helping ~ HC.Early$Prime)
aov.OnTime <- aov(HC.OnTime$Helping ~ HC.OnTime$Prime)
aov.Late <- aov(HC.Late$Helping ~ HC.Late$Prime)
summary(aov.Early)
summary(aov.OnTime)
summary(aov.Late)

# Question 9: What is the partial eta-squared value for the
# effect of Prime when people were early?
# (round to 2 decimal places).
round(etaSquared(aov.Early, anova = T), 2)

# Question 10: Which one of the following statements best
# illustrates the main finding of the study?

