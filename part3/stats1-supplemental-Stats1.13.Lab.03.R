#!/usr/bin/env Rscript

# Statistics One, 2013, Lab 3

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlational analyses
#   Examine relationships among variables using scatterplots

# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal (vermem)
#     Memory composite visual (vismem)
#     Visual motor speed composite (vms)
#     Reaction time composite (rt)
#     Impulse control composite (ic)
#     Total symptom score (sym)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# Installation of rgl package gave error:
# > "configure: error: missing required header GL/gl.h...
# >  * removing ‘/home/asarcar/R/x86_64-pc-linux-gnu-library/2.15/rgl’"
# Hence used the ubuntu binary distribution:
# > sudo apt-get install -y r-cran-rgl
# install.packages(c("psych", "gclus"))

# Load packages
library(psych)
library(gclus)
library(rgl)

# Read data into a dataframe called impact
impact <- read.table("stats1-datafiles-Stats1.13.Lab.03.txt", header = T) 

# If you want to view the data
#View(impact)
edit(impact)

# Summary statistics
describe(impact) 

describeBy(impact, impact$condition)

# Correlation analysis of baseline measures 
cor(impact[3:8]) # Columns 3 to 8 contain the 6 baseline measures

round(cor(impact[3:8]), 2) # Round to 2 decimal places 

# Create two subsets, control and concussed
control <- subset(impact, impact[, 2]=="control")
control
concussed <- subset(impact, impact[, 2]=="concussed")
concussed

# Correlation analysis of the control group, all measures
round(cor(control[3:14]), 2)

# Correlation analysis of the concussed group, all measures
round(cor(concussed[3:14]), 2)

# Does baseline impulse control predict memory impairment after a concussion?
concussed$verbal.impair <- (concussed$vermem1 - concussed$vermem2)
concussed$visual.impair <- (concussed$vismem1 - concussed$vismem2)
concussed$memory.impair <- (concussed$verbal.impair + concussed$visual.impair) / 2

cor(concussed$memory.impair, concussed$ic1)

# Scatterplots 
# Note: Scatterplot functions are available in many packages and offer an array of advanced features. For the sake of time, I will demonstrate just a few examples here. I encourage you to explore beyond these functions and options.

# Standard scatterplot: Drawing smooth curve through the scatterplot: Ory Added
plot(impact$vermem1 ~ impact$vismem1)
lines(lowess(impact$vermem1 ~ impact$vismem1), col="red")

# Standard scatterplot with regression line
plot(impact$vermem1 ~ impact$vismem1)
abline(lm(impact$vermem1 ~ impact$vismem1), col = "green")

# Scatterplot matrix
# Using Formula
pairs(~vermem1 + vismem1 + vms1 + rt1 + ic1 + sym1, data = impact, labels = c("verbal memory", "visual memory", "visual motor speed", "reaction time", "impulse control", "symptom"), cex.labels = 1.2)
# Using Table: Ory Added
pairs(impact[3:8], labels = c("verbal memory", "visual memory", "visual motor speed", "reaction time", "impulse control", "symptom"), main = "Correlation of Memory Tests", upper.panel = NULL, cex.labels = 1.2)


# Color scatterplot matrix, colored and ordered by magnitude of r
base <- impact[3:8]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

# Scatterplot in 3D
plot3d(impact$vismem1, impact$sym1, impact$vermem1, main = "3D Plot")
plot3d(impact$vismem2, impact$sym2, impact$vermem2, main = "3D Plot")

