#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Author: Enrijeta Shino                                                       #
# Last update: 20-March-2018                                                   #
#------------------------------------------------------------------------------#
# Install/downliad R packages/functions
# FOREIGN: install.packages('foreign')
# GGPLOT2: install.packages('foreign')
# DATA.TABLE: install.packages('ggplot2')
# DPLYR: install.packages('dplyr')
# SANDWICH: install.packages('sandwich')
# LMTEST: install.packages('lmtest')
# VIF: install.packages('VIF')
# LFE: install.packages('lfe')
# PLM: install.packages('plm')
# HMISC: install.packages('Hmisc')
# GRID: install.packages('grid')
# GRIDEXTRA: install.packages('gridExtra')
# XTABLE: install.packages('xtable')
# RDD: install.packages("rdd")
# LPDENSITY: install.packages('lpdensity')
# RDDENSITY: install.packages('rddensity')
# RDLOCRAND: install.packages('rdlocrand')
# RDROBUST: install.packages('rdrobust')
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())

library(foreign)
library(ggplot2)
library(data.table)
library(dplyr)
library(sandwich)
library(lmtest)
library(VIF)
library(lfe)
library(Hmisc)
library(stargazer)
library(rdrobust)
library(rdd)
library(rddensity)
library(gridExtra)
library(grid)

options(width=280)
par(mar = rep(2, 4))

#----------------------------------#
# (Sharp) Regression Discontinuity #
#----------------------------------#

#-------------------------------------------------------------------------------------------------------#
# (Sharp) Regression Discontinuity. Take any real data set with a real X (covariate) and                #
# real Y (outcome) that are related in some say. You can use any dataset from the link in Problem 1.    #
# Construct a placebo treatment by choosing some rule so that Ti=1 when Xi ≥ x0 for some x0. That is,   #
# modify the outcome variable Yi for those units with Xi ≥ x0 by adding a constant treatment effect,    #
# for example, add one standard deviation of the outcome plus some noise.                               #
#-------------------------------------------------------------------------------------------------------#

smoke_data <- read.dta("/Users/enrijetashino/Desktop/UF PoliSci/PS4/smoke.dta")
smoke_data <- data.table(smoke_data)

#-------------------------------------------------------------------------------------------------------#
# 2.1. Plot the outcome by forcing variable [the standard graph showing the discontinuity]              #
#-------------------------------------------------------------------------------------------------------#

# Create a placebo treatment using the rule stated above. 
smoke_data <- smoke_data[, T := ifelse(smoke_data$age < 45, 1, 0)]
smoke_data <- smoke_data[, cigs_new := ifelse(smoke_data$age <= 45, smoke_data$cigs + sd(smoke_data$cigs) + 
                                                runif(nrow(smoke_data), 0, 1), smoke_data$cigs)]

smoke_data <- smoke_data[, c('cigpric', 'educ', 'income', 'age' , 'cigs','cigs_new')]

# Running variable and outcome 
Y <- smoke_data$cigs_new
X <- smoke_data$age

# Density test [Implements the McCrary (2008) test]
DCdensity(X, 45, plot = TRUE)

# RD plot using 40 bins of equal length
out <- rdplot(smoke_data$cigs_new, smoke_data$age, c = 45, nbins = c(20,20), binselect = 'esmv', 
              title = 'Consumption of Cigarettes by Age',
              y.label = 'Consumption of Cigarettes', x.label = 'Age')
summary(out)
dev.off()

# 40 Evenly-spaced bins 
out <- rdplot(smoke_data$cigs_new, smoke_data$age, c = 45, nbins = c(20,20), binselect = 'es', 
              y.label = 'Consumption of Cigarettes', x.label = 'Age')
summary(out)

# IMSE RD plot with quantile-spaced bins
out = rdplot(Y, X,  c = 45, binselect = 'qs', x.lim = c(15,90))
summary(out)

# RD plots for predetermined covariates
par(mfrow = c(2,2))

rdplot(smoke_data$cigpric ,smoke_data$age, c = 45, p = 1, 
       y.label = 'Price of Cigarettes', x.label = 'Age')

rdplot(smoke_data$educ ,smoke_data$age, c = 45, p = 1, 
       y.label = 'Education', x.label = 'Age')

rdplot(smoke_data$income ,smoke_data$age, c = 45, p = 1, 
       y.label = 'Income', x.label = 'Age')

# smoke_data <- smoke_data[, age_groups := cut(smoke_data$age, seq(from = min(smoke_data$age) - 1, 
# to = max(smoke_data$age) + 1, by = 0.01))]
# table(cut(smoke_data$age, seq(from = min(smoke_data$age) - 1, to = max(smoke_data$age) + 1, by = 5))) 
# Number of observations in each bin 

#-------------------------------------------------------------------------------------------------------#
# 2.2. Plot the density of the forcing variable                                                         #
#-------------------------------------------------------------------------------------------------------#

# Density of the forcing variable
# Using rddensity
out <- rddensity(X, c = 45, kernel = 'uniform')
summary(out)

dens <- aggregate(rep.int(1, length(X))~X, FUN = sum)
names(dens)<-c("val","freq")

# Density 
rdplot(dens$freq, dens$val, c = 45, nbins = c(20,20), p = 3, 
       x.label = 'Running variable', y.label = 'Number of observations', 
       col.lines = 'red', col.dots = 'gray')

# Another way to plot the density of the forcing variable
rdd <- rddensity(X = X, c = 45)
rdplotdensity(rdd, X, CIcol = c('blue','red'), type = 'line', 
              xlabel = 'Age', ylabel = 'Density')

# Histogram 
ggplot(data = smoke_data, aes(x = smoke_data$age)) + 
  geom_histogram(data = smoke_data, aes(x = smoke_data$age, y= ..count..), breaks = seq(15, 45, 1.5), 
                 fill = "deepskyblue1", col = "black", alpha = 1) + 
  geom_histogram(data = smoke_data, aes(x = smoke_data$age, y= ..count..), breaks = seq(45, 90, 1.5), 
                 fill = "firebrick1", col = "black", alpha = 1) +
  geom_vline(xintercept = 45, colour="green", linetype = "dashed", size = 0.5) + coord_equal() +
  geom_segment(aes(x = 55, y = 45, xend = 46, yend = 45), arrow = arrow(length = unit(0.3, "cm"))) +
  scale_y_continuous(limits = c(0,60), expand = c(0, 0)) +
  scale_x_continuous(breaks = round(seq(15, max(smoke_data$age), by = 5),1)) + 
  annotate("text", x = 59, y = 45, label = "cutt-off") + 
  xlab("Age") + ylab("Number of Observations") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(colour = "black"))  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#-------------------------------------------------------------------------------------------------------#
# 2.3. Estimate the effect using a local linear regression                                              # 
#-------------------------------------------------------------------------------------------------------#

# This is a simple linear regression model (y = a + bx + e)

# Local linear regression 
local_reg <- rdrobust(Y, X, c = 45, kernel = 'uniform', p = 1, bwselect = "mserd", vce = 'HC1')
summary(local_reg)

# Use the bandwidth(tuning parameter) computed through rdrobust() function prior. 
bw <- local_reg$bws[1,1]

# RHS regression
local_RHS <- lm(cigs_new ~ I(age-45), smoke_data[age < (45+bw) & age >= 45])

# LHS Regression 
local_LHS <- lm(cigs_new ~ I(age-45), smoke_data[age > (45-bw) & age < 45])

# Average treatment effect (ATE)
ATE1 <- coef(local_RHS)[1] - coef(local_LHS)[1]
ATE1

#-------------------------------------------------------------------------------------------------------#
# 2.4. Estimate the effect using a local polynomial [of order 2 and 3] regression                       # 
#-------------------------------------------------------------------------------------------------------#

#-----------------------------------------#
# Local polynomial regression [2nd order] #
#-----------------------------------------#

local_poly2 <- rdrobust(Y, X, c = 45, kernel = 'uniform', p = 2, bwselect = "mserd", vce = 'HC1')
summary(local_poly2)

# Use the bandwidth(tuning parameter) computed through rdrobust() function prior. 
bw <- local_poly2$bws[1,1]

# RHS regression
poly2_RHS <- lm(cigs_new ~ I(age-45) + I((age-45)^2), smoke_data[age < (45+bw) & age >= 45])
# LHS regression
poly2_LHS <- lm(cigs_new ~ I(age-45) + I((age-45)^2), smoke_data[age > (45-bw) & age < 45])

# Average treatment effect (ATE)
ATE2 <- coef(poly2_RHS)[1] - coef(poly2_LHS)[1]
ATE2

#-----------------------------------------#
# Local polynomial regression [3nd order] #
#-----------------------------------------#

local_poly3 <- rdrobust(Y, X, c = 45, kernel = 'uniform', p = 3, bwselect = "mserd", vce = 'HC1')
summary(local_poly3)

# Use the bandwidth(tuning parameter) computed through rdrobust() function prior. 
bw <- local_poly3$bws[1,1]

# RHS regression
poly3_RHS <- lm(cigs_new ~ I(age-45) + I((age-45)^2) + I((age-45)^3), smoke_data[age < (45+bw) & age >= 45])
# LHS regression
poly3_LHS <- lm(cigs_new ~ I(age-45) + I((age-45)^2) + I((age-45)^3), smoke_data[age > (45-bw) & age < 45])

# Average treatment effect (ATE)
ATE3 <- coef(poly3_RHS)[1] - coef(poly3_LHS)[1]
ATE3

#-------------------------------------------------------------------#
# Regression discontinuity: Choosing different bandwidths           #
# Bandwidth is computed using Imbens-Kalyanaraman Optimal Bandwidth #
#-------------------------------------------------------------------#

# Uniform Kernel: K(u) = 1/2
# Triangular Kernel: K(u) = (1-|u|)^2
# Epanechnikov Kernel: K(u) = 3/4(1-u^2)
# Quartic Kernel: K(u) = 15/16(1-u^2)^2

# Imbens-Kalyanaraman Optimal Bandwidth Calculation (Imbens, Guido and Karthik Kalyanaraman. (2009))

bw_rec <- IKbandwidth(X, Y, cutpoint = 45, kernel = "rectangular")   
bw_tri <- IKbandwidth(X, Y, cutpoint = 45, kernel = "triangular")    
bw_epa <- IKbandwidth(X, Y, cutpoint = 45, kernel = "epanechnikov")  
bw_qua <- IKbandwidth(X, Y, cutpoint = 45, kernel = "quartic")  

# Using Uniform Kernel

plot_uni <- ggplot(data = smoke_data, aes(age, cigs_new)) + 
  geom_point(colour = ifelse(smoke_data$age < 45-bw_rec & smoke_data$age >= 45+bw_rec, 'green4', 
                             ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_rec, 'blue', 
                                    ifelse(smoke_data$age < 45+bw_rec & smoke_data$age >= 45-bw_rec, 'red', 'green4')))) +
  geom_point(shape =  ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_rec, 0, 
                             ifelse(smoke_data$age < 45+bw_rec & smoke_data$age >= 45, 1, 16)), size = 0) + 
  geom_vline(xintercept = 45, colour="black", linetype = "dashed", size = 0.5) + 
  geom_vline(xintercept = 45-bw_rec, colour="black", linetype = 4, size = 1) +
  geom_vline(xintercept = 45+bw_rec, colour="black", linetype = 4, size = 1) +
  xlab('Age') + ylab('Consumption of cigarettes') + theme_light() + 
  geom_segment(aes(x = 48, y = 83, xend = 46, yend = 83), arrow = arrow(length = unit(0.1, "cm"))) + 
  annotate("text", x = 52, y = 83, label = "cutt-off") + ggtitle('Uniform Kernel') + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(data = smoke_data[age >= 45-bw_rec & age <= 45], method = "lm", colour = 'blue', se = T) + 
  geom_smooth(data = smoke_data[age <= 45+bw_rec & age >= 45], method = "lm", colour = 'red', se = T)

# Using Triangular Kernel 

plot_tri <- ggplot(data = smoke_data, aes(age, cigs_new)) + 
  geom_point(colour = ifelse(smoke_data$age < 45-bw_tri & smoke_data$age >= 45+bw_tri, 'green4', 
                             ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_tri, 'blue', 
                                    ifelse(smoke_data$age < 45+bw_tri & smoke_data$age >= 45-bw_tri, 'red', 'green4')))) +
  geom_point(shape =  ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_tri, 0, 
                             ifelse(smoke_data$age < 45+bw_tri & smoke_data$age >= 45, 1, 16)), size = 0) + 
  geom_vline(xintercept = 45, colour="black", linetype = "dashed", size = 0.5) + 
  geom_vline(xintercept = 45-bw_tri, colour="black", linetype = 4, size = 1) +
  geom_vline(xintercept = 45+bw_tri, colour="black", linetype = 4, size = 1) +
  xlab('Age') + ylab('Consumption of cigarettes') + theme_light() + 
  geom_segment(aes(x = 48, y = 83, xend = 46, yend = 83), arrow = arrow(length = unit(0.1, "cm"))) + 
  annotate("text", x = 52, y = 83, label = "cutt-off") + ggtitle('Triangular Kernel') + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(data = smoke_data[age >= 45-bw_tri & age <= 45], method = "lm", colour = 'blue', se = T) + 
  geom_smooth(data = smoke_data[age <= 45+bw_tri & age >= 45], method = "lm", colour = 'red', se = T)


# Epanechnikov Kernel

plot_epa <- ggplot(data = smoke_data, aes(age, cigs_new)) + 
  geom_point(colour = ifelse(smoke_data$age < 45-bw_epa & smoke_data$age >= 45+bw_epa, 'green4', 
                             ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_epa, 'blue', 
                                    ifelse(smoke_data$age < 45+bw_epa & smoke_data$age >= 45-bw_epa, 'red', 'green4')))) +
  geom_point(shape =  ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_epa, 0, 
                             ifelse(smoke_data$age < 45+bw_epa & smoke_data$age >= 45, 1, 16)), size = 0) + 
  geom_vline(xintercept = 45, colour="black", linetype = "dashed", size = 0.5) + 
  geom_vline(xintercept = 45-bw_epa, colour="black", linetype = 4, size = 1) +
  geom_vline(xintercept = 45+bw_epa, colour="black", linetype = 4, size = 1) +
  xlab('Age') + ylab('Consumption of cigarettes') + theme_light() + 
  geom_segment(aes(x = 48, y = 83, xend = 46, yend = 83), arrow = arrow(length = unit(0.1, "cm"))) + 
  annotate("text", x = 51, y = 83, label = "cutt-off") + ggtitle('Epanechnikov Kernel') + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(data = smoke_data[age >= 45-bw_epa & age <= 45], method = "lm", colour = 'blue', se = T) + 
  geom_smooth(data = smoke_data[age <= 45+bw_epa & age >= 45], method = "lm", colour = 'red', se = T)

# Quartic Kernel 

plot_qua <- ggplot(data = smoke_data, aes(age, cigs_new)) + 
  geom_point(colour = ifelse(smoke_data$age < 45-bw_qua & smoke_data$age >= 45+bw_qua, 'green4', 
                             ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_qua, 'blue', 
                                    ifelse(smoke_data$age < 45+bw_qua & smoke_data$age >= 45-bw_qua, 'red', 'green4')))) +
  geom_point(shape =  ifelse(smoke_data$age < 45 & smoke_data$age >= 45-bw_qua, 0, 
                             ifelse(smoke_data$age < 45+bw_qua & smoke_data$age >= 45, 1, 16)), size = 0) + 
  geom_vline(xintercept = 45, colour="black", linetype = "dashed", size = 0.5) + 
  geom_vline(xintercept = 45-bw_qua, colour="black", linetype = 4, size = 1) +
  geom_vline(xintercept = 45+bw_qua, colour="black", linetype = 4, size = 1) +
  xlab('Age') + ylab('Consumption of cigarettes') + theme_light() + 
  geom_segment(aes(x = 48, y = 83, xend = 46, yend = 83), arrow = arrow(length = unit(0.1, "cm"))) + 
  annotate("text", x = 52, y = 83, label = "cutt-off") + ggtitle('Quartic Kernel') + theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(data = smoke_data[age >= 45-bw_qua & age <= 45], method = "lm", colour = 'blue', se = T) + 
  geom_smooth(data = smoke_data[age <= 45+bw_qua & age >= 45], method = "lm", colour = 'red', se = T)

grid.arrange(plot_uni, plot_tri, plot_epa, plot_qua, nrow = 2, ncol = 2)
