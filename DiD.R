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

# Load the data

train_data <- read.dta("/Users/enrijetashino/Desktop/UF PoliSci/PS4/jtrain1.dta")
train_data <- data.table(train_data)

#--------------------------------------------#
# Difference-in-Difference and RDD estimator #
#--------------------------------------------#

#-------------------------------------------------------------------------------------------------------#
# Panel Data:                                                                                           #
# Take the data set “jtrain1” from http://www.stata.com/texts/eacsap/.                                  #
# This file has data on firms and the amount of job training they get                                   #
#-------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------#
# 1.1 Use data from 1987 and 1988 only. Construct the difference-in-differences estimator in the        #
# following two ways:                                                                                   #
#-------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------#
# 1.1.a. Construct the four means (control, treatment×before, after) and calculate the differences.     #
#-------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------#
# Replicate Table 2: Holzer et al. (1993)                     #
# Year 1987 [This will replicate the first column in Table 2] #
#-------------------------------------------------------------#

# Firms Receiving Grants in 1988
train_data_1988_1987 <- train_data[year != 1989]
z1 <- matrix(0, nrow(train_data_1988_1987))

for(i in 1:nrow(train_data_1988_1987)){
  if(train_data_1988_1987$grant[i] == 1 & train_data_1988_1987$d88[i] == 1){
    z1[i] <- train_data_1988_1987$hrsemp[i-1]
  }
  else 
    z1[i] <- NA
}

z1 %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
z1 %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation 

# Sample size for the firms receiving a grant in 1988
z1 %>% na.omit() %>% nrow()

# Firms Receiving Grants in 1989
train_data_1989_1987 <- train_data[year != 1988]
z2 <- matrix(0, nrow(train_data_1989_1987))

for(i in 1:nrow(train_data_1989_1987)){
  if(train_data_1989_1987$grant[i] == 1 & train_data_1989_1987$d89[i] == 1){
    z2[i] <- train_data_1989_1987$hrsemp[i-1]
  }
  else 
    z2[i] <- NA
}

z2 <- data.table(z2) %>% na.omit
z2 %>% unlist() %>% as.numeric() %>% mean()   # Mean
z2 %>% unlist() %>% as.numeric() %>% sd()     # Standard Deviation

# Sample size for the firms receiving a grant in 1989
z2 %>% na.omit() %>% nrow()

# Firms Not Receiving Grants in 1987 (Note: No Firm received grants in both years 1988 and 1989)
train_data[year == 1987 & grant == 0][, 'hrsemp'] %>% unlist %>% na.omit() %>% mean()
train_data[year == 1987 & grant == 0][, 'hrsemp'] %>% unlist %>% na.omit() %>% sd()


# Year 1988 [This will replicate the second column in Table 2]

# Firms Receiving Grants in 1988
train_data[d88 == 1 & grant == 1][, "hrsemp"] %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
train_data[d88 == 1 & grant == 1][, "hrsemp"] %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation

# Firms Receiving Grants in 1989
train_data_1989_1988 <- train_data[year != 1987]
z2 <- matrix(0, nrow(train_data_1989_1988))

for(i in 1:nrow(train_data_1989_1988)){
  if(train_data_1989_1988$grant[i] == 1 & train_data_1989_1988$d89[i] == 1){
    z2[i] <- train_data_1989_1988$hrsemp[i-1]
  }
  else
    z2[i] <- NA
}

z2 %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
z2 %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation

# Firms Not Receiving Grants in 1988
train_data[year == 1988 & grant == 0][, 'hrsemp'] %>% unlist %>% na.omit() %>% mean()
train_data[year == 1988 & grant == 0][, 'hrsemp'] %>% unlist %>% na.omit() %>% sd()


# Year 1989 [This will replicate the third column in Table 2]

# Firms Receiving Grants in 1988 [Note: No firm that received a grant in 1988 received it in 1989]
train_data_1988_1989 <- train_data[year != 1987]
z1 <- matrix(0, nrow(train_data_1988_1989))

for(i in 1:nrow(train_data_1988_1989)){
  if(train_data_1988_1989$grant[i] == 1 & train_data_1988_1989$d88[i] == 1){
    z1[i] <- train_data_1988_1989$hrsemp[i+1]
  }
  else
    z1[i] <- NA
}

z1 %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
z1 %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation 

# Firms Receiving Grants in 1989
train_data[d89 == 1 & grant == 1][, "hrsemp"] %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
train_data[d89 == 1 & grant == 1][, "hrsemp"] %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation

# Firms Not Receiving Grants in 1989
train_data[train_data$d89 == 1 & grant == 0][, "hrsemp"] %>% unlist() %>% na.omit() %>% mean()
train_data[train_data$d89 == 1 & grant == 0][, "hrsemp"] %>% unlist() %>% na.omit() %>% sd()

z3 <- matrix(0, nrow(train_data))

for(i in 3:nrow(train_data)){
  if(train_data$d89[i] == 1 & train_data$grant[i] == 0 & train_data$grant[i-1] == 0 & train_data$grant[i-2] == 0){
    z3[i] = train_data$hrsemp[i]
  }
  else 
    z3[i] = NA
}

z3 %>% unlist() %>% na.omit %>% as.numeric() %>% mean()   # Mean
z3 %>% unlist() %>% na.omit %>% as.numeric() %>% sd()     # Standard Deviation 

#-------------------------------------------------------------------------------------------------------#
# 1.1.b. Run the regression: (Use the data for 1987 and 1988 only)                                      #
#-------------------------------------------------------------------------------------------------------#

# Modify the variable grant [Call the new variable mod_grant]
# Note that the variable grant is actually the interaction between d88xgrant
train_data <- train_data[year != 1989]
train_data <- train_data[, mod_grant := 0]
n <- nrow(train_data) - 1 

for(i in 1:n){
  if(train_data$fcode[i] == train_data$fcode[i+1] & train_data$grant[i+1] == 1 & train_data$d88[i+1] == 1){
    train_data$mod_grant[i] <- train_data$grant[i+1]
  }
  else 
    train_data$mod_grant[i] <- train_data$grant[i]
}

train_data_reg <- train_data[, c('year', 'hrsemp', 'grant', 'd88', 'mod_grant', 'fcode', 'employ')] %>% na.omit()
train_data_reg <- train_data_reg[!apply(train_data_reg[,2] == 0, 1, FUN = any, na.rm = TRUE),]
reg1 <- lm(log(hrsemp) ~ mod_grant + d88 + grant, data = train_data_reg)
reg1 %>% summary()

reg2 <- lm(hrsemp ~ mod_grant + d88 + grant, data = train_data)
reg2 %>% summary() 

stargazer(reg1, reg2)


# Bootstrap the standard errors
N <- nrow(train_data_reg)                 # Replace with replacement
B <- 1000                                 # Set bootstrap to 1000
boot_se <- matrix(0, nrow = B, ncol = 4)  # Create a matrix for bootstrapped standard errors

for(i in 1:B){
  train_data_reg_boot <- train_data_reg[sample(1:N, size = N, replace = TRUE), ]
  reg1_boot <- lm(log(hrsemp) ~ mod_grant + d88 + grant, data = train_data_reg_boot)
  boot_se[i,] <- t(as.vector(summary(reg1_boot)[["coefficients"]][,2]))
}

colMeans(boot_se)                         # Average of the bootstrapped standard errors 

#-------------------------------------------------------------------------------------------------------#
# 1.2. Use the same data from 1987 and 1988. Run the fixed effect regression.                           #
#-------------------------------------------------------------------------------------------------------#

# Fixed effects. Fixed effect variable: fcode

reg1_fe <- lm(log(hrsemp) ~ mod_grant + d88 + grant + fcode, data = train_data_reg)
reg2_fe <- lm(hrsemp ~ mod_grant + d88 + grant + fcode, data = train_data)
reg1_fe_fit1 <- coeftest(reg1_fe, function(x) vcovHC(x, type="HC1"))
reg2_fe_fit2 <- coeftest(reg2_fe, function(x) vcovHC(x, type="HC1"))
stargazer(reg1_fe, reg2_fe, se = list(reg1_fe_fit1[,"Std. Error"], reg2_fe_fit2[,"Std. Error"]))

#-------------------------------------------------------------------------------------------------------#
# 1.3. Do you get exactly the same answer between 1.1(a,b) and 1.2, why or why not?                     #
#-------------------------------------------------------------------------------------------------------#

# No we do not get the same answer on the interaction term as in part 1.1(a,b) when compared to 1.2. 
# The estimated coefficient on the interaction which represents the DID estimate is lower when 
# compared to both parts 1.1(a,b). The reason is that we did not consider the fact that
# firms may be different for reasons unobservable to the econometricians however we control
# for firms fixed effects under the assumptions that they do not change over time. (e.g. we 
# cannot observe if some firms are more experienced and have better managerial skills than
# other firms.)