# -------------------------------------------------
# #  Practice Code for  LASSO REGRESSION 
# -------------------------------------------------
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())
#--------------------------------------------------
library(glmnet)

#--------------------------------------------------
# We use the built in mtcars dataset 
#--------------------------------------------------
head(mtcars)

# Let us choose one variable to be dependent and two others to be independent 
# v0 - Dependent
# v1, v2 Independent 
# Let us choose to determine miles per gallon dependent on disp and hp 
# ------------------------------------------------------------------------------

v0 <- "mpg"
v1 <- "disp"
v2 <- "hp"


# Examine Correlations between these variables 
round(cor(mtcars[, c(v0,v1,v2)]), 2)


#  Define function to standardize 
standardize <- function(x) {(x-mean(x))/sd(x)}

# Let us Standardize the Variables 
z0 <- standardize(mtcars[, v0])
z1 <- standardize(mtcars[, v1])
z2 <- standardize(mtcars[, v2])


# Fit linear model without intercept term 
lstsq <- lm(z0~z1+z2-1)

# Get the coefficients 
lstsq_beta <- coef(lstsq)

# Check the Coefficients
lstsq_beta

# Observation : for disp = -0.62;  hp = -0.28 



