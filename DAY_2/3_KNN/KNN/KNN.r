#---    Classification using Nearest Neighbors -------------------
# Code Developer  : Anish Roychowdhury 2017 
#-----------------------------------------------------------------
#---------  House Cleaning ----------

# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())

# Setting up Packages ---------------------
# install.packages("class")
# install.packages("gmodels")


## Example: Classifying Cancer Samples ----
##  Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv("D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_2\\3_KNN\\KNN\\wisc_bc_data.csv", stringsAsFactors = FALSE)

# Check the Structure of the input data 
str(wbcd)


# Discard the Unique id column i.e.  column 1 
wbcd <- wbcd[-1]

#  Get a Summary count of the Diagnosis column 
table(wbcd$diagnosis)
 
# Target feature is to be Re-coded as a factor 
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))

# Get a Percentage feel of the diagnosis factor
# Note Prop,table(x)  would give  x/ sum(x)
round(prop.table(table(wbcd$diagnosis))*100,digits = 1)

# We need to normalize the numeric data 
# Create a user defined normalize function 

normalize <- function (x)
{
 return  ((x - min(x))/(max(x) - min(x)))
}


# apply this operation en_masse using lapply 
# Note we are skipping the first column here which is now the Diagnosis 
wbcd_n <-  as.data.frame(lapply(wbcd[2:31],normalize))

# Check 
summary(wbcd_n)


# Partition into train and test data sets 
# 100 data points are for test patients 
wbcd_train <- wbcd_n[1:469,]
wbcd_test  <- wbcd_n[470:569,]

# store labels as factors 
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]


# load package class
library(class)

# classify using the knn algorithm 
# k value 21 is closest to square root of train count and its a odd number 
wbcd_test_pred <- knn(train = wbcd_train, test=wbcd_test, cl= wbcd_train_labels,k=21)
# 
#----------------------------------------------------------------
#  Evaluating Model Performance 
#----------------------------------------------------------------

# Let us see what the prediction looks like 
wbcd_test_pred
# install package gmodels 
library(gmodels)

# Cross Tabulate Predicted Labels with test labels to evaluate performance 
# Use the simple table function first 
table(x=wbcd_test_labels,y=wbcd_test_pred)

# Now check with cross table 
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)
 

#####################################
#  How to improve the model 
#####################################
# Method 1 :   Z Score standardization 
#---------------------------------------

# Scale every feature except the first column which is the label 

# Z transform 
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# Check 
summary(wbcd_z)


# Divide into test and training data sets
wbcd_train_z <- wbcd_z[1:469,]
wbcd_test_z <- wbcd_z[470:569,]


wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

wbcd_test_z_pred <- knn(train = wbcd_train_z, test=wbcd_test_z, cl= wbcd_train_labels,k=21)

CrossTable(x=wbcd_test_labels,y=wbcd_test_z_pred,prop.chisq = FALSE)

# Note that output accuracy has actually worsened in this case 

