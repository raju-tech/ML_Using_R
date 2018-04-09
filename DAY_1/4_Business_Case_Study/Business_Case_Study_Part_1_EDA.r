# **************************************************
# *  Basic Business Case Study - Customer Segmentation 
# **************************************************


# ***********   Init Concepts    ***************** #
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())

#############   Include Packages #########################
# install.packages("dplyr")
# install.packages("sqldf")
# install.packages("corrplot")
# install.packages("ggplot2")
#--------------------------------------------------------
# Note You need to install the below mentioned packages first !
library(plyr)
library(sqldf)
library(ggplot2)
library(corrplot)
########################################################

#  Read data file 
# CSV file read - to be kept in the  same folder as this code 
data <- read.csv(file='D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_1\\4_Business_Case_Study\\CUST_DATA.csv', header=T, sep=',')

# Get a glimpse of the data 
head(data)
#############   Exploratory Data Analysis on the  Data at hand #############
#-------------------------------------------------------------------------------

# -- Tackling the high level Questions first 

# How many customers?
  UniqueCust<- unique(data$Cust_id)

# Display
  length(UniqueCust)
#-------------------------------------  

# How many Transactions 
  NumTran <- nrow(data)
  
# Display 
  NumTran
#-------------------------------------
  
# How many Transactions per day 
  TranData <- as.data.frame(table(data$Transaction_date))
  
# Display 
  TranData 
  
#-------------------------------------
  
# Date Max transactions occurred   
  
# Rename Columns of TranData
  names(TranData)[1] <- "Tran_Date"
  names(TranData)[2] <- "Count"
  
# Get Date with Max Transactions   
  Max_Tran_Date <- TranData$Tran_Date[which.max(TranData$Count)]
  
# Display
  Max_Tran_Date
  
# Create a computed column 
  data$Tran_Amt <- data$Qty*data$Price
  
# Transaction with the highest value 
  High_Tran <- data[which.max(data$Tran_Amt),]

# Display 
  High_Tran 
  
# Transaction with the lowest value 
  Low_Tran <- data[which.min(data$Tran_Amt),]
  
# Display 
  Low_Tran 
  
# Product id occuring max number of times 
Prod_id_cnt <- as.data.frame(table(data$Product_id))

head(Prod_id_cnt)

# Rename Columns of TranData
names(Prod_id_cnt)[1] <- "Prod_Id"
names(Prod_id_cnt)[2] <- "Count"


Max_Sell_Prod_id <-  Prod_id_cnt[which.max(Prod_id_cnt$Count),]

#Display Max selling Product id  and quantity 
Max_Sell_Prod_id

#---------------------------------------------------------------------------------------
#  Drill Down Analysis  for a particular customer 
#---------------------------------------------------------------------------------------

# Fetch Records For Customer iD 539166
Cust_539166_data <-subset(data,Cust_id==539166)

# Display Records 
head(Cust_539166_data)


# Find Total Amount Spent by this customer 
Tot_Spend_539166 <- sum(Cust_539166_data$Tran_Amt)

#Display
Tot_Spend_539166

# using dplyr package
library(dplyr)
# Which customer has the maximum total spend 
# Select relevant columns 
Cust_Spend <- select(data,Cust_id,Tran_Amt)

# Aggregate  by Customer id - summing on Tran_Amt 
Cust_Tot_Spend  <- as.data.frame(Cust_Spend %>% group_by(Cust_id) %>% summarise(Tot_Spend=sum(Tran_Amt)))

# Select Cust_Id for Max Tot_Spend 
Max_Spend_Cust <- Cust_Tot_Spend$Cust_id[which.max(Cust_Tot_Spend$Tot_Spend)]

#Display 
Max_Spend_Cust

#------------------------------------------------------------------------------------
#  End of Basic  Business Case Study 
#------------------------------------------------------------------------------------




