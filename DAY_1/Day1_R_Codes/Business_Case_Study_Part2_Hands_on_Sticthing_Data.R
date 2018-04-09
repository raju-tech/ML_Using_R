#**************************************************
#   Aggregating and Adding information o data 
#   for customer segmentation 
# ---------------------------------------------------
# ***********   Init Concepts    ***************** #
# clear all existing variables from console
  cat("\014") 
# clear all data variables 
  rm(list=ls())
#############   Include Packages #########################
# Note You need to install the below mentioned packages first !

library(dplyr)
library(sqldf)
library(ggplot2)
library(corrplot)
########################################################

#  Read data file 
# CSV file read - to be kept in the  same folder as this code 
data <- read.csv(file='CUST_DATA.csv', header=T, sep=',')

# Create a Total Spend Column for the data 
data$Total_Spend  <- data$Qty*data$Price

# Check the Inclusion of the new variable in the system 
head(data)


#-------------   Manipulating Date Data ------------------
# Check the class  of Date column 
class(data$Transaction_date)

# Convert character data to Date format 
#data$Transaction_date<-as.Date(data$Transaction_date)
data$Transaction_date<-as.Date(as.character(data$Transaction_date),format="%d-%m-%Y")


head(data)
class(data$Transaction_date)

# Set Max Date as 1 day later than latest transaction date 
#  This will be used to compute Recency later in this program 
Max_date<-max(data$Transaction_date)+1

#-- Creating New columns from Date field -----------------------------------------
# Day of Week 
data$week_day<-weekdays(as.Date(data[,2]))

#Extract Day of the Week  number eg/Mon = 1; TUE = 2 etc. 
data$week_number<-as.numeric(format((data[,2]),"%w"))

#Extract Month  Number 
data$month<-as.numeric(format((data[,2]), "%m"))

# Extract Year 
data$year<-as.numeric(format((data[,2]), "%Y"))


# Now display new data with additional columns 
head(data)
#---------------------------------------------------------------------------------
# Data Aggregation
#---------------------------------------------------------------------------------
# STEP 1 : First Level Aggregation: Capture Spend Pattern based on day of the week  
#---------------------------------------------------------------------------------
# ********************************************************************************
#Create Aggregated Temp Table for Spend Pattern 
#
Spend_Pattern <-sqldf("select Cust_id,
                        sum(case when week_number not in ('0','6') then Total_Spend end) as weekday_spend,
                        sum(case when week_number in ('0','6') then Total_Spend end) as weekend_spend, 
                        sum(case when week_number not in ('0','6') then 1 else 0 end) as weekday_visits,
                        sum(case when week_number in ('0','6') then 1 else 0 end) as weekend_visits, 
                        Max(Age) as Age_band from data group by 1")

# Display 
head(Spend_Pattern)

#------------------------------------------------------------------------------------
#  Step 2 - Aggregate Spend Freq and Recency 
#
#------------------------------------------------------------------------------------
library(plyr)
library(dplyr)
# Note the Length Function is used to get a count of the Number of rows returned 
#  per grouping column 
#------------------------------------------------------------------------------------

Spend_Freq <- ddply(data,.(Cust_id),summarize, Freq=length(Cust_id), Monetary=sum(Total_Spend),
                  Recency=round(as.numeric(difftime(Max_date,max(Transaction_date),units="days"))))

data<-data[order(data$Transaction_date),]

head(Spend_Freq)

#--------------------------------------------------------------------------------------------------
#  STEP 3 - Aggregate 
#--------------------------------------------------------------------------------------------------  

#  Explanatory notes in computing Inter Purchase Interval 

#--------------------------------------------------------------------------------------------------
# Step 1 - Get the Purchase Interval vector for each customer 
#--------------------------------------------------------------------------------------------------
#  ddply allows for a new computed column called diff time to be created in a datframe iPI 
#  This has the  1st level forward differences of dates.  The Vector of time differences is 
# of size n-1 where n is the number of readings . Thus the first difference is treated as zero
# Thus the  Data Frame IPI has as many rows as that of the original data frame called data 
#-----------------------------------------------------------------------------------------
IPI<-ddply(data,"Cust_id",transform,inner_time=c(0,diff(Transaction_date)))

# Check the data for cust id = 1113
IPI_1113 <- IPI[which(IPI$Cust_id==1113),]
IPI_1113
#------------------------------------------------------------------------------------------
# Step 2   Get the Mean Interpurchase Interval per customer 
#------------------------------------------------------------------------------------------
#  Notes:    
#  We are using the aggregate function in R to aggregate the data and group by or
#  list by Cust Id
#-----------------------------------------------------------------------------------------
 ipi<-round(aggregate(IPI$inner_time,list(IPI$Cust_id),mean)) 

#-----------------------------------------------------------------------------------------
# After this operation we need to  give a proper column name to the data frame ipi for 
# Both its columns 
# Column 1 should be Cust_id   and Col 2 should be Inter_purchase_interval
#
# Rename columns 

# Rename col 1
names(ipi)[1] <- "Cust_id"

# Rename col 2 
names(ipi)[2] <- "Inter_purchase_interval"


####################################################
#  Combining Data 
# Let us look at what are all the aggregated data  we have at hand at this stage 
#
#  1.  Spend_Freq -- it has the columns 
#  Cust_id   Freq      Monetary       Recency
####################################################
# 
#  2.  ipi --- it has the columns 
#  Cust_id    Inter_purchase_interval
####################################################
#
#  3. Spend_Pattern  --- it has the columns 
#  Cust_id weekday_spend weekend_spend weekday_visits weekend_visits Age_band
# ################################################### 
#  Check first 6 rows of all three dataframes 

# Spend_Freq
head(Spend_Freq)

# Spend_Pattern
head(Spend_Pattern)

#inter_Purchase_Interval 
head(ipi)


#  Step 1 -- Combine Spend_Freq with ipi col 2 
   Agg_data<-cbind(Spend_Freq,ipi[2])
#
#  At this Stage let us look at the columns in Agg_data 
#  Note We can do a column bind as both the Data frames have Cust_id as unique and sorted 
#
#------------------------------------------------------------------------------------------
#  Step 2 
#  We now column bind Agg_data with All columns of derived variable except Cust_id
   Agg_data<-cbind(Agg_data,Spend_Pattern[,c(2:6)])
#  Thus now Agg_data has all the aggregated columns per Cust_id
   head(Agg_data)
#------------------------------------------------------------------------------------------
#  Now  let us check for missing values -- expected in weekend spend
#  and in weekday spend 
#------------------------------------------------------------------------------------------
#
# This tells us how  many NA values in weekend_spend 
length(which(is.na(Agg_data$weekend_spend)))

# This tells us how many NA values in weekday_spend 
length(which(is.na(Agg_data$weekday_spend)))

#  Replace NA with zeros in weekday_spend 
Agg_data[is.na(Agg_data$weekday_spend),"weekday_spend"]<-0

# Replace NA with zeros in weekend_spend 
Agg_data[is.na(Agg_data$weekend_spend),"weekend_spend"]<-0


############  Let us Check for NA values once more now 

# This tells us how  many NA values in weekend_spend 
length(which(is.na(Agg_data$weekend_spend)))

# This tells us how many NA values in weekday_spend 
length(which(is.na(Agg_data$weekday_spend)))

# Write Data to output file 
write.table(Agg_data, file="Agg_data.csv", sep=",", row.names=FALSE, col.names=TRUE, append=FALSE)

#----------------------------   DATA AGGREGATION PROCESS is COMPLETE ------------------------------
