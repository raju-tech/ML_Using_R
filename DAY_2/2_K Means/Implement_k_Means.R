#**************************************************
#   Implementing K means 
#   
# ---------------------------------------------------
# ***********   Init Concepts    ***************** #
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())
#--------------   Include Packages ----------------------------
# Note You need to install the below mentioned packages first !

library(dplyr)
library(sqldf)
library(ggplot2)
library(corrplot)

#--------------------------------------------------------------
# Read data file 
# CSV file read - to be kept in the  same folder as this code 
Agg_data <- read.csv(file='D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_2\\2_K Means\\Agg_data.csv', header=T, sep=',')
#-----------------------------------------------------------------------------------------------------------
#                     Segmentation Analysis Using K means CLustering   
#-----------------------------------------------------------------------------------------------------------
#  We  Now Eliminate The Cust_Id, Age bracket,Weekday spend and Weekday_visit 
#  from our Data set for Clustering Analysis 
#-----------------------------------------------------------------------------------------------------------
  Reduced_data <- Agg_data[,c(-1,-6,-8,-10)]

#  Scaling the data 
#  1) Subracting columns means from corresponding columns
#  2) Dividing  by Standard Deviation 
#-----------------------------------------------------------------------------------------------------------

Scaled_data<-scale(Reduced_data) 


# Let us try to find optimal k value using the elbow method 

set.seed(4)
kr<-NULL
for(k in 2:10)
{
  xk<-kmeans(Scaled_data,k,iter.max=200)
  print(xk$betweenss/xk$totss*100)
  kr<-c(kr, xk$betweenss/xk$totss*100)
}
kr
plot(c(2:10),kr,type='l',xlab="Cluster_Number",ylab="PC_of_variance_explained")

#---------------------------------------------------------------------------------------
#  We observe from the plot that k = 4 seems an optimal choice of clustering parameter 
#---------------------------------------------------------------------------------------



# Generating clustering vector 
#------------------------------------------------
 Final_Agg_Data <- as.data.frame(Scaled_data)
 Xk4<- kmeans(Final_Agg_Data,4,iter.max=200)
 Kcluster <- Xk4$cluster


# Include the clustering vector to the Aggregate Data
#---------------------------------------------------
 Agg_data$KVector <- Kcluster 

# Now let us look at a snapshot of the Agg_data 
#-------------------------------------------------- 
 head(Agg_data)

#-------------------------------------------------------------------------------------------------
#   This Concludes the K means clustering  implementation 
#-------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------
# POST PROCESSING 
# **************************************************************************************************  
#  Now Let us Get the Summary for each Customer group--- This is the DO IT YourSelf  Part !


# ********************************************   Final Summary  ***************************************
#  
Agg_Summary_1<-sqldf("select KVector,
                     count(Cust_id) as Cnt,
                     sum(Monetary) as Revenue,
                     avg(Freq) as Avg_Freq, 
                     avg(Recency) as Avg_Recency,
                     avg(weekend_spend) as Avg_wknd_spend, 
                     avg(weekend_visits) as Avg_wknd_visit from Agg_data group by 1")

# Let us now look at the entire content of Agg_Summary_1 
Agg_Summary_1

# Let us create a computed column call Perc_Revenue and  and Perc_Customer  along with the aggregated data 
# in Agg_Summary_1

Agg_Summary_1$Perc_Revenue  <- 100*Agg_Summary_1$Revenue/(sum(Agg_Summary_1$Revenue)) 

Agg_Summary_1$Perc_Customer <-  100*Agg_Summary_1$Cnt/(sum(Agg_Summary_1$Cnt))

# Let us now Look at Agg_Summary again 

Agg_Summary_1

##############################################################################

# Let us now format the summarized data for output 

Agg_Out  <- data.frame(matrix(ncol =0, nrow = 4))

#  Group Number 
Agg_Out$Group_Num <- Agg_Summary_1$KVector

#  Customer Share 
Agg_Out$Cust_Share <- round(Agg_Summary_1$Perc_Customer)

#  Revenue Share 
Agg_Out$Revenue_Share <- round(Agg_Summary_1$Perc_Revenue)

# Average Freq of Visit 
Agg_Out$Freq_Visit <- round(Agg_Summary_1$Avg_Freq)


# Average Weekend spend 
Agg_Out$Avg_Wknd_spend <- round(Agg_Summary_1$Avg_wknd_spend)

# Average Weekend visit 
Agg_Out$Avg_wknd_visit <- round(Agg_Summary_1$Avg_wknd_visit)


# Have a look at the Output Structure
Agg_Out

# Write Output Structure to file 

#  Write the data frames to csv file 
write.table(Agg_Out, file="CUstomer_Segmentation.csv", sep=",", row.names=FALSE, col.names=TRUE, append=FALSE)

# ********************** End   of  Code  *******************************************************************  # 

