# **************************************************
# **********   K Means Algorithm 
# **************************************************
# Developed by Anish Roychowdhury   Jan   2017     **
# **************************************************
# ***********   Init Concepts    ***************** #
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())

#############   Include Packages #########################
# Note You need to install the below mentioned packages first !

library(plyr)
library(sqldf)
library(ggplot2)
library(corrplot)
########################################################

#  Read data file 
# CSV file read - to be kept in the  same folder as this code 
data <- read.csv(file='MGADS.csv', header=T, sep=',')

# Check the class of 'data'
class(data)

# Get a glimpse of the data 
head(data)
#############   Exploratory Data Analysis on the  Data at hand #############

# Extract Number of columns (We Expect S OF NOW 4 )
Colnum <- ncol(data)
Colnum


# Extract Number of Rows of Data ( Total Rows)
Rownum <- nrow(data)
#  How many transactions 
Rownum

# How many customers?
length(unique(data$Cust_id))

# Fetch Records For Customer iD 539166
Cdata <-subset(data,Cust_id==539166)

# Display Records 
Cdata

# Find Total Amount Spent by this customer 
Cdata$Amt_Spend <- Cdata$Qty*Cdata$Price 

# Then let us aggregate the Spend column 
Tot_Cust_Spend <- sum(Cdata$Amt_Spend)

# How many  Transaction occured on each day 
# We use frequency table function here 
Tranlist <- as.data.frame(table(data$Transaction_date))

# Customize column names 
colnames(Tranlist)<-c("Transaction_date","Count")

#
Tranlist

# Sorting now 
Order_Tran <- Tranlist[order(-Tranlist$Count),]

# Display entire sorted list 
Order_Tran

# capture  the Maximum only 
Max_Order <- Order_Tran[1,]

# Display the Max
Max_Order

# Direct way to get the Max 
Direct_Max <- Tranlist[which(Tranlist$Count==max(Tranlist$Count)),]

# Display Max
Direct_Max 

#####################################################################################
#  Creating New Variables 
#####################################################################################

# Create a Total Spend Column for the data 
data$Total_Spend  <- data$Qty*data$Price

# Check the Inclusion of the new variable in the system 
head(data)


########   Manipulating Date Data ######
# Check the class  of Date column 

class(data$Transaction_date)

data$Transaction_date<-as.Date(as.character(data$Transaction_date),format="%d-%m-%Y")


class(data$Transaction_date)

# Set Max Date as 1 day later than latest transaction date 
#  This will be used to compute Recency later in this program 
Max_date<-max(data$Transaction_date)+1



#   Date Manipulation 
# Extract The Day of the week 
data$week_day<-weekdays(as.Date(data[,2]))

#Extract Day of the Week  number eg/Mon = 1; TUE = 2 etc. 
data$week_number<-as.numeric(format((data[,2]),"%w"))

#Extract Month  Number 
data$month<-as.numeric(format((data[,2]), "%m"))

# Extract Year 
data$year<-as.numeric(format((data[,2]), "%Y"))


# Now display new data with additional columns 
tail(data)
head(data)

#*******************************************************************************
# Aggregation of the data 
# We are creating a temporary table called derived variable 
# This has aggregate names 
# ******************************************************************************
derived_variable<-sqldf("select Cust_id,
sum(case when week_number not in ('0','6') then Total_Spend end) as weekday_spend,
                        sum(case when week_number in ('0','6') then Total_Spend end) as weekend_spend, 
                        sum(case when week_number not in ('0','6') then 1 else 0 end) as weekday_visits,
                        sum(case when week_number in ('0','6') then 1 else 0 end) as weekend_visits, 
                        Max(Age) as Age_band from data group by 1")

# We now have an internal variable of the class data.frame 

##############################################################################
# We use the ddply function to aggregate the data grouped by Cust_id 
# Note the Length Function is used to get a count of the Number of rows returned 
#  per grouping column 
Agg_data <- ddply(data,.(Cust_id),summarize, Freq=length(Cust_id), Monetary=sum(Total_Spend),
                Recency=round(as.numeric(difftime(Max_date,max(Transaction_date),units="days"))))

data<-data[order(data$Transaction_date),]


#  Computing the Inter Purchase Interval for each customer 
#  

#  Explanatory notes in computing Inter Purchase Interval 

#############
# Step 1 - Get the Purchase Interval vector for each customer 
#############
#  ddply allows for a new computed column called diff time to be created in a datframe iPI 
#  This has the  1st level forward differences of dates.  The Vector of time differences is 
# of size n-1 where n is the number of readings . Thus the first difference is treated as zero
# Thus the  Data Frame IPI has as many rows as that of the original dta frame called data 






IPI<-ddply(data,"Cust_id",transform,inner_time=c(0,diff(Transaction_date)))


#################
# Step 2   Get the Mean Interpurchase Interval per customer 
################

#  Notes ----   
#  We are using the aggregate function in R to aggregate the data and group by or
# list by Cust Id
ipi<-round(aggregate(IPI$inner_time,list(IPI$Cust_id),mean)) 
#  After this operation we need to  give a proper column name to the data frame ipi for 
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
#  1.  Agg_data -- it has the columns 
#  Cust_id   Freq      Monetary       Recency
####################################################
# 
#  2.  ipi --- it has the columns 
#  Cust_id    Inter_purchase_interval
####################################################
#
#  3. derived_variable  --- it has the columns 
#  Cust_id weekday_spend weekend_spend weekday_visits weekend_visits Age_band
# ################################################### 
#  Step 1 -- Combine Agg_data with ipi col 2 
#   
   Agg_data<-cbind(Agg_data,ipi[2])
#
#  At this Stage let us look at the columns in Agg_data 
#  Note We can do a column bind as both the Data frames have Cust_id as unique and sorted 
#
# ################################
#
#  Step 2 
#  We now column bind Agg_data with All columns of derived variable except Cust_id
   Agg_data<-cbind(Agg_data,derived_variable[,c(2:6)])
#  Thus now Agg_data has all the aggregated columns per Cust_id
#
##########################################################################
#
#  Now  let us check for missing values -- expected in weekend spend
#  and in weekday spend 
######################################
#
# This tells us how  many NA values in weekend_spend 
length(which(is.na(Agg_data$weekend_spend)))

# This tells us how many NA values in weekday_spend 
length(which(is.na(Agg_data$weekday_spend)))

###  Now to replace NA values in these two columns with zeros 
# 

#  Replace NA with zeros in weekday_spend 
Agg_data[is.na(Agg_data$weekday_spend),"weekday_spend"]<-0

# Replace NA with zeros in weekend_spend 
Agg_data[is.na(Agg_data$weekend_spend),"weekend_spend"]<-0


############  Let us Check for NA values once more now 

# This tells us how  many NA values in weekend_spend 
length(which(is.na(Agg_data$weekend_spend)))

# This tells us how many NA values in weekday_spend 
length(which(is.na(Agg_data$weekday_spend)))

# ################  DATA AGGREGATION PROCESS is COMPLETE ######
###################################################################################
###################################################################################
#
#                              DATA PLOTTING STARTS 
#   We  Use ggplot     in R for plotx 
#           
###################################################################################
#   Get  visual insight on how the Frequency of visit varies 
#   We plot a histogram in ggplot, - we need only to supply the x vector in this case
#   it is the column "Freq"  in the dataframe  Agg_data  
###
#  The aes keyword refers to aesthetics - that which is seen in the plot 
#
    ggplot(Agg_data,aes(x = Freq)) +
    geom_histogram()               +
    xlab("Visits")                 +
    ylab("Count")              +
    ggtitle("Distribution of Number of Visits") +
    theme(plot.title = element_text(lineheight=.8, face="bold"))  +
    xlim(0,100)  +
   theme(axis.title.x = element_text(size = rel(1.4)),axis.text.x=element_text(size=rel(1.5)))
#
######
#   From the Plot We find majority of the customers visit 25 times or less = lets find out 
#   

  LT25cnt  <- nrow(subset(Agg_data,Freq <= 25,select="Cust_id"))
  Tot_Cust <- nrow(Agg_data)
  LT25perc <- 100*(LT25cnt/Tot_Cust)
# Display percentage of customers with visit count LT 25 
  LT25perc
#
################  
#  We find 83.2 percent of customers have <= 25 visits 
#
########################################################################################
#  Analysis of Total spend through plots 
#
  ggplot(Agg_data,aes(Monetary))                 +
  geom_histogram(color="white",fill="blue")      +
  xlab("Monetary")                               +
  ylab("Count")                                  +
  ggtitle("Distribution of Monetary")            +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  xlim(100,8000)                                 +
  theme(axis.title.x = element_text(size = rel(1.4)),axis.text.x=element_text(size=rel(1.5)))
  
  
#  We need to get the quantile figures to get a better hold of the numbers 
   quantile(Agg_data$Monetary,seq(0,1,.1))
  
# Observation :  60% of the customers spend is less than 2500
#
######################################################################
#  
#  Let us now analyse recency of visit 
#
   ggplot(Agg_data,aes(Recency))               +
   geom_histogram(color="white",fill="blue")   +
   xlab("Recency")                             +
   ylab("Count")                               +
   ggtitle("Distribution of Recency")           +
   theme(plot.title = element_text(lineheight=.8,face="bold"))  +
   theme(axis.title.x = element_text(size = rel(1.4)),axis.text.x=element_text(size=rel(1.5)))
#
#   Let us do a Quartile analysis of recency 
#
   quantile(Agg_data$Recency,seq(0,1,.1))
##  Observation 
#   70% of customers have visited with 22 days 
##
#
   
# ***********    Correlation Analysis    ******************************************************************
# Let us quikcly have a snapshot of Aggregate Data as of now 
#
  head(Agg_data)
  
# We can neglect columns 1 an 10 . Let us do a Correlation study on the rest of the columns 
# 
  # needs library corrplot ... 
  correlation<-cor(Agg_data[,c(-1,-10)])
  corrplot(correlation)
  
##  We can now find from plot the Highly Correlated variables
#*
#  Highly correlated variable are:
#  Freq and weekday_visits 
# Monetary and weekday_spend 
  
  
#  We can choose to keep any one of the variable from each pair of highly correlated variables.  
#   Thus we use the correlation plot to simplify our variable count for Clustering Analysis
#   
  
# * ******************    End of plot Based Analysis *******************************************************
#*
# **********************************************************************************************************
#                     Segmentation Analysis Using K means CLustering   
# **********************************************************************************************************
#  We  Now Eliminate The Cust_Id, Age bracket,   Weekday spend and Weekday_visit from our Data set for Clustering Analysis 
# * 
   Reduced_data <- Agg_data[,c(-1,-6,-8,-10)]
  
#*  Let us now scale the Remaining data columns by subracting columns means from corresonding columns    and  dividing  by sd 
   
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
   
 #  We observe from the plot that k = 4 seems an optimal choice of clustering parameter 
   
#  Let us now Generate the clustering vector with k = 4
   
   # First convert Scale_data to data frame 
Final_Agg_Data <- as.data.frame(Scaled_data)
   
#   Execute the k means algorithm for 4 cluster seggregation 
Xk4 <- kmeans(Final_Agg_Data,4)
  
# Obtain the clustering vector 
  Kcluster <- Xk4$cluster
  
# Now include the clustering vector to the Aggregate Data
  Agg_data$KVector <- Kcluster 
  
# Now let us look at a snapshot of the Agg_data 
  head(Agg_data)
   
  
  
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