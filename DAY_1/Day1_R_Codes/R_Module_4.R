# ***********   Module 3  *********** #
#----------------------------------------------
# clear all existing variables from console
 cat("\014") 
# clear all data variables 
rm(list=ls())

#--------------------------------------------------------------
#  Slicing Vectors
#--------------------------------------------------------------
# Define the vector 
vec1 <- c(12,4,4,6,9,3)

# extract the third element 
third_elem <- vec1[3]    

# display the third element 
third_elem



#--------------------------------------------------------------
#  Slicing Matrices
#--------------------------------------------------------------

# Define a 2X3 Matrix 
A <- matrix(c(2, 4, 3, 1, 5, 7), nrow=2,ncol=3,byrow=TRUE)

# display the matrix
A

# Extracting element at 2nd row, 3rd column
A23 <- A[2, 3]      

# Extracting a Vector (2nd row) from the Matrix 
ARow2Vec <-  A[2, ]     

# Extracting a Sub Matrix from the given Matrix 
A2by2 <- A[1:2,1:2]

#-----------------------------------------------------------
#  Slicing Data frames 
#-----------------------------------------------------------

Slicing Data Frames 
#-------------------------------------------------

#  Data Frames 
#Create a dataframe from vectors
n  <-  c(2,3,5)
s  <-  c("aa","bb","cc")
b  <-  c(TRUE,FALSE,TRUE)

df <-  data.frame(n,s,b)   #df is a data frame

#Display Dataframe
df

# Extract element in 1st row and 2nd col 
df[1,2]

# Extract the 1st row 
df[1,]

# Extract the 2nd col
df[,2]

# ---------------------------------
# Subsetting   of Data frames
#----------------------------------

# Using the Subset Feature 

# Create Building Blocks
Make  <- c("Toyota", "Hyundai","Maruti","Toyota","Hyundai","Maruti","Toyota","Hyundai","Maruti")
Model <- c("Corolla","i20","Alto","Camry","i10","Baleno","Fortuner","Sonata","Astra")
Sales <- c(345,800,2000,150,200,350,50,68,120)

# Create data frame 
CarData <- data.frame(Make,Model,Sales)

# Display Data Frame 
CarData


# Choose car Make and Model, Sales  for sales > 300 and list in Descending Order 

#TopSales <- subset(CarData,Sales>500,select=c(Make,Model,Sales))
TopSales <- subset(CarData,Sales>300)
Sorted_TopSales <- TopSales[order(-TopSales$Sales),]

# Display Sorted Data
Sorted_TopSales
#-------------------------
# Using the Which function   to subset 
#------------------------

# Get Index/ Row  positions meeting any particular criteria 
IDX_POS <- which(CarData$Sales>300)

# Get the Subset using Index positions 
CarSale_GT_300 <- CarData[IDX_POS,]

#Display Subset
CarSale_GT_300

# Get the Rows for which Sales are LT 300
CarSale_LT_300 <- CarData[-IDX_POS,]

# Display Subset
CarSale_LT_300

#Get the Model Name for the Max Sale Value 
Max_Sale_Model <- CarData$Model[which.max(CarData$Sales)]

# Display Max Sale Model 
Max_Sale_Model


# ------------   Important Verbs in DPLYR Package ---------------------------
# install.packages("dplyr")
library(dplyr)
#----------------------------------
# The <select>  verb 

#  selecting models column from CarData
  Car_Models <- select(CarData,Model)
   
# Dispay Models Column 
  Car_Models

#--------------------------------
# The <filter> verb 

# select rows which have sales > 500   
  Sales_GT_500 <- filter(CarData, Sales > 500) 
  
#Display 
  Sales_GT_500
  
# --------------------------------
# The <arrange> verb 
  
# Sort CarData in descending order of highest sales 
  Sorted_CarData <-  arrange(CarData,desc(Sales))  
  
# Display   
  Sorted_CarData

#--------------------------------
# The <mutate> verb 
  
# Create  new column - Perc_Tot_Sales
  CarData<-mutate(CarData,Perc_Tot_Sales = (100*Sales)/(sum(Sales)))    
  
# Display 
  CarData
  
#-------------------------------
# The <summarise> verb 
  
# Get the summary Statistics 
  Sale_Summary <- summarise(CarData,avg_sales = mean(Sales),
            min_sales = min(Sales),
            max_sales = max(Sales),
            total_Sales = sum(Sales))
  
# Display 
  Sale_Summary 
  
#--------------------------------
# The <group_by > verb 
  
# Get summary statistics average sales by car manufacturer  
  Mf_Avg_Sales <- CarData %>% group_by(Make) %>% summarise(Avg_Sales=mean(Sales))
  
# Display 
  Mf_Avg_Sales
  
#------------------------------------------------------------------------

#---------------   Convert between data types ---------------------------

# The example below shows three vectors converted to 3 data frames 

df1 <- as.data.frame(c("Anish","John","Kumar"))
df2 <- as.data.frame(c("Anish2","John2","Kumar2"))
df3 <- as.data.frame(c("Anish3","John3","Kumar3"))


# Convert  Data frames to List 

#  Define List - with 3 Elements 
my.list <- vector("list", 3)



# Populate List with data frames 
my.list[[1]] <- list(df1)
my.list[[2]] <- list(df2)
my.list[[3]] <- list(df3)


# Display List
my.list

# Now convert List to dataframe 
Finaldf <- as.data.frame(my.list)

# Set Column Names 
names(Finaldf) <- c("Week1","Week2","Week3")

# Display Final df
Finaldf
#----------   End of Module 3 ---------------------------------------------#

