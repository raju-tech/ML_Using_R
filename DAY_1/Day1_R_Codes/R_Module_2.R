# ***********   Module 2    *********** #
#----------------------------------------------
# clear all existing variables from console
 cat("\014") 
# clear all data variables 
rm(list=ls())
#---------------------------------------------------
#  FILE I/O 
# --------------------------------------------------
# Reporting Car data for Models haveing Sales > 500 
#---------------------------------------------------


# Read the Car Sales Csv file 
Car_Data<-read.csv(file="CarSales.csv")

# Display  car sales data  
Car_Data

# Subset- Choose car Make and Model, Sales  for sales > 300 
TopSales <- subset(Car_Data,Sales>300)

# and list in Descending Order 
Sorted_TopSales <- TopSales[order(-TopSales$Sales),]

# Display Sorted Data
Sorted_TopSales

# Write output file 
write.table(Sorted_TopSales,file="Sorted_Top_Sales.csv", sep=",",row.names=FALSE)
#------------------------------------------------------------------
#  Misc. Control Functions 
#-----------------------------------------------------------------
#-----------------------
# Conditional functions
#-----------------------
# Create a vector of 5 numbers 
Vec1<-c(15,2,32,45,50)

# Return 1 if value is 2 return 0 otherwise 
ifelse(Vec1==2,1,0)

# Check for odd and even  

 TestVec = c(5,9,2,3)
 ResultVec <- ifelse(TestVec %% 2 == 0,"even","odd")
 # Dispay Result Vector
 ResultVec

#----------------------
# Concatenation
#----------------------

paste("a","b")

paste("Hello","World",sep="_")


String1<-c("hello","world")
paste(String1)

paste(String1,collapse=" ")

paste(String1,collapse="")

#---------------------------
# Checking for null
#--------------------------
# Create a vector with NULL 
Vec1 <-c(1,2,3,NA,4,5,NA,NA)

# Check for  NULL
is.na(Vec1)

# Copy non Null values to another vector
No_Null_Vec <- Vec1[!is.na(Vec1)]

# Display No Null Vec
No_Null_Vec


# Replace Null with zeros in the original Vector
Vec1[is.na(Vec1)] <-0

#Display after replace 
Vec1

#---------------------------------------------------------------------------
# Merge dataframes
#---------------------------------------------------------------------------

# Create Database With same col name for the Key Column
Cust_Data   <- data.frame(Cust_ID= c(1,2,3,4,5), Name = c("Vinod","Ramesh","Ashish","Venkat","Raj"))
Sales_Data  <- data.frame(Cust_ID = c(1,2,3,4,5), Purchase  = c(450,60,75,83,92))

# Merge Customer and sales Data 
Merged_Data <- merge(Cust_Data, Sales_Data, by.Cust_Data = "Cust_ID",by.Sales_Data="Cust_Number")

# Display Merged Data 
Merged_Data



#-----------------------------------------------------------------------------
# Load and save
ls()
rm(list=ls())                        # clean up
y.vector = runif(20)                 # a vector of 20 random numbers
ls()
save(y.vector, file="yvec.saved")    # save to the working directory
rm(y.vector)                         # removed
ls()
load("yvec.saved")                   # loaded back in
class(ls())
x<-"a"
rm(list=c("x","y.vector"))

#---------------------------------------------------------------------------
#---  One way and Two way tables 
#---------------------------------------------------------------------------

# Create Gender Vector 
Gender<-c("male","male","female","male","female")
# Create Marital Status Vector 
Mstatus<-c("Married","Single","Married","Single","Married")

# One way tabulation 
table(Gender)

# Two way tabulation 
table(Gender,Mstatus)

#-------------------------------------------------------------------------
#  LOOPS 
#-------------------------------------------------------------------------
# For loop

for(i in 1:5){
  print(i)
}

for(j in 1:5){
  j
}

# Display 'j' outside loop
j


i<-1:5

for(j in i){
  print(j)
}

#---------------------------
# While loop
#---------------------------

x<-0
while(x <5){ 
  print( x<-x+1) 
}

#--------------------------------------
# Repeat - similar to a do-while loop
#-------------------------------------
y<-0
repeat{
  print(y<-y+1)
  if(y>5){
    break
  }
}

#-----------------------
# USER DEFINED FUNCTIONS 
#-----------------------
# Writing a user defined function

# Function Definition without arguements 
Hello_World<-function(){
  print("Hello World")
  return()
}

# Function Call 
Hello_World()


# Function with arguements 
Sum_Two_Nos<-function(a,b){
  sum2 <- a + b 
  print(paste("Sum is ",sum2)) 
}

# Function Call 
ReturnVal <-Sum_Two_Nos(5,10)

# - Multiple Input Values , Multiple Output 
# Function definition 
Compute_mean_std_Tot<-function(x,y){
  Tot<- x + y 
  mu<-mean(x+y)
  sigma<-sd(x+y)
  return(list(mean=mu,stand_dev=sigma,Totals=Tot))
}

# Prepare Data 
X<-c(1,2,3,4,5)
Y<-c(10,10,10,10,10)

# Function Call
Compute_mean_std_Tot(X,Y)
#-------------------  END    of MODULE 2  #------------------------------------------------
  

