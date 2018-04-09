# -------------------------------------------------
# #  Practice Code for   Plots 
# -------------------------------------------------
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())


# ------------------------------------------------------
#   Just Some Basics 
#-------------------------------------------------------


# Create a Vector of 100 points between 1 and 10 
X <- seq(1, 10, length.out= 100)

#  Create Y Values as Sin X + COS X   
Y <- sin(X) + cos(X)

X
Y
# Just a  Basic plot 
plot(X, Y, main = "Plot of sin(x) + cos(x)")


# a Juiced up version 
plot(X, Y, main = "Plot of sin(x) + cos(x)", xlab = "x values", ylab = "sin(x) + cos(x)", type = "h", lwd = 2.5)


#--------------------------------------------------------
# Plotting Scatter Data and fitting Linear Regression Line 
#--------------------------------------------------------

# Read the data file 
Data<- read.csv(file='D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_1\\2_R_Fundamentals\\Module_3\\HR_1N.csv', header=T, sep=',')

Data
Month  <- Data[,2]
POS    <- Data[,3]

Month

plot(Month, POS, main="POS Sum Qty - Monthly", 
     xlab="Month", ylab="POS Sum Qty", pch=19)

# Simple Regression 
POS.simple.reg <- lm(POS~Month)
POS.simple.reg
# Get Linear Model coeff
coeffs = coefficients(POS.simple.reg); 
# Plot Regression Line
abline(POS.simple.reg)

#--------------------------------------------------------
#  Plotting Histograms 
#--------------------------------------------------------


#  get some Normally Distributed Data with Zero Mean and Std = 5 
x <- rnorm(100, mean = 0, sd = 5)

# Round Off the Data 
Xrounded <- floor(x)

# Generate a Simple Histogram 
hist(Xrounded)



hist(Xrounded, 
     main="Histogram of Normally Dist. data",   
     xlab="100 Normally distributed Random Nos with Mean=0 Std=5", 
     border="black", 
     col="red",
     breaks=5)

#---------------------------------------------------------
#   Pie Charts 
#--------------------------------------------------------

# Read Bike Sales file 
BikeSales<- read.csv(file='D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_1\\2_R_Fundamentals\\Module_3\\BikeSales.csv', header=T, sep=',')


# Pie chart 
pie(BikeSales[, 2], labels = BikeSales[, 1])


# Get Percentages
Xdata <- BikeSales[,2]
piepercent<- round(100*Xdata/sum(Xdata), 1)

# Plot the chart.
pie(Xdata, labels = piepercent, main = "Bike Sales July 2016 ",col = rainbow(length(Xdata)))

# Provide Legend
LegendData <-  BikeSales[,1]
legend("topright", legend=LegendData, cex = 0.8,fill = rainbow(length(Xdata)))


#---------------------------------------------------------
#  Box Plots 
#--------------------------------------------------------


# Create a Population of 5 Sets with Mean = 5 and stdev increasing  as 0.25, 0.5, 0.75, 1.0, 1.25 

     Pop1 <-data.frame(Set1=rnorm(100,mean=5,sd=0.25),
                       Set2=rnorm(100,mean=5,sd=0.5),
                       Set3=rnorm(100,mean=5,sd=0.75),
                       Set4=rnorm(100,mean=5,sd=1.0),
                       Set5=rnorm(100,mean=5,sd=1.25))
                       
# Create a Box Plot 
  boxplot(Pop1)

# Create a Population of 5 Sets with Mean = 5,6,7,8, 9 and stdev = 0.25 

Pop2 <-data.frame(Set1=rnorm(100,mean=5,sd=0.25),
                  Set2=rnorm(100,mean=6,sd=0.25),
                  Set3=rnorm(100,mean=7,sd=0.25),
                  Set4=rnorm(100,mean=8,sd=0.25),
                  Set5=rnorm(100,mean=9,sd=0.25))
  
# Create a Box Plot 
  boxplot(Pop2)
  
  
  