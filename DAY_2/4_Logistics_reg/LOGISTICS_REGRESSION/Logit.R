# -------------------------------------------------
# #  Practice Code for   Logistics  Regression
# -------------------------------------------------
# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list=ls())


# Need to Install this pakage 
# install.packages("data.table")

#########################################
# Classifying with Linear Regression
#########################################

# Generate 200 random numbers with mean 15 and sd 15 
set.seed(8967563)
no_funding = rnorm(200, 15,15)

# Ensure negatives are replaced by zeros 
no_funding = sapply(no_funding,function(x) max(x,0))

# Bind a column of 200 zeros and create a df 
no_funding_df = cbind(0,no_funding)

# Generate 120  random numbers with mean 50 and sd 15 
funding = rnorm(120,50,15)

# Ensure negatives are replaced by zeros 
funding = sapply(funding,function(x) max(x,0))

# Bind a column of ones  and create a df 
funding_df = cbind(1,funding)

#  Merge the rows  
funding_ds = rbind(funding_df, no_funding_df)

# Create a final  df 
fdata = data.frame(funding_ds)

# Provide col names 
names(fdata)=c("y","x1")

# Jumble up the rows randomly 
fdata = fdata[sample(nrow(fdata)),]

# Reset the row numbers which got scrambled 
library(data.table)
(setattr(fdata, "row.names", 1:nrow(fdata)))
head(fdata)

# fit linear model Y regressed on Data 
myfit = lm(y~x1, data=fdata)

# Find X Value at which Y value i 0,5
int0.5 <- (0.5 - coef(myfit)[1]) / coef(myfit)[2]

library(ggplot2)
# Plot scatter 
p <- qplot(x1,y,data=fdata) 
p <- p + ggtitle("Classifying with Simple Linear Regression")  
# Plot Regression line 
p <- p + geom_abline(intercept=coef(myfit)[1],slope=coef(myfit)[2],  size=1.2, show_guide=T) 
# Plot Vertical line at 0.5 Intercept 
p <- p + geom_vline(xintercept = int0.5, linetype = "longdash") 
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold"))
p


# Generate New data with two outliers 
skewed_fdata=rbind(fdata,c(1,120),c(1,150))
myfit2 = lm(y~x1, data=skewed_fdata)
new_int0.5 <- (0.5 - coef(myfit2)[1]) / coef(myfit2)[2]


# Plot New Result with the Outliers 
p2 <- qplot(x1,y,data=skewed_fdata) 
p2 <- p2 + ggtitle("Classifying with Simple Linear Regression")  
p2 <- p2 + geom_abline(intercept=coef(myfit2)[1],slope=coef(myfit2)[2],  size=1.2, show_guide=T, linetype = "dashed") 
p2 <- p2 + geom_abline(intercept=coef(myfit)[1],slope=coef(myfit)[2],  size=1.2, show_guide=T) 
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold"))
p2


#########################################
# Heart Dataset
#########################################

# Read the Heart Data file 
heart <- read.table("D:\\Raju_work\\Training\\R and R\\CII_Second_WSHOP_Sep_2017\\DAY_2\\4_Logistics_reg\\LOGISTICS_REGRESSION\\heart.dat")


# Check the data frame read 
head(heart)

# Add Relevant Col names 
names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")


#----------------------------------------
# Handling Categorical Variables 
#----------------------------------------

# Note R will Create Dummy Variables 

# Note the Below are Categorical variables which need to be set up as factors 

# Thus setting them up as factors 
heart$CHESTPAIN = factor(heart$CHESTPAIN)
heart$ECG = factor(heart$ECG)
heart$THAL = factor(heart$THAL)
heart$EXERCISE = factor(heart$EXERCISE)


# Re set the Output column as a 0 or 1 column 
# class 2 corresponds to heart disease - converting to 0 1 format 
heart$OUTPUT = heart$OUTPUT-1


# look at the heart data set after modification 
head(heart)

# Write the data to csv format 
write.table(heart, file="D:\\Heart_Data_raj.csv", sep=",", row.names=FALSE, col.names=TRUE, append=FALSE)


# Let us Randomly Partition data into train and test set 85: 15 

# Create a Partition 85: 15  Train Set:Test Set 
library(caret)
set.seed(987954)

# Create Sampling Vector 
heart_sampling_vector <- createDataPartition(heart$OUTPUT, p = 0.85, list = FALSE)

head(heart_sampling_vector)


# Note the Sampling Vector is an array of Row Positions 

# Create Train Set 
heart_train <- heart[heart_sampling_vector,]

# Extract Train labels 
heart_train_labels <- heart$OUTPUT[heart_sampling_vector]

# Create Test Data Set 
heart_test <- heart[-heart_sampling_vector,]

head(heart_test)

# Create Test Labels 
heart_test_labels <- heart$OUTPUT[-heart_sampling_vector]


# Train Model with Train Data Set 

# Create Logit Model 
heart_model = glm(OUTPUT~.,data=heart_train,family=binomial("logit"))

# Display Summary 
summary(heart_model)

# Create a Logit Model with only Age and Check Model Summary 
heart_model2 = glm(OUTPUT~AGE,data=heart_train,family=binomial("logit"))
summary(heart_model2)

#########################################
# Assessing logistic regression
#########################################


# Define functions for computing Log Likelihood and Deviance 

# Compute the log likelihoods of a vector of individual observations
log_likelihoods <- function(y_labels,y_probs) {
  y_a = as.numeric(y_labels)
  y_p = as.numeric(y_probs)
  y_a * log(y_p) + (1 - y_a) * log(1 - y_p)
}

# Compute the log likelihood of the entire data set
dataset_log_likelihood <- function(y_labels,y_probs) {
  sum(log_likelihoods(y_labels,y_probs))
}

# Compute the deviances of a the individual observations
deviances <- function(y_labels,y_probs) {
  -2*log_likelihoods(y_labels,y_probs)
}

# Compute the deviance of the entire data set
dataset_deviance <- function(y_labels, y_probs) {
  sum(deviances(y_labels,y_probs))
}

# Compute the deviance of a model
model_deviance <- function(model, data, output_column) {
  y_labels = data[[output_column]]
  y_probs = predict(model, newdata=data, type = "response")
  dataset_deviance(y_labels, y_probs)
}

# Note in Computing NULL Deviance the ULL prob is proportion of 1's in the full set of the Output column 
# Compute the null deviance of data
null_deviance <- function(data, output_column) {
  y_labels = data[[output_column]]
  y_probs = mean(data[[output_column]])
  dataset_deviance(y_labels, y_probs)
}

# Compute Pseudo r Squared in Analogous fashion to Linear Regression 
model_pseudo_r_squared <- function(model, data, output_column) {
  1 - (model_deviance(model, data, output_column)/null_deviance(data, output_column))
}

model_pseudo_r_squared(heart_model,data=heart_train,output_column="OUTPUT")


# Compute p value based on difference in deviance between model and null 
model_chi_squared_p_value <-  function(model, data, output_column) {
  null_df = nrow(data) - 1
  model_df = nrow(data) - length(model$coefficients)
  difference_df = null_df - model_df
  null_deviance = null_deviance(data, output_column)
  m_deviance = model_deviance(model, data, output_column)
  difference_deviance = null_deviance - m_deviance
  pchisq(difference_deviance, difference_df,lower.tail=F)
}

# Compute the model residuals
model_deviance_residuals <- function(model, data, output_column) {
  y_labels = data[[output_column]]
  y_probs = predict(model, newdata=data, type = "response")
  residual_sign = sign(y_labels - y_probs)
  residuals = sqrt(deviances(y_labels,y_probs))
  residual_sign*residuals
}

#########################################
# Test Set Performance
#########################################
train_predictions = predict(heart_model, newdata=heart_train, type="response")
train_class_predictions = as.numeric(train_predictions>0.5)
mean(train_class_predictions==heart_train$OUTPUT)
test_predictions = predict(heart_model, newdata=heart_test, type="response")
test_class_predictions = as.numeric(test_predictions>0.5)
mean(test_class_predictions==heart_test$OUTPUT)


#########################################
#  Capture Results in Data Frame 
#########################################
# Add Predicted Column to heart_test Output 
heart_test_Out <- heart_test
heart_test_Out$predicted <- test_class_predictions

# Write the data to csv format 
write.table(heart_test_Out, file="D:\\Heart_Data_Out.csv", sep=",", row.names=FALSE, col.names=TRUE, append=FALSE)




#
# ---------------------------------------------------------------------------------
# Model Improvement  - Out of Scope of This Workshop 
# ---------------------------------------------------------------------------------

